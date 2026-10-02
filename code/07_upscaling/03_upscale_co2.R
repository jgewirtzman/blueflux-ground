# =============================================================================
# Upscale CO2 (ecosystem respiration) to TLS Plot-Level Estimates
# =============================================================================
# Companion to upscale_methane_to_plots.R. Builds a bottom-up CO2 budget:
#
#   Reco_bottomup = R_soil + R_wood(trunk+branch) + R_root + R_water + R_leaf_canopy
#   NEE = Reco_bottomup - GPP
#
# Key differences from the CH4 script (see output/co2_budget_plan.md):
#   - STEM: CO2 has NO stem-height effect (p=0.89), so stems scale with a
#     CONSTANT flux (mean CO2 flux x trunk+branch SA). No exp/zero scenarios,
#     no ebullition.
#   - LEAF: foliar respiration is a LITERATURE term (TLS has no foliage SA;
#     leaf chambers were transparent/in-light = net, can't isolate Rd). Scaled
#     via Heskel(2016) T-correction driven by tower TA, species-weighted Rd25,
#     and an LAI range (GPP_literature_outputs/). Ghost = 0 (no canopy).
#   - GPP/NEE: tower (US-Skr / SRS-6) for healthy; ghost GPP ~ 0 -> NEE ~ Reco.
#   - Do NOT force closure to tower Reco (lateral DIC export; lit C-08).
#
# Units: work in areal umol CO2 m-2 ground s-1 throughout (directly comparable
# to the tower), convert to g CO2 m-2 yr-1 for forcing.
# =============================================================================

library(MASS)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(scales)

set.seed(42)

# --- Paths -------------------------------------------------------------------
project_dir <- here::here()
tls_dir     <- Sys.getenv("BLUEFLUX_TLS_DIR", "data/tls")
gpp_file    <- file.path(project_dir, "output", "gpp",
                         "US-Skr_GPP_halfhourly_Mar2022_Oct2022_Mar2023.csv")
output_dir  <- file.path(project_dir, "output", "upscaling")
fig_dir     <- file.path(project_dir, "output", "figures", "other")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

theme_pub <- function(base_size = 11) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      axis.title = element_text(size = base_size, face = "bold"),
      axis.text = element_text(size = base_size - 1),
      strip.text = element_text(size = base_size - 1, face = "bold"),
      strip.background = element_rect(fill = "grey95", color = "grey70"),
      legend.title = element_text(size = base_size - 1, face = "bold"),
      legend.text = element_text(size = base_size - 2),
      panel.grid.minor = element_blank(),
      plot.tag = element_text(size = base_size + 3, face = "bold")
    )
}
save_pub <- function(p, name, w, h, u = "mm") {
  ggsave(file.path(fig_dir, paste0(name, ".pdf")), p, width = w, height = h, units = u)
  ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = w, height = h, units = u, dpi = 300)
  cat("  Saved:", name, "\n")
}

MW_CO2  <- 44.01
# areal umol m-2 s-1 -> g CO2 m-2 yr-1
umol_to_g_yr <- MW_CO2 * 1e-6 * 86400 * 365

# =============================================================================
# 1. TLS surface areas (identical to CH4 script)
# =============================================================================
cat("=== 1. Loading TLS data ===\n")
tls_all <- read.csv(file.path(tls_dir, "all_sites_summary.csv")) %>%
  select(site, segment_class, height_bin, height_bin_num,
         Total_volume_m3, Total_surface_area_m2)
tree_stats <- read.csv(file.path(tls_dir, "tree_stats_per_site.csv"))

ground_xsec <- tls_all %>%
  filter(height_bin_num == 0, segment_class %in% c("trunk", "root")) %>%
  group_by(site) %>%
  summarise(ground_xsec_m2 = sum(Total_volume_m3, na.rm = TRUE) / 0.5, .groups = "drop")

tree_stats <- tree_stats %>%
  left_join(ground_xsec, by = "site") %>%
  mutate(ground_area_m2 = area_m2 - ground_xsec_m2)

site_meta <- data.frame(
  site = c("CP40", "FLM30", "SRS5", "SRS6"),
  disturbance_level = c("ghost", "ghost", "healthy", "healthy"),
  stringsAsFactors = FALSE
)
tls_sites <- c("CP40", "FLM30", "SRS5", "SRS6")
campaigns <- c("Oct 2022", "Mar 2023")

# Stem SA = trunk + branch (branch respiration captured here)
tls_stem_total <- tls_all %>%
  filter(segment_class %in% c("trunk", "branch")) %>%
  group_by(site) %>%
  summarise(total_stem_SA_m2 = sum(Total_surface_area_m2, na.rm = TRUE), .groups = "drop")

tls_root_sa <- tls_all %>%
  filter(segment_class == "root") %>%
  group_by(site) %>%
  summarise(root_SA_m2 = sum(Total_surface_area_m2, na.rm = TRUE), .groups = "drop")

cat("TLS surface areas:\n")
tree_stats %>% select(site, area_m2, ground_area_m2) %>%
  left_join(tls_stem_total, by = "site") %>%
  left_join(tls_root_sa, by = "site") %>% as.data.frame() %>% print()

# =============================================================================
# 2. Load chamber flux data; bootstrap CO2 component means
# =============================================================================
cat("\n=== 2. Loading flux data (CO2) ===\n")
flux_raw <- read.csv(file.path(project_dir, "output", "data_products", "combined_gas_flux_dataset.csv")) %>%
  filter(plot %in% tls_sites) %>%
  mutate(
    disturbance_level = site_meta$disturbance_level[match(plot, site_meta$site)],
    campaign = factor(month_year, levels = c("2022-10", "2023-03"), labels = campaigns)
  ) %>%
  filter(!is.na(campaign))

boot_mean <- function(x, R = 5000) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(data.frame(n = 0L, mean = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_))
  if (length(x) == 1) return(data.frame(n = 1L, mean = x, ci_lo = NA_real_, ci_hi = NA_real_))
  bm <- replicate(R, mean(sample(x, replace = TRUE)))
  data.frame(n = length(x), mean = mean(bm),
             ci_lo = quantile(bm, 0.025), ci_hi = quantile(bm, 0.975))
}

# Non-stem components (soil, water, root, cwd)
nonstem_boot <- flux_raw %>%
  filter(component %in% c("soil", "water", "root", "cwd"), !is.na(CO2_best.flux)) %>%
  group_by(plot, campaign, component) %>%
  do(boot_mean(.$CO2_best.flux)) %>% ungroup()

# Stem: CONSTANT flux (no height model) -> bootstrap site x campaign mean
stem_boot <- flux_raw %>%
  filter(component == "stem", !is.na(CO2_best.flux)) %>%
  group_by(plot, campaign) %>%
  do(boot_mean(.$CO2_best.flux)) %>% ungroup() %>%
  mutate(component = "stem")

cat("Stem CO2 (constant) bootstrap:\n")
stem_boot %>% select(plot, campaign, n, mean, ci_lo, ci_hi) %>% as.data.frame() %>% print()

# =============================================================================
# 3. Targeted gap-filling (same scheme as CH4 script)
# =============================================================================
cat("\n=== 3. Targeted gap-filling ===\n")
get_site_flux <- function(boot_df, site_name, camp, comp) {
  row <- boot_df %>% filter(plot == site_name, campaign == camp, component == comp)
  if (nrow(row) > 0 && !is.na(row$mean[1])) {
    return(list(rate = row$mean[1], ci_lo = row$ci_lo[1], ci_hi = row$ci_hi[1],
                n = row$n[1], source = "site"))
  }
  NULL
}

# water CO2 from dissolved CO2 x k where no chamber water CO2 exists
# (code/03_fit/02_water_flux_from_dissolved.R), ahead of the SRS5 -> SRS6 gap-fill
water_est <- read.csv(file.path(project_dir, "output", "flux", "03_fit", "water_flux_estimates.csv"))
get_water_estimate <- function(site_name, camp) {
  row <- water_est[water_est$site == site_name & water_est$campaign == camp & water_est$gas == "CO2", ]
  if (nrow(row) == 0) return(NULL)
  list(rate = row$flux_rate[1], ci_lo = row$ci_lo[1], ci_hi = row$ci_hi[1], n = 0L,
       source = "dissolved CO2 x k (code/03_fit/02_water_flux_from_dissolved.R)")
}

source(file.path(project_dir, "code", "00_lib", "ghost_floor.R"))
ghost_floor <- ghost_floor_setup(project_dir)     # ghost floor without standing water (Mar 2023)
flux_table <- list()
for (camp in campaigns) for (site_name in tls_sites) for (comp in c("root","soil","water","cwd","stem")) {
  bd <- if (comp == "stem") stem_boot else nonstem_boot
  fl <- get_site_flux(bd, site_name, camp, comp)
  if (is.null(fl) && comp == "water") fl <- get_water_estimate(site_name, camp)
  if (is.null(fl)) {
    if (comp == "root" && site_name == "CP40") {
      fl <- get_site_flux(nonstem_boot, "FLM30", camp, "root")
      if (is.null(fl)) fl <- get_site_flux(nonstem_boot, "FLM30", "Oct 2022", "root")
      if (!is.null(fl)) fl$source <- "gap: FLM30 root"
    } else if (comp == "root" && site_name == "FLM30" && camp == "Mar 2023") {
      fl <- get_site_flux(nonstem_boot, "FLM30", "Oct 2022", "root")
      if (!is.null(fl)) fl$source <- "gap: FLM30 root Oct 2022"
    } else if (comp == "water" && site_name == "SRS6" && camp == "Oct 2022") {
      fl <- get_site_flux(nonstem_boot, "SRS5", "Oct 2022", "water")
      if (!is.null(fl)) fl$source <- "gap: SRS5 water Oct 2022"
    } else if (comp == "soil" && site_name %in% c("CP40", "FLM30")) {
      fl <- ghost_floor$soil_flux("CO2")   # 00_lib/ghost_floor.R
    }
  }
  if (is.null(fl)) fl <- list(rate = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_, n = 0L, source = "missing")
  flux_table <- c(flux_table, list(data.frame(
    site = site_name, campaign = camp, component = comp,
    flux_rate = fl$rate, ci_lo = fl$ci_lo, ci_hi = fl$ci_hi,
    n_obs = fl$n, fill_source = fl$source, stringsAsFactors = FALSE)))
}
flux_table <- bind_rows(flux_table) %>% mutate(campaign = factor(campaign, levels = campaigns))
cat("CO2 flux rate table (umol m-2 s-1):\n")
flux_table %>% select(site, campaign, component, flux_rate, n_obs, fill_source) %>%
  as.data.frame() %>% print()

# =============================================================================
# 4. Leaf canopy respiration term (literature; GPP_literature_outputs/)
# =============================================================================
cat("\n=== 4. Leaf canopy respiration (literature) ===\n")

# Species Rd25 (umol m-2 leaf s-1) at 25C: LR-01 R.mangle 1.62; LR-02 A.germinans 1.28-1.54.
# STOPGAP species weight (R.mangle-dominant riverine SRS-6) pending FCE LTER inventory.
Rd25_central <- 1.55          # R.mangle-weighted central
Rd25_lo      <- 1.28          # A.germinans low
Rd25_hi      <- 1.62          # R.mangle (Barr 2009 at-site)
LAI_central  <- 2.8           # SRS-6 ground LAI (Barr, unpubl., in Troxler et al. 2015)
LAI_lo       <- 2.3           # LAI-01 ground optical (lower)
LAI_hi       <- 5.55          # MODIS at US-Skr (Reed et al. 2025; 24-day maxima, reads high)
# Canopy LAI recovered within ~1 yr of Wilma and Irma (Reed et al. 2025), so no
# post-hurricane reduction is applied for 2022-23.

# Beer's-law canopy integration (C-07): leaf respiratory capacity scales with light
# through the canopy, so total canopy R uses an EFFECTIVE LAI rather than flat Rd*LAI.
# effective_LAI = (1 - exp(-k*LAI)) / k ; k = canopy light-extinction coefficient.
K_EXT     <- 0.5
eff_LAI   <- function(L) (1 - exp(-K_EXT * L)) / K_EXT
LAIe_central <- eff_LAI(LAI_central)   # 2.8  -> ~1.51
LAIe_lo      <- eff_LAI(LAI_lo)
LAIe_hi      <- eff_LAI(LAI_hi)
F_INHIB_DAY  <- as.numeric(Sys.getenv("LEAF_INHIB", "0.30"))   # daytime light inhibition of leaf R
# (Kok effect; global mean ~30 %, Atkin et al. 2014; canopy range ~20-50 %, in the MC)

# Heskel et al. 2016 short-term T response (C-02)
f_T_heskel <- function(T_leaf) exp(0.1012 * (T_leaf - 25) - 0.0005 * (T_leaf^2 - 25^2))

# Tower TA half-hourly -> mean leaf respiration multiplier per campaign window.
gpp_raw <- read.csv(gpp_file)
# Match tower data to the chamber campaign months (Oct 2022 wet, Mar 2023 dry)
gpp_raw$campaign <- with(gpp_raw, dplyr::case_when(
  year == 2022 & month == 10 ~ "Oct 2022",
  year == 2023 & month == 3  ~ "Mar 2023",
  TRUE ~ NA_character_))
# is_day from SW_IN (model-filled column SW_IN_model used when SW_IN missing)
gpp_raw <- gpp_raw %>%
  mutate(sw = ifelse(!is.na(SW_IN) & SW_IN > -900, SW_IN, SW_IN_model),
         is_day = ifelse(!is.na(sw) & sw > 5, 1, 0),
         TA_use = ifelse(!is.na(TA) & TA > -900, TA, TA_model))

leaf_term <- gpp_raw %>%
  filter(campaign %in% campaigns) %>%
  group_by(campaign) %>%
  summarise(
    fT_mean      = mean(f_T_heskel(TA_use), na.rm = TRUE),
    fT_day       = mean(f_T_heskel(TA_use) * (1 - F_INHIB_DAY * is_day), na.rm = TRUE),
    TA_mean      = mean(TA_use, na.rm = TRUE),
    day_frac     = mean(is_day, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # R_leaf_canopy (umol m-2 ground s-1) = Rd25 * fT_day_adjusted * effective_LAI
  # (Beer's-law canopy integration, C-07): leaves deeper in canopy are shaded and
  # have lower respiratory capacity, so effective LAI < LAI.
  mutate(
    Rleaf_central = Rd25_central * fT_day * LAIe_central,
    Rleaf_lo      = Rd25_lo      * fT_day * LAIe_lo,
    Rleaf_hi      = Rd25_hi      * fT_day * LAIe_hi
  )
cat("Leaf canopy respiration (umol CO2 m-2 ground s-1), healthy canopy:\n")
leaf_term %>% mutate(across(where(is.numeric), ~round(.x,3))) %>% as.data.frame() %>% print()

# =============================================================================
# 4b. Time-of-day temperature correction of chamber respiration
# -----------------------------------------------------------------------------
# Chambers were run in daytime (mean ~13:00) and their fluxes stand for the
# whole day. Respiration components (stem, root, soil, CWD) are scaled from the
# temperatures at measurement (tower TA, = Tcham) to the campaign month's 24-h
# tower TA with an exponential (Q10) response:
#   factor = mean_24h(Q10^(T/10)) / mean_measurements(Q10^(T/10))
# Q10 (central) is the tower's within-month night-respiration Q10
# (01_tower_gpp.R; ~1.15), i.e. the response to day-night and day-to-day
# temperature swings, which is what this correction spans. Our stem chambers
# give a much steeper slope (log(CO2) ~ T + (1 | site x campaign), ~4.3),
# reported here for comparison: within a day stem CO2 efflux tracks sap flow and
# xylem CO2 transport as well as temperature, so it overstates the respiration
# response. Sensitivity: CO2_Q10 = <number> or "stem".
# Water CO2 (air-water gas exchange) and the leaf term (already 24-h) are not
# corrected. Q10, its CI and the factors: output/upscaling/co2_temperature_correction.csv.
# =============================================================================
cat("\n=== 4b. Time-of-day temperature correction ===\n")
q10_dat <- flux_raw %>% filter(component == "stem", CO2_best.flux > 0, !is.na(air_temp)) %>%
  mutate(grp = paste(plot, campaign))
q10_fit <- lme4::lmer(log(CO2_best.flux) ~ air_temp + (1 | grp), data = q10_dat)
stem_b <- lme4::fixef(q10_fit)[["air_temp"]]; stem_se <- sqrt(as.matrix(vcov(q10_fit))["air_temp", "air_temp"])
tower_q10 <- read.csv(file.path(project_dir, "output", "gpp", "US-Skr_Q10_within_month.csv"))
q10_src <- Sys.getenv("CO2_Q10", "tower")
if (q10_src == "tower") {
  q10_b <- log(tower_q10$Q10) / 10; q10_se <- (log(tower_q10$Q10_hi) - log(tower_q10$Q10_lo)) / (2 * 1.96 * 10)
} else if (q10_src == "stem") {
  q10_b <- stem_b; q10_se <- stem_se
} else {
  q10_b <- log(as.numeric(q10_src)) / 10; q10_se <- 0                                    # sensitivity override
}
Q10 <- exp(10 * q10_b)
f_q10 <- function(T) exp(q10_b * T)
T24 <- gpp_raw %>% filter(campaign %in% campaigns) %>% group_by(campaign) %>%
  summarise(T24_mean = mean(TA_use, na.rm = TRUE), f24 = mean(f_q10(TA_use), na.rm = TRUE), .groups = "drop")
resp_comps <- c("stem", "root", "soil", "cwd")
t_corr <- flux_raw %>% filter(component %in% resp_comps, !is.na(CO2_best.flux), !is.na(air_temp)) %>%
  group_by(campaign = as.character(campaign), component) %>%
  summarise(n = n(), T_meas_mean = mean(air_temp), f_meas = mean(f_q10(air_temp)), .groups = "drop") %>%
  left_join(T24 %>% mutate(campaign = as.character(campaign)), by = "campaign") %>%
  mutate(factor = f24 / f_meas, Q10 = Q10, Q10_lo = exp(10 * (q10_b - 1.96 * q10_se)), Q10_hi = exp(10 * (q10_b + 1.96 * q10_se)),
         Q10_source = q10_src, Q10_stem_chambers = exp(10 * stem_b), Q10_stem_n = nrow(q10_dat))
write.csv(t_corr, file.path(output_dir, "co2_temperature_correction.csv"), row.names = FALSE)
cat(sprintf("Q10 used (%s) = %.2f [%.2f-%.2f]; stem chambers (n = %d) = %.2f [%.2f-%.2f]\n", q10_src, Q10, t_corr$Q10_lo[1],
            t_corr$Q10_hi[1], nrow(q10_dat), exp(10 * stem_b), exp(10 * (stem_b - 1.96 * stem_se)), exp(10 * (stem_b + 1.96 * stem_se))))
print(as.data.frame(t_corr %>% select(campaign, component, n, T_meas_mean, T24_mean, factor) %>% mutate(across(where(is.numeric), ~ round(.x, 3)))))
flux_table <- flux_table %>% mutate(.c = as.character(campaign)) %>%
  left_join(t_corr %>% select(.c = campaign, component, .f = factor), by = c(".c", "component")) %>%
  mutate(.f = ifelse(component %in% resp_comps, coalesce(.f, 1), 1),
         flux_rate = flux_rate * .f, ci_lo = ci_lo * .f, ci_hi = ci_hi * .f, temp_factor = .f) %>%
  select(-.c, -.f)

# =============================================================================
# 5. Flooding / tide assignment (same as CH4)
# =============================================================================
assign_flood <- function(site, camp) {
  # ghost sites: share of the floor without standing water (00_lib/ghost_floor.R)
  if (site %in% c("CP40","FLM30")) { e <- ghost_floor$share(site, camp); return(c(water = 1 - e, soil = e)) }
  c(water = NA, soil = NA)
}
is_tidal <- function(site) site %in% c("SRS5", "SRS6")
# Downed CWD: Krauss et al. 2005 wood volume -> surface (Troxler et al. 2015),
# exposed to the air only above the water (code/00_lib/cwd_scaling.R)
source(file.path(project_dir, "code", "00_lib", "cwd_scaling.R"))
cwd_exposed <- cwd_exposure_setup(flux_raw)

# =============================================================================
# 6. Scale to plot level -> areal Reco (umol m-2 ground s-1)
# =============================================================================
cat("\n=== 6. Scaling CO2 to plot level ===\n")
source(file.path(project_dir, "code", "00_lib", "tide_weights.R"))
# above-water exposure of stems and prop roots (00_lib/exposure.R): bark below
# the water exchanges with the water, not the air; time-averaged over the
# water-depth samples of each site x campaign
source(file.path(project_dir, "code", "00_lib", "exposure.R"))
depth_samples <- depth_samples_setup(flux_raw, project_dir)
woody_bins <- tls_all %>% filter(segment_class %in% c("trunk", "branch", "root")) %>%
  mutate(cls = ifelse(segment_class == "root", "root", "stem")) %>%
  group_by(site, cls, height_bin_num) %>% summarise(SA = sum(Total_surface_area_m2, na.rm = TRUE), .groups = "drop")
exposed_SA <- function(site, camp, cl) { b <- woody_bins[woody_bins$site == site & woody_bins$cls == cl, ]
  w <- depth_samples(site, camp); sum(b$SA * sapply(b$height_bin_num, function(z) exposed_frac(z, z + 0.5, w))) }
results <- list()
for (camp in campaigns) {
  for (site_name in tls_sites) {
    ground_area <- tree_stats %>% filter(site == site_name) %>% pull(ground_area_m2)
    plot_area   <- tree_stats %>% filter(site == site_name) %>% pull(area_m2)
    root_sa     <- exposed_SA(site_name, camp, "root")     # above-water root surface (time-averaged)
    stem_sa_tot <- exposed_SA(site_name, camp, "stem")     # above-water trunk + branch surface
    if (length(root_sa) == 0) root_sa <- 0
    dist <- site_meta$disturbance_level[match(site_name, site_meta$site)]

    ft <- flux_table %>% filter(site == site_name, campaign == camp)
    rate <- function(cc) { v <- ft %>% filter(component == cc) %>% pull(flux_rate); if (length(v)==0) NA else v }
    stem_rate <- rate("stem"); root_rate <- rate("root")
    soil_rate <- rate("soil"); water_rate <- rate("water"); cwd_rate <- rate("cwd")

    # Total umol/s over plot
    stem_tot <- ifelse(!is.na(stem_rate), stem_rate * stem_sa_tot, 0)
    root_tot <- ifelse(!is.na(root_rate), root_rate * root_sa, 0)
    cwd_tot  <- ifelse(!is.na(cwd_rate),  cwd_rate * cwd_sa_of(plot_area), 0)

    # leaf canopy term (per m2 ground) -> only live-canopy (healthy); ghost = 0
    lt <- leaf_term %>% filter(campaign == camp)
    Rleaf <- if (dist == "healthy" && nrow(lt) == 1) lt$Rleaf_central else 0
    Rleaf_lo <- if (dist == "healthy" && nrow(lt) == 1) lt$Rleaf_lo else 0
    Rleaf_hi <- if (dist == "healthy" && nrow(lt) == 1) lt$Rleaf_hi else 0

    tide_states <- if (is_tidal(site_name)) c("high_tide","low_tide") else "fixed"
    for (tide in tide_states) {
      if (tide == "fixed") { fl <- assign_flood(site_name, camp); fw <- fl["water"]; fs <- fl["soil"] }
      else { fw <- if (tide == "high_tide") 1 else 0; fs <- 1 - fw }
      root_tot_t <- root_tot

      soil_tot  <- ifelse(!is.na(soil_rate),  soil_rate * ground_area * fs, 0)
      cwd_tot_t <- cwd_tot * cwd_exposed(site_name, camp, tide)
      water_tot <- ifelse(!is.na(water_rate), water_rate * ground_area * fw, 0)

      # areal umol m-2 ground s-1
      to_areal <- function(tot) tot / plot_area
      areal <- data.frame(
        site = site_name, campaign = camp, tide_state = tide, disturbance_level = dist,
        stem  = to_areal(stem_tot),
        root  = to_areal(root_tot_t),
        soil  = to_areal(soil_tot),
        water = to_areal(water_tot),
        cwd   = to_areal(cwd_tot_t),
        leaf  = Rleaf, leaf_lo = Rleaf_lo, leaf_hi = Rleaf_hi,
        stringsAsFactors = FALSE
      )
      areal$Reco        <- areal$stem + areal$root + areal$soil + areal$water + areal$cwd + areal$leaf
      areal$Reco_noleaf <- areal$stem + areal$root + areal$soil + areal$water + areal$cwd
      results <- c(results, list(areal))
    }
  }
}
results_df <- bind_rows(results)

# g CO2 m-2 yr-1
for (cc in c("stem","root","soil","water","cwd","leaf","Reco","Reco_noleaf")) {
  results_df[[paste0(cc, "_g")]] <- results_df[[cc]] * umol_to_g_yr
}

cat("\n=== Areal CO2 respiration (umol m-2 ground s-1) ===\n")
results_df %>%
  select(site, campaign, tide_state, disturbance_level,
         stem, root, soil, water, cwd, leaf, Reco) %>%
  mutate(across(where(is.numeric), ~round(.x, 3))) %>% as.data.frame() %>% print()

# =============================================================================
# 7. Tower CO2 (US-Skr / SRS-6) campaign-window means; NEE = Reco - GPP
# =============================================================================
cat("\n=== 7. Tower CO2 (healthy reference) ===\n")
tower_co2 <- gpp_raw %>%
  filter(campaign %in% campaigns) %>%
  group_by(campaign) %>%
  summarise(
    # GPP from the US-Skr partitioning workflow (the standard partitioned GPP,
    # gap-filled by the PAR light-response model where NEE is missing). This is an
    # INPUT to the bottom-up budget; the top-down closure target is CARAFE FCO2.
    GPP_tower  = mean(GPP, na.rm = TRUE),
    Reco_tower_part = mean(Reco, na.rm = TRUE),      # EC-partitioned Reco (modeled; literature check only)
    NEE_tower  = mean(NEE_gapfilled, na.rm = TRUE),  # tower NEE (internal consistency note only)
    .groups = "drop"
  )
cat("Tower campaign-window means (umol m-2 s-1; NEE>0 = source):\n")
tower_co2 %>% mutate(across(where(is.numeric), ~round(.x,3))) %>% as.data.frame() %>% print()

# Tide-average tidal sites, weighting high tide by the measured fraction of
# time the floor is flooded that month (00_lib/tide_weights.R); ghost = fixed.
# Both tide states are retained in results_df / plot_level output.
source(file.path(project_dir, "code", "00_lib", "tide_weights.R"))
tide_weight <- tide_weight_setup(project_dir)
results_df <- results_df %>% mutate(tide_weight = tide_weight(site, as.character(campaign), tide_state))
comp_cols <- c("stem","root","soil","water","cwd","leaf","leaf_lo","leaf_hi",
               "Reco","Reco_noleaf")
tide_avg <- results_df %>%
  group_by(site, campaign, disturbance_level) %>%
  summarise(across(all_of(comp_cols), ~ weighted.mean(.x, tide_weight, na.rm = TRUE)), .groups = "drop") %>%
  mutate(tide_state = "tide_avg")

# Bottom-up NEE on the tide-averaged budget: healthy uses tower GPP; ghost GPP ~ 0
nee_df <- tide_avg %>%
  left_join(tower_co2, by = "campaign") %>%
  mutate(
    GPP_used = ifelse(disturbance_level == "healthy", GPP_tower, 0),
    NEE_bottomup = Reco - GPP_used,
    NEE_bottomup_g = NEE_bottomup * umol_to_g_yr,
    Reco_g = Reco * umol_to_g_yr
  )

# =============================================================================
# 8. Outputs
# =============================================================================
cat("\n=== 8. Writing outputs ===\n")
# Component summary (tide-averaged)
summary_co2 <- nee_df %>%
  select(site, campaign, disturbance_level, stem, root, soil, water, cwd, leaf,
         Reco, Reco_noleaf, Reco_g)
write.csv(summary_co2, file.path(output_dir, "summary_CO2_by_component.csv"), row.names = FALSE)

# Per-tide detail (both tide states retained) + tide-avg NEE
plot_level_co2 <- bind_rows(
  results_df %>% select(site, campaign, tide_state, tide_weight, disturbance_level,
                        stem, root, soil, water, cwd, leaf, Reco, Reco_noleaf),
  nee_df %>% select(site, campaign, tide_state, disturbance_level,
                    stem, root, soil, water, cwd, leaf, Reco, Reco_noleaf)
) %>% arrange(site, campaign, tide_state)
write.csv(nee_df %>% select(site, campaign, disturbance_level,
                            stem, root, soil, water, cwd, leaf, Reco, Reco_noleaf,
                            GPP_used, NEE_bottomup, Reco_g, NEE_bottomup_g,
                            GPP_tower, Reco_tower_part, NEE_tower),
          file.path(output_dir, "plot_level_CO2_totals.csv"), row.names = FALSE)
write.csv(plot_level_co2, file.path(output_dir, "CO2_by_tide_state.csv"), row.names = FALSE)

cat("\n=== Bottom-up FCO2 (NEE) per class — to compare vs CARAFE top-down (umol m-2 s-1; NEE>0 = source) ===\n")
cat("  NEE_bottomup = below-canopy Rs (chambers) + leaf Rs (lit) - GPP (tower); ghost GPP~0.\n")
cat("  tower NEE shown for internal consistency only; CARAFE is the closure target.\n")
nee_df %>% select(site, campaign, disturbance_level, Reco_noleaf, leaf, Reco,
                  GPP_used, NEE_bottomup, NEE_tower) %>%
  mutate(across(where(is.numeric), ~round(.x,3))) %>% as.data.frame() %>% print()

cat("\n=== Literature sanity checks (independent of CARAFE) ===\n")
cat("  belowcanopy/part.Reco vs AH-02 (0.45-0.65); leaf/Reco vs FR (~0.33)\n")
healthy_cmp <- nee_df %>%
  filter(disturbance_level == "healthy") %>%
  mutate(
    belowcanopy = soil + water + root + cwd,
    belowcanopy_frac = belowcanopy / Reco_tower_part,  # AH-02: 0.45-0.65 of total Reco
    leaf_frac = leaf / Reco,                           # FR: ~0.33 of bottom-up Reco
    chambers_vs_partReco = Reco_noleaf / Reco_tower_part
  ) %>%
  select(site, campaign, Reco_noleaf, Reco, Reco_tower_part, chambers_vs_partReco,
         leaf, leaf_frac, belowcanopy_frac)
healthy_cmp %>% mutate(across(where(is.numeric), ~round(.x,3))) %>% as.data.frame() %>% print()

cat("\nDone. Outputs in", output_dir, "\n")
