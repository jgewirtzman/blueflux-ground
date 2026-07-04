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
fig_dir     <- file.path(output_dir, "figures")
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
flux_raw <- read.csv(file.path(project_dir, "output", "combined_gas_flux_dataset.csv")) %>%
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

flux_table <- list()
for (camp in campaigns) for (site_name in tls_sites) for (comp in c("root","soil","water","cwd","stem")) {
  bd <- if (comp == "stem") stem_boot else nonstem_boot
  fl <- get_site_flux(bd, site_name, camp, comp)
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
LAI_central  <- 2.3           # LAI-01 ground-measured optical (site value; central)
LAI_lo       <- 2.1           # ground optical lower CI
LAI_hi       <- 5.55          # LAI-02 MODIS recovered upper bound (reads high; sensitivity)

# Beer's-law canopy integration (C-07): leaf respiratory capacity scales with light
# through the canopy, so total canopy R uses an EFFECTIVE LAI rather than flat Rd*LAI.
# effective_LAI = (1 - exp(-k*LAI)) / k ; k = canopy light-extinction coefficient.
K_EXT     <- 0.5
eff_LAI   <- function(L) (1 - exp(-K_EXT * L)) / K_EXT
LAIe_central <- eff_LAI(LAI_central)   # 2.3  -> ~1.37
LAIe_lo      <- eff_LAI(LAI_lo)
LAIe_hi      <- eff_LAI(LAI_hi)
F_INHIB_DAY  <- 0.30          # daytime light inhibition of leaf R

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
# 5. Flooding / tide assignment (same as CH4)
# =============================================================================
assign_flood <- function(site, camp) {
  if (site %in% c("CP40","FLM30")) return(c(water = 1, soil = 0))
  c(water = NA, soil = NA)
}
is_tidal <- function(site) site %in% c("SRS5", "SRS6")
cwd_sa_default <- 10

# =============================================================================
# 6. Scale to plot level -> areal Reco (umol m-2 ground s-1)
# =============================================================================
cat("\n=== 6. Scaling CO2 to plot level ===\n")
results <- list()
for (camp in campaigns) {
  for (site_name in tls_sites) {
    ground_area <- tree_stats %>% filter(site == site_name) %>% pull(ground_area_m2)
    plot_area   <- tree_stats %>% filter(site == site_name) %>% pull(area_m2)
    root_sa     <- tls_root_sa %>% filter(site == site_name) %>% pull(root_SA_m2)
    stem_sa_tot <- tls_stem_total %>% filter(site == site_name) %>% pull(total_stem_SA_m2)
    if (length(root_sa) == 0) root_sa <- 0
    dist <- site_meta$disturbance_level[match(site_name, site_meta$site)]

    ft <- flux_table %>% filter(site == site_name, campaign == camp)
    rate <- function(cc) { v <- ft %>% filter(component == cc) %>% pull(flux_rate); if (length(v)==0) NA else v }
    stem_rate <- rate("stem"); root_rate <- rate("root")
    soil_rate <- rate("soil"); water_rate <- rate("water"); cwd_rate <- rate("cwd")

    # Total umol/s over plot
    stem_tot <- ifelse(!is.na(stem_rate), stem_rate * stem_sa_tot, 0)
    root_tot <- ifelse(!is.na(root_rate), root_rate * root_sa, 0)
    cwd_tot  <- ifelse(!is.na(cwd_rate),  cwd_rate * cwd_sa_default, 0)

    # leaf canopy term (per m2 ground) -> only live-canopy (healthy); ghost = 0
    lt <- leaf_term %>% filter(campaign == camp)
    Rleaf <- if (dist == "healthy" && nrow(lt) == 1) lt$Rleaf_central else 0
    Rleaf_lo <- if (dist == "healthy" && nrow(lt) == 1) lt$Rleaf_lo else 0
    Rleaf_hi <- if (dist == "healthy" && nrow(lt) == 1) lt$Rleaf_hi else 0

    tide_states <- if (is_tidal(site_name)) c("high_tide","low_tide") else "fixed"
    for (tide in tide_states) {
      if (tide == "fixed") { fl <- assign_flood(site_name, camp); fw <- fl["water"]; fs <- fl["soil"] }
      else { fw <- if (tide == "high_tide") 1 else 0; fs <- 1 - fw }

      soil_tot  <- ifelse(!is.na(soil_rate),  soil_rate * ground_area * fs, 0)
      water_tot <- ifelse(!is.na(water_rate), water_rate * ground_area * fw, 0)

      # areal umol m-2 ground s-1
      to_areal <- function(tot) tot / plot_area
      areal <- data.frame(
        site = site_name, campaign = camp, tide_state = tide, disturbance_level = dist,
        stem  = to_areal(stem_tot),
        root  = to_areal(root_tot),
        soil  = to_areal(soil_tot),
        water = to_areal(water_tot),
        cwd   = to_areal(cwd_tot),
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

# Tide-average tidal sites (50/50 high/low) for tower comparison; ghost = fixed.
# Both tide states are retained in results_df / plot_level output.
comp_cols <- c("stem","root","soil","water","cwd","leaf","leaf_lo","leaf_hi",
               "Reco","Reco_noleaf")
tide_avg <- results_df %>%
  group_by(site, campaign, disturbance_level) %>%
  summarise(across(all_of(comp_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
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
  results_df %>% select(site, campaign, tide_state, disturbance_level,
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
