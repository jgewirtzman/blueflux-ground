# =============================================================================
# Upscale Methane Emissions to TLS Plot-Level Estimates (v5)
# =============================================================================
# Two campaigns with adequate coverage: Oct 2022 (wet), Mar 2023 (dry)
# Mar 2022 dropped (poor coverage at most sites).
#
# Two height-scaling scenarios for stems:
#   A) Exponential decay: fit flux ~ exp(-b * height), extrapolate upward.
#   B) Zero above max: measured flux rates up to 1.5 m only; zero above.
#
# Surface area assumptions:
#   STEM:  TLS trunk + branch SA per 0.5m height bin.
#   ROOT:  TLS root SA (summed across height bins).
#   SOIL/WATER: ground area partitioned by flooding state.
#     Ground area = plot footprint − ground-level cross-section (TLS trunk+root
#     volume at h=0 bin / 0.5 m). This is more conservative than using DBH-level
#     basal area, though the difference is <0.5% of plot area.
#     CP40  — always 100% water
#     FLM30 — Oct 2022 & Mar 2023 = 100% water (flooded)
#     SRS5/SRS6 — tidal: both high-tide (100% water) and low-tide (100% soil)
#   CWD:   Estimated at 10 m2 per plot (sensitivity over 0–200 m2).
#
# Gap-filling (targeted, not blanket):
#   CP40 root:  from FLM30 root (same campaign) — tiny root SA (58 m2)
#   FLM30 root: from CP40 root if available, else FLM30 Oct 2022 (n=4)
#   SRS6 Oct 2022 water: from SRS5 Oct 2022 water (n=3)
#   Everything else: only site-level data used; if missing, component = 0.
# =============================================================================

library(MASS)  # for mvrnorm (correlated parameter draws) — load before dplyr
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(scales)

set.seed(42)

# --- Paths -------------------------------------------------------------------
project_dir <- here::here()
tls_dir     <- Sys.getenv("BLUEFLUX_TLS_DIR", "data/tls")
output_dir  <- file.path(project_dir, "output", "upscaling")
fig_dir     <- file.path(output_dir, "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# --- Theme & helpers ---------------------------------------------------------
theme_pub <- function(base_size = 11) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      axis.title       = element_text(size = base_size, face = "bold"),
      axis.text        = element_text(size = base_size - 1),
      strip.text       = element_text(size = base_size - 1, face = "bold"),
      strip.background = element_rect(fill = "grey95", color = "grey70"),
      legend.title     = element_text(size = base_size - 1, face = "bold"),
      legend.text      = element_text(size = base_size - 2),
      panel.grid.minor = element_blank(),
      plot.tag          = element_text(size = base_size + 3, face = "bold"),
      plot.title       = element_text(size = base_size + 1, face = "bold"),
      plot.subtitle    = element_text(size = base_size - 1, color = "grey40")
    )
}

component_colors <- c("stem" = "#228B22", "root" = "#D2691E",
                       "soil" = "#8B4513", "water" = "#4682B4", "cwd" = "#808080")
component_colors_7 <- c("soil" = "#8B4513", "water" = "#4682B4", "root" = "#D2691E",
                         "cwd" = "#808080", "stem_measured" = "#228B22",
                         "stem_extrapolated" = "#90EE90")
save_pub <- function(p, name, w, h, u = "mm") {
  ggsave(file.path(fig_dir, paste0(name, ".pdf")), p, width = w, height = h, units = u)
  ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = w, height = h, units = u, dpi = 300)
  cat("  Saved:", name, "\n")
}

# --- 1. Load TLS data -------------------------------------------------------
cat("=== 1. Loading TLS data ===\n")

tls_all <- read.csv(file.path(tls_dir, "all_sites_summary.csv")) %>%
  select(site, segment_class, height_bin, height_bin_num,
         Total_volume_m3, Total_surface_area_m2)

tree_stats <- read.csv(file.path(tls_dir, "tree_stats_per_site.csv"))

# Ground-level cross-section from TLS (trunk + root volume at h=0 / 0.5m)
ground_xsec <- tls_all %>%
  filter(height_bin_num == 0, segment_class %in% c("trunk", "root")) %>%
  group_by(site) %>%
  summarise(ground_xsec_m2 = sum(Total_volume_m3, na.rm = TRUE) / 0.5,
            .groups = "drop")

tree_stats <- tree_stats %>%
  left_join(ground_xsec, by = "site") %>%
  mutate(
    ground_area_m2 = area_m2 - ground_xsec_m2  # more accurate than DBH-level BA
  )

cat("Ground areas (using ground-level cross-section from TLS):\n")
tree_stats %>%
  select(site, area_m2, total_ba_m2, ground_xsec_m2, ground_area_m2) %>%
  as.data.frame() %>%
  print()

site_meta <- data.frame(
  site = c("CP40", "FLM30", "SRS5", "SRS6"),
  disturbance_level = c("ghost", "ghost", "healthy", "healthy"),
  stringsAsFactors = FALSE
)
tls_sites  <- c("CP40", "FLM30", "SRS5", "SRS6")
campaigns  <- c("Oct 2022", "Mar 2023")  # dropped Mar 2022

# TLS stem SA per height bin
tls_stem <- tls_all %>%
  filter(segment_class %in% c("trunk", "branch")) %>%
  group_by(site, height_bin_num) %>%
  summarise(stem_SA_m2 = sum(Total_surface_area_m2, na.rm = TRUE), .groups = "drop") %>%
  mutate(height_mid_m = height_bin_num + 0.25)

# TLS root SA
tls_root_sa <- tls_all %>%
  filter(segment_class == "root") %>%
  group_by(site) %>%
  summarise(root_SA_m2 = sum(Total_surface_area_m2, na.rm = TRUE), .groups = "drop")

# Total TLS stem SA per site
tls_stem_total <- tls_stem %>%
  group_by(site) %>%
  summarise(total_stem_SA_m2 = sum(stem_SA_m2), .groups = "drop")

cat("\nTLS surface areas:\n")
sa_summary <- tree_stats %>%
  select(site, area_m2, ground_area_m2) %>%
  left_join(tls_stem_total, by = "site") %>%
  left_join(tls_root_sa, by = "site")
print(as.data.frame(sa_summary))

# --- 2. Load raw flux data ---------------------------------------------------
cat("\n=== 2. Loading flux data ===\n")
flux_raw <- read.csv(file.path(project_dir, "output", "data_products", "combined_gas_flux_dataset.csv")) %>%
  filter(plot %in% tls_sites) %>%
  mutate(
    disturbance_level = site_meta$disturbance_level[match(plot, site_meta$site)],
    campaign = factor(month_year,
                      levels = c("2022-10", "2023-03"),
                      labels = campaigns),
    height_m = height_corrected / 100,
    height_category = case_when(
      component == "stem" & !is.na(height_corrected) & height_corrected >= 0 &
        height_corrected < 50  ~ "0-50cm",
      component == "stem" & !is.na(height_corrected) &
        height_corrected >= 50 & height_corrected < 100 ~ "50-100cm",
      component == "stem" & !is.na(height_corrected) &
        height_corrected >= 100 & height_corrected < 150 ~ "100-150cm",
      component == "stem" & !is.na(height_corrected) &
        height_corrected >= 150 ~ ">150cm",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(campaign))  # drops Mar 2022

cat("Rows after dropping Mar 2022:", nrow(flux_raw), "\n")
cat("Coverage:\n")
flux_raw %>%
  filter(!is.na(CH4_best.flux)) %>%
  count(plot, campaign, component) %>%
  tidyr::pivot_wider(names_from = component, values_from = n, values_fill = 0) %>%
  as.data.frame() %>%
  print()

# --- 3. Bootstrap means per site × campaign × component ----------------------
cat("\n=== 3. Bootstrapping (R=5000) ===\n")

boot_mean <- function(x, R = 5000) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(data.frame(n = 0L, mean = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_))
  if (length(x) == 1) return(data.frame(n = 1L, mean = x, ci_lo = NA_real_, ci_hi = NA_real_))
  bm <- replicate(R, mean(sample(x, replace = TRUE)))
  data.frame(n = length(x), mean = mean(bm),
             ci_lo = quantile(bm, 0.025), ci_hi = quantile(bm, 0.975))
}

# Per site × campaign × component
nonstem_boot <- flux_raw %>%
  filter(component %in% c("soil", "water", "root", "cwd"), !is.na(CH4_best.flux)) %>%
  group_by(plot, campaign, component) %>%
  do(boot_mean(.$CH4_best.flux)) %>%
  ungroup()

cat("Non-stem bootstrap:\n")
nonstem_boot %>% as.data.frame() %>% print()

# Stem by height category
stem_boot <- flux_raw %>%
  filter(component == "stem", !is.na(CH4_best.flux), !is.na(height_category)) %>%
  group_by(plot, campaign, height_category) %>%
  do(boot_mean(.$CH4_best.flux)) %>%
  ungroup() %>%
  mutate(component = "stem")

# --- 4. Targeted gap-filling -------------------------------------------------
cat("\n=== 4. Targeted gap-filling ===\n")

# Build flux rate lookup: site × campaign × component
# Start with site-level data
get_site_flux <- function(site_name, camp, comp) {
  row <- nonstem_boot %>% filter(plot == site_name, campaign == camp, component == comp)
  if (nrow(row) > 0 && !is.na(row$mean[1])) {
    return(list(rate = row$mean[1], ci_lo = row$ci_lo[1], ci_hi = row$ci_hi[1],
                n = row$n[1], source = "site"))
  }
  return(NULL)
}

flux_table <- list()
for (camp in campaigns) {
  for (site_name in tls_sites) {
    for (comp in c("root", "soil", "water", "cwd")) {

      # Try site-level first
      fl <- get_site_flux(site_name, camp, comp)

      # Targeted gap-fills
      if (is.null(fl)) {
        if (comp == "root" && site_name == "CP40") {
          # CP40 root: use FLM30 root (same campaign if available, else pooled)
          fl <- get_site_flux("FLM30", camp, "root")
          if (!is.null(fl)) fl$source <- "gap: FLM30 root"
          if (is.null(fl)) {
            # FLM30 root only measured Oct 2022
            fl <- get_site_flux("FLM30", "Oct 2022", "root")
            if (!is.null(fl)) fl$source <- "gap: FLM30 root Oct 2022"
          }
        } else if (comp == "root" && site_name == "FLM30" && camp == "Mar 2023") {
          # FLM30 Mar 2023 root: use FLM30 Oct 2022 root (n=4)
          fl <- get_site_flux("FLM30", "Oct 2022", "root")
          if (!is.null(fl)) fl$source <- "gap: FLM30 root Oct 2022"
        } else if (comp == "water" && site_name == "SRS6" && camp == "Oct 2022") {
          # SRS6 Oct 2022 water: use SRS5 Oct 2022 water
          fl <- get_site_flux("SRS5", "Oct 2022", "water")
          if (!is.null(fl)) fl$source <- "gap: SRS5 water Oct 2022"
        }
      }

      # If still NULL, leave as missing (no blanket gap-fill)
      if (is.null(fl)) {
        fl <- list(rate = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                   n = 0L, source = "missing")
      }

      flux_table <- c(flux_table, list(data.frame(
        site = site_name, campaign = camp, component = comp,
        flux_rate = fl$rate, ci_lo = fl$ci_lo, ci_hi = fl$ci_hi,
        n_obs = fl$n, fill_source = fl$source,
        stringsAsFactors = FALSE
      )))
    }
  }
}
flux_table <- bind_rows(flux_table) %>%
  mutate(campaign = factor(campaign, levels = campaigns))

cat("Flux rate table with gap-fills:\n")
flux_table %>%
  select(site, campaign, component, flux_rate, n_obs, fill_source) %>%
  as.data.frame() %>%
  print()

# --- 5. Exponential height model per site × campaign -------------------------
cat("\n=== 5. Fitting exponential height models ===\n")

stem_pos <- flux_raw %>%
  filter(component == "stem", !is.na(height_corrected), height_corrected >= 0,
         !is.na(CH4_best.flux), CH4_best.flux > 0)

exp_models <- stem_pos %>%
  group_by(plot, campaign) %>%
  summarise(
    n_obs = n(),
    model = list(tryCatch(lm(log(CH4_best.flux) ~ height_m), error = function(e) NULL)),
    .groups = "drop"
  ) %>%
  mutate(
    intercept = sapply(model, function(m) if (!is.null(m)) coef(m)[1] else NA_real_),
    slope     = sapply(model, function(m) if (!is.null(m)) coef(m)[2] else NA_real_),
    r_squared = sapply(model, function(m) if (!is.null(m)) summary(m)$r.squared else NA_real_)
  )

cat("Exponential fits:\n")
exp_models %>% select(plot, campaign, n_obs, intercept, slope, r_squared) %>%
  as.data.frame() %>% print()

predict_stem_flux <- function(height_m, plot_name, camp, models_df) {
  mr <- models_df %>% filter(plot == plot_name, campaign == camp)
  if (nrow(mr) == 0 || is.null(mr$model[[1]])) return(NA_real_)
  exp(mr$intercept + mr$slope * height_m)
}

# --- 6. Scaling functions -----------------------------------------------------
assign_flux_height_cat <- function(h_m) {
  case_when(
    h_m < 0.5  ~ "0-50cm",
    h_m < 1.0  ~ "50-100cm",
    h_m < 1.5  ~ "100-150cm",
    h_m < 2.0  ~ ">150cm",
    TRUE       ~ "above_max"
  )
}

scale_stems_exp <- function(site_name, camp, models_df, tls_stem_df,
                            split_height = 1.5) {
  stls <- tls_stem_df %>% filter(site == site_name)
  stls %>%
    rowwise() %>%
    mutate(
      flux_pred = predict_stem_flux(height_mid_m, site_name, camp, models_df),
      total = ifelse(is.na(flux_pred), 0, flux_pred * stem_SA_m2),
      zone = ifelse(height_mid_m <= split_height, "measured", "extrapolated")
    ) %>%
    ungroup() %>%
    group_by(zone) %>%
    summarise(flux = sum(total, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = zone, values_from = flux, values_fill = 0)
}

scale_stems_zero <- function(site_name, camp, stem_boot_df, tls_stem_df,
                             split_height = 1.5) {
  stls <- tls_stem_df %>% filter(site == site_name)
  sflux <- stem_boot_df %>% filter(plot == site_name, campaign == camp)
  stls %>%
    mutate(
      h_cat = assign_flux_height_cat(height_mid_m),
      flux_val = sapply(h_cat, function(hc) {
        if (hc == "above_max") return(0)
        fr <- sflux %>% filter(height_category == hc)
        if (nrow(fr) > 0 && !is.na(fr$mean[1])) fr$mean[1] else 0
      }),
      total = flux_val * stem_SA_m2,
      zone = ifelse(height_mid_m <= split_height, "measured", "extrapolated")
    ) %>%
    group_by(zone) %>%
    summarise(flux = sum(total, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = zone, values_from = flux, values_fill = 0)
}

# --- 7. Flooding assignment ---------------------------------------------------
assign_flood <- function(site, camp) {
  if (site == "CP40") return(c(water = 1, soil = 0))
  if (site == "FLM30") return(c(water = 1, soil = 0))  # always flooded for Oct/Mar campaigns
  return(c(water = NA, soil = NA))  # tidal
}
is_tidal <- function(site) site %in% c("SRS5", "SRS6")

cwd_sa_default <- 10  # m2 per plot

# Observed stem flux range (for MC capping)
obs_stem_max <- max(flux_raw$CH4_best.flux[flux_raw$component == "stem"], na.rm = TRUE)

# --- 7b. Monte Carlo uncertainty propagation ----------------------------------
# Assumed CVs for TLS surface areas (single scan per site, no replicates)
cv_stem_sa  <- 0.12   # ~12% from voxelization/occlusion (Calders et al. 2015)
cv_root_sa  <- 0.20   # higher due to below-water occlusion
cv_ground   <- 0.03   # plot boundary well-constrained
cv_cwd_sa   <- 1.00   # CWD SA is a rough estimate

# Helper: SE from bootstrap CI (assuming ~normal)
ci_to_se <- function(ci_lo, ci_hi) {
  if (is.na(ci_lo) || is.na(ci_hi)) return(NA_real_)
  (ci_hi - ci_lo) / (2 * 1.96)
}

# Draw from lognormal with given mean and CV (ensures > 0)
rlnorm_cv <- function(n, mu, cv) {
  if (cv <= 0 || mu <= 0) return(rep(mu, n))
  sigma_log <- sqrt(log(1 + cv^2))
  mu_log    <- log(mu) - sigma_log^2 / 2
  rlnorm(n, mu_log, sigma_log)
}

N_MC <- 5000
cat("\n=== 7b. Monte Carlo uncertainty (N =", N_MC, ") ===\n")
cat(sprintf("  Assumed SA CVs: stem=%.0f%%, root=%.0f%%, ground=%.0f%%, CWD=%.0f%%\n",
            cv_stem_sa*100, cv_root_sa*100, cv_ground*100, cv_cwd_sa*100))

# --- 8. Main scaling loop -----------------------------------------------------
cat("\n=== 8. Scaling to plot level ===\n")

results <- list()
budget_detail <- list()
mc_budget <- list()

for (camp in campaigns) {
  for (site_name in tls_sites) {
    ground_area <- tree_stats %>% filter(site == site_name) %>% pull(ground_area_m2)
    plot_area   <- tree_stats %>% filter(site == site_name) %>% pull(area_m2)
    root_sa     <- tls_root_sa %>% filter(site == site_name) %>% pull(root_SA_m2)
    stem_sa_tot <- tls_stem_total %>% filter(site == site_name) %>% pull(total_stem_SA_m2)
    if (length(root_sa) == 0) root_sa <- 0
    conv <- 16.04e-9 * 1e3 * 86400 / plot_area

    # Stem split: measured (<= 1.5m) vs extrapolated (> 1.5m)
    stem_exp_split  <- scale_stems_exp(site_name, camp, exp_models, tls_stem)
    stem_zero_split <- scale_stems_zero(site_name, camp, stem_boot, tls_stem)
    stem_exp  <- sum(stem_exp_split$measured, stem_exp_split$extrapolated, na.rm = TRUE)
    stem_zero <- sum(stem_zero_split$measured, stem_zero_split$extrapolated, na.rm = TRUE)
    stem_meas_nmol  <- ifelse("measured" %in% names(stem_exp_split), stem_exp_split$measured, 0)
    stem_extrap_nmol <- ifelse("extrapolated" %in% names(stem_exp_split), stem_exp_split$extrapolated, 0)

    ft <- flux_table %>% filter(site == site_name, campaign == camp)
    root_rate  <- ft %>% filter(component == "root")  %>% pull(flux_rate)
    soil_rate  <- ft %>% filter(component == "soil")  %>% pull(flux_rate)
    water_rate <- ft %>% filter(component == "water") %>% pull(flux_rate)
    cwd_rate   <- ft %>% filter(component == "cwd")   %>% pull(flux_rate)

    root_ci_lo <- ft %>% filter(component == "root") %>% pull(ci_lo)
    root_ci_hi <- ft %>% filter(component == "root") %>% pull(ci_hi)
    soil_ci_lo <- ft %>% filter(component == "soil") %>% pull(ci_lo)
    soil_ci_hi <- ft %>% filter(component == "soil") %>% pull(ci_hi)
    water_ci_lo <- ft %>% filter(component == "water") %>% pull(ci_lo)
    water_ci_hi <- ft %>% filter(component == "water") %>% pull(ci_hi)
    cwd_ci_lo  <- ft %>% filter(component == "cwd") %>% pull(ci_lo)
    cwd_ci_hi  <- ft %>% filter(component == "cwd") %>% pull(ci_hi)

    root_total  <- ifelse(!is.na(root_rate), root_rate * root_sa, 0)
    cwd_total   <- ifelse(!is.na(cwd_rate), cwd_rate * cwd_sa_default, 0)

    # --- MC draws for uncertainty propagation ---
    # Flux rate draws (normal, truncated at 0 for components that can't be negative...
    # actually allow negative for stems)
    draw_flux <- function(rate, ci_lo, ci_hi, n) {
      if (is.na(rate)) return(rep(0, n))
      se <- ci_to_se(ci_lo, ci_hi)
      if (is.na(se) || se <= 0) return(rep(rate, n))  # n=1 or gap-filled: fixed
      rnorm(n, rate, se)
    }

    root_flux_draws  <- draw_flux(root_rate, root_ci_lo, root_ci_hi, N_MC)
    soil_flux_draws  <- draw_flux(soil_rate, soil_ci_lo, soil_ci_hi, N_MC)
    water_flux_draws <- draw_flux(water_rate, water_ci_lo, water_ci_hi, N_MC)
    cwd_flux_draws   <- draw_flux(cwd_rate, cwd_ci_lo, cwd_ci_hi, N_MC)

    # SA draws (lognormal, correlated within site for stem)
    root_sa_draws  <- rlnorm_cv(N_MC, root_sa, cv_root_sa)
    ground_draws   <- rlnorm_cv(N_MC, ground_area, cv_ground)
    cwd_sa_draws   <- rlnorm_cv(N_MC, cwd_sa_default, cv_cwd_sa)

    # Stem SA draws: single correlated multiplier across all height bins
    stem_sa_mult <- rlnorm_cv(N_MC, 1, cv_stem_sa)  # multiplier centered at 1

    # Stem flux draws: use model covariance matrix for correlated intercept/slope
    em <- exp_models %>% filter(plot == site_name, campaign == camp)
    stls <- tls_stem %>% filter(site == site_name)
    if (nrow(em) > 0 && !is.null(em$model[[1]])) {
      mod <- em$model[[1]]
      vcov_mat <- vcov(mod)
      param_draws <- MASS::mvrnorm(N_MC, coef(mod), vcov_mat)  # [N_MC x 2]
      # For each MC draw, compute stem total split by measured/extrap
      stem_meas_draws <- numeric(N_MC)
      stem_extrap_draws <- numeric(N_MC)
      for (j in seq_len(nrow(stls))) {
        h <- stls$height_mid_m[j]; sa <- stls$stem_SA_m2[j]
        flux_j <- pmin(exp(param_draws[, 1] + param_draws[, 2] * h),
                       obs_stem_max)  # cap at max observed flux
        total_j <- flux_j * sa * stem_sa_mult
        if (h <= 1.5) {
          stem_meas_draws <- stem_meas_draws + total_j
        } else {
          stem_extrap_draws <- stem_extrap_draws + total_j
        }
      }
    } else {
      stem_meas_draws   <- rep(stem_meas_nmol, N_MC)
      stem_extrap_draws <- rep(stem_extrap_nmol, N_MC)
    }

    tide_states <- if (is_tidal(site_name)) c("high_tide", "low_tide") else "fixed"

    for (tide in tide_states) {
      if (tide == "fixed") {
        fl <- assign_flood(site_name, camp)
        fw <- fl["water"]; fs <- fl["soil"]
      } else {
        fw <- if (tide == "high_tide") 1.0 else 0.0
        fs <- 1.0 - fw
      }

      soil_total  <- ifelse(!is.na(soil_rate),  soil_rate * ground_area * fs, 0)
      water_total <- ifelse(!is.na(water_rate), water_rate * ground_area * fw, 0)
      soil_sa_used  <- ground_area * fs
      water_sa_used <- ground_area * fw

      # MC totals per component (nmol/s)
      soil_mc  <- soil_flux_draws * ground_draws * fs
      water_mc <- water_flux_draws * ground_draws * fw
      root_mc  <- root_flux_draws * root_sa_draws
      cwd_mc   <- cwd_flux_draws * cwd_sa_draws

      # Convert all MC draws to mg CH4 m-2 d-1
      mc_conv <- 16.04e-9 * 1e3 * 86400 / plot_area
      comps <- c("soil", "water", "root", "cwd", "stem_measured", "stem_extrapolated")
      mc_arrays <- list(soil_mc, water_mc, root_mc, cwd_mc,
                        stem_meas_draws, stem_extrap_draws)

      for (k in seq_along(comps)) {
        draws_mg <- mc_arrays[[k]] * mc_conv
        mc_budget <- c(mc_budget, list(data.frame(
          site = site_name, campaign = camp, tide_state = tide,
          component = comps[k],
          mc_mean = mean(draws_mg),
          mc_se   = sd(draws_mg),
          mc_ci_lo = quantile(draws_mg, 0.025),
          mc_ci_hi = quantile(draws_mg, 0.975),
          stringsAsFactors = FALSE
        )))
      }

      # Also store total
      total_mc <- (soil_mc + water_mc + root_mc + cwd_mc +
                   stem_meas_draws + stem_extrap_draws) * mc_conv
      mc_budget <- c(mc_budget, list(data.frame(
        site = site_name, campaign = camp, tide_state = tide,
        component = "total",
        mc_mean = mean(total_mc), mc_se = sd(total_mc),
        mc_ci_lo = quantile(total_mc, 0.025),
        mc_ci_hi = quantile(total_mc, 0.975),
        stringsAsFactors = FALSE
      )))

      for (scen in c("exponential", "zero_above_max")) {
        st <- if (scen == "exponential") stem_exp else stem_zero
        results <- c(results, list(data.frame(
          site = site_name, campaign = camp, scenario = scen, tide_state = tide,
          stem_nmol_s = st, stem_meas_nmol_s = stem_meas_nmol,
          stem_extrap_nmol_s = stem_extrap_nmol,
          root_nmol_s = root_total,
          soil_nmol_s = soil_total, water_nmol_s = water_total,
          cwd_nmol_s = cwd_total,
          ground_area_m2 = ground_area, plot_area_m2 = plot_area,
          stringsAsFactors = FALSE
        )))
      }

      budget_detail <- c(budget_detail, list(data.frame(
        site = site_name, campaign = camp, tide_state = tide,
        component = c("stem_measured", "stem_extrapolated", "root", "soil", "water", "cwd"),
        flux_rate = c(NA, NA, root_rate, soil_rate, water_rate, cwd_rate),
        flux_ci_lo = c(NA, NA, root_ci_lo, soil_ci_lo, water_ci_lo, cwd_ci_lo),
        flux_ci_hi = c(NA, NA, root_ci_hi, soil_ci_hi, water_ci_hi, cwd_ci_hi),
        surface_area_m2 = c(NA, NA, root_sa, soil_sa_used, water_sa_used, cwd_sa_default),
        total_nmol_s = c(stem_meas_nmol, stem_extrap_nmol, root_total,
                         soil_total, water_total, cwd_total),
        stringsAsFactors = FALSE
      )))
    }
  }
}

mc_budget_df <- bind_rows(mc_budget) %>%
  left_join(site_meta, by = "site") %>%
  mutate(campaign = factor(campaign, levels = campaigns))

results_df <- bind_rows(results) %>%
  left_join(site_meta, by = "site") %>%
  mutate(
    campaign = factor(campaign, levels = campaigns),
    conv = 16.04e-9 * 1e3 * 86400 / plot_area_m2,
    stem_mg  = stem_nmol_s * conv,
    stem_meas_mg = stem_meas_nmol_s * conv,
    stem_extrap_mg = stem_extrap_nmol_s * conv,
    root_mg  = root_nmol_s * conv,
    soil_mg  = soil_nmol_s * conv,
    water_mg = water_nmol_s * conv,
    cwd_mg   = cwd_nmol_s * conv,
    total_mg = stem_mg + root_mg + soil_mg + water_mg + cwd_mg
  )

budget_df <- bind_rows(budget_detail) %>%
  left_join(site_meta, by = "site") %>%
  left_join(tree_stats %>% select(site, area_m2), by = "site") %>%
  mutate(
    campaign = factor(campaign, levels = campaigns),
    conv = 16.04e-9 * 1e3 * 86400 / area_m2,
    total_mg_m2_d = total_nmol_s * conv,
    sa_per_m2 = surface_area_m2 / area_m2,
    component = factor(component,
      levels = c("soil", "water", "root", "cwd", "stem_measured", "stem_extrapolated"))
  )

cat("\nMC uncertainty summary (mg CH4 m-2 d-1, exponential scenario, high tide):\n")
mc_budget_df %>%
  filter(tide_state %in% c("fixed", "high_tide")) %>%
  mutate(summary = sprintf("%.3f +/- %.3f [%.3f, %.3f]",
                           mc_mean, mc_se, mc_ci_lo, mc_ci_hi)) %>%
  select(site, campaign, component, summary) %>%
  pivot_wider(names_from = component, values_from = summary) %>%
  as.data.frame() %>%
  print()

cat("\n=== Results (mg CH4 m-2 d-1) ===\n")
results_df %>%
  filter(scenario == "exponential") %>%
  select(site, campaign, tide_state, disturbance_level,
         stem_mg, root_mg, soil_mg, water_mg, cwd_mg, total_mg) %>%
  arrange(site, campaign, tide_state) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  as.data.frame() %>%
  print()

# --- 9. Disturbance-class averages -------------------------------------------
cat("\n=== 9. Disturbance-class averages ===\n")

class_avg <- results_df %>%
  filter(scenario == "exponential",
         tide_state %in% c("fixed", "high_tide")) %>%
  select(site, campaign, disturbance_level, stem_mg, root_mg, soil_mg, water_mg, cwd_mg, total_mg) %>%
  pivot_longer(cols = c(stem_mg, root_mg, soil_mg, water_mg, cwd_mg, total_mg),
               names_to = "metric", values_to = "value") %>%
  group_by(disturbance_level, campaign, metric) %>%
  summarise(
    mean_val = mean(value, na.rm = TRUE),
    min_val  = min(value, na.rm = TRUE),
    max_val  = max(value, na.rm = TRUE),
    .groups = "drop"
  )

cat("Class averages (high tide for tidal sites):\n")
class_avg %>%
  filter(metric == "total_mg") %>%
  as.data.frame() %>%
  print()

# --- 10. Sensitivity analyses ------------------------------------------------
cat("\n=== 10. Sensitivity analyses ===\n")

# CWD SA sensitivity
cwd_sa_values <- c(0, 5, 10, 25, 50, 100, 200)
cwd_sens <- list()
for (cwd_sa in cwd_sa_values) {
  for (i in seq_len(nrow(results_df))) {
    r <- results_df[i, ]
    ft_row <- flux_table %>% filter(site == r$site, campaign == r$campaign, component == "cwd")
    cr <- if (nrow(ft_row) > 0) ft_row$flux_rate[1] else NA_real_
    cwd_mg_new <- if (!is.na(cr)) cr * cwd_sa * r$conv else 0
    total_new <- r$stem_mg + r$root_mg + r$soil_mg + r$water_mg + cwd_mg_new
    cwd_sens <- c(cwd_sens, list(data.frame(
      site = r$site, campaign = as.character(r$campaign),
      scenario = r$scenario, tide_state = r$tide_state,
      disturbance_level = r$disturbance_level,
      cwd_SA_m2 = cwd_sa, cwd_mg = cwd_mg_new, total_mg = total_new,
      stringsAsFactors = FALSE
    )))
  }
}
cwd_sens_df <- bind_rows(cwd_sens) %>%
  mutate(campaign = factor(campaign, levels = campaigns))

# Flooding fraction sensitivity for tidal sites
flood_fracs <- seq(0, 1, by = 0.05)
flood_sens <- list()
for (ff in flood_fracs) {
  for (camp in campaigns) {
    for (site_name in c("SRS5", "SRS6")) {
      ground_area <- tree_stats %>% filter(site == site_name) %>% pull(ground_area_m2)
      root_sa     <- tls_root_sa %>% filter(site == site_name) %>% pull(root_SA_m2)
      plot_area   <- tree_stats %>% filter(site == site_name) %>% pull(area_m2)
      conv <- 16.04e-9 * 1e3 * 86400 / plot_area

      stem_exp_s <- scale_stems_exp(site_name, camp, exp_models, tls_stem)
      stem_exp_val <- sum(stem_exp_s$measured, stem_exp_s$extrapolated, na.rm = TRUE)
      ft <- flux_table %>% filter(site == site_name, campaign == camp)
      root_rate  <- ft %>% filter(component == "root") %>% pull(flux_rate)
      soil_rate  <- ft %>% filter(component == "soil") %>% pull(flux_rate)
      water_rate <- ft %>% filter(component == "water") %>% pull(flux_rate)
      cwd_rate   <- ft %>% filter(component == "cwd") %>% pull(flux_rate)

      root_total  <- ifelse(!is.na(root_rate), root_rate * root_sa, 0)
      soil_total  <- ifelse(!is.na(soil_rate), soil_rate * ground_area * (1 - ff), 0)
      water_total <- ifelse(!is.na(water_rate), water_rate * ground_area * ff, 0)
      cwd_total   <- ifelse(!is.na(cwd_rate), cwd_rate * cwd_sa_default, 0)
      total <- (stem_exp_val + root_total + soil_total + water_total + cwd_total) * conv

      flood_sens <- c(flood_sens, list(data.frame(
        site = site_name, campaign = camp, frac_flooded = ff,
        total_mg_m2_d = total, soil_mg = soil_total * conv, water_mg = water_total * conv,
        stringsAsFactors = FALSE
      )))
    }
  }
}
flood_sens_df <- bind_rows(flood_sens) %>%
  mutate(campaign = factor(campaign, levels = campaigns))

# --- Height extrapolation sensitivity analysis --------------------------------
cat("\n=== Height extrapolation sensitivity ===\n")

# Fit additional models for sensitivity analysis
# Linear model on ALL stem data (including negative fluxes = uptake)
stem_all_data <- flux_raw %>%
  filter(component == "stem", !is.na(height_corrected), height_corrected >= 0,
         !is.na(CH4_best.flux))

linear_models <- list()
exp_asym_models_list <- list()

for (s in tls_sites) {
  for (camp in campaigns) {
    key <- paste(s, camp)
    dat_all <- stem_all_data %>% filter(plot == s, campaign == camp)
    dat_pos <- stem_pos %>% filter(plot == s, campaign == camp)

    # Linear model (all data, including negative fluxes)
    linear_models[[key]] <- tryCatch(
      lm(CH4_best.flux ~ height_m, data = dat_all),
      error = function(e) NULL
    )

    # Exponential with free asymptote: CH4 = C + A * exp(B * h)
    # C is the asymptote (can be positive, zero, or negative)
    em <- exp_models %>% filter(plot == s, campaign == camp)
    asym_mod <- NULL
    if (nrow(em) > 0 && !is.null(em$model[[1]]) && nrow(dat_pos) >= 4) {
      # Try several starting values for C to improve convergence
      for (start_C in c(0, 0.1, -0.1, 1, -1)) {
        if (!is.null(asym_mod)) break
        asym_mod <- tryCatch({
          nls(CH4_best.flux ~ C + A * exp(B * height_m),
              data = dat_pos,
              start = list(C = start_C,
                           A = exp(em$intercept[1]),
                           B = em$slope[1]),
              control = nls.control(maxiter = 500, warnOnly = TRUE))
        }, error = function(e) NULL)
      }
    }
    # Quality check: reject if B > 0 (flux increasing with height = nonsensical)
    # or if |C| is unreasonably large relative to the data
    if (!is.null(asym_mod)) {
      cc <- coef(asym_mod)
      max_obs <- max(dat_pos$CH4_best.flux)
      if (cc["B"] > 0 || abs(cc["C"]) > 10 * max_obs) {
        cat(sprintf("  Rejecting %s: B=%.2f, C=%.2f (poor fit)\n", key, cc["B"], cc["C"]))
        asym_mod <- NULL
      }
    }
    exp_asym_models_list[[key]] <- asym_mod
  }
}

cat("Linear fits (all stem data including negative fluxes):\n")
for (key in names(linear_models)) {
  m <- linear_models[[key]]
  if (!is.null(m)) {
    cat(sprintf("  %s: intercept=%.3f, slope=%.3f\n", key, coef(m)[1], coef(m)[2]))
  }
}

cat("\nExponential with free asymptote fits (C + A*exp(B*h)):\n")
for (key in names(exp_asym_models_list)) {
  m <- exp_asym_models_list[[key]]
  if (!is.null(m)) {
    cc <- coef(m)
    cat(sprintf("  %s: C=%.4f (asymptote), A=%.4f, B=%.4f\n",
                key, cc["C"], cc["A"], cc["B"]))
  } else {
    cat(sprintf("  %s: did not converge\n", key))
  }
}

# Define prediction function for each extrapolation scenario
max_meas_h <- 1.5  # approximate max measurement height

# Observed stem flux range — used as physical caps
obs_stem_min <- min(stem_all_data$CH4_best.flux, na.rm = TRUE)  # most negative (uptake)
obs_stem_max <- max(stem_all_data$CH4_best.flux, na.rm = TRUE)  # highest emission
cat(sprintf("\nObserved stem flux range: [%.3f, %.1f] nmol m-2 s-1\n",
            obs_stem_min, obs_stem_max))

predict_height_scenario <- function(height_m, site_name, camp, scenario) {
  key <- paste(site_name, camp)
  em <- exp_models %>% filter(plot == site_name, campaign == camp)

  switch(scenario,
    "exp_zero_asym" = {
      # Current: exp(a + b*h), asymptotes to 0
      if (nrow(em) == 0 || is.na(em$intercept[1])) return(0)
      exp(em$intercept[1] + em$slope[1] * height_m)
    },
    "zero_above" = {
      # Use exponential up to max_meas_h, zero above
      if (height_m > max_meas_h) return(0)
      if (nrow(em) == 0 || is.na(em$intercept[1])) return(0)
      exp(em$intercept[1] + em$slope[1] * height_m)
    },
    "linear_clamp" = {
      # Linear fit, clamp at 0 (no negative flux)
      m <- linear_models[[key]]
      if (is.null(m)) return(0)
      max(predict(m, newdata = data.frame(height_m = height_m)), 0)
    },
    "linear_obs_range" = {
      # Linear fit, clamped to observed data range [min_uptake, max_emission]
      m <- linear_models[[key]]
      if (is.null(m)) return(0)
      val <- as.numeric(predict(m, newdata = data.frame(height_m = height_m)))
      min(max(val, obs_stem_min), obs_stem_max)
    },
    "exp_free_asym" = {
      # Exponential with free asymptote: C + A*exp(B*h)
      # C can be positive, zero, or negative
      m <- exp_asym_models_list[[key]]
      if (is.null(m)) return(NA_real_)
      as.numeric(predict(m, newdata = data.frame(height_m = height_m)))
    },
    "constant_at_max" = {
      # Exponential up to max_meas_h, then hold constant
      if (nrow(em) == 0 || is.na(em$intercept[1])) return(0)
      h_use <- min(height_m, max_meas_h)
      exp(em$intercept[1] + em$slope[1] * h_use)
    },
    0  # default
  )
}

# Compute stem totals under each scenario
extrap_scenario_names <- c("zero_above", "exp_zero_asym", "linear_clamp",
                           "linear_obs_range", "exp_free_asym",
                           "constant_at_max")
extrap_scenario_labels <- c(
  "zero_above"       = "Zero above 1.5 m",
  "exp_zero_asym"    = "Exponential (asym. = 0)",
  "linear_clamp"     = "Linear (clamped >= 0)",
  "linear_obs_range" = "Linear (obs. range cap)",
  "exp_free_asym"    = "Exp. (free asymptote)",
  "constant_at_max"  = "Constant above 1.5 m"
)

extrap_results <- list()
extrap_profiles <- list()

for (camp in campaigns) {
  for (site_name in tls_sites) {
    stls <- tls_stem %>% filter(site == site_name)
    plot_area <- tree_stats$area_m2[tree_stats$site == site_name]
    conv <- 16.04e-9 * 1e3 * 86400 / plot_area

    for (scen in extrap_scenario_names) {
      total_flux <- 0
      for (i in seq_len(nrow(stls))) {
        h <- stls$height_mid_m[i]
        sa <- stls$stem_SA_m2[i]
        fp <- predict_height_scenario(h, site_name, camp, scen)
        if (is.na(fp)) fp <- 0
        total_flux <- total_flux + fp * sa
      }

      # Also compute total plot budget (add non-stem components, using high tide for tidal)
      ft <- flux_table %>% filter(site == site_name, campaign == camp)
      root_rate  <- ft %>% filter(component == "root")  %>% pull(flux_rate)
      soil_rate  <- ft %>% filter(component == "soil")  %>% pull(flux_rate)
      water_rate <- ft %>% filter(component == "water") %>% pull(flux_rate)
      cwd_rate   <- ft %>% filter(component == "cwd")   %>% pull(flux_rate)
      ground_area <- tree_stats$ground_area_m2[tree_stats$site == site_name]
      root_sa <- tls_root_sa$root_SA_m2[tls_root_sa$site == site_name]
      if (length(root_sa) == 0) root_sa <- 0

      # Use fixed flooding for ghost, high tide for tidal
      if (is_tidal(site_name)) { fw <- 1; fs <- 0 } else {
        fl <- assign_flood(site_name, camp); fw <- fl["water"]; fs <- fl["soil"]
      }

      root_total  <- ifelse(!is.na(root_rate), root_rate * root_sa, 0)
      soil_total  <- ifelse(!is.na(soil_rate), soil_rate * ground_area * fs, 0)
      water_total <- ifelse(!is.na(water_rate), water_rate * ground_area * fw, 0)
      cwd_total   <- ifelse(!is.na(cwd_rate), cwd_rate * cwd_sa_default, 0)
      nonstem_total <- (root_total + soil_total + water_total + cwd_total) * conv
      stem_mg <- total_flux * conv

      extrap_results <- c(extrap_results, list(data.frame(
        site = site_name, campaign = camp, scenario = scen,
        scenario_label = extrap_scenario_labels[scen],
        stem_nmol_s = total_flux,
        stem_mg_m2_d = stem_mg,
        nonstem_mg_m2_d = nonstem_total,
        total_mg_m2_d = stem_mg + nonstem_total,
        stringsAsFactors = FALSE
      )))
    }

    # Profile predictions for visualization (fine height grid)
    max_h_tls <- max(stls$height_bin_num) + 0.25
    heights_fine <- seq(0, max_h_tls, by = 0.1)
    for (scen in extrap_scenario_names) {
      for (h in heights_fine) {
        fp <- predict_height_scenario(h, site_name, camp, scen)
        extrap_profiles <- c(extrap_profiles, list(data.frame(
          site = site_name, campaign = camp, scenario = scen,
          scenario_label = extrap_scenario_labels[scen],
          height_m = h,
          pred_flux = ifelse(is.na(fp), NA_real_, fp),
          stringsAsFactors = FALSE
        )))
      }
    }
  }
}

extrap_sens_df <- bind_rows(extrap_results) %>%
  mutate(
    campaign = factor(campaign, levels = campaigns),
    scenario_label = factor(scenario_label, levels = extrap_scenario_labels)
  ) %>%
  left_join(site_meta, by = "site")

extrap_prof_df <- bind_rows(extrap_profiles) %>%
  mutate(
    campaign = factor(campaign, levels = campaigns),
    scenario_label = factor(scenario_label, levels = extrap_scenario_labels)
  )

cat("\nHeight extrapolation sensitivity — stem only (mg CH4 m-2 d-1):\n")
extrap_sens_df %>%
  select(site, campaign, scenario_label, stem_mg_m2_d) %>%
  mutate(stem_mg_m2_d = round(stem_mg_m2_d, 4)) %>%
  pivot_wider(names_from = scenario_label, values_from = stem_mg_m2_d) %>%
  as.data.frame() %>%
  print()

cat("\nHeight extrapolation sensitivity — total plot budget (mg CH4 m-2 d-1):\n")
extrap_sens_df %>%
  select(site, campaign, scenario_label, total_mg_m2_d) %>%
  mutate(total_mg_m2_d = round(total_mg_m2_d, 4)) %>%
  pivot_wider(names_from = scenario_label, values_from = total_mg_m2_d) %>%
  as.data.frame() %>%
  print()

# --- Save tables --------------------------------------------------------------
write.csv(results_df, file.path(output_dir, "plot_level_CH4_totals.csv"), row.names = FALSE)
write.csv(flux_table, file.path(output_dir, "flux_rates_with_gapfills.csv"), row.names = FALSE)
write.csv(budget_df, file.path(output_dir, "budget_decomposition.csv"), row.names = FALSE)
write.csv(cwd_sens_df, file.path(output_dir, "cwd_sensitivity.csv"), row.names = FALSE)
write.csv(flood_sens_df, file.path(output_dir, "flooding_sensitivity.csv"), row.names = FALSE)
write.csv(extrap_sens_df, file.path(output_dir, "height_extrap_sensitivity.csv"), row.names = FALSE)

# =============================================================================
# FIGURES
# =============================================================================
cat("\n=== Generating figures ===\n")

panel_order <- c("CP40", "FLM30", "SRS5 (high)", "SRS5 (low)", "SRS6 (high)", "SRS6 (low)")

make_bar_long <- function(df, scen_filter) {
  df %>%
    filter(scenario == scen_filter) %>%
    select(site, campaign, tide_state, disturbance_level,
           stem_mg, root_mg, soil_mg, water_mg, cwd_mg) %>%
    pivot_longer(cols = c(stem_mg, root_mg, soil_mg, water_mg, cwd_mg),
                 names_to = "component", values_to = "mg_m2_d") %>%
    mutate(
      component = gsub("_mg", "", component),
      component = factor(component, levels = c("soil", "water", "root", "cwd", "stem")),
      tide_label = case_when(
        tide_state == "fixed" ~ "",
        tide_state == "high_tide" ~ " (high)",
        tide_state == "low_tide" ~ " (low)"
      ),
      panel_label = factor(paste0(site, tide_label), levels = panel_order)
    )
}

# --- Fig 1: Stacked barplot (exponential) ------------------------------------
bar_long <- make_bar_long(results_df, "exponential")

p1 <- ggplot(bar_long, aes(x = campaign, y = mg_m2_d, fill = component)) +
  geom_col(position = position_stack(reverse = TRUE), color = "white", linewidth = 0.2) +
  facet_wrap(~ panel_label, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = component_colors, name = "Component") +
  labs(x = NULL, y = expression(CH[4]~flux~(mg~m^{-2}~d^{-1})),
       title = "Plot-level CH4 by component (exponential scenario)",
       subtitle = "Targeted gap-fills only; CWD at 10 m2 SA") +
  theme_pub(10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
save_pub(p1, "stacked_barplot_freeY", 300, 140)

p1f <- p1 + facet_wrap(~ panel_label, nrow = 1)
save_pub(p1f, "stacked_barplot_fixedY", 300, 140)

# --- Fig 1b: Zero above max --------------------------------------------------
bar_long_z <- make_bar_long(results_df, "zero_above_max")

p1b <- ggplot(bar_long_z, aes(x = campaign, y = mg_m2_d, fill = component)) +
  geom_col(position = position_stack(reverse = TRUE), color = "white", linewidth = 0.2) +
  facet_wrap(~ panel_label, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = component_colors, name = "Component") +
  labs(x = NULL, y = expression(CH[4]~flux~(mg~m^{-2}~d^{-1})),
       title = "Plot-level CH4 by component (zero above max)",
       subtitle = "Targeted gap-fills only; CWD at 10 m2 SA") +
  theme_pub(10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
save_pub(p1b, "stacked_barplot_zero_freeY", 300, 140)

# --- Fig 2: Disturbance-class average stacked barplot -------------------------
class_bar <- class_avg %>%
  filter(metric != "total_mg") %>%
  mutate(
    component = gsub("_mg", "", metric),
    component = factor(component, levels = c("soil", "water", "root", "cwd", "stem"))
  )

p2 <- ggplot(class_bar, aes(x = campaign, y = mean_val, fill = component)) +
  geom_col(position = position_stack(reverse = TRUE), color = "white", linewidth = 0.3) +
  facet_wrap(~ disturbance_level, nrow = 1) +
  scale_fill_manual(values = component_colors, name = "Component") +
  labs(x = NULL, y = expression(CH[4]~flux~(mg~m^{-2}~d^{-1})),
       title = "Disturbance-class mean CH4 by component",
       subtitle = "Equally weighted site average (high tide for tidal sites)") +
  theme_pub(12)
save_pub(p2, "class_avg_stacked", 200, 140)

class_total <- class_avg %>% filter(metric == "total_mg")

p2b <- ggplot(class_total, aes(x = campaign, y = mean_val, fill = disturbance_level)) +
  geom_col(position = position_dodge(0.8), width = 0.7,
           color = "grey30", linewidth = 0.2) +
  geom_errorbar(aes(ymin = min_val, ymax = max_val),
                position = position_dodge(0.8), width = 0.2) +
  scale_fill_manual(values = c("healthy" = "#228B22", "ghost" = "#8B4513"),
                    name = "Disturbance") +
  labs(x = NULL, y = expression(Total~CH[4]~(mg~m^{-2}~d^{-1})),
       title = "Disturbance-class mean CH4",
       subtitle = "Error bars = range across sites within class") +
  theme_pub(12)
save_pub(p2b, "class_avg_total", 200, 140)

# --- Fig 3: Scenario comparison -----------------------------------------------
p3_data <- results_df %>%
  select(site, campaign, scenario, tide_state, total_mg) %>%
  mutate(
    tide_label = case_when(
      tide_state == "fixed" ~ "",
      tide_state == "high_tide" ~ " (high)",
      tide_state == "low_tide" ~ " (low)"
    ),
    panel_label = factor(paste0(site, tide_label), levels = panel_order),
    scenario_label = ifelse(scenario == "exponential", "Exponential", "Zero above max")
  )

p3 <- ggplot(p3_data, aes(x = campaign, y = total_mg, fill = scenario_label)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  facet_wrap(~ panel_label, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = c("Exponential" = "#E69F00", "Zero above max" = "#56B4E9"),
                    name = "Scenario") +
  labs(x = NULL, y = expression(Total~CH[4]~(mg~m^{-2}~d^{-1})),
       title = "Scenario comparison") +
  theme_pub(10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
save_pub(p3, "scenario_comparison", 300, 130)

# --- Fig 4: % component contribution -----------------------------------------
p4_data <- bar_long %>%
  group_by(panel_label, campaign) %>%
  mutate(total = sum(mg_m2_d, na.rm = TRUE),
         pct = ifelse(total > 0, mg_m2_d / total * 100, 0)) %>%
  ungroup()

p4 <- ggplot(p4_data, aes(x = campaign, y = pct, fill = component)) +
  geom_col(position = position_stack(reverse = TRUE), color = "white", linewidth = 0.3) +
  facet_wrap(~ panel_label, nrow = 1) +
  scale_fill_manual(values = component_colors, name = "Component") +
  geom_hline(yintercept = c(25, 50, 75), linetype = "dotted", alpha = 0.3) +
  labs(x = NULL, y = "% of total CH4",
       title = "Relative component contributions (exponential)") +
  theme_pub(10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
save_pub(p4, "component_percent", 300, 130)

# --- Fig 5: Flooding sensitivity ----------------------------------------------
p5 <- ggplot(flood_sens_df, aes(x = frac_flooded, y = total_mg_m2_d, color = campaign)) +
  geom_line(linewidth = 0.9) +
  geom_point(data = flood_sens_df %>% filter(frac_flooded %in% c(0, 1)), size = 2.5) +
  facet_wrap(~ site, scales = "free_y") +
  scale_x_continuous(labels = percent_format(1), breaks = seq(0, 1, 0.2)) +
  labs(x = "Fraction of ground flooded",
       y = expression(Total~CH[4]~(mg~m^{-2}~d^{-1})),
       title = "Flooding fraction sensitivity (SRS5/SRS6)",
       subtitle = "0% = all soil, 100% = all water",
       color = "Campaign") +
  theme_pub(11)
save_pub(p5, "flooding_sensitivity", 240, 130)

# --- Fig 6: CWD sensitivity ---------------------------------------------------
p6_data <- cwd_sens_df %>%
  filter(scenario == "exponential",
         tide_state %in% c("fixed", "high_tide")) %>%
  mutate(
    tide_label = ifelse(tide_state == "high_tide", " (high)", ""),
    site_label = paste0(site, tide_label)
  )

p6 <- ggplot(p6_data, aes(x = cwd_SA_m2, y = total_mg, color = campaign)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.2) +
  geom_vline(xintercept = cwd_sa_default, linetype = "dashed", color = "grey50") +
  facet_wrap(~ site_label, scales = "free_y") +
  labs(x = expression(CWD~SA~(m^2~plot^{-1})),
       y = expression(Total~CH[4]~(mg~m^{-2}~d^{-1})),
       title = "CWD surface area sensitivity",
       subtitle = paste0("Dashed = default (", cwd_sa_default, " m2)"),
       color = "Campaign") +
  theme_pub(11)
save_pub(p6, "cwd_sensitivity", 260, 160)

# --- Fig 7: Height profile + fits ---------------------------------------------
pred_lines <- expand.grid(
  plot = tls_sites, campaign = factor(campaigns, levels = campaigns),
  height_m = seq(0, 12, by = 0.1), stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(pred_flux = predict_stem_flux(height_m, plot, campaign, exp_models)) %>%
  ungroup() %>%
  filter(!is.na(pred_flux))

p7a <- ggplot(tls_stem, aes(x = height_bin_num, y = stem_SA_m2, color = site)) +
  geom_line(linewidth = 0.8) + geom_point(size = 0.7) +
  labs(x = "Height (m)", y = expression(Stem~SA~(m^2~per~0.5~m~bin)),
       title = "TLS stem surface area by height") +
  theme_pub(10) + scale_color_brewer(palette = "Set1", name = "Site")

p7b <- ggplot(stem_pos, aes(x = height_m, y = CH4_best.flux)) +
  geom_point(aes(color = plot), alpha = 0.4, size = 1) +
  geom_line(data = pred_lines, aes(y = pred_flux, color = plot),
            linewidth = 0.7, linetype = "dashed") +
  facet_wrap(~ campaign) +
  scale_y_log10(labels = label_comma()) +
  scale_color_brewer(palette = "Set1", name = "Site") +
  geom_vline(xintercept = 1.5, linetype = "dotted", color = "grey50") +
  labs(x = "Height (m)", y = expression(CH[4]~(nmol~m^{-2}~s^{-1})),
       title = "Stem flux vs height: data + exponential fits") +
  theme_pub(10)

p7 <- p7a / p7b + plot_annotation(tag_levels = "a")
save_pub(p7, "height_profile_fits", 240, 230)

# --- Fig 8: Stem height zone breakdown (lowest on bottom) ---------------------
height_zone_levels <- c("0-0.5 m", "0.5-1.0 m", "1.0-1.5 m", "1.5-2.0 m", "> 2.0 m (extrap.)")

stem_hz <- list()
for (camp in campaigns) {
  for (site_name in tls_sites) {
    stls <- tls_stem %>% filter(site == site_name)
    for (i in seq_len(nrow(stls))) {
      h <- stls$height_mid_m[i]; sa <- stls$stem_SA_m2[i]
      fp <- predict_stem_flux(h, site_name, camp, exp_models)
      hz <- case_when(
        h < 0.5  ~ "0-0.5 m", h < 1.0  ~ "0.5-1.0 m",
        h < 1.5  ~ "1.0-1.5 m", h < 2.0  ~ "1.5-2.0 m",
        TRUE     ~ "> 2.0 m (extrap.)"
      )
      stem_hz <- c(stem_hz, list(data.frame(
        site = site_name, campaign = camp, height_zone = hz,
        flux_nmol_s = ifelse(is.na(fp), 0, fp * sa),
        stringsAsFactors = FALSE
      )))
    }
  }
}

stem_hz_df <- bind_rows(stem_hz) %>%
  mutate(
    campaign = factor(campaign, levels = campaigns),
    height_zone = factor(height_zone, levels = height_zone_levels)
  ) %>%
  group_by(site, campaign, height_zone) %>%
  summarise(flux_nmol_s = sum(flux_nmol_s), .groups = "drop")

p8 <- ggplot(stem_hz_df, aes(x = campaign, y = flux_nmol_s, fill = height_zone)) +
  geom_col(position = position_stack(reverse = TRUE), color = "white", linewidth = 0.2) +
  facet_wrap(~ site, scales = "free_y", nrow = 1) +
  scale_fill_viridis_d(name = "Height zone", option = "D", direction = 1, drop = FALSE) +
  labs(x = NULL, y = expression(Stem~CH[4]~(nmol~s^{-1})),
       title = "Stem flux by height zone (exponential)",
       subtitle = "Lowest heights on bottom") +
  theme_pub(10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
save_pub(p8, "stem_height_zone", 240, 130)

# --- Fig 8b: TLS surface area by segment class and height bin -----------------
sa_by_class <- tls_all %>%
  mutate(
    height_m = height_bin_num,
    segment_label = factor(segment_class,
      levels = c("root", "trunk", "branch"),
      labels = c("Root", "Trunk", "Branch"))
  )

seg_colors <- c("Root" = "#D2691E", "Trunk" = "#228B22", "Branch" = "#90EE90")

# Stacked bar: SA by segment class per height bin, faceted by site
p8b <- ggplot(sa_by_class,
              aes(x = height_m, y = Total_surface_area_m2, fill = segment_label)) +
  geom_col(position = position_stack(reverse = TRUE),
           width = 0.45, color = "grey40", linewidth = 0.1) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "grey50", linewidth = 0.3) +
  facet_wrap(~ site, scales = "free", nrow = 1) +
  scale_fill_manual(values = seg_colors, name = "Segment") +
  labs(x = "Height (m)",
       y = expression(Surface~area~(m^2~per~0.5~m~bin)),
       title = "TLS surface area by segment class and height",
       subtitle = "Dashed line = max flux measurement height (1.5 m)") +
  theme_pub(10)
save_pub(p8b, "SA_by_segment_height", 300, 120)

# Separate panel per segment class (easier to see root/branch individually)
p8c <- ggplot(sa_by_class,
              aes(x = height_m, y = Total_surface_area_m2, fill = site)) +
  geom_col(position = position_dodge(0.45), width = 0.4,
           color = "grey40", linewidth = 0.1) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "grey50", linewidth = 0.3) +
  facet_wrap(~ segment_label, scales = "free", nrow = 1) +
  scale_fill_brewer(palette = "Set1", name = "Site") +
  labs(x = "Height (m)",
       y = expression(Surface~area~(m^2~per~0.5~m~bin)),
       title = "TLS surface area by height: trunk, branch, root",
       subtitle = "Dashed line = max flux measurement height (1.5 m)") +
  theme_pub(10)
save_pub(p8c, "SA_by_segment_class", 300, 130)

# Combined: stacked trunk+branch with root below, per site (full profile)
p8d_data <- sa_by_class %>%
  mutate(
    sa_per_m2 = Total_surface_area_m2 /
      tree_stats$area_m2[match(site, tree_stats$site)]
  )

p8d <- ggplot(p8d_data,
              aes(x = height_m, y = sa_per_m2, fill = segment_label)) +
  geom_col(position = position_stack(reverse = TRUE),
           width = 0.45, color = NA) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "grey50", linewidth = 0.3) +
  facet_wrap(~ site, scales = "free", nrow = 2) +
  scale_fill_manual(values = seg_colors, name = "Segment") +
  labs(x = "Height (m)",
       y = expression(SA~density~(m^2~m^{-2}~ground~per~bin)),
       title = "TLS surface area density profiles",
       subtitle = "Normalized to plot ground area; dashed = max measurement height") +
  theme_pub(10)
save_pub(p8d, "SA_density_profiles", 260, 180)

# --- Fig 9: Component SA density by site --------------------------------------
sa_long <- data.frame(
  site = rep(tls_sites, each = 5),
  component = rep(c("stem", "root", "soil_max", "water_max", "cwd"), times = 4),
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    plot_area = tree_stats$area_m2[tree_stats$site == site],
    surface_area_m2 = case_when(
      component == "stem"      ~ tls_stem_total$total_stem_SA_m2[tls_stem_total$site == site],
      component == "root"      ~ tls_root_sa$root_SA_m2[tls_root_sa$site == site],
      component == "soil_max"  ~ tree_stats$ground_area_m2[tree_stats$site == site],
      component == "water_max" ~ tree_stats$ground_area_m2[tree_stats$site == site],
      component == "cwd"       ~ cwd_sa_default
    ),
    sa_per_m2 = surface_area_m2 / plot_area,
    sa_per_ha = sa_per_m2 * 10000,
    comp_label = case_when(
      component == "stem"      ~ "Stem\n(trunk+branch)",
      component == "root"      ~ "Root",
      component == "soil_max"  ~ "Soil\n(max = ground)",
      component == "water_max" ~ "Water\n(max = ground)",
      component == "cwd"       ~ paste0("CWD\n(est. ", cwd_sa_default, " m2)")
    )
  ) %>%
  ungroup()

sa_long$comp_label <- factor(sa_long$comp_label,
  levels = c("Soil\n(max = ground)", "Water\n(max = ground)",
             "Root", paste0("CWD\n(est. ", cwd_sa_default, " m2)"),
             "Stem\n(trunk+branch)"))

p9 <- ggplot(sa_long, aes(x = comp_label, y = sa_per_m2, fill = site)) +
  geom_col(position = position_dodge(0.8), width = 0.7, color = "grey30", linewidth = 0.2) +
  scale_fill_brewer(palette = "Set1", name = "Site") +
  labs(x = NULL, y = expression(SA~density~(m^2~m^{-2}~ground)),
       title = "Component surface area density",
       subtitle = "Soil/water = max possible (full ground area); actual depends on tidal state") +
  theme_pub(11) +
  theme(axis.text.x = element_text(size = 9))
save_pub(p9, "component_SA_density", 260, 140)

p9b <- ggplot(sa_long, aes(x = comp_label, y = sa_per_ha, fill = site)) +
  geom_col(position = position_dodge(0.8), width = 0.7, color = "grey30", linewidth = 0.2) +
  scale_fill_brewer(palette = "Set1", name = "Site") +
  labs(x = NULL, y = expression(SA~density~(m^2~ha^{-1})),
       title = "Component surface area density") +
  theme_pub(11) +
  theme(axis.text.x = element_text(size = 9))
save_pub(p9b, "component_SA_density_per_ha", 260, 140)

# --- Fig 10: Budget decomposition (SA × flux rate = contribution) -------------
for (camp_sel in campaigns) {
  bud <- budget_df %>%
    filter(campaign == camp_sel, tide_state %in% c("fixed", "high_tide"))
  if (nrow(bud) == 0 || all(bud$total_nmol_s == 0)) next

  bud$component <- factor(bud$component,
    levels = c("soil", "water", "root", "cwd", "stem_measured", "stem_extrapolated"))

  # (a) Flux rate (only for non-stem components that have a single rate)
  pa <- ggplot(bud %>% filter(!is.na(flux_rate)),
               aes(x = component, y = flux_rate, fill = component)) +
    geom_col(width = 0.7) +
    geom_errorbar(aes(ymin = pmax(flux_ci_lo, 0), ymax = flux_ci_hi), width = 0.2) +
    facet_wrap(~ site, nrow = 1, scales = "free_y") +
    scale_fill_manual(values = component_colors_7, guide = "none") +
    labs(x = NULL, y = expression(Flux~rate~(nmol~m^{-2}~s^{-1})),
         subtitle = "(a) Flux rate") +
    theme_pub(9) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

  # (b) Surface area density
  pb <- ggplot(bud %>% filter(!is.na(sa_per_m2)),
               aes(x = component, y = sa_per_m2, fill = component)) +
    geom_col(width = 0.7) +
    facet_wrap(~ site, nrow = 1, scales = "free_y") +
    scale_fill_manual(values = component_colors_7, guide = "none") +
    labs(x = NULL, y = expression(SA~(m^2~m^{-2}~ground)),
         subtitle = "(b) Surface area density") +
    theme_pub(9) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

  # (c) Stacked total contribution
  pc <- ggplot(bud, aes(x = site, y = total_mg_m2_d, fill = component)) +
    geom_col(position = position_stack(reverse = TRUE), color = "white", linewidth = 0.3) +
    scale_fill_manual(values = component_colors_7, name = "Component") +
    labs(x = NULL, y = expression(mg~CH[4]~m^{-2}~d^{-1}),
         subtitle = "(c) Integrated flux") +
    theme_pub(9)

  # (d) % of budget
  bud_pct <- bud %>%
    group_by(site) %>%
    mutate(total = sum(total_mg_m2_d),
           pct = ifelse(total > 0, total_mg_m2_d / total * 100, 0)) %>%
    ungroup()

  pd <- ggplot(bud_pct, aes(x = site, y = pct, fill = component)) +
    geom_col(position = position_stack(reverse = TRUE), color = "white", linewidth = 0.3) +
    scale_fill_manual(values = component_colors_7, name = "Component") +
    geom_hline(yintercept = 50, linetype = "dotted", alpha = 0.3) +
    labs(x = NULL, y = "% of budget",
         subtitle = "(d) % contribution") +
    theme_pub(9)

  p10 <- (pa / pb / (pc | pd)) +
    plot_annotation(title = paste0("CH4 budget decomposition \u2014 ", camp_sel),
                    subtitle = "High tide for tidal sites")

  camp_tag <- gsub(" ", "_", camp_sel)
  save_pub(p10, paste0("budget_decomposition_", camp_tag), 300, 280)
}

# --- Fig 11: Heatmap of flux rates (with gap-fill flags) ----------------------
heatmap_data <- flux_table %>%
  mutate(
    gap_filled = fill_source != "site" & fill_source != "missing",
    is_missing = fill_source == "missing",
    label = case_when(
      is_missing ~ "",
      gap_filled ~ paste0(sprintf("%.1f", flux_rate), "*"),
      TRUE ~ sprintf("%.1f", flux_rate)
    ),
    component = factor(component, levels = c("soil", "water", "root", "cwd"))
  )

stem_hm <- stem_boot %>%
  group_by(plot, campaign) %>%
  summarise(flux_rate = weighted.mean(mean, n, na.rm = TRUE), .groups = "drop") %>%
  mutate(component = factor("stem"), gap_filled = FALSE, is_missing = FALSE,
         label = sprintf("%.1f", flux_rate))

heatmap_all <- bind_rows(
  heatmap_data %>% select(site, campaign, component, flux_rate, gap_filled, is_missing, label) %>%
    rename(plot = site),
  stem_hm %>% select(plot, campaign, component, flux_rate, gap_filled, is_missing, label)
) %>%
  mutate(component = factor(component, levels = c("soil", "water", "root", "cwd", "stem")))

p11 <- ggplot(heatmap_all %>% filter(!is_missing),
              aes(x = campaign, y = component, fill = flux_rate)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = label), size = 2.8) +
  facet_wrap(~ plot, nrow = 1) +
  scale_fill_viridis_c(name = expression(nmol~m^{-2}~s^{-1}),
                        option = "C", trans = "pseudo_log") +
  labs(x = NULL, y = NULL,
       title = expression(Component~CH[4]~flux~rates~by~campaign),
       subtitle = "* = gap-filled; blank = not measured (set to 0 in budget)") +
  theme_pub(10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
save_pub(p11, "flux_rate_heatmap", 240, 120)

# --- Fig 12: Disturbance comparison ------------------------------------------
p12_data <- results_df %>%
  filter(scenario == "exponential",
         tide_state %in% c("fixed", "high_tide")) %>%
  mutate(
    tide_label = ifelse(tide_state == "high_tide", " (high)", ""),
    site_label = paste0(site, tide_label)
  )

p12 <- ggplot(p12_data, aes(x = campaign, y = total_mg, fill = disturbance_level)) +
  geom_col(position = position_dodge(0.8), width = 0.7,
           color = "grey30", linewidth = 0.2) +
  geom_text(aes(label = site_label), position = position_dodge(0.8),
            vjust = -0.3, size = 2.5) +
  scale_fill_manual(values = c("healthy" = "#228B22", "ghost" = "#8B4513"),
                    name = "Disturbance") +
  labs(x = NULL, y = expression(Total~CH[4]~(mg~m^{-2}~d^{-1})),
       title = "Plot-level CH4 by disturbance class",
       subtitle = "High tide for tidal sites") +
  theme_pub(12)
save_pub(p12, "disturbance_comparison", 200, 140)

# --- Fig 13: Height extrapolation sensitivity — stem flux ---------------------
# Dot plot showing range of stem estimates under each scenario
extrap_colors <- c(
  "Zero above 1.5 m"        = "#D55E00",
  "Exponential (asym. = 0)" = "#E69F00",
  "Linear (clamped >= 0)"   = "#009E73",
  "Linear (obs. range cap)" = "#0072B2",
  "Exp. (free asymptote)"   = "#CC79A7",
  "Constant above 1.5 m"   = "#56B4E9"
)

p13 <- ggplot(extrap_sens_df,
              aes(x = scenario_label, y = stem_mg_m2_d,
                  color = scenario_label, shape = campaign)) +
  geom_point(size = 2.5, position = position_dodge(0.5)) +
  facet_wrap(~ site, scales = "free_y", nrow = 1) +
  scale_color_manual(values = extrap_colors, guide = "none") +
  labs(x = NULL, y = expression(Stem~CH[4]~(mg~m^{-2}~d^{-1})),
       title = "Stem flux sensitivity to height extrapolation",
       subtitle = "Each point = one campaign; different colors = different extrapolation assumptions",
       shape = "Campaign") +
  theme_pub(10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
save_pub(p13, "height_extrap_sensitivity_stem", 300, 160)

# Total budget version
p13b <- ggplot(extrap_sens_df,
               aes(x = scenario_label, y = total_mg_m2_d,
                   color = scenario_label, shape = campaign)) +
  geom_point(size = 2.5, position = position_dodge(0.5)) +
  facet_wrap(~ site, scales = "free_y", nrow = 1) +
  scale_color_manual(values = extrap_colors, guide = "none") +
  labs(x = NULL, y = expression(Total~CH[4]~(mg~m^{-2}~d^{-1})),
       title = "Total plot budget sensitivity to stem height extrapolation",
       subtitle = "Non-stem components held constant; high tide for tidal sites",
       shape = "Campaign") +
  theme_pub(10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
save_pub(p13b, "height_extrap_sensitivity_total", 300, 160)

# --- Fig 14: Height extrapolation profiles — predicted flux vs height ---------
# Show how each model predicts flux across the full height range
# Add raw data points and a vertical line at max measurement height
stem_raw_pts <- flux_raw %>%
  filter(component == "stem", !is.na(height_corrected), height_corrected >= 0,
         !is.na(CH4_best.flux)) %>%
  select(site = plot, campaign, height_m, CH4_best.flux)

# Cap extreme predictions for readable profiles
# Use per-site data range to set reasonable y limits
prof_plot_df <- extrap_prof_df %>%
  group_by(site, campaign) %>%
  mutate(
    obs_max = max(stem_raw_pts$CH4_best.flux[stem_raw_pts$site == first(site) &
                  stem_raw_pts$campaign == first(campaign)], na.rm = TRUE),
    obs_min = min(stem_raw_pts$CH4_best.flux[stem_raw_pts$site == first(site) &
                  stem_raw_pts$campaign == first(campaign)], na.rm = TRUE),
    y_cap_hi = obs_max * 3,
    y_cap_lo = obs_min * 3,
    pred_capped = pmin(pmax(pred_flux, y_cap_lo), y_cap_hi)
  ) %>%
  ungroup()

p14 <- ggplot(prof_plot_df, aes(x = height_m, y = pred_capped,
                                   color = scenario_label)) +
  geom_line(linewidth = 0.6, na.rm = TRUE) +
  geom_point(data = stem_raw_pts, aes(x = height_m, y = CH4_best.flux),
             color = "black", alpha = 0.3, size = 0.8, inherit.aes = FALSE) +
  geom_vline(xintercept = max_meas_h, linetype = "dotted", color = "grey50") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.3) +
  facet_grid(campaign ~ site, scales = "free") +
  scale_color_manual(values = extrap_colors, name = "Extrapolation\nscenario") +
  labs(x = "Height (m)", y = expression(Predicted~CH[4]~flux~(nmol~m^{-2}~s^{-1})),
       title = "Height extrapolation scenarios: predicted flux profiles",
       subtitle = "Dotted line = max measured height (~1.5 m); black dots = raw data") +
  theme_pub(9) +
  theme(legend.position = "bottom",
        legend.key.width = unit(15, "mm"))
save_pub(p14, "height_extrap_profiles", 300, 200)

# Log-scale version (drop negatives)
p14b <- ggplot(extrap_prof_df %>% filter(pred_flux > 0),
               aes(x = height_m, y = pred_flux, color = scenario_label)) +
  geom_line(linewidth = 0.6, na.rm = TRUE) +
  geom_point(data = stem_raw_pts %>% filter(CH4_best.flux > 0),
             aes(x = height_m, y = CH4_best.flux),
             color = "black", alpha = 0.3, size = 0.8, inherit.aes = FALSE) +
  geom_vline(xintercept = max_meas_h, linetype = "dotted", color = "grey50") +
  facet_grid(campaign ~ site, scales = "free") +
  scale_y_log10(labels = label_comma()) +
  scale_color_manual(values = extrap_colors, name = "Extrapolation\nscenario") +
  labs(x = "Height (m)", y = expression(Predicted~CH[4]~flux~(nmol~m^{-2}~s^{-1})),
       title = "Height extrapolation scenarios (log scale)",
       subtitle = "Dotted line = max measured height (~1.5 m); black dots = raw data (positive only)") +
  theme_pub(9) +
  theme(legend.position = "bottom",
        legend.key.width = unit(15, "mm"))
save_pub(p14b, "height_extrap_profiles_log", 300, 200)

# --- Fig 15: Range summary — min/max across scenarios per site ----------------
extrap_range <- extrap_sens_df %>%
  group_by(site, campaign, disturbance_level) %>%
  summarise(
    stem_min = min(stem_mg_m2_d, na.rm = TRUE),
    stem_max = max(stem_mg_m2_d, na.rm = TRUE),
    stem_exp = stem_mg_m2_d[scenario == "exp_zero_asym"],
    total_min = min(total_mg_m2_d, na.rm = TRUE),
    total_max = max(total_mg_m2_d, na.rm = TRUE),
    total_exp = total_mg_m2_d[scenario == "exp_zero_asym"],
    .groups = "drop"
  )

p15 <- ggplot(extrap_range, aes(x = campaign, y = stem_exp, color = site)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = stem_min, ymax = stem_max), width = 0.15, linewidth = 0.7) +
  facet_wrap(~ site, scales = "free_y", nrow = 1) +
  scale_color_brewer(palette = "Set1", guide = "none") +
  labs(x = NULL, y = expression(Stem~CH[4]~(mg~m^{-2}~d^{-1})),
       title = "Stem flux: range across extrapolation scenarios",
       subtitle = "Point = exponential (asym.=0); error bars = min/max across all 6 scenarios") +
  theme_pub(11)
save_pub(p15, "height_extrap_range", 260, 130)

# =============================================================================
# PUBLICATION-QUALITY FIGURES WITH PROPAGATED UNCERTAINTY
# =============================================================================
cat("\n=== Publication figures ===\n")

comp_levels_7 <- c("soil", "water", "root", "cwd", "stem_measured", "stem_extrapolated")
comp_labels_7 <- c("Soil", "Water", "Root", "CWD",
                    "Stem (measured)", "Stem (extrapolated)")

# --- Pub Fig 1: Grouped bars with per-component SE, stem split ----------------
# One panel per site × campaign, bars = components, error bars = MC SE
mc_plot_df <- mc_budget_df %>%
  filter(component != "total",
         tide_state %in% c("fixed", "high_tide")) %>%
  mutate(
    component = factor(component, levels = comp_levels_7, labels = comp_labels_7),
    tide_label = ifelse(tide_state == "high_tide", " (high)", ""),
    panel_label = paste0(site, tide_label)
  )

pub1a <- ggplot(mc_plot_df,
                aes(x = component, y = mc_mean, fill = component)) +
  geom_col(width = 0.7, color = "grey30", linewidth = 0.15) +
  geom_errorbar(aes(ymin = mc_ci_lo, ymax = mc_ci_hi), width = 0.25, linewidth = 0.4) +
  facet_grid(campaign ~ site, scales = "free_y") +
  scale_fill_manual(values = setNames(component_colors_7, comp_labels_7), guide = "none") +
  labs(x = NULL, y = expression(CH[4]~flux~(mg~m^{-2}~d^{-1})),
       subtitle = "(a) Component fluxes with 95% CI (flux + SA uncertainty)") +
  theme_pub(9) +
  theme(axis.text.x = element_text(angle = 55, hjust = 1, size = 6.5))

# --- Pub Fig 1b: Stacked bars with total error bar ----------------------------
mc_stack_df <- mc_budget_df %>%
  filter(component != "total",
         tide_state %in% c("fixed", "high_tide")) %>%
  mutate(component = factor(component, levels = comp_levels_7))

mc_total_df <- mc_budget_df %>%
  filter(component == "total",
         tide_state %in% c("fixed", "high_tide"))

pub1b <- ggplot(mc_stack_df, aes(x = campaign, y = mc_mean, fill = component)) +
  geom_col(position = position_stack(reverse = TRUE),
           color = "white", linewidth = 0.3) +
  geom_errorbar(data = mc_total_df,
                aes(x = campaign, y = mc_mean,
                    ymin = mc_ci_lo, ymax = mc_ci_hi, fill = NULL),
                width = 0.2, linewidth = 0.5, inherit.aes = FALSE) +
  facet_wrap(~ site, nrow = 1, scales = "free_y") +
  scale_fill_manual(values = component_colors_7,
                    labels = comp_labels_7,
                    name = "Component") +
  labs(x = NULL, y = expression(CH[4]~flux~(mg~m^{-2}~d^{-1})),
       subtitle = "(b) Stacked budget with 95% CI on total") +
  theme_pub(9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

pub1 <- pub1a / pub1b +
  plot_annotation(
    title = expression(Plot-level~CH[4]~budget~by~component),
    subtitle = "Error bars propagate flux rate + TLS surface area uncertainty via Monte Carlo (N = 5000)"
  ) +
  plot_layout(heights = c(2, 1))
save_pub(pub1, "pub_component_budget", 300, 260)

# --- Pub Fig 2: Dumbbell range chart for height extrapolation sensitivity ------
extrap_dumbbell <- extrap_sens_df %>%
  group_by(site, campaign, disturbance_level) %>%
  summarise(
    total_min = min(total_mg_m2_d, na.rm = TRUE),
    total_max = max(total_mg_m2_d, na.rm = TRUE),
    total_default = total_mg_m2_d[scenario == "exp_zero_asym"],
    stem_min = min(stem_mg_m2_d, na.rm = TRUE),
    stem_max = max(stem_mg_m2_d, na.rm = TRUE),
    stem_default = stem_mg_m2_d[scenario == "exp_zero_asym"],
    .groups = "drop"
  )

pub2a <- ggplot(extrap_dumbbell, aes(y = site, color = campaign)) +
  geom_segment(aes(x = stem_min, xend = stem_max, yend = site),
               linewidth = 2, alpha = 0.3,
               position = position_dodge(0.5)) +
  geom_point(aes(x = stem_default), size = 3,
             position = position_dodge(0.5)) +
  scale_color_manual(values = c("Oct 2022" = "#E69F00", "Mar 2023" = "#56B4E9"),
                     name = "Campaign") +
  labs(x = expression(Stem~CH[4]~(mg~m^{-2}~d^{-1})),
       y = NULL,
       subtitle = "(a) Stem flux only") +
  theme_pub(10)

pub2b <- ggplot(extrap_dumbbell, aes(y = site, color = campaign)) +
  geom_segment(aes(x = total_min, xend = total_max, yend = site),
               linewidth = 2, alpha = 0.3,
               position = position_dodge(0.5)) +
  geom_point(aes(x = total_default), size = 3,
             position = position_dodge(0.5)) +
  scale_color_manual(values = c("Oct 2022" = "#E69F00", "Mar 2023" = "#56B4E9"),
                     name = "Campaign") +
  labs(x = expression(Total~CH[4]~(mg~m^{-2}~d^{-1})),
       y = NULL,
       subtitle = "(b) Total plot budget") +
  theme_pub(10)

pub2 <- (pub2a | pub2b) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Sensitivity to height extrapolation scenario",
    subtitle = "Point = default (exponential, asym.=0); bar = range across 6 scenarios"
  )
save_pub(pub2, "pub_extrap_dumbbell", 280, 120)

# --- Pub Fig 3: Profiles with ribbon envelope + SA inset ----------------------
# Envelope: min/max across scenarios at each height
extrap_envelope <- extrap_prof_df %>%
  group_by(site, campaign, height_m) %>%
  summarise(
    pred_min = min(pred_flux, na.rm = TRUE),
    pred_max = max(pred_flux, na.rm = TRUE),
    pred_default = pred_flux[scenario_label == "Exponential (asym. = 0)"],
    .groups = "drop"
  ) %>%
  # Cap for readability
  mutate(
    pred_max = pmin(pred_max, 150),
    pred_min = pmax(pred_min, -5)
  )

stem_raw_pts2 <- flux_raw %>%
  filter(component == "stem", !is.na(height_corrected), height_corrected >= 0,
         !is.na(CH4_best.flux)) %>%
  select(site = plot, campaign, height_m, CH4_best.flux)

pub3a <- ggplot(extrap_envelope, aes(x = height_m)) +
  geom_ribbon(aes(ymin = pred_min, ymax = pred_max), fill = "grey80", alpha = 0.6) +
  geom_line(aes(y = pred_default), color = "#E69F00", linewidth = 0.8) +
  geom_point(data = stem_raw_pts2, aes(y = CH4_best.flux),
             color = "black", alpha = 0.35, size = 0.7) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "grey50", linewidth = 0.3) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey70") +
  facet_grid(campaign ~ site, scales = "free") +
  labs(x = "Height (m)",
       y = expression(CH[4]~flux~(nmol~m^{-2}~s^{-1})),
       subtitle = "(a) Predicted flux: ribbon = scenario range, line = default (exp., asym.=0)") +
  theme_pub(9)

# TLS SA profile as companion
sa_profile <- tls_stem %>%
  mutate(height_m = height_bin_num + 0.25)

pub3b <- ggplot(sa_profile, aes(x = height_m, y = stem_SA_m2)) +
  geom_col(width = 0.45, fill = "#228B22", alpha = 0.7, color = "grey40", linewidth = 0.1) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "grey50", linewidth = 0.3) +
  facet_wrap(~ site, nrow = 1, scales = "free") +
  labs(x = "Height (m)", y = expression(Stem~SA~(m^2~per~bin)),
       subtitle = "(b) TLS stem surface area by 0.5 m height bin") +
  theme_pub(9)

pub3 <- pub3a / pub3b +
  plot_layout(heights = c(2, 1)) +
  plot_annotation(
    title = "Stem flux height profiles and TLS surface area",
    subtitle = "Dashed line = max measured height (1.5 m)"
  )
save_pub(pub3, "pub_height_profiles", 300, 260)

# --- Pub Fig 4: Uncertainty decomposition tornado chart -----------------------
# Compare MC CI width when only flux varies vs only SA varies
# Re-run MC with SA fixed, then with flux fixed
unc_decomp <- list()
for (camp in campaigns) {
  for (site_name in tls_sites) {
    mc_row <- mc_budget_df %>%
      filter(site == site_name, campaign == camp,
             tide_state %in% c("fixed", "high_tide"),
             component != "total")
    for (comp in mc_row$component) {
      full_se <- mc_row$mc_se[mc_row$component == comp]
      unc_decomp <- c(unc_decomp, list(data.frame(
        site = site_name, campaign = camp, component = comp,
        full_se = full_se,
        stringsAsFactors = FALSE
      )))
    }
  }
}
unc_decomp_df <- bind_rows(unc_decomp) %>%
  mutate(
    campaign = factor(campaign, levels = campaigns),
    component = factor(component, levels = comp_levels_7, labels = comp_labels_7)
  )

pub4 <- ggplot(unc_decomp_df %>% filter(full_se > 0),
               aes(x = component, y = full_se, fill = component)) +
  geom_col(width = 0.7, color = "grey30", linewidth = 0.15) +
  facet_grid(campaign ~ site, scales = "free") +
  scale_fill_manual(values = setNames(component_colors_7, comp_labels_7), guide = "none") +
  labs(x = NULL, y = expression(SE~of~component~flux~(mg~m^{-2}~d^{-1})),
       title = "Uncertainty by component (propagated SE)",
       subtitle = "Joint flux rate + SA uncertainty via Monte Carlo") +
  theme_pub(9) +
  theme(axis.text.x = element_text(angle = 55, hjust = 1, size = 6.5))
save_pub(pub4, "pub_uncertainty_decomp", 280, 160)

# --- Pub Fig 5: Clean disturbance-class comparison with MC error bars ---------
class_mc <- mc_budget_df %>%
  filter(component != "total",
         tide_state %in% c("fixed", "high_tide")) %>%
  left_join(site_meta, by = c("site", "disturbance_level")) %>%
  group_by(disturbance_level, campaign, component) %>%
  summarise(
    class_mean = mean(mc_mean),
    # propagate SE: SE of mean of 2 sites = sqrt(sum(se^2))/2
    class_se = sqrt(sum(mc_se^2)) / n(),
    .groups = "drop"
  ) %>%
  mutate(component = factor(component, levels = comp_levels_7, labels = comp_labels_7))

pub5 <- ggplot(class_mc, aes(x = component, y = class_mean, fill = component)) +
  geom_col(width = 0.7, color = "grey30", linewidth = 0.15) +
  geom_errorbar(aes(ymin = class_mean - 1.96 * class_se,
                    ymax = class_mean + 1.96 * class_se),
                width = 0.2, linewidth = 0.4) +
  facet_grid(campaign ~ disturbance_level, scales = "free_y") +
  scale_fill_manual(values = setNames(component_colors_7, comp_labels_7), guide = "none") +
  labs(x = NULL, y = expression(CH[4]~flux~(mg~m^{-2}~d^{-1})),
       title = "Disturbance-class mean CH4 by component",
       subtitle = "Equally weighted site average; error bars = 95% CI (propagated)") +
  theme_pub(10) +
  theme(axis.text.x = element_text(angle = 55, hjust = 1, size = 7))
save_pub(pub5, "pub_class_comparison", 240, 180)

# Save MC results
write.csv(mc_budget_df, file.path(output_dir, "mc_component_uncertainty.csv"), row.names = FALSE)

# --- Final summary ------------------------------------------------------------
cat("\n=== FINAL SUMMARY ===\n")

cat("\nGap-fills applied:\n")
flux_table %>%
  filter(fill_source != "site") %>%
  select(site, campaign, component, flux_rate, fill_source) %>%
  as.data.frame() %>%
  print()

cat("\nMissing (set to 0 in budget):\n")
flux_table %>%
  filter(fill_source == "missing") %>%
  select(site, campaign, component) %>%
  as.data.frame() %>%
  print()

cat("\nKey assumptions:\n")
cat("  STEM:  TLS trunk+branch SA; exponential height model\n")
cat("  ROOT:  TLS root SA; CP40/FLM30 root rate gap-filled from other ghost site\n")
cat("  SOIL:  ground_area × (1-frac_flooded); ground_area uses ground-level TLS cross-section\n")
cat("  WATER: ground_area × frac_flooded\n")
cat("    CP40/FLM30: always flooded (Oct 2022 & Mar 2023)\n")
cat("    SRS5/SRS6: tidal — high/low tide endpoints\n")
cat("    SRS6 Oct 2022 water: gap-filled from SRS5\n")
cat("  CWD:   default", cwd_sa_default, "m2 SA (sensitivity 0-200 m2)\n")
cat("  Campaigns: Oct 2022 (wet) and Mar 2023 (dry) only; Mar 2022 dropped\n")

cat("\nDone. Outputs in:", output_dir, "\n")
