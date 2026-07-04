# ==============================================================================
# goflux_reprocess_ebullition.R
#
# Reprocesses detected chamber placement traces through goFlux to get proper
# HM/LM diffusive flux estimates, then combines with ebullitive component
# for total partitioned CH4 flux.
#
# Runs AFTER detect_ebullition.R (requires all_traces.rds output).
# ==============================================================================

library(goFlux)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(patchwork)

# ---- PATHS -------------------------------------------------------------------

OUTPUT_DIR <- "output/ebullition"
PLACEMENTS_CSV <- file.path(OUTPUT_DIR, "placements_summary.csv")
TRACES_RDS     <- file.path(OUTPUT_DIR, "all_traces.rds")
PROCESSED_FLUX <- "output/data_products/soil_water_surface_fluxes.csv"

# ---- LOAD DATA ---------------------------------------------------------------

cat("Loading data...\n")

all_traces   <- readRDS(TRACES_RDS)
placements   <- read_csv(PLACEMENTS_CSV, show_col_types = FALSE) %>%
  filter(!excluded)
water_fluxes <- read_csv(PROCESSED_FLUX, show_col_types = FALSE) %>%
  filter(component == "water")

cat("Traces:", length(all_traces), "\n")
cat("Good placements:", nrow(placements), "\n")
cat("Processed water fluxes:", nrow(water_fluxes), "\n")

# ---- CHAMBER PARAMETERS -----------------------------------------------------
# All water measurements use "Floating 8 in" chamber.
# Volume components differ only by analyzer (LGR vs Picarro).

CHAMBER_PARAMS <- list(
  LGR = list(
    Area   = 324.3,      # cm2
    offset = 0,          # cm (floating chamber)
    Vcham  = 3370,       # cm3 (chamber + collar)
    Vtube  = 29,         # cm3
    Vinst  = 919,        # cm3 (analyzer cell 70 + drierite large 849)
    Vtot   = 4.318,      # L
    Pcham  = 101.325     # kPa
  ),
  Picarro = list(
    Area   = 324.3,
    offset = 0,
    Vcham  = 3370,
    Vtube  = 29,
    Vinst  = 884,        # analyzer cell 35 + drierite large 849
    Vtot   = 4.283,
    Pcham  = 101.325
  )
)

# Temperature lookup: mean air_temp per site × date from processed fluxes
temp_lookup <- water_fluxes %>%
  group_by(plot, date) %>%
  summarise(mean_Tcham = mean(air_temp, na.rm = TRUE), .groups = "drop")

get_params <- function(analyzer, site, meas_date) {
  key <- if (grepl("LGR", analyzer)) "LGR" else "Picarro"
  params <- CHAMBER_PARAMS[[key]]

  # Get temperature
  tl <- temp_lookup %>% filter(plot == site, date == meas_date)
  if (nrow(tl) > 0 && !is.na(tl$mean_Tcham[1])) {
    params$Tcham <- tl$mean_Tcham[1]
  } else {
    # Fallback: seasonal default
    params$Tcham <- if (month(meas_date) == 10) 28.0 else 25.0
  }
  params
}

# ---- BUILD goFLUX-COMPATIBLE DATAFRAMES --------------------------------------

cat("\nConstructing goFlux inputs from cleaned traces...\n")

MIN_CLEAN_PTS <- 6   # minimum non-jump points to run goFlux

goflux_dfs   <- list()
ebull_info   <- list()
skipped_pids <- character()

for (pid in placements$placement_id) {
  if (!(pid %in% names(all_traces))) {
    skipped_pids <- c(skipped_pids, pid)
    next
  }

  res <- all_traces[[pid]]
  tr  <- res$trace
  pl  <- placements %>% filter(placement_id == pid)

  # Use de-ebulliated trace: cumulative jump magnitude subtracted from
  # all post-jump points, so the diffusive fit only sees the steady
  # background rise without the persistent step-ups from bubbles.
  # Also exclude ±15s buffer around each jump (equilibration period).
  if (!"CH4_deebull" %in% names(tr)) {
    tr$cum_jump <- cumsum(ifelse(tr$is_jump, tr$dCH4, 0))
    tr$CH4_deebull <- tr$CH4_ppm - tr$cum_jump
  }
  EBULL_BUFFER_SEC <- 15
  jump_times <- tr$elapsed_sec[tr$is_jump]
  if (!"near_jump" %in% names(tr)) {
    tr$near_jump <- if (length(jump_times) > 0) {
      sapply(tr$elapsed_sec, function(t) any(abs(t - jump_times) <= EBULL_BUFFER_SEC))
    } else {
      rep(FALSE, nrow(tr))
    }
  }
  clean <- tr %>% filter(!near_jump)

  if (nrow(clean) < MIN_CLEAN_PTS) {
    skipped_pids <- c(skipped_pids, pid)
    next
  }

  # Chamber parameters
  params <- get_params(pl$analyzer[1], pl$site[1], pl$date[1])

  # Instrument precision (ppb for CH4, ppm for CO2)
  ch4_prec <- if (grepl("LGR", pl$analyzer[1])) 0.9 else 1.0  # ppb
  co2_prec <- if (grepl("LGR", pl$analyzer[1])) 0.35 else 0.2 # ppm

  # Build goFlux-compatible dataframe
  # flag: 1 = measurement data, 2 = end of measurement (goFlux only uses flag >= 1)
  n_clean <- nrow(clean)
  flag_vec <- rep(1L, n_clean)
  flag_vec[n_clean] <- 2L    # end of measurement

  gf_df <- tibble(
    UniqueID    = pid,
    POSIX.time  = clean$datetime,
    CH4dry_ppb  = clean$CH4_deebull * 1000,  # de-ebulliated ppm -> ppb
    CO2dry_ppm  = clean$CO2_ppm,
    H2O_ppm     = clean$H2O_ppm,
    CH4_prec    = ch4_prec,
    CO2_prec    = co2_prec,
    flag        = flag_vec,
    Etime       = as.numeric(difftime(clean$datetime, min(clean$datetime), units = "secs")),
    Area        = params$Area,
    offset      = params$offset,
    Vcham       = params$Vcham,
    Vtube       = params$Vtube,
    Vinst       = params$Vinst,
    Vtot        = params$Vtot,
    Tcham       = params$Tcham,
    Pcham       = params$Pcham
  )

  goflux_dfs[[pid]] <- gf_df

  # Store ebullition info for later combination
  ebull_info[[pid]] <- tibble(
    placement_id         = pid,
    n_jumps              = pl$n_jumps[1],
    total_ebullitive_ppm = pl$total_ebullitive_ppm[1],
    duration_sec         = pl$duration_sec[1],
    n_points_clean       = nrow(clean),
    n_points_total       = nrow(tr),
    Vtot                 = params$Vtot,
    Area                 = params$Area,
    Tcham                = params$Tcham,
    Pcham                = params$Pcham
  )
}

cat("Built goFlux inputs for", length(goflux_dfs), "traces\n")
if (length(skipped_pids) > 0) {
  cat("Skipped", length(skipped_pids), "traces (missing or too few clean points)\n")
}

# Combine into one dataframe for goFlux
all_cleaned <- bind_rows(goflux_dfs)
cat("Total observations for goFlux:", nrow(all_cleaned), "\n")

# ---- RUN goFLUX --------------------------------------------------------------

cat("\nRunning goFlux() for CH4...\n")

CH4_flux <- tryCatch(

  goFlux(dataframe = all_cleaned, gastype = "CH4dry_ppb",
         H2O_col = "H2O_ppm", warn.length = 15),
  error = function(e) {
    cat("goFlux error:", conditionMessage(e), "\n")
    NULL
  }
)

if (is.null(CH4_flux)) {
  stop("goFlux() failed. Check input data format.")
}

cat("goFlux completed for", length(unique(CH4_flux$UniqueID)), "traces\n")

# ---- RUN best.flux -----------------------------------------------------------

cat("Running best.flux() for model selection...\n")

CH4_best <- best.flux(
  flux.result = CH4_flux,
  criteria = c("MAE", "RMSE", "AICc", "SE", "g.factor", "kappa", "MDF",
               "nb.obs", "intercept", "p-value"),
  g.limit = 2,
  p.val = 0.05,
  k.ratio = 1,
  warn.length = 15
)

cat("Model selection:\n")
table(CH4_best$model) %>% print()

# ---- CALCULATE EBULLITIVE FLUX -----------------------------------------------

cat("\nCalculating ebullitive flux component...\n")

R_gas <- 8.314  # J/(mol·K)

ebull_df <- bind_rows(ebull_info) %>%
  mutate(
    T_K       = Tcham + 273.15,
    Area_m2   = Area / 1e4,
    # Conversion factor: ppm/s -> nmol/m2/s
    conv      = Pcham * Vtot / (R_gas * T_K * Area_m2) * 1e3,
    # Ebullitive flux = total jump magnitude / duration * conversion
    ebull_rate_ppm_s   = ifelse(n_jumps > 0, total_ebullitive_ppm / duration_sec, 0),
    ebull_flux_nmol    = ebull_rate_ppm_s * conv
  )

# ---- COMBINE PARTITIONED OUTPUT ----------------------------------------------

cat("Combining diffusive + ebullitive into partitioned fluxes...\n")

# Extract key columns from best.flux output
# goFlux reports flux in µmol/m2/s for CH4dry_ppb input — convert to nmol/m2/s
diffusive <- CH4_best %>%
  select(
    placement_id = UniqueID,
    diffusive_flux_umol = best.flux,
    diffusive_model     = model,
    diffusive_quality   = quality.check,
    LM_flux  = LM.flux,
    HM_flux  = HM.flux,
    LM_r2    = LM.r2,
    HM_r2    = HM.r2,
    LM_p_val = LM.p.val
  ) %>%
  mutate(
    # goFlux CH4 flux is in nmol/m2/s (since input is ppb = nmol/mol)
    diffusive_flux_nmol = diffusive_flux_umol,
    LM_flux_nmol = LM_flux,
    HM_flux_nmol = HM_flux
  )

# Join with ebullition data and placement metadata
partitioned <- placements %>%
  select(placement_id, site, date, season, analyzer, trace_type,
         matched_flux_id, diffusive_rate_clean_ppm_s) %>%
  inner_join(diffusive, by = "placement_id") %>%
  inner_join(
    ebull_df %>% select(placement_id, n_jumps, total_ebullitive_ppm,
                        duration_sec, n_points_clean, ebull_flux_nmol, conv),
    by = "placement_id"
  ) %>%
  mutate(
    # Simple LM rate converted for comparison
    simple_LM_flux_nmol = diffusive_rate_clean_ppm_s * conv,
    # Total = diffusive (goFlux) + ebullitive
    total_flux_nmol     = diffusive_flux_nmol + ebull_flux_nmol,
    ebullitive_fraction = ifelse(total_flux_nmol > 0,
                                 ebull_flux_nmol / total_flux_nmol, 0)
  )

cat("\nPartitioned fluxes: ", nrow(partitioned), " traces\n")
cat("HM selected:", sum(partitioned$diffusive_model == "HM", na.rm = TRUE), "\n")
cat("LM selected:", sum(partitioned$diffusive_model == "LM", na.rm = TRUE), "\n")

write_csv(partitioned, file.path(OUTPUT_DIR, "partitioned_fluxes.csv"))
cat("Saved to:", file.path(OUTPUT_DIR, "partitioned_fluxes.csv"), "\n")

# ---- SITE × SEASON SUMMARY --------------------------------------------------

cat("\n=== SITE × SEASON PARTITIONED SUMMARY ===\n\n")

# Processed flux means for comparison
proc_means <- water_fluxes %>%
  mutate(season = ifelse(season == "wet", "wet (Oct 2022)", "dry (Mar 2023)")) %>%
  group_by(site = plot, season) %>%
  summarise(n_wflux = n(), mean_proc_flux = mean(CH4_best.flux, na.rm = TRUE),
            .groups = "drop")

site_summary <- partitioned %>%
  group_by(site, season) %>%
  summarise(
    n_traces      = n(),
    n_processed   = sum(trace_type == "processed"),
    n_additional  = sum(trace_type == "additional"),
    n_HM          = sum(diffusive_model == "HM", na.rm = TRUE),
    n_LM          = sum(diffusive_model == "LM", na.rm = TRUE),
    n_with_ebull  = sum(n_jumps > 0),
    mean_diff     = mean(diffusive_flux_nmol, na.rm = TRUE),
    mean_ebull    = mean(ebull_flux_nmol, na.rm = TRUE),
    mean_total    = mean(total_flux_nmol, na.rm = TRUE),
    mean_simpleLM = mean(simple_LM_flux_nmol, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(proc_means, by = c("site", "season")) %>%
  mutate(
    diff_vs_proc  = mean_diff / mean_proc_flux,
    total_vs_proc = mean_total / mean_proc_flux,
    pct_ebullitive = round(mean_ebull / mean_total * 100, 1)
  )

write_csv(site_summary, file.path(OUTPUT_DIR, "site_season_partitioned.csv"))

# Print comparison table
cat(sprintf("%-6s %-18s %3s %3s %2s/%2s | %8s %8s %8s | %8s | %7s %7s | %5s\n",
    "Site", "Season", "Trc", "Ebu", "HM", "LM",
    "Diff(gF)", "Ebull.", "Total", "Proc.Fx",
    "Dif/Prc", "Tot/Prc", "% Ebu"))
cat(strrep("-", 115), "\n")
for (i in seq_len(nrow(site_summary))) {
  r <- site_summary[i, ]
  cat(sprintf("%-6s %-18s %3d %3d %2d/%2d | %8.2f %8.2f %8.2f | %8.2f | %6.2fx %6.2fx | %4.1f%%\n",
      r$site, r$season, r$n_traces, r$n_with_ebull, r$n_HM, r$n_LM,
      r$mean_diff, r$mean_ebull, r$mean_total,
      ifelse(is.na(r$mean_proc_flux), NA, r$mean_proc_flux),
      ifelse(is.na(r$diff_vs_proc), NA, r$diff_vs_proc),
      ifelse(is.na(r$total_vs_proc), NA, r$total_vs_proc),
      ifelse(is.na(r$pct_ebullitive), 0, r$pct_ebullitive)))
}

# Also compare simple LM vs goFlux diffusive
cat("\n=== SIMPLE LM vs goFLUX DIFFUSIVE (nmol/m2/s) ===\n\n")
cat(sprintf("%-6s %-18s | %8s %8s %8s | %8s\n",
    "Site", "Season", "SimpleLM", "goFlux", "Ratio", "Model mix"))
cat(strrep("-", 75), "\n")
for (i in seq_len(nrow(site_summary))) {
  r <- site_summary[i, ]
  ratio <- r$mean_diff / r$mean_simpleLM
  cat(sprintf("%-6s %-18s | %8.2f %8.2f %7.2fx | %dHM/%dLM\n",
      r$site, r$season, r$mean_simpleLM, r$mean_diff, ratio, r$n_HM, r$n_LM))
}

# ---- PER-PLACEMENT TRACE PLOTS ----------------------------------------------

cat("\n=== GENERATING PER-PLACEMENT PARTITIONED TRACE PLOTS ===\n")

trace_plots <- list()

for (i in seq_len(nrow(partitioned))) {
  row <- partitioned[i, ]
  pid <- row$placement_id

  if (!(pid %in% names(all_traces))) next
  res <- all_traces[[pid]]
  tr  <- res$trace

  if (nrow(tr) == 0) next

  # Get goFlux model parameters for this trace
  gf_row <- CH4_best %>% filter(UniqueID == pid)
  if (nrow(gf_row) == 0) next

  # Trace type styling
  trace_color <- if (row$trace_type == "processed") "steelblue" else "darkorange"
  type_label  <- if (row$trace_type == "processed") "Processed" else "Additional"

  trace_start <- min(tr$datetime)
  trace_end   <- max(tr$datetime)
  elapsed     <- as.numeric(difftime(tr$datetime, trace_start, units = "secs"))

  # Clean points for model overlay
  clean <- tr %>% filter(!is_jump)
  clean_elapsed <- as.numeric(difftime(clean$datetime, min(clean$datetime), units = "secs"))

  # Generate model prediction curve on clean time axis
  # LM: C(t) = C0 + slope * t  (C0 in ppb, slope from LM)
  lm_C0    <- gf_row$LM.C0
  lm_slope <- gf_row$LM.slope  # ppb/s ? or ppm/s
  # HM: C(t) uses Hutchinson-Mosier formula
  # For display, use the best model fit
  best_model <- gf_row$model

  # LM predicted values (in ppb -> ppm)
  lm_pred_ppb <- lm_C0 + gf_row$LM.slope * clean_elapsed
  lm_pred_ppm <- lm_pred_ppb / 1000

  # HM predicted values if available
  hm_pred_ppm <- NULL
  if (!is.na(gf_row$HM.C0) && !is.na(gf_row$HM.k)) {
    hm_C0 <- gf_row$HM.C0
    hm_Ci <- gf_row$HM.Ci
    hm_k  <- gf_row$HM.k
    # HM model: C(t) = Ci + (C0 - Ci) * exp(-k * t)
    hm_pred_ppb <- hm_Ci + (hm_C0 - hm_Ci) * exp(-hm_k * clean_elapsed)
    hm_pred_ppm <- hm_pred_ppb / 1000
  }

  # Best model prediction
  best_pred_ppm <- if (best_model == "HM" && !is.null(hm_pred_ppm)) hm_pred_ppm else lm_pred_ppm

  clean_plot <- tibble(datetime = clean$datetime, best_fit = best_pred_ppm,
                       lm_fit = lm_pred_ppm)
  if (!is.null(hm_pred_ppm)) {
    clean_plot$hm_fit <- hm_pred_ppm
  }

  jumps <- tr %>% filter(is_jump)

  # ---- Panel 1: Raw CH4 trace with goFlux fit ----
  pl_season <- ifelse(month(row$date) == 10, "Wet (Oct 2022)", "Dry (Mar 2023)")
  title_text <- sprintf("%s | %s | %s %s\u2013%s | [%s]",
                        row$site, pl_season, format(row$date, "%Y-%m-%d"),
                        format(trace_start, "%H:%M"), format(trace_end, "%H:%M"),
                        type_label)

  subtitle_text <- sprintf(
    "Diff: %.2f nmol/m\u00b2/s (%s, R\u00b2=%.3f) | Ebull: %.2f nmol/m\u00b2/s (%d jumps) | Total: %.2f | Ebull frac: %.1f%%",
    row$diffusive_flux_nmol, best_model,
    ifelse(best_model == "HM", gf_row$HM.r2, gf_row$LM.r2),
    row$ebull_flux_nmol, row$n_jumps,
    row$total_flux_nmol,
    row$ebullitive_fraction * 100)

  p1 <- ggplot() +
    geom_line(data = tr, aes(x = datetime, y = CH4_ppm),
              color = "grey40", linewidth = 0.5) +
    geom_point(data = tr %>% filter(!is_jump),
               aes(x = datetime, y = CH4_ppm), color = "grey40", size = 0.8) +
    geom_line(data = clean_plot, aes(x = datetime, y = best_fit),
              color = trace_color, linewidth = 0.8, linetype = "dashed") +
    theme_bw(base_size = 10) +
    labs(title = title_text, subtitle = subtitle_text,
         y = "CH4 dry (ppm)")

  if (nrow(jumps) > 0) {
    p1 <- p1 +
      geom_point(data = jumps, aes(x = datetime, y = CH4_ppm),
                 color = "red", size = 2.5, shape = 17) +
      geom_segment(data = jumps,
                   aes(x = datetime, xend = datetime,
                       y = CH4_ppm - dCH4, yend = CH4_ppm),
                   color = "red", linewidth = 0.6,
                   arrow = arrow(length = unit(0.08, "inches")))
  }

  # ---- Panel 2: Cumulative flux decomposition ----
  # Cumulative CH4 change over time, split into diffusive and ebullitive
  tr2 <- tr %>%
    mutate(
      elapsed = as.numeric(difftime(datetime, min(datetime), units = "secs")),
      cum_total = CH4_ppm - first(CH4_ppm),
      cum_ebull = cumsum(ifelse(is_jump, dCH4, 0)),
      cum_diff  = cum_total - cum_ebull
    )

  # Also add the goFlux model curve (as cumulative from C0)
  clean_cum <- clean_plot %>%
    mutate(cum_model = best_fit - first(best_fit))

  p2 <- ggplot() +
    geom_line(data = tr2, aes(x = datetime, y = cum_total, color = "Total"),
              linewidth = 0.6) +
    geom_line(data = tr2, aes(x = datetime, y = cum_diff, color = "Diffusive"),
              linewidth = 0.6) +
    geom_step(data = tr2, aes(x = datetime, y = cum_ebull, color = "Ebullitive"),
              linewidth = 0.6) +
    geom_line(data = clean_cum, aes(x = datetime, y = cum_model),
              color = trace_color, linewidth = 0.7, linetype = "dashed",
              alpha = 0.7) +
    scale_color_manual(
      values = c("Total" = "black", "Diffusive" = "steelblue", "Ebullitive" = "red"),
      name = NULL
    ) +
    theme_bw(base_size = 10) +
    theme(legend.position = "bottom") +
    labs(y = expression(Delta * "CH4 (ppm)"), x = "Time")

  # ---- Panel 3: Point-to-point CH4 changes ----
  jt <- res$summary$jump_thresh_used

  p3 <- ggplot(tr %>% filter(!is.na(dCH4)), aes(x = datetime, y = dCH4)) +
    geom_col(aes(fill = is_jump), width = 3, show.legend = FALSE) +
    scale_fill_manual(values = c("FALSE" = "grey60", "TRUE" = "red")) +
    geom_hline(yintercept = jt, color = "red", linetype = "dashed") +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    theme_bw(base_size = 10) +
    labs(x = "Time", y = expression(Delta * "CH4 (ppm)"))

  combined <- p1 / p2 / p3 + plot_layout(heights = c(3, 3, 2))
  trace_plots[[pid]] <- combined
}

# Sort by site then date
plot_order <- partitioned %>%
  arrange(site, date, placement_id) %>%
  pull(placement_id)
plot_order <- plot_order[plot_order %in% names(trace_plots)]

if (length(plot_order) > 0) {
  pdf_path <- file.path(OUTPUT_DIR, "partitioned_traces.pdf")
  pdf(pdf_path, width = 12, height = 12)
  for (pid in plot_order) {
    print(trace_plots[[pid]])
  }
  dev.off()
  cat("Saved", length(plot_order), "partitioned trace plots to", pdf_path, "\n")
}

# ---- SUMMARY PLOTS -----------------------------------------------------------

cat("\n=== GENERATING SUMMARY PLOTS ===\n")

# Plot 1: Simple LM vs goFlux diffusive
p_scatter <- ggplot(partitioned, aes(x = simple_LM_flux_nmol, y = diffusive_flux_nmol)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(aes(color = diffusive_model, shape = trace_type), size = 2.5, alpha = 0.7) +
  scale_color_manual(values = c("HM" = "firebrick", "LM" = "steelblue"), name = "goFlux model") +
  scale_shape_manual(values = c("processed" = 16, "additional" = 17), name = "Trace type") +
  theme_bw(base_size = 12) +
  labs(title = "Simple Linear vs goFlux Diffusive CH4 Flux",
       subtitle = "Points above 1:1 line = goFlux estimates higher (HM captures saturation)",
       x = expression("Simple LM diffusive (nmol CH"[4]*"/m"^2*"/s)"),
       y = expression("goFlux diffusive (nmol CH"[4]*"/m"^2*"/s)"))

# Plot 2: Stacked bar chart by site × season
bar_data <- site_summary %>%
  select(site, season, mean_diff, mean_ebull, mean_proc_flux) %>%
  tidyr::pivot_longer(cols = c(mean_diff, mean_ebull),
                      names_to = "component", values_to = "flux") %>%
  mutate(component = ifelse(component == "mean_diff", "Diffusive (goFlux)",
                            "Ebullitive"),
         site_season = paste(site, "\n", season))

p_bar <- ggplot(bar_data, aes(x = site_season, y = flux, fill = component)) +
  geom_col(position = "stack", width = 0.6) +
  geom_point(data = site_summary %>%
               mutate(site_season = paste(site, "\n", season)),
             aes(x = site_season, y = mean_proc_flux),
             inherit.aes = FALSE, shape = 4, size = 3, stroke = 1.5) +
  scale_fill_manual(values = c("Diffusive (goFlux)" = "steelblue",
                                "Ebullitive" = "red"),
                    name = NULL) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(size = 8)) +
  labs(title = "Partitioned CH4 Flux by Site \u00d7 Season",
       subtitle = "\u00d7 = processed flux mean (original pipeline)",
       x = NULL, y = expression("CH"[4]*" flux (nmol/m"^2*"/s)"))

pdf_path <- file.path(OUTPUT_DIR, "goflux_reprocess_summary.pdf")
pdf(pdf_path, width = 14, height = 7)
print(p_scatter)
print(p_bar)
dev.off()
cat("Saved summary plots to", pdf_path, "\n")

# ---- DONE --------------------------------------------------------------------

cat("\n=== DONE ===\n")
cat("Output directory:", OUTPUT_DIR, "\n")
cat("Key outputs:\n")
cat("  - partitioned_fluxes.csv        (per-trace: diffusive + ebullitive + total)\n")
cat("  - site_season_partitioned.csv   (site × season summary)\n")
cat("  - partitioned_traces.pdf        (per-trace: CH4 trace, model fit, decomposition)\n")
cat("  - goflux_reprocess_summary.pdf  (scatter + bar chart comparisons)\n")
