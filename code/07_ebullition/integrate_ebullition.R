# =============================================================================
# Integrate Ebullition-Partitioned Water Fluxes into Combined Dataset
# =============================================================================
# Updates combined_gas_flux_dataset.csv:
#   1. For "processed" traces: replaces CH4_best.flux with total (diffusive +
#      ebullitive) from reprocessing, preserving the goFlux model diagnostics
#   2. For "additional" traces: adds new water rows with total CH4 flux
#   3. Generates an SI figure showing partitioned diffusive vs ebullitive flux
#
# Depends on:
#   - output/ebullition/partitioned_fluxes.csv  (from goflux_reprocess_ebullition.R)
#   - output/ebullition/placements_summary.csv  (from detect_ebullition.R)
#   - output/combined_gas_flux_dataset.csv      (original combined dataset)
# =============================================================================

library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(patchwork)
library(forcats)

# ---- Load data ---------------------------------------------------------------

pf <- read_csv("output/ebullition/partitioned_fluxes.csv", show_col_types = FALSE)
pl <- read_csv("output/ebullition/placements_summary.csv", show_col_types = FALSE) %>%
  filter(!excluded)
df <- read_csv("output/combined_gas_flux_dataset.csv", show_col_types = FALSE)

# Remove any previously added ebullition rows (idempotent)
if ("data_source" %in% names(df)) {
  n_prev <- sum(df$data_source == "ebullition_reprocessing", na.rm = TRUE)
  if (n_prev > 0) {
    df <- df %>% filter(is.na(data_source) | data_source != "ebullition_reprocessing")
    cat("Removed", n_prev, "previously added ebullition rows\n")
  }
}
# Remove any previously added ebullition columns
for (col in c("CH4_ebull_flux", "CH4_diffusive_flux", "CH4_ebullitive_fraction",
              "CH4_n_ebull_events", "ebullition_reprocessed")) {
  if (col %in% names(df)) df[[col]] <- NULL
}

cat("Original dataset:", nrow(df), "rows\n")
cat("  Water rows:", sum(df$component == "water"), "\n")
cat("  Water with valid CH4:", sum(df$component == "water" & df$CH4_flux_status == "valid", na.rm = TRUE), "\n")
cat("Partitioned traces:", nrow(pf), "\n")
cat("  Processed:", sum(pf$trace_type == "processed"), "\n")
cat("  Additional:", sum(pf$trace_type == "additional"), "\n")

# ---- STEP 1: Update processed water fluxes with total flux ------------------

# Match processed traces to their original flux_ids.
# When multiple traces match the same flux_id (e.g., a long placement split into
# 10-min sub-segments that all overlap one logged flux window), keep only the first
# match as the "processed" update and reclassify the rest as "additional".
proc_all <- pf %>% filter(trace_type == "processed", !is.na(matched_flux_id))
proc <- proc_all %>%
  group_by(matched_flux_id) %>%
  slice(1) %>%
  ungroup()
proc_extra <- proc_all %>%
  anti_join(proc, by = "placement_id") %>%
  mutate(trace_type = "additional")

cat("\nProcessed traces:", nrow(proc_all),
    "→", nrow(proc), "unique flux matches +",
    nrow(proc_extra), "reclassified as additional\n")

# Add reclassified extras to the additional pool
add_reclassified <- proc_extra

cat("Updating", nrow(proc), "processed water fluxes with total (diffusive + ebullitive) CH4...\n")

# Initialize new columns
df$CH4_ebull_flux <- NA_real_
df$CH4_diffusive_flux <- NA_real_
df$CH4_ebullitive_fraction <- NA_real_
df$CH4_n_ebull_events <- NA_integer_
df$ebullition_reprocessed <- FALSE

# For each matched processed trace, update CH4_best.flux to total_flux_nmol
for (i in seq_len(nrow(proc))) {
  fid <- proc$matched_flux_id[i]
  idx <- which(df$flux_id == fid)
  if (length(idx) == 1) {
    old_val <- df$CH4_best.flux[idx]
    new_val <- proc$total_flux_nmol[i]
    df$CH4_best.flux[idx] <- new_val
    # Add ebullition metadata columns
    df$CH4_ebull_flux[idx] <- proc$ebull_flux_nmol[i]
    df$CH4_diffusive_flux[idx] <- proc$diffusive_flux_nmol[i]
    df$CH4_ebullitive_fraction[idx] <- proc$ebullitive_fraction[i]
    df$CH4_n_ebull_events[idx] <- proc$n_jumps[i]
    df$ebullition_reprocessed[idx] <- TRUE
  }
}

# Fill ebullition columns for non-water rows
df <- df %>%
  mutate(
    CH4_ebull_flux = ifelse(is.na(CH4_ebull_flux), 0, CH4_ebull_flux),
    CH4_diffusive_flux = ifelse(component == "water" & is.na(CH4_diffusive_flux),
                                CH4_best.flux, CH4_diffusive_flux),
    CH4_ebullitive_fraction = ifelse(is.na(CH4_ebullitive_fraction), 0, CH4_ebullitive_fraction),
    CH4_n_ebull_events = ifelse(is.na(CH4_n_ebull_events), 0, CH4_n_ebull_events),
    ebullition_reprocessed = ifelse(is.na(ebullition_reprocessed), FALSE, ebullition_reprocessed)
  )

# ---- STEP 2: Add additional traces as new water rows ------------------------

add <- bind_rows(
  pf %>% filter(trace_type == "additional"),
  add_reclassified
)
cat("Adding", nrow(add), "additional water traces (",
    sum(pf$trace_type == "additional"), "original +",
    nrow(add_reclassified), "reclassified from multi-match)...\n")

# Get representative water row for column template
water_template <- df %>% filter(component == "water") %>% slice(1)

# Build new rows matching the combined dataset schema
new_rows <- add %>%
  mutate(
    flux_id = placement_id,
    index = placement_id,
    plot = site,
    date = as.Date(date),
    start_time = hms::as_hms(format(pl$start_time[match(placement_id, pl$placement_id)], "%H:%M:%S")),
    end_time = hms::as_hms(format(pl$end_time[match(placement_id, pl$placement_id)], "%H:%M:%S")),
    measurement_type = "surface",
    component = "water",
    analyzer_source = analyzer,
    data_source = "ebullition_reprocessing",
    surface_type = "water",
    # CH4 flux columns
    CH4_best.flux = total_flux_nmol,
    CH4_model = diffusive_model,
    CH4_quality.check = diffusive_quality,
    CH4_flux_status = "valid",
    CH4_below_MDF = FALSE,
    CH4_flagged = FALSE,
    CH4_LM.flux = LM_flux_nmol,
    CH4_LM.r2 = LM_r2,
    CH4_LM.p.val = LM_p_val,
    CH4_HM.flux = HM_flux_nmol,
    CH4_HM.r2 = HM_r2,
    # Ebullition columns
    CH4_ebull_flux = ebull_flux_nmol,
    CH4_diffusive_flux = diffusive_flux_nmol,
    CH4_ebullitive_fraction = ebullitive_fraction,
    CH4_n_ebull_events = n_jumps,
    ebullition_reprocessed = TRUE,
    # Metadata
    year = year(as.Date(date)),
    month = month(as.Date(date)),
    month_year = format(as.Date(date), "%Y-%m"),
    season = ifelse(month == 10, "wet", "dry"),
    # Chamber params (from existing water fluxes at this site)
    chamber_volume_cm3 = water_template$chamber_volume_cm3,
    surface_area_cm2 = water_template$surface_area_cm2,
    total_system_volume_cm3 = water_template$total_system_volume_cm3,
    total_system_volume_L = water_template$total_system_volume_L,
    analyzer_cell_volume_cm3 = water_template$analyzer_cell_volume_cm3,
    tubing_volume_cm3 = water_template$tubing_volume_cm3,
    flux_status = "valid",
    notes = "additional trace from ebullition reprocessing"
  )

# Match to site disturbance level
site_dist <- df %>%
  filter(component == "water") %>%
  distinct(plot, disturbance_level)

new_rows <- new_rows %>%
  left_join(site_dist, by = "plot")

# Select only columns that exist in df (fill missing with NA)
all_cols <- names(df)
new_rows_clean <- new_rows %>%
  select(any_of(all_cols))

# Add missing columns as NA
for (col in setdiff(all_cols, names(new_rows_clean))) {
  new_rows_clean[[col]] <- NA
}
new_rows_clean <- new_rows_clean[, all_cols]

# Append
df <- bind_rows(df, new_rows_clean)

cat("\nUpdated dataset:", nrow(df), "rows\n")
cat("  Water rows:", sum(df$component == "water"), "\n")
cat("  Water with valid CH4:", sum(df$component == "water" & df$CH4_flux_status == "valid", na.rm = TRUE), "\n")
cat("  Ebullition-reprocessed:", sum(df$ebullition_reprocessed, na.rm = TRUE), "\n")

# ---- STEP 3: Save updated dataset -------------------------------------------

write_csv(df, "output/combined_gas_flux_dataset.csv")
cat("\nSaved updated dataset to: output/combined_gas_flux_dataset.csv\n")

# Also save a water-only version for convenience
df_water <- df %>% filter(component == "water", CH4_flux_status == "valid")
write_csv(df_water, "output/ebullition/water_fluxes_with_ebullition.csv")
cat("Saved water-only dataset to: output/ebullition/water_fluxes_with_ebullition.csv\n")

# ---- STEP 4: SI Figure — Partitioned Water Flux by Season × Site ------------

cat("\n--- Generating SI Ebullition Figure ---\n")

# Shared aesthetics from publication_figures.R
theme_pub <- function(base_size = 11) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      axis.title        = element_text(size = base_size, face = "bold"),
      axis.text         = element_text(size = base_size - 1),
      strip.text        = element_text(size = base_size - 1, face = "bold"),
      strip.background  = element_rect(fill = "grey95", color = "grey70"),
      legend.title      = element_text(size = base_size - 1, face = "bold"),
      legend.text       = element_text(size = base_size - 2),
      panel.grid.minor  = element_blank(),
      plot.tag          = element_text(size = base_size + 3, face = "bold"),
      plot.title        = element_text(size = base_size + 1, face = "bold"),
      plot.subtitle     = element_text(size = base_size - 1, color = "grey40")
    )
}

asinh_brk_pos <- c(0, 1, 10, 100, 1000)
asinh_labels <- function(x) {
  ifelse(x == 0, "0", format(x, scientific = FALSE, big.mark = "", drop0trailing = TRUE))
}

# Prepare data: long-form with flux type
si_data <- df_water %>%
  select(flux_id, plot, season, disturbance_level,
         CH4_diffusive_flux, CH4_ebull_flux, CH4_best.flux) %>%
  pivot_longer(
    cols = c(CH4_diffusive_flux, CH4_ebull_flux),
    names_to = "flux_component",
    values_to = "flux_nmol"
  ) %>%
  mutate(
    flux_component = factor(
      ifelse(flux_component == "CH4_diffusive_flux", "Diffusive", "Ebullitive"),
      levels = c("Diffusive", "Ebullitive")
    ),
    season_display = factor(
      ifelse(season == "wet", "Wet", "Dry"),
      levels = c("Wet", "Dry")
    ),
    plot = factor(plot, levels = c("BL60", "CP40", "FLM30", "SE1", "SRS5", "SRS6"))
  )

# Colors for flux components
flux_comp_colors <- c(
  "Diffusive" = "#4682B4",   # Steel blue (matches water)
  "Ebullitive" = "#FF8C00"   # Dark orange
)

# Stacked bar: mean flux partitioned into diffusive + ebullitive per site × season
si_means <- si_data %>%
  group_by(plot, season_display, flux_component) %>%
  summarise(
    mean_flux = mean(flux_nmol, na.rm = TRUE),
    se_flux = sd(flux_nmol, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

# Total flux per site × season (for error bars on total)
si_totals <- df_water %>%
  mutate(
    season_display = factor(ifelse(season == "wet", "Wet", "Dry"), levels = c("Wet", "Dry")),
    plot = factor(plot, levels = c("BL60", "CP40", "FLM30", "SE1", "SRS5", "SRS6"))
  ) %>%
  group_by(plot, season_display) %>%
  summarise(
    mean_total = mean(CH4_best.flux, na.rm = TRUE),
    se_total = sd(CH4_best.flux, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

# Panel (a): Stacked bar plot
fig_si_a <- ggplot(si_means,
                   aes(x = season_display, y = mean_flux, fill = flux_component)) +
  geom_col(position = "stack", width = 0.6, color = "black", linewidth = 0.3) +
  geom_errorbar(data = si_totals,
                aes(x = season_display, y = mean_total,
                    ymin = mean_total - se_total, ymax = mean_total + se_total,
                    fill = NULL),
                width = 0.15, linewidth = 0.5, inherit.aes = FALSE) +
  geom_text(data = si_totals,
            aes(x = season_display, y = -Inf, label = paste0("n=", n), fill = NULL),
            vjust = 1.5, size = 2.8, inherit.aes = FALSE) +
  facet_wrap(~ plot, nrow = 1, scales = "free_y") +
  scale_fill_manual(values = flux_comp_colors, name = "Flux component") +
  scale_y_continuous(expand = expansion(mult = c(0.15, 0.1))) +
  labs(x = "Season", y = expression(bold(CH[4]~Flux~(nmol~m^{-2}~s^{-1}))),
       tag = "(a)") +
  theme_pub(base_size = 10) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        panel.spacing = unit(0.5, "lines"))

# Panel (b): Jitter + box showing individual observations, colored by component
fig_si_b <- si_data %>%
  ggplot(aes(x = season_display, y = flux_nmol, fill = flux_component)) +
  geom_point(aes(color = flux_component),
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6),
             alpha = 0.5, size = 1.2) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA, color = "black",
               width = 0.5, position = position_dodge(width = 0.6),
               linewidth = 0.3) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2,
               fill = "white", color = "black", stroke = 0.5,
               position = position_dodge(width = 0.6)) +
  facet_wrap(~ plot, nrow = 1, scales = "free_y") +
  scale_fill_manual(values = flux_comp_colors, name = "Flux component") +
  scale_color_manual(values = flux_comp_colors, name = "Flux component") +
  scale_y_continuous(trans = "asinh", breaks = asinh_brk_pos, labels = asinh_labels) +
  labs(x = "Season", y = expression(bold(CH[4]~Flux~(nmol~m^{-2}~s^{-1}))),
       tag = "(b)") +
  theme_pub(base_size = 10) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        panel.spacing = unit(0.5, "lines"))

# Combine panels
fig_si <- (fig_si_a / fig_si_b) +
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(legend.position = "bottom"))

# Save
ggsave("output/figures/pub_SI_ebullition_partition.pdf", fig_si,
       width = 260, height = 180, units = "mm")
ggsave("output/figures/pub_SI_ebullition_partition.png", fig_si,
       width = 260, height = 180, units = "mm", dpi = 300)
cat("Saved: pub_SI_ebullition_partition.pdf/.png\n")

# ---- Summary stats -----------------------------------------------------------

cat("\n=== UPDATED WATER FLUX SUMMARY ===\n\n")
df_water %>%
  mutate(season_display = ifelse(season == "wet", "Wet", "Dry")) %>%
  group_by(plot, season_display) %>%
  summarise(
    n = n(),
    mean_total = round(mean(CH4_best.flux, na.rm = TRUE), 2),
    mean_diffusive = round(mean(CH4_diffusive_flux, na.rm = TRUE), 2),
    mean_ebullitive = round(mean(CH4_ebull_flux, na.rm = TRUE), 2),
    pct_ebullitive = round(mean(CH4_ebullitive_fraction, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cat("\n=== DONE ===\n")
