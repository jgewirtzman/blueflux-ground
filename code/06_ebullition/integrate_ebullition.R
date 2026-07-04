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
#   - output/data_products/combined_gas_flux_dataset.csv      (original combined dataset)
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
# Read the combined dataset AFTER negative flux corrections have been applied.
# Pipeline order: (1) apply_negative_flux_corrections.R writes corrected dataset,
# (2) this script reads it and adds ebullition integration.
# Both scripts start from combined_gas_flux_dataset_ORIGINAL.csv to avoid
# circular dependency, but this one must run AFTER corrections.
df <- read_csv("output/data_products/combined_gas_flux_dataset.csv", show_col_types = FALSE)

# Safety check: remove any previous ebullition_reprocessing rows to make idempotent
n_before <- nrow(df)
df <- df %>% filter(is.na(data_source) | data_source != "ebullition_reprocessing")
if (nrow(df) < n_before) {
  cat("Removed", n_before - nrow(df), "previous ebullition_reprocessing rows for clean re-run\n")
}

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

write_csv(df, "output/data_products/combined_gas_flux_dataset.csv")
cat("\nSaved updated dataset to: output/data_products/combined_gas_flux_dataset.csv\n")

# Also save a water-only version for convenience
df_water <- df %>% filter(component == "water", CH4_flux_status == "valid")
write_csv(df_water, "output/ebullition/water_fluxes_with_ebullition.csv")
cat("Saved water-only dataset to: output/ebullition/water_fluxes_with_ebullition.csv\n")

# NOTE: SI ebullition figure is generated by code/10_figures/figS1_ebullition.R
# (single source of truth for that figure)

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
