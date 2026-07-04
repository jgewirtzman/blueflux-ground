#!/usr/bin/env Rscript
# apply_chamber_corrections.R
# Applies HA/HB chamber volume/area corrections to the combined dataset.
# The HA/HB chambers had circumferences recorded in inches but processed as cm,
# causing surface areas to be 2.54x too large and chamber volumes to be wrong.
# This script reads the correction factors from intermediate/ha_hb_flux_corrections.csv
# and scales the affected fluxes accordingly.
#
# Must run AFTER: assemble_clean_dataset.R
# Must run BEFORE: integrate_ebullition.R, apply_negative_flux_corrections.R

library(readr)
library(dplyr)

cat("=== Applying HA/HB chamber corrections ===\n")

corrections_file <- "intermediate/ha_hb_flux_corrections.csv"
dataset_file <- "output/data_products/combined_gas_flux_dataset.csv"

if (!file.exists(corrections_file)) {
  cat("No corrections file found at", corrections_file, "— skipping\n")
  quit(status = 0)
}

corrections <- read_csv(corrections_file, show_col_types = FALSE)
df <- read_csv(dataset_file, show_col_types = FALSE)
cat("Input dataset:", nrow(df), "rows\n")
cat("Corrections to apply:", nrow(corrections), "\n")

n_corrected <- 0
for (i in seq_len(nrow(corrections))) {
  r <- corrections[i, ]
  idx <- which(df$flux_id == r$flux_id)
  if (length(idx) == 0) next

  # Scale all flux values by correction factor
  df$CH4_best.flux[idx] <- r$CH4_best_new
  df$CO2_best.flux[idx] <- r$CO2_best_new
  df$CH4_LM.flux[idx]   <- df$CH4_LM.flux[idx] * r$correction
  df$CH4_HM.flux[idx]   <- df$CH4_HM.flux[idx] * r$correction
  df$CO2_LM.flux[idx]   <- df$CO2_LM.flux[idx] * r$correction
  df$CO2_HM.flux[idx]   <- df$CO2_HM.flux[idx] * r$correction

  # Update areas and volumes
  df$surface_area_cm2[idx]         <- r$new_Area
  df$total_system_volume_cm3[idx]  <- r$new_Vtot

  n_corrected <- n_corrected + 1
}

write_csv(df, dataset_file)
cat("Corrected", n_corrected, "measurements\n")
cat("Output dataset:", nrow(df), "rows\n")
cat("=== Done ===\n")
