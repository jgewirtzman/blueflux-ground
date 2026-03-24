# create_data_products.R
# Creates clean data subsets for archival / ORNL DAAC submission
# Run from the project root directory

library(dplyr)
library(readr)

# Read master dataset
df <- read_csv("output/combined_gas_flux_dataset.csv", show_col_types = FALSE)
cat("Read master dataset:", nrow(df), "rows x", ncol(df), "columns\n\n")

# ---- Define clean column set for archival ----
# These are the most important columns for a data archive user
clean_cols <- c(
  # Identification
  "flux_id", "date", "plot", "measurement_type", "component",
  "surface_type", "analyzer_source", "data_source",
  # Temporal
  "start_time", "end_time", "year", "month", "season",
  # Environmental conditions
  "air_temp", "soil_temp", "water_temp", "water_depth",
  "pressure_start", "rh_start",
  # Chamber specifications
  "chamber_volume_cm3", "surface_area_cm2",
  "total_system_volume_cm3",
  # CH4 fluxes (best estimate)
  "CH4_best.flux", "CH4_model", "CH4_quality.check",
  "CH4_flux_status", "CH4_below_MDF", "CH4_flagged", "CH4_SNR",
  "CH4_LM.flux", "CH4_LM.r2", "CH4_LM.p.val",
  "CH4_HM.flux", "CH4_HM.r2",
  # CO2 fluxes (best estimate)
  "CO2_best.flux", "CO2_model", "CO2_quality.check",
  "CO2_flux_status", "CO2_below_MDF", "CO2_flagged", "CO2_SNR",
  "CO2_LM.flux", "CO2_LM.r2", "CO2_LM.p.val",
  "CO2_HM.flux", "CO2_HM.r2",
  # Ebullition partitioning (water fluxes only)
  "CH4_ebull_flux", "CH4_diffusive_flux", "CH4_ebullitive_fraction",
  "CH4_n_ebull_events", "ebullition_reprocessed",
  # Derived / metadata
  "flux_status", "disturbance_level", "notes"
)

# Select only columns that exist
available_cols <- clean_cols[clean_cols %in% names(df)]
missing_cols <- clean_cols[!clean_cols %in% names(df)]
if (length(missing_cols) > 0) {
  cat("Note: these requested columns were not found:", paste(missing_cols, collapse = ", "), "\n")
}

# ---- Create clean archival dataset ----
df_clean <- df %>% select(all_of(available_cols))
write_csv(df_clean, "output/combined_gas_flux_dataset_archival.csv")
cat("Wrote output/combined_gas_flux_dataset_archival.csv:", nrow(df_clean), "rows x", ncol(df_clean), "columns\n")

# ---- Create tree-only subset ----
df_trees <- df_clean %>% filter(measurement_type == "tree")
write_csv(df_trees, "output/tree_stem_fluxes.csv")
cat("Wrote output/tree_stem_fluxes.csv:", nrow(df_trees), "rows\n")

# ---- Create soil/water-only subset ----
df_surface <- df_clean %>% filter(measurement_type == "surface")
write_csv(df_surface, "output/soil_water_surface_fluxes.csv")
cat("Wrote output/soil_water_surface_fluxes.csv:", nrow(df_surface), "rows\n")

cat("\nData products created successfully.\n")
