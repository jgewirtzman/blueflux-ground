#!/usr/bin/env Rscript
# ===============================================================================
# Assemble Clean Gas Flux Dataset
# ===============================================================================
#
# Replaces the previous 3-script pipeline:
#   stitch_all_files.R → rescue_auxfile_merge.R → date_harmonize.R
#
# Produces a single clean CSV: output/combined_gas_flux_dataset.csv
#   - 834 unique measurements (one row per measurement)
#   - Rescued fluxes merged INTO original rows (no duplicates)
#   - Field metadata backfilled for rescued tree rows
#   - Standardized column names and values
#   - QC flags per ch4-data-filtering methodology (keep all, flag individually)
#
# QC approach:
#   - ALL measurements retained (never removed, never zeroed)
#   - CH4/CO2_below_MDF: boolean flag for |flux| <= MDF
#   - CH4/CO2_flagged: boolean flag for goFlux quality.check warnings
#   - CH4/CO2_SNR: signal-to-noise ratio (|flux| / SE from selected model)
#   - flux_status: "valid" (goFlux produced a result) or "no_data" (goFlux never ran)
#   - Below-MDF values retain original flux, NOT set to zero
# ===============================================================================

library(dplyr)
library(readr)
library(readxl)
library(lubridate)
library(stringr)

# Set working directory to project root (two levels up from this script's location)
project_root <- normalizePath(file.path(dirname(
  if (interactive()) rstudioapi::getSourceEditorContext()$path
  else commandArgs(trailingOnly = FALSE)[grep("--file=", commandArgs(trailingOnly = FALSE))] |>
    sub("--file=", "", x = _)
), "..", ".."), mustWork = FALSE)

# Fallback: detect project root by looking for the output/ directory
if (!dir.exists(file.path(project_root, "output"))) {
  wd <- getwd()
  while (nchar(wd) > 1 && !dir.exists(file.path(wd, "output"))) {
    wd <- dirname(wd)
  }
  if (dir.exists(file.path(wd, "output"))) {
    project_root <- wd
  } else {
    stop("Cannot find project root. Please setwd() to blueflux-ground/ before running.")
  }
}

setwd(project_root)
cat("Working directory:", getwd(), "\n\n")

cat("=== ASSEMBLING CLEAN GAS FLUX DATASET ===\n\n")

# =============================================================================
# STEP 1: Read all inputs
# =============================================================================

cat("Step 1: Reading input files...\n")

# --- goFlux column suffixes (47 per gas, unprefixed in rescued files) ---
goflux_suffixes <- c(
  "LM.flux", "LM.C0", "LM.Ct", "LM.slope", "LM.MAE", "LM.RMSE",
  "LM.AICc", "LM.SE", "LM.se.rel", "LM.r2", "LM.p.val",
  "HM.flux", "HM.C0", "HM.Ci", "HM.slope", "HM.MAE", "HM.RMSE",
  "HM.AICc", "HM.SE", "HM.se.rel", "HM.r2", "HM.k",
  "C0", "Ct", "MDF", "prec", "flux.term", "nb.obs",
  "k.max", "k.mult", "g.fact",
  "HM.diagnose", "LM.diagnose", "best.flux", "model", "quality.check",
  "HM.score", "LM.score",
  "RMSE.lim", "MAE.lim", "SE.lim", "g.limit", "g.reverse",
  "k.ratio.lim", "p.val.lim", "MDF.lim", "warn.nb.obs"
)

# --- Helper: read a tree result file ---
read_tree_file <- function(path) {
  df <- read_csv(path, show_col_types = FALSE)
  analyzer <- case_when(
    grepl("LGR1", path) ~ "LGR1",
    grepl("LGR2", path) ~ "LGR2",
    grepl("LGR3", path) ~ "LGR3",
    grepl("Picarro|picarro", path) ~ "Picarro",
    TRUE ~ NA_character_
  )
  df %>% mutate(
    measurement_type = "tree",
    analyzer_source = analyzer,
    source_file = basename(path),
    # Coerce to consistent types
    no = as.character(no),
    height = as.numeric(height),
    lenticels = as.character(lenticels),
    index = as.character(index)
  )
}

# --- Helper: read a surface result file ---
read_surface_file <- function(path) {
  df <- read_csv(path, show_col_types = FALSE)
  analyzer <- case_when(
    grepl("LGR1", path) ~ "LGR1",
    grepl("LGR2", path) ~ "LGR2",
    grepl("LGR3", path) ~ "LGR3",
    grepl("Picarro|picarro", path) ~ "Picarro",
    TRUE ~ NA_character_
  )
  df %>%
    # Rename to harmonize with tree columns
    rename(
      surface_type = surface,
      soil_temp = soil_temp_c,
      water_depth = water_depth_cm
    ) %>%
    rename_with(~ case_when(
      . == "Offset_cm" ~ "collar_offset_cm",
      . == "Collar_Volume_cm3" ~ "collar_volume_cm3",
      . == "Collar_Volume_L" ~ "collar_volume_L",
      . == "Chamber+Collar_Volume_L" ~ "chamber_plus_collar_volume_L",
      . == "collar_notes" ~ "collar_id",
      . == "collar_location_notes" ~ "collar_location",
      TRUE ~ .
    )) %>%
    mutate(
      component = tolower(surface_type),
      measurement_type = "surface",
      analyzer_source = analyzer,
      source_file = basename(path),
      index = as.character(index),
      # Combine notes
      notes = coalesce(as.character(notes_1), as.character(notes_2))
    ) %>%
    # Drop empty trailing columns and duplicates
    select(-starts_with("..."),
           -any_of(c("notes_1", "notes_2",
                      "Diameter_cm", "Height_cm",
                      "Chamber_Volume_cm3", "Chamber_Volume_L",
                      "Ground_Surface_Area_cm2")))
}

# Read 5 tree files
tree_files <- c(
  "intermediate/results_trees/LGR1_final_complete_dataset.csv",
  "intermediate/results_trees/LGR2_final_complete_dataset.csv",
  "intermediate/results_trees/LGR3_final_complete_dataset.csv",
  "intermediate/results_trees/LGR3_final_complete_dataset_additional.csv",
  "intermediate/results_trees/Picarro_CH4_final_complete_dataset.csv"
)
all_trees <- lapply(tree_files, read_tree_file) %>% bind_rows()
cat("  Trees:", nrow(all_trees), "rows from", length(tree_files), "files\n")

# Read 4 surface files
surface_files <- c(
  "intermediate/results_surface/LGR1_final_complete_dataset_soil.csv",
  "intermediate/results_surface/LGR2_final_complete_dataset_soil.csv",
  "intermediate/results_surface/LGR3_final_complete_dataset_soil.csv",
  "intermediate/results_surface/Picarro_soilwater_CH4_final_complete_dataset.csv"
)
all_surfaces <- lapply(surface_files, read_surface_file) %>% bind_rows()
cat("  Surfaces:", nrow(all_surfaces), "rows from", length(surface_files), "files\n")

# Read rescued flux files
rescued_ch4 <- read_csv("intermediate/rescue/ALL_RESCUED_CH4_BEST_FLUX.csv", show_col_types = FALSE)
rescued_co2 <- read_csv("intermediate/rescue/ALL_RESCUED_CO2_BEST_FLUX.csv", show_col_types = FALSE)
rescued_ids <- unique(c(rescued_ch4$UniqueID, rescued_co2$UniqueID))
cat("  Rescued: ", length(rescued_ids), "unique measurements (CH4:", nrow(rescued_ch4),
    "rows, CO2:", nrow(rescued_co2), "rows)\n")

# Read compiled field notes for metadata backfill
field_notes <- read_excel(
  "data/field_notes/entered_data/blueflux compiled tree fluxes.xlsx",
  sheet = "compiled"
)
cat("  Field notes:", nrow(field_notes), "rows\n")

# =============================================================================
# STEP 2-3: Bind all original rows
# =============================================================================

cat("\nStep 2-3: Combining original files...\n")

combined <- bind_rows(all_trees, all_surfaces)
cat("  Combined:", nrow(combined), "rows x", ncol(combined), "columns\n")

# Assert
stopifnot(nrow(combined) == nrow(all_trees) + nrow(all_surfaces))
n_dup <- sum(duplicated(combined$flux_id))
if (n_dup > 0) {
  cat("  WARNING:", n_dup, "duplicate flux_ids found\n")
  # Show duplicates
  dup_ids <- combined$flux_id[duplicated(combined$flux_id)]
  cat("  Duplicate IDs:", paste(head(dup_ids, 5), collapse = ", "), "\n")
} else {
  cat("  OK: No duplicate flux_ids\n")
}

# Tag all as original initially
combined$data_source <- "original"

# =============================================================================
# STEP 4: Merge rescued fluxes INTO original rows
# =============================================================================

cat("\nStep 4: Merging rescued fluxes into original rows...\n")

# Identify which goFlux columns exist in each rescued file (unprefixed)
# (CO2 file may have columns CH4 file doesn't, e.g. p.val.lim from recovered fluxes)
ch4_goflux_cols <- intersect(goflux_suffixes, names(rescued_ch4))
co2_goflux_cols <- intersect(goflux_suffixes, names(rescued_co2))

# --- Merge CH4 rescued ---
rescued_ch4_clean <- rescued_ch4 %>%
  select(UniqueID, all_of(ch4_goflux_cols)) %>%
  rename_with(~ paste0("CH4_", .), -UniqueID)

# Left join and coalesce
ch4_rescued_cols <- paste0("CH4_", ch4_goflux_cols)

combined <- combined %>%
  left_join(rescued_ch4_clean, by = c("flux_id" = "UniqueID"), suffix = c("", ".rescued"))

# Coalesce: original takes priority, rescued fills NAs
for (col in ch4_rescued_cols) {
  rescued_col <- paste0(col, ".rescued")
  if (rescued_col %in% names(combined)) {
    combined[[col]] <- coalesce(combined[[col]], combined[[rescued_col]])
    combined[[rescued_col]] <- NULL
  }
}

# --- Merge CO2 rescued ---
rescued_co2_clean <- rescued_co2 %>%
  select(UniqueID, all_of(co2_goflux_cols)) %>%
  rename_with(~ paste0("CO2_", .), -UniqueID)

co2_rescued_cols <- paste0("CO2_", co2_goflux_cols)

combined <- combined %>%
  left_join(rescued_co2_clean, by = c("flux_id" = "UniqueID"), suffix = c("", ".rescued"))

for (col in co2_rescued_cols) {
  rescued_col <- paste0(col, ".rescued")
  if (rescued_col %in% names(combined)) {
    combined[[col]] <- coalesce(combined[[col]], combined[[rescued_col]])
    combined[[rescued_col]] <- NULL
  }
}

# Mark rescued rows
combined <- combined %>%
  mutate(data_source = if_else(flux_id %in% rescued_ids, "rescued", "original"))

cat("  Rescued rows merged:", sum(combined$data_source == "rescued"), "\n")
cat("  Total rows (should be unchanged):", nrow(combined), "\n")

# Validate rescued rows have flux data
rescued_rows <- combined %>% filter(data_source == "rescued")
cat("  Rescued CH4 non-NA:", sum(!is.na(rescued_rows$CH4_best.flux)), "/", nrow(rescued_rows), "\n")
cat("  Rescued CO2 non-NA:", sum(!is.na(rescued_rows$CO2_best.flux)), "/", nrow(rescued_rows), "\n")

# =============================================================================
# STEP 5: Backfill field metadata for rescued tree rows
# =============================================================================

cat("\nStep 5: Backfilling field metadata for rescued rows...\n")

# Metadata columns to backfill from field notes
metadata_backfill <- field_notes %>%
  filter(flux_id %in% rescued_ids) %>%
  select(flux_id, species, status, height, diameter, chamber, chamber_class,
         stem_temp, soil_temp, lenticels, above, water_depth) %>%
  mutate(height = as.numeric(height),
         diameter = as.numeric(diameter),
         stem_temp = as.numeric(stem_temp),
         soil_temp = as.numeric(soil_temp),
         water_depth = as.numeric(water_depth))

cat("  Field notes matched:", nrow(metadata_backfill), "of", length(rescued_ids), "rescued IDs\n")

combined <- combined %>%
  left_join(metadata_backfill, by = "flux_id", suffix = c("", ".fn")) %>%
  mutate(
    species = coalesce(species, species.fn),
    status = coalesce(status, status.fn),
    height = coalesce(as.numeric(height), height.fn),
    diameter = coalesce(as.numeric(diameter), diameter.fn),
    chamber = coalesce(chamber, chamber.fn),
    chamber_class = coalesce(chamber_class, chamber_class.fn),
    stem_temp = coalesce(as.numeric(stem_temp), stem_temp.fn),
    soil_temp = coalesce(as.numeric(soil_temp), soil_temp.fn),
    lenticels = coalesce(lenticels, lenticels.fn),
    above = coalesce(above, above.fn),
    water_depth = coalesce(as.numeric(water_depth), water_depth.fn)
  ) %>%
  select(-ends_with(".fn"))

# Fix rescued plot extraction bug: Oct_22_1_FLM30_stem → plot should be "FLM30" not "O"
# Re-extract plot from flux_id for rescued rows
combined <- combined %>%
  mutate(
    plot = if_else(
      data_source == "rescued" & measurement_type == "tree",
      str_extract(flux_id, "(SRS[56]|BL60|CP40|FLM30|RB10|SE1|MI)"),
      plot
    )
  )

rescued_trees <- combined %>% filter(data_source == "rescued" & measurement_type == "tree")
cat("  Rescued tree rows with species:", sum(!is.na(rescued_trees$species)), "/", nrow(rescued_trees), "\n")
cat("  Rescued tree rows with status:", sum(!is.na(rescued_trees$status)), "/", nrow(rescued_trees), "\n")

# =============================================================================
# STEP 6: Harmonize values
# =============================================================================

cat("\nStep 6: Harmonizing categorical values...\n")

combined <- combined %>%
  mutate(
    status = case_when(
      tolower(status) == "alive" ~ "alive",
      tolower(status) == "dead" ~ "dead",
      toupper(status) == "CWD" ~ "CWD",
      TRUE ~ status
    ),
    above = tolower(above),
    component = tolower(component),
    lenticels = tolower(lenticels),
    surface_type = tolower(surface_type)
  )

cat("  status values:", paste(sort(unique(combined$status[!is.na(combined$status)])), collapse = ", "), "\n")
cat("  above values:", paste(sort(unique(combined$above[!is.na(combined$above)])), collapse = ", "), "\n")
cat("  component values:", paste(sort(unique(combined$component)), collapse = ", "), "\n")

# =============================================================================
# STEP 7: Height corrections
# =============================================================================

cat("\nStep 7: Applying height corrections...\n")

combined <- combined %>%
  mutate(
    height = as.numeric(height),
    water_depth = as.numeric(water_depth),
    height_corrected = height,
    # Negative heights → submerged roots, set to 0
    component = if_else(!is.na(height) & height < 0, "root", component),
    height_corrected = if_else(!is.na(height_corrected) & height_corrected < 0, 0, height_corrected),
    # Correct for water depth on sediment-based measurements
    height_corrected = if_else(
      !is.na(above) & above == "sediment" &
        !is.na(water_depth) & water_depth > 0 &
        !is.na(height_corrected) &
        component %in% c("stem", "root"),
      height_corrected - water_depth,
      height_corrected
    )
  )

n_neg_fixed <- sum(!is.na(combined$height) & combined$height < 0)
cat("  Negative heights corrected:", n_neg_fixed, "\n")

# =============================================================================
# STEP 7b: Known date corrections
# =============================================================================

cat("\nStep 7b: Applying known date corrections...\n")

# BL60 water measurements 168-171 were recorded as 2023-03-22 in field notes,
# but the actual measurement date was 2023-03-16 (confirmed by matching LGR3
# analyzer data from the continuation file micro_2023-03-16_f0001.txt).
water_date_fix_ids <- c("45007_BL60_Water_168", "45007_BL60_Water_169",
                        "45007_BL60_Water_170", "45007_BL60_Water_171")
n_fix <- sum(combined$flux_id %in% water_date_fix_ids)
if (n_fix > 0) {
  # Convert date columns to character for safe string replacement
  combined$date <- as.character(combined$date)
  combined$date_parsed <- as.character(combined$date_parsed)
  combined$date_only <- as.character(combined$date_only)
  combined$datetime <- as.character(combined$datetime)

  fix_mask <- combined$flux_id %in% water_date_fix_ids
  combined$date[fix_mask] <- gsub("3/22/23", "3/16/23", combined$date[fix_mask])
  combined$date_parsed[fix_mask] <- gsub("2023-03-22", "2023-03-16", combined$date_parsed[fix_mask])
  combined$date_only[fix_mask] <- gsub("2023-03-22", "2023-03-16", combined$date_only[fix_mask])
  combined$datetime[fix_mask] <- gsub("2023-03-22", "2023-03-16", combined$datetime[fix_mask])

  cat("  Corrected date for", n_fix, "BL60 water measurements (2023-03-22 → 2023-03-16)\n")
} else {
  cat("  No BL60 water date corrections needed\n")
}

# =============================================================================
# STEP 8: Date parsing and temporal columns
# =============================================================================

cat("\nStep 8: Parsing dates and creating temporal columns...\n")

combined <- combined %>%
  mutate(
    # Parse date from multiple sources (priority cascade)
    date_clean = case_when(
      # Surface files have date_parsed already in a parseable format
      !is.na(date_parsed) ~ as.Date(date_parsed),
      # Tree files have datetime
      !is.na(datetime) ~ as.Date(datetime),
      # Fallback: parse date column (M/D/YY format)
      !is.na(date) & grepl("/", date) ~ as.Date(date, format = "%m/%d/%y"),
      TRUE ~ NA_Date_
    ),
    year = as.integer(year(date_clean)),
    month = as.integer(month(date_clean)),
    month_year = format(date_clean, "%Y-%m"),
    season = case_when(
      month == 3 ~ "dry",
      month == 10 ~ "wet",
      month == 12 ~ "dry",  # December campaign
      TRUE ~ NA_character_
    )
  )

# For any remaining NAs, try to extract from flux_id pattern
combined <- combined %>%
  mutate(
    month_year = case_when(
      !is.na(month_year) ~ month_year,
      grepl("^Oct_22", flux_id) ~ "2022-10",
      grepl("^Mar_22", flux_id) ~ "2022-03",
      grepl("^Mar_23", flux_id) ~ "2023-03",
      grepl("^Dec_23", flux_id) ~ "2023-12",
      TRUE ~ month_year
    ),
    year = coalesce(year, as.integer(str_extract(month_year, "^\\d{4}"))),
    month = coalesce(month, as.integer(str_extract(month_year, "\\d+$")))
  )

cat("  Dates parsed:", sum(!is.na(combined$month_year)), "/", nrow(combined), "\n")

# =============================================================================
# STEP 9: QC flag columns
# =============================================================================

cat("\nStep 9: Adding QC flag columns...\n")

combined <- combined %>%
  mutate(
    # Flux status
    CH4_flux_status = if_else(!is.na(CH4_best.flux), "valid", "no_data"),
    CO2_flux_status = if_else(!is.na(CO2_best.flux), "valid", "no_data"),

    # Below MDF (manufacturer precision-based)
    CH4_below_MDF = !is.na(CH4_best.flux) & !is.na(CH4_MDF) & abs(CH4_best.flux) <= CH4_MDF,
    CO2_below_MDF = !is.na(CO2_best.flux) & !is.na(CO2_MDF) & abs(CO2_best.flux) <= CO2_MDF,

    # Flagged by goFlux quality.check
    CH4_flagged = !is.na(CH4_quality.check) & nchar(trimws(as.character(CH4_quality.check))) > 0,
    CO2_flagged = !is.na(CO2_quality.check) & nchar(trimws(as.character(CO2_quality.check))) > 0,

    # Signal-to-noise: |flux| / SE (from selected model)
    CH4_SE_selected = case_when(
      CH4_model == "LM" ~ CH4_LM.SE,
      CH4_model == "HM" ~ CH4_HM.SE,
      TRUE ~ CH4_LM.SE  # fallback
    ),
    CH4_SNR = if_else(
      !is.na(CH4_best.flux) & !is.na(CH4_SE_selected) & CH4_SE_selected > 0,
      abs(CH4_best.flux) / CH4_SE_selected, NA_real_
    ),
    CO2_SE_selected = case_when(
      CO2_model == "LM" ~ CO2_LM.SE,
      CO2_model == "HM" ~ CO2_HM.SE,
      TRUE ~ CO2_LM.SE
    ),
    CO2_SNR = if_else(
      !is.na(CO2_best.flux) & !is.na(CO2_SE_selected) & CO2_SE_selected > 0,
      abs(CO2_best.flux) / CO2_SE_selected, NA_real_
    ),

    # Overall measurement status
    flux_status = if_else(
      CH4_flux_status == "valid" | CO2_flux_status == "valid",
      "valid", "no_data"
    )
  ) %>%
  select(-CH4_SE_selected, -CO2_SE_selected)

cat("  CH4 valid:", sum(combined$CH4_flux_status == "valid"), "\n")
cat("  CO2 valid:", sum(combined$CO2_flux_status == "valid"), "\n")
cat("  CH4 below MDF:", sum(combined$CH4_below_MDF, na.rm = TRUE), "\n")
cat("  CO2 below MDF:", sum(combined$CO2_below_MDF, na.rm = TRUE), "\n")
cat("  CH4 flagged:", sum(combined$CH4_flagged, na.rm = TRUE), "\n")
cat("  CO2 flagged:", sum(combined$CO2_flagged, na.rm = TRUE), "\n")
cat("  No data (both):", sum(combined$flux_status == "no_data"), "\n")

# =============================================================================
# STEP 10: Derived columns
# =============================================================================

cat("\nStep 10: Computing derived columns...\n")

combined <- combined %>%
  mutate(
    # Pneumatophore density (soil surface measurements only)
    pneumatophore_density = if_else(
      !is.na(pneumatophore_count) & !is.na(surface_area_cm2) & surface_area_cm2 > 0,
      as.numeric(pneumatophore_count) / (surface_area_cm2 / 10000),
      NA_real_
    ),
    # Disturbance level
    disturbance_level = case_when(
      plot %in% c("SRS5", "SRS6", "RB10") ~ "healthy",
      plot == "BL60" ~ "regenerating",
      plot %in% c("CP40", "FLM30", "MI") ~ "ghost",
      plot == "SE1" ~ "scrub",
      TRUE ~ NA_character_
    )
  )

# =============================================================================
# STEP 11: Select and order final columns
# =============================================================================

cat("\nStep 11: Selecting final columns...\n")

# Ensure notes column exists (already coalesced during read_surface_file)
if (!"notes" %in% names(combined)) {
  combined$notes <- NA_character_
} else {
  combined$notes <- as.character(combined$notes)
}

# Rename water_temp_c if it exists
if ("water_temp_c" %in% names(combined)) {
  combined <- combined %>% rename(water_temp = water_temp_c)
} else if (!"water_temp" %in% names(combined)) {
  combined$water_temp <- NA_real_
}

final <- combined %>%
  select(
    # Group A: Identity
    flux_id, index, plot, date = date_clean, start_time, end_time,
    measurement_type, component, analyzer_source, data_source,

    # Group B: Tree metadata (NA for surface)
    species, status, height, height_corrected, diameter,
    lenticels, above, chamber_class,

    # Group C: Surface metadata (NA for trees)
    any_of(c("surface_type", "collar_id", "collar_location", "chamber_id",
             "pneumatophore_count", "collar_offset_cm", "collar_volume_cm3")),

    # Group D: Environmental
    air_temp,
    any_of(c("stem_temp", "soil_temp", "water_temp", "water_depth",
             "pressure_start", "rh_start")),

    # Group E: Chamber geometry
    any_of(c("chamber_volume_cm3", "surface_area_cm2",
             "total_system_volume_cm3", "total_system_volume_L",
             "analyzer_cell_volume_cm3", "tubing_volume_cm3")),

    # Group F: CH4 results + QC
    CH4_best.flux, CH4_model, CH4_quality.check,
    CH4_flux_status, CH4_below_MDF, CH4_flagged, CH4_SNR,
    CH4_LM.flux, CH4_LM.SE, CH4_LM.r2, CH4_LM.p.val,
    CH4_HM.flux, CH4_HM.SE, CH4_HM.r2,
    CH4_MDF, CH4_prec, CH4_nb.obs, CH4_flux.term,
    CH4_LM.diagnose, CH4_HM.diagnose,
    CH4_LM.score, CH4_HM.score, CH4_g.fact,
    any_of(c("CH4_k.ratio.lim", "CH4_MDF.lim", "CH4_warn.nb.obs")),

    # Group G: CO2 results + QC
    CO2_best.flux, CO2_model, CO2_quality.check,
    CO2_flux_status, CO2_below_MDF, CO2_flagged, CO2_SNR,
    CO2_LM.flux, CO2_LM.SE, CO2_LM.r2, CO2_LM.p.val,
    CO2_HM.flux, CO2_HM.SE, CO2_HM.r2,
    CO2_MDF, CO2_prec, CO2_nb.obs, CO2_flux.term,
    CO2_LM.diagnose, CO2_HM.diagnose,
    CO2_LM.score, CO2_HM.score, CO2_g.fact,
    any_of(c("CO2_k.ratio.lim", "CO2_MDF.lim", "CO2_warn.nb.obs")),

    # Group H: Derived
    year, month, month_year, season, disturbance_level,
    pneumatophore_density, flux_status, notes, source_file
  )

cat("  Final dimensions:", nrow(final), "rows x", ncol(final), "columns\n")

# =============================================================================
# STEP 12: Validation
# =============================================================================

cat("\nStep 12: Validating...\n")

errors <- 0

# Row count
if (nrow(final) != nrow(all_trees) + nrow(all_surfaces)) {
  cat("  ERROR: Row count mismatch! Expected", nrow(all_trees) + nrow(all_surfaces), "got", nrow(final), "\n")
  errors <- errors + 1
} else {
  cat("  OK: Row count:", nrow(final), "\n")
}

# No duplicate flux_ids
n_dup <- sum(duplicated(final$flux_id))
if (n_dup > 0) {
  cat("  ERROR:", n_dup, "duplicate flux_ids\n")
  errors <- errors + 1
} else {
  cat("  OK: No duplicate flux_ids\n")
}

# Rescued rows have flux data
rescued_final <- final %>% filter(data_source == "rescued")
n_ch4_na <- sum(is.na(rescued_final$CH4_best.flux))
n_co2_na <- sum(is.na(rescued_final$CO2_best.flux))
if (n_ch4_na > 0) {
  cat("  NOTE:", n_ch4_na, "rescued rows missing CH4 (CO2-only recoveries)\n")
}
if (n_co2_na > 0) {
  cat("  WARNING:", n_co2_na, "rescued rows still missing CO2\n")
}
cat("  Rescued CH4 non-NA:", sum(!is.na(rescued_final$CH4_best.flux)), "/", nrow(rescued_final), "\n")
cat("  Rescued CO2 non-NA:", sum(!is.na(rescued_final$CO2_best.flux)), "/", nrow(rescued_final), "\n")

# Status standardized
bad_status <- final$status[!is.na(final$status) & !(final$status %in% c("alive", "dead", "CWD"))]
if (length(bad_status) > 0) {
  cat("  ERROR: Non-standard status values:", paste(unique(bad_status), collapse = ", "), "\n")
  errors <- errors + 1
} else {
  cat("  OK: Status values standardized\n")
}

# Above standardized
bad_above <- final$above[!is.na(final$above) & !(final$above %in% c("sediment", "water"))]
if (length(bad_above) > 0) {
  cat("  ERROR: Non-standard above values:", paste(unique(bad_above), collapse = ", "), "\n")
  errors <- errors + 1
} else {
  cat("  OK: Above values standardized\n")
}

if (errors == 0) {
  cat("  ALL VALIDATIONS PASSED\n")
} else {
  cat("  ", errors, "VALIDATION ERRORS - review output carefully\n")
}

# =============================================================================
# WRITE OUTPUT
# =============================================================================

outfile <- "output/combined_gas_flux_dataset.csv"
write_csv(final, outfile)
cat("\nWrote:", outfile, "\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n===============================================================================\n")
cat("DATASET SUMMARY\n")
cat("===============================================================================\n\n")

cat(sprintf("Total measurements: %d (%d columns)\n", nrow(final), ncol(final)))
cat(sprintf("  Trees: %d\n", sum(final$measurement_type == "tree")))
cat(sprintf("  Surface: %d\n", sum(final$measurement_type == "surface")))

cat("\nBy component:\n")
final %>% count(component) %>% print()

cat("\nBy data source:\n")
final %>% count(data_source) %>% print()

cat("\nFlux status:\n")
cat(sprintf("  CH4 valid: %d / %d (%.1f%%)\n",
            sum(final$CH4_flux_status == "valid"),
            nrow(final),
            100 * mean(final$CH4_flux_status == "valid")))
cat(sprintf("  CO2 valid: %d / %d (%.1f%%)\n",
            sum(final$CO2_flux_status == "valid"),
            nrow(final),
            100 * mean(final$CO2_flux_status == "valid")))
cat(sprintf("  No data (both gases): %d\n", sum(final$flux_status == "no_data")))

cat("\nQC flags (among valid fluxes):\n")
valid_ch4 <- final %>% filter(CH4_flux_status == "valid")
valid_co2 <- final %>% filter(CO2_flux_status == "valid")
cat(sprintf("  CH4 below MDF: %d / %d (%.1f%%)\n",
            sum(valid_ch4$CH4_below_MDF), nrow(valid_ch4),
            100 * mean(valid_ch4$CH4_below_MDF)))
cat(sprintf("  CO2 below MDF: %d / %d (%.1f%%)\n",
            sum(valid_co2$CO2_below_MDF), nrow(valid_co2),
            100 * mean(valid_co2$CO2_below_MDF)))
cat(sprintf("  CH4 flagged (quality.check): %d / %d (%.1f%%)\n",
            sum(valid_ch4$CH4_flagged), nrow(valid_ch4),
            100 * mean(valid_ch4$CH4_flagged)))
cat(sprintf("  CO2 flagged (quality.check): %d / %d (%.1f%%)\n",
            sum(valid_co2$CO2_flagged), nrow(valid_co2),
            100 * mean(valid_co2$CO2_flagged)))
cat(sprintf("  CH4 median SNR: %.1f\n", median(valid_ch4$CH4_SNR, na.rm = TRUE)))
cat(sprintf("  CO2 median SNR: %.1f\n", median(valid_co2$CO2_SNR, na.rm = TRUE)))

cat("\nBy site:\n")
final %>% count(plot) %>% print()

cat("\nBy season:\n")
final %>% count(season) %>% print()

cat("\n=== DONE ===\n")
