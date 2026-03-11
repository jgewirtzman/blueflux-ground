#!/usr/bin/env Rscript
# ===============================================================================
# Build Missing Auxfiles for 23 No-Auxfile Measurements
# ===============================================================================
#
# These 23 measurements have field metadata but were never given auxfile entries
# (chamber specs + start times in goFlux format), so goFlux couldn't process them.
#
# This script:
# 1. Gathers all available info for each measurement
# 2. Calculates chamber specs where possible (RA, R2 from dilution data)
# 3. Produces a review CSV for the user to verify/fill gaps
# 4. Builds goFlux-format auxfiles for measurements with complete specs
#
# CHAMBERS NEEDING USER INPUT:
#   RZ  — no volume measurement exists. What chamber is this?
#   P   — no volume measurement. What chamber is this?
#   "pneumataphore chamber" — no volume measurement.
#   R2  — volume known (229 cm3) but surface area unknown
#   RA  — volume known (269 cm3) but surface area unknown
#
# After filling in gaps, re-run this script to generate the auxfiles,
# then run recover_failed_measurements.R to process them.
# ===============================================================================

library(dplyr)
library(readr)
library(readxl)
library(lubridate)

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

cat("=== BUILD MISSING AUXFILES ===\n\n")

# =============================================================================
# STEP 1: Load all data sources
# =============================================================================

cat("Step 1: Loading data...\n")

# Clean dataset (for the 23 no-data measurements)
df <- read_csv("output/combined_gas_flux_dataset.csv", show_col_types = FALSE)
failed <- df %>% filter(flux_status == "no_data")

# All existing auxfile IDs
all_aux <- bind_rows(
  read_csv("intermediate/auxfiles/tree_auxfile_lgr1_complete.csv", show_col_types = FALSE),
  read_csv("intermediate/auxfiles/tree_auxfile_lgr2_complete.csv", show_col_types = FALSE),
  read_csv("intermediate/auxfiles/tree_auxfile_lgr3_complete.csv", show_col_types = FALSE),
  read_csv("intermediate/auxfiles/tree_auxfile_additionallgr3.csv", show_col_types = FALSE),
  read_csv("intermediate/auxfiles/tree_auxfile_picarro_complete.csv", show_col_types = FALSE),
  read_csv("intermediate/auxfiles/soilwater_auxfile_lgr1_goflux.csv", show_col_types = FALSE),
  read_csv("intermediate/auxfiles/soilwater_auxfile_lgr2_goflux.csv", show_col_types = FALSE),
  read_csv("intermediate/auxfiles/soilwater_auxfile_lgr3_goflux.csv", show_col_types = FALSE),
  read_csv("intermediate/auxfiles/soilwater_auxfile_picarro_goflux.csv", show_col_types = FALSE)
)
no_aux_ids <- failed$flux_id[!failed$flux_id %in% all_aux$UniqueID]

cat("  Measurements without auxfile:", length(no_aux_ids), "\n")

# Additional field notes (has 21 of the 23)
add_fn <- read_excel(
  "data/field_notes/entered_data/blueflux compiled tree fluxes_additional.xlsx",
  sheet = "Sheet1"
)

# Compiled field notes (has Mar_23_13_BL60_root)
compiled_fn <- read_excel(
  "data/field_notes/entered_data/blueflux compiled tree fluxes.xlsx",
  sheet = "compiled"
)

# Analyzer specs
# LGR:    analyzer_cell = 70 cm3, tubing = 29 cm3
# Picarro: analyzer_cell = 35 cm3, tubing = 29 cm3
# March 2022: small drierite (277 cm3)
# Oct 2022 / March 2023: large drierite (849 cm3)

analyzer_specs <- list(
  LGR = list(cell = 70, tubing = 29,
             Vinst_mar22 = 70 + 277,   # = 347
             Vinst_oct22 = 70 + 849),   # = 919
  Picarro = list(cell = 35, tubing = 29,
                 Vinst_mar22 = 35 + 277, # = 312
                 Vinst_oct22 = 35 + 849) # = 884
)

# =============================================================================
# STEP 2: Known chamber volumes from dilution measurements
# =============================================================================

cat("\nStep 2: Chamber specs from dilution measurements...\n")

# From simplified_volume.csv, total = Vcham + tubing(29) + cell(70) [LGR, no drierite]
# So Vcham = total - 99
chamber_vcham <- list(
  A  = 213,   # verified: mean(312-99) = 213
  B  = 419,   # verified: mean(518-99) = 419
  C  = 1065,  # verified: mean(1164-99) = 1065
  D  = 2191,  # from auxfile
  RA = 269,   # from dilution: mean(368-99) = 269
  R2 = 229    # from dilution: 328-99 = 229
)

# Surface areas from surface_area.csv
chamber_area <- list(
  A = 40.4,
  B = 108,
  C = 213,
  D = 462
  # RA, R2, RZ, P — UNKNOWN, need user input
)

cat("  Known Vcham: A=213, B=419, C=1065, D=2191, RA=269, R2=229\n")
cat("  Known Area:  A=40, B=108, C=213, D=462\n")
cat("  Unknown Area: RA, R2, RZ, P, pneumatophore chamber\n")
cat("  Unknown Vcham: RZ, P, pneumatophore chamber\n")

# =============================================================================
# STEP 3: Build review table with all available info
# =============================================================================

cat("\nStep 3: Building review table...\n")

# Combine info from both field note sources
review <- tibble(flux_id = no_aux_ids) %>%
  # Join additional field notes (21 matches)
  left_join(
    add_fn %>%
      select(flux_id, date_add = date, start_time_add = start_time, end_time_add = end_time,
             component_add = component, species_add = species, analyzer_add = analyzer,
             chamber_add = chamber, diameter_add = diameter, height_add = height,
             stem_temp_add = stem_temp, air_temp_add = air_temp),
    by = "flux_id"
  ) %>%
  # Join compiled field notes (1 match: Mar_23_13_BL60_root)
  left_join(
    compiled_fn %>%
      select(flux_id, chamber_comp = chamber, chamber_class_comp = chamber_class,
             diameter_comp = diameter),
    by = "flux_id"
  ) %>%
  # Join clean dataset for any other info
  left_join(
    df %>%
      filter(flux_id %in% no_aux_ids) %>%
      select(flux_id, plot, date_clean = date, start_time_ds = start_time, end_time_ds = end_time,
             component, analyzer_source, measurement_type),
    by = "flux_id"
  ) %>%
  mutate(
    # Consolidate fields
    chamber = coalesce(chamber_add, chamber_comp),
    chamber_class = chamber_class_comp,
    analyzer = coalesce(analyzer_add, analyzer_source),
    date_field = coalesce(as.Date(date_add), date_clean),
    start_time_field = format(start_time_add, "%H:%M:%S"),
    end_time_field = format(end_time_add, "%H:%M:%S"),
    diameter_inches = coalesce(diameter_add, as.numeric(diameter_comp)),

    # Map chamber to known Vcham
    Vcham = case_when(
      chamber %in% c("A1","A2","A3","A4","A5","A6","A7") ~ 213,
      chamber %in% c("B1","B2","B3","B4","B5","B6","B7","B","S3","S4") ~ 419,
      chamber %in% c("C1","C2","C3","C4","C5","S1") ~ 1065,
      chamber %in% c("D1","D2","D4","S6") ~ 2191,
      chamber == "RA" ~ 269,
      chamber == "R2" ~ 229,
      chamber_class == "HA" ~ NA_real_,  # needs diameter-based calculation
      TRUE ~ NA_real_  # RZ, P, pneumataphore chamber — UNKNOWN
    ),

    # Map to Area (surface area cm2)
    Area = case_when(
      chamber_class == "HA" & !is.na(diameter_inches) ~ {
        # HA: lateral surface area of cylinder = 2 * pi * r * h
        # where r = stem radius, h = A-series chamber height (3 inches)
        r_cm <- (diameter_inches / 2) * 2.54
        h_cm <- 3 * 2.54  # A-series height = 3 inches
        2 * pi * r_cm * h_cm
      },
      TRUE ~ NA_real_  # Will need user input for R2, RA, RZ, P
    ),

    # Determine Vinst based on analyzer and date
    trip = case_when(
      month(date_field) == 3 & year(date_field) == 2022 ~ "mar22",
      month(date_field) == 10 & year(date_field) == 2022 ~ "oct22",
      month(date_field) == 3 & year(date_field) == 2023 ~ "mar23",
      TRUE ~ NA_character_
    ),
    Vinst = case_when(
      analyzer == "Picarro" & trip == "mar22" ~ 312,  # 35 + 277
      analyzer == "Picarro" & trip %in% c("oct22", "mar23") ~ 884,  # 35 + 849
      analyzer == "LGR" & trip == "mar22" ~ 347,   # 70 + 277
      analyzer == "LGR" & trip %in% c("oct22", "mar23") ~ 919,  # 70 + 849
      TRUE ~ NA_real_
    ),
    Vtube = 29,

    # Vtot in liters
    Vtot = ifelse(!is.na(Vcham) & !is.na(Vinst),
                  (Vcham + Vtube + Vinst) / 1000, NA_real_),

    # Temperature and pressure
    Tcham = coalesce(stem_temp_add, air_temp_add),
    Pcham = 101.325,

    # Time issues
    time_status = case_when(
      is.na(start_time_add) & is.na(start_time_ds) ~ "NO_START_TIME",
      is.na(end_time_add) ~ "NO_END_TIME",
      !is.na(start_time_field) & !is.na(end_time_field) &
        start_time_field > end_time_field ~ "END_BEFORE_START",
      TRUE ~ "OK"
    ),

    # Overall readiness
    ready = !is.na(Vcham) & !is.na(Area) & !is.na(Vinst) & time_status == "OK"
  )

# =============================================================================
# STEP 4: Print review table
# =============================================================================

cat("\n===============================================================================\n")
cat("REVIEW TABLE — All 23 measurements\n")
cat("===============================================================================\n\n")

review %>%
  select(flux_id, chamber, analyzer, component, time_status,
         Vcham, Area, Vinst, Vtot, ready) %>%
  print(n = Inf, width = 200)

cat("\n=== STATUS SUMMARY ===\n")
cat("Ready to build auxfile:", sum(review$ready), "\n")
cat("Missing chamber specs:", sum(!review$ready & review$time_status == "OK"), "\n")
cat("Time issues:", sum(review$time_status != "OK"), "\n\n")

cat("By chamber type:\n")
review %>%
  group_by(chamber, time_status) %>%
  summarise(
    n = n(),
    has_Vcham = sum(!is.na(Vcham)),
    has_Area = sum(!is.na(Area)),
    ready = sum(ready),
    .groups = "drop"
  ) %>%
  print(n = Inf)

# =============================================================================
# STEP 5: Save review CSV for user to fill in gaps
# =============================================================================

review_out <- review %>%
  select(
    flux_id, plot, date_field, component, analyzer, chamber, chamber_class,
    diameter_inches, start_time_field, end_time_field, time_status,
    Vcham, Area, Vtube, Vinst, Vtot, Tcham, Pcham, ready
  )

write_csv(review_out, "intermediate/rescue/missing_auxfile_review.csv")
cat("\nSaved: intermediate/rescue/missing_auxfile_review.csv\n")
cat("INSTRUCTIONS: Fill in missing Vcham and Area values, then re-run this script.\n")

# =============================================================================
# STEP 6: Build auxfiles for any that are ready
# =============================================================================

ready_rows <- review %>% filter(ready)
cat("\n")

if (nrow(ready_rows) > 0) {
  cat("Building auxfiles for", nrow(ready_rows), "ready measurements...\n")

  auxfile_out <- ready_rows %>%
    transmute(
      UniqueID = flux_id,
      DATE = as.character(date_field),
      TIME = start_time_field,
      start.time = paste(DATE, TIME),
      Area = Area,
      offset = 0,
      Vcham = Vcham,
      Vtube = Vtube,
      Vinst = Vinst,
      Vtot = Vtot,
      Tcham = ifelse(is.na(Tcham), 30, Tcham),  # default 30C if missing
      Pcham = Pcham
    )

  # Split by analyzer for separate auxfiles
  for (anlz in unique(ready_rows$analyzer)) {
    subset <- auxfile_out %>%
      filter(UniqueID %in% ready_rows$flux_id[ready_rows$analyzer == anlz])
    outname <- paste0("intermediate/auxfiles/recovery_auxfile_", tolower(anlz), ".csv")
    write_csv(subset, outname)
    cat("  Saved:", outname, "(", nrow(subset), "rows)\n")
  }
} else {
  cat("No measurements are fully ready — fill in the review CSV first.\n")
}

# =============================================================================
# STEP 7: Guidance for user
# =============================================================================

cat("\n===============================================================================\n")
cat("WHAT YOU NEED TO PROVIDE\n")
cat("===============================================================================\n\n")

unknown_chambers <- review %>%
  filter(is.na(Vcham) | is.na(Area)) %>%
  distinct(chamber) %>%
  pull(chamber)

for (ch in unknown_chambers) {
  ch_rows <- review %>% filter(chamber == ch)
  cat(sprintf("Chamber '%s' (%d measurements):\n", ch, nrow(ch_rows)))
  if (is.na(ch_rows$Vcham[1])) {
    cat("  NEED: Chamber volume (cm3) — Vcham\n")
  }
  if (all(is.na(ch_rows$Area))) {
    cat("  NEED: Chamber surface area (cm2) — Area\n")
  }
  cat("  Used for:", paste(unique(ch_rows$component), collapse = ", "), "\n")
  cat("  Analyzer:", paste(unique(ch_rows$analyzer), collapse = ", "), "\n\n")
}

# Time issues
time_issues <- review %>% filter(time_status != "OK")
if (nrow(time_issues) > 0) {
  cat("MEASUREMENTS WITH TIME ISSUES:\n")
  time_issues %>%
    select(flux_id, chamber, start_time_field, end_time_field, time_status) %>%
    print(n = Inf)
  cat("\nFor END_BEFORE_START: check field notes for correct times.\n")
  cat("For NO_START_TIME: cannot process without start times.\n")
}

cat("\n=== DONE ===\n")
