#!/usr/bin/env Rscript
# PATCHED stitch_all_files.R
# Fixes column type mismatches between original and rescued datasets

library(dplyr)
library(readr)

cat("=== GAS FLUX DATASET COMPILATION WITH RESCUED FLUXES (PATCHED) ===\n\n")

# =============================================================================
# STEP 1: DEFINE FILE PATHS
# =============================================================================

# Original complete datasets
file_paths <- c(
  "intermediate/results_surface/Picarro_soilwater_CH4_final_complete_dataset.csv",
  "intermediate/results_surface/LGR3_final_complete_dataset_soil.csv", 
  "intermediate/results_surface/LGR2_final_complete_dataset_soil.csv",
  "intermediate/results_surface/LGR1_final_complete_dataset_soil.csv",
  "intermediate/results_trees/Picarro_CH4_final_complete_dataset.csv",
  "intermediate/results_trees/LGR3_final_complete_dataset.csv",
  "intermediate/results_trees/LGR3_final_complete_dataset_additional.csv",
  "intermediate/results_trees/LGR2_final_complete_dataset.csv",
  "intermediate/results_trees/LGR1_final_complete_dataset.csv"
)

# Rescued flux files
rescued_files <- c(
  "intermediate/rescue/ALL_RESCUED_CH4_BEST_FLUX.csv",
  "intermediate/rescue/ALL_RESCUED_CO2_BEST_FLUX.csv"
)

# =============================================================================
# STEP 2: DEFINE HELPER FUNCTIONS
# =============================================================================

# Standardize column types to avoid binding conflicts
standardize_column_types <- function(df) {
  # Convert problematic columns to character to avoid type conflicts
  if ("index" %in% names(df)) {
    df$index <- as.character(df$index)
  }
  if ("no" %in% names(df)) {
    df$no <- as.character(df$no)
  }
  if ("height" %in% names(df)) {
    df$height <- as.character(df$height)
  }
  if ("lenticels" %in% names(df)) {
    df$lenticels <- as.character(df$lenticels)
  }
  
  # CRITICAL FIX: Convert month to character (rescued data might have it as numeric)
  if ("month" %in% names(df)) {
    df$month <- as.character(df$month)
  }
  
  # CRITICAL FIX: Convert year to numeric (for consistency)
  if ("year" %in% names(df)) {
    df$year <- as.numeric(df$year)
  }
  
  # Convert date columns to character to avoid parsing issues
  date_cols <- c("date", "start_time", "end_time", "datetime", "date_only", "date_parsed", "processing_date")
  for (col in date_cols) {
    if (col %in% names(df)) {
      df[[col]] <- as.character(df[[col]])
    }
  }
  
  return(df)
}

# Read and standardize original datasets
read_and_standardize <- function(file_path) {
  
  cat("Reading:", file_path, "\n")
  
  # Read the CSV file
  df <- read_csv(file_path, show_col_types = FALSE)
  
  # Standardize column types to avoid binding issues
  df <- standardize_column_types(df)
  
  # Add source file information
  df$source_file <- basename(file_path)
  
  # Extract measurement type from file path
  if (grepl("surface", file_path)) {
    df$measurement_location <- "surface"
  } else if (grepl("trees", file_path)) {
    df$measurement_location <- "trees"
  } else {
    df$measurement_location <- "unknown"
  }
  
  # Extract analyzer type from filename
  if (grepl("Picarro", file_path)) {
    df$analyzer_type <- "Picarro"
    if (grepl("soilwater", file_path)) {
      df$measurement_category <- "soil_water"
    } else {
      df$measurement_category <- "standard"
    }
  } else if (grepl("LGR", file_path)) {
    # Extract LGR number (1, 2, or 3)
    lgr_num <- regmatches(file_path, regexpr("LGR[0-9]", file_path))
    df$analyzer_type <- lgr_num
    
    if (grepl("soil", file_path)) {
      df$measurement_category <- "soil"
    } else if (grepl("additional", file_path)) {
      df$measurement_category <- "additional"
    } else {
      df$measurement_category <- "standard"
    }
  }
  
  # Mark as original (not rescued)
  df$data_source <- "original"
  
  return(df)
}

# Read and standardize rescued flux datasets
read_rescued_data <- function(file_path) {
  
  cat("\nReading rescued fluxes:", file_path, "\n")
  
  # Read the rescued flux file
  df <- read_csv(file_path, show_col_types = FALSE)
  
  # Standardize column types BEFORE adding metadata
  df <- standardize_column_types(df)
  
  # Add metadata
  df$source_file <- basename(file_path)
  df$data_source <- "rescued"
  df$analyzer_type <- df$rescued_analyzer
  df$measurement_category <- "rescued"
  
  # Determine measurement location from UniqueID patterns
  df$measurement_location <- case_when(
    grepl("_Water_|_Soil_", df$UniqueID) ~ "surface",
    grepl("_stem|_root|_branch", df$UniqueID) ~ "trees",
    grepl("_CWD", df$UniqueID) ~ "trees",  # Added CWD pattern
    TRUE ~ "unknown"
  )
  
  # IMPORTANT: Create a composite ID that includes gas type
  # This prevents "duplicates" when the same UniqueID has both CH4 and CO2
  df$UniqueID_Gas <- paste(df$UniqueID, df$rescued_gas, sep = "_")
  
  cat("Loaded", nrow(df), "rescued measurements\n")
  cat("  - Gas:", unique(df$rescued_gas), "\n")
  cat("  - Quality:", sum(df$quality.check == "clean" | is.na(df$quality.check)), "clean,", 
      sum(df$quality.check != "clean" & !is.na(df$quality.check)), "flagged\n")
  
  return(df)
}

# =============================================================================
# STEP 3: READ ALL DATASETS
# =============================================================================

cat("\n=== READING ORIGINAL DATASETS ===\n")
datasets <- lapply(file_paths, read_and_standardize)

cat("\n=== READING RESCUED FLUX DATASETS ===\n")
rescued_datasets <- list()
for (rescued_file in rescued_files) {
  if (file.exists(rescued_file)) {
    rescued_datasets[[rescued_file]] <- read_rescued_data(rescued_file)
  } else {
    cat("Warning: File not found:", rescued_file, "\n")
  }
}

# =============================================================================
# STEP 4: COMBINE ALL DATASETS
# =============================================================================

cat("\n=== COMBINING ALL DATASETS ===\n")

# Combine original datasets
cat("Combining original datasets...\n")
combined_original <- bind_rows(datasets)

# Combine rescued datasets
if (length(rescued_datasets) > 0) {
  cat("Combining rescued datasets...\n")
  combined_rescued <- bind_rows(rescued_datasets)
} else {
  cat("No rescued datasets to combine.\n")
  combined_rescued <- NULL
}

# Combine everything
if (!is.null(combined_rescued)) {
  cat("Merging original and rescued datasets...\n")
  combined_full <- bind_rows(combined_original, combined_rescued)
} else {
  combined_full <- combined_original
}

# =============================================================================
# STEP 5: DATASET SUMMARY
# =============================================================================

cat("\n=== DATASET SUMMARY ===\n")
cat(sprintf("Final combined dataset: %d rows, %d columns\n", nrow(combined_full), ncol(combined_full)))

# Show breakdown by data source
cat("\nBreakdown by data source:\n")
print(table(combined_full$data_source))

# Show breakdown by analyzer type and location
cat("\nBreakdown by analyzer type and measurement location:\n")
print(table(combined_full$analyzer_type, combined_full$measurement_location))

cat("\nBreakdown by measurement category and location:\n")
print(table(combined_full$measurement_category, combined_full$measurement_location))

# Rescued flux summary
if (!is.null(combined_rescued)) {
  cat("\n=== RESCUED FLUX INTEGRATION SUMMARY ===\n")
  cat("Total rescued measurements added:", nrow(combined_rescued), "\n")
  
  # Show rescued by gas type
  cat("\nRescued measurements by gas:\n")
  print(table(combined_rescued$rescued_gas))
  
  # Quality breakdown for rescued data
  if ("quality.check" %in% names(combined_rescued)) {
    rescued_quality <- combined_rescued %>%
      mutate(quality_status = case_when(
        is.na(quality.check) | quality.check == "" ~ "clean",
        TRUE ~ "flagged"
      )) %>%
      group_by(rescued_gas, quality_status) %>%
      summarise(count = n(), .groups = 'drop')
    
    cat("\nRescued flux quality breakdown:\n")
    print(rescued_quality)
  }
  
  # Check metadata completeness for rescued fluxes
  cat("\n=== RESCUED FLUX METADATA COMPLETENESS ===\n")
  if ("plot" %in% names(combined_rescued)) {
    metadata_completeness <- combined_rescued %>%
      summarise(
        total = n(),
        with_plot = sum(!is.na(plot)),
        with_month = sum(!is.na(month)),
        with_year = sum(!is.na(year)),
        with_component = sum(!is.na(component))
      )
    print(metadata_completeness)
    
    if (metadata_completeness$with_plot < metadata_completeness$total) {
      cat("\nWARNING:", metadata_completeness$total - metadata_completeness$with_plot, 
          "rescued fluxes missing plot information!\n")
      cat("Run merge_rescued_metadata.R to fix this.\n")
    } else {
      cat("\n✓ All rescued fluxes have complete metadata!\n")
    }
  }
  
  # Check for same sampling event measured for both gases
  cat("\n=== RESCUED MEASUREMENT STRUCTURE ===\n")
  cat("Note: Many UniqueIDs appear twice (once for CH4, once for CO2)\n")
  cat("This is expected - they represent different gas measurements from the same sampling event\n\n")
  
  unique_sampling_events <- combined_rescued %>%
    select(UniqueID) %>%
    distinct() %>%
    nrow()
  
  cat(sprintf("Unique sampling events: %d\n", unique_sampling_events))
  cat(sprintf("Total gas measurements: %d\n", nrow(combined_rescued)))
  cat(sprintf("Average measurements per event: %.1f\n", nrow(combined_rescued) / unique_sampling_events))
}

# =============================================================================
# STEP 6: HEIGHT CORRECTIONS
# =============================================================================

cat("\n=== APPLYING HEIGHT CORRECTIONS ===\n")

# Convert height and water_depth to numeric
combined_full$height <- as.numeric(combined_full$height)
combined_full$water_depth <- as.numeric(combined_full$water_depth)

# Create the corrected height column (start with original values for ALL rows)
combined_full$height_corrected <- combined_full$height

# Fix negative heights (submerged tap roots) - set to 0 and change component to root
cat("Fixing negative heights (submerged tap roots):\n")
negative_heights <- !is.na(combined_full$height) & combined_full$height < 0
combined_full$height_corrected[negative_heights] <- 0
combined_full$component[negative_heights] <- "root"
cat("Number of negative heights fixed:", sum(negative_heights, na.rm = TRUE), "\n\n")

# Correct height adjustment - only for stem or root components
cat("Applying height correction using water_depth column (for stem/root only):\n\n")

# Find rows where we need to make the correction
needs_correction <- !is.na(combined_full$above) &
  tolower(combined_full$above) == "sediment" & 
  !is.na(combined_full$water_depth) & 
  combined_full$water_depth > 0 &
  !is.na(combined_full$height_corrected) &
  !is.na(combined_full$component) &
  tolower(combined_full$component) %in% c("stem", "root")

# Apply the correction
combined_full$height_corrected[needs_correction] <- 
  combined_full$height_corrected[needs_correction] - combined_full$water_depth[needs_correction]

# Check results
cat("Number of rows corrected for water depth:", sum(needs_correction, na.rm = TRUE), "\n\n")

# View examples of the correction
if(sum(needs_correction) > 0) {
  cat("Examples of corrections made:\n")
  correction_examples <- combined_full[needs_correction, 
                                       c("component", "above", "height", 
                                         "water_depth", "height_corrected", 
                                         "surface", "measurement_location")][1:min(20, sum(needs_correction)),]
  print(correction_examples)
  
  # Show summary of changes
  changes <- combined_full$height[needs_correction] - combined_full$height_corrected[needs_correction]
  cat("\n\nSummary of height adjustments (amount subtracted):\n")
  print(summary(changes))
  
  cat("\n\nOriginal height range for corrected rows:\n")
  print(summary(combined_full$height[needs_correction]))
  
  cat("\nCorrected height range:\n")
  print(summary(combined_full$height_corrected[needs_correction]))
  
  # Check for any negative heights after correction
  negative_heights_after <- sum(combined_full$height_corrected[needs_correction] < 0, na.rm = TRUE)
  if(negative_heights_after > 0) {
    cat("\nWARNING:", negative_heights_after, "rows have negative heights after correction.\n")
    cat("These may need review:\n")
    problem_rows <- combined_full[needs_correction & combined_full$height_corrected < 0, 
                                  c("component", "above", "height", 
                                    "water_depth", "height_corrected")]
    print(problem_rows)
  } else {
    cat("\nNo negative heights after correction. All adjustments look good!\n")
  }
  
  # Show breakdown by component
  cat("\n\nBreakdown of corrections by component:\n")
  print(table(combined_full$component[needs_correction]))
}

# Summary of all corrections
cat("\n\n=== SUMMARY OF ALL CORRECTIONS ===\n")
cat("Negative heights fixed (tap roots):", sum(negative_heights, na.rm = TRUE), "\n")
cat("Heights adjusted for water depth:", sum(needs_correction, na.rm = TRUE), "\n")
cat("Total rows with corrected heights:", 
    sum(combined_full$height != combined_full$height_corrected, na.rm = TRUE), "\n")

# =============================================================================
# STEP 7: UPDATE COMPONENT BASED ON SURFACE
# =============================================================================

# Update component based on surface column
combined_full <- combined_full %>%
  mutate(component = case_when(
    surface == "Soil" ~ "soil",
    surface == "Water" ~ "water",
    TRUE ~ component  # Keep original component value for all other cases
  ))

# =============================================================================
# STEP 8: SAVE OUTPUT FILES
# =============================================================================

cat("\n=== SAVING OUTPUT FILES ===\n")

# Save the complete combined dataset
output_file <- "output/combined_gas_flux_dataset.csv"
write_csv(combined_full, output_file)
cat("✓ Saved complete dataset:", output_file, "\n")
cat("  Total rows:", nrow(combined_full), "\n")
cat("  Rescued rows:", sum(combined_full$data_source == "rescued", na.rm = TRUE), "\n")

cat("\n=== PROCESSING COMPLETE ===\n")