# R Code to Combine All Gas Flux Datasets
# This script combines data from Picarro and LGR analyzers into one final dataset

library(dplyr)
library(readr)

# Define file paths - adjust these to your actual file locations
file_paths <- c(
  "flux_code/results/surface/Picarro_soilwater_CH4_final_complete_dataset.csv",
  "flux_code/results/surface/LGR3_final_complete_dataset_soil.csv", 
  "flux_code/results/surface/LGR2_final_complete_dataset_soil.csv",
  "flux_code/results/surface/LGR1_final_complete_dataset_soil.csv",
  "flux_code/results/trees/Picarro_CH4_final_complete_dataset.csv",
  "flux_code/results/trees/LGR3_final_complete_dataset.csv",
  "flux_code/results/trees/LGR3_final_complete_dataset_additional.csv",
  "flux_code/results/trees/LGR2_final_complete_dataset.csv",
  "flux_code/results/trees/LGR1_final_complete_dataset.csv"
)

# Step 1: Define the standardize_column_types function first
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
  
  # Convert date columns to character to avoid parsing issues
  date_cols <- c("date", "start_time", "end_time", "datetime", "date_only", "date_parsed", "processing_date")
  for (col in date_cols) {
    if (col %in% names(df)) {
      df[[col]] <- as.character(df[[col]])
    }
  }
  
  return(df)
}

# Step 2: Define the updated read_and_standardize function
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
  
  return(df)
}

# Step 3: Now read all datasets with the corrected function
cat("Reading all datasets with standardized types...\n")
datasets <- lapply(file_paths, read_and_standardize)

# Step 4: Continue with the rest of the original script
all_columns <- unique(unlist(lapply(datasets, names)))
common_columns <- Reduce(intersect, lapply(datasets, names))

#sort(all_columns)

cat("\nDataset summary:\n")
for (i in 1:length(datasets)) {
  cat(sprintf("Dataset %d (%s): %d rows, %d columns\n", 
              i, basename(file_paths[i]), nrow(datasets[[i]]), ncol(datasets[[i]])))
}

cat(sprintf("\nTotal columns across all datasets: %d\n", length(all_columns)))
cat(sprintf("Common columns across all datasets: %d\n", length(common_columns)))

# Step 5: Now try combining - this should work!
cat("\nCombining datasets using full join (keeping all columns)...\n")
combined_full <- bind_rows(datasets)

# Step 6: Continue with summary and saving
cat("Success! Combined dataset created.\n")
cat(sprintf("Final combined dataset: %d rows, %d columns\n", nrow(combined_full), ncol(combined_full)))

# Show breakdown by analyzer type and location
cat("\nBreakdown by analyzer type and measurement location:\n")
print(table(combined_full$analyzer_type, combined_full$measurement_location))
print(table(combined_full$measurement_category, combined_full$measurement_location))

# Save the combined dataset
write_csv(combined_full, "combined_gas_flux_dataset_complete.csv")
cat("✓ Saved: combined_gas_flux_dataset_complete.csv\n")
