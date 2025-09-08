# STEP 4A: LOAD DATA AND CREATE OBSERVATION WINDOWS FOR SOIL/WATER
# This prepares the data for manual identification (Step 4B)
# Corrected based on tree flux workflow patterns from project knowledge

library(goFlux)
library(dplyr)
library(readr)
library(lubridate)

cat("==================================================================\n")
cat("    STEP 4A: LOAD DATA AND CREATE OBSERVATION WINDOWS\n")
cat("==================================================================\n\n")

# =============================================================================
# LOAD AUXFILES AND SET UP PROCESSING
# =============================================================================

cat("=== LOADING AUXFILES AND SETTING UP PROCESSING ===\n")

# Check that auxfiles exist from Step 3
auxfiles_dir <- "flux_code/auxfiles"
if (!dir.exists(auxfiles_dir)) {
  stop("Auxfiles directory not found. Please run Step 3 (convert_to_auxfile_soil_water.R) first.")
}

# Define analyzer paths based on your directory structure
analyzer_paths <- list(
  LGR1 = "analyzer_data/LGR_GLA131/LGR1",
  LGR2 = "analyzer_data/LGR_GLA131/LGR2", 
  LGR3 = "analyzer_data/LGR_GLA131/LGR3",
  Picarro = "analyzer_data/Picarro_G4301"
)

# Check which data directories actually exist
cat("Checking data directory structure:\n")
for (analyzer in names(analyzer_paths)) {
  path <- analyzer_paths[[analyzer]]
  exists <- dir.exists(path)
  cat("- ", analyzer, ": ", path, " - ", ifelse(exists, "EXISTS", "NOT FOUND"), "\n")
  
  if (exists) {
    # Show what's in the directory
    files <- list.files(path, recursive = TRUE)
    cat("  Contains ", length(files), " files\n")
    if (length(files) > 0) {
      # Show file types
      extensions <- table(tools::file_ext(files))
      cat("  File types: ", paste(names(extensions), "=", extensions, collapse = ", "), "\n")
    }
  }
}

# Define auxfile paths
auxfile_paths <- list(
  LGR1 = file.path(auxfiles_dir, "soilwater_auxfile_lgr1_goflux.csv"),
  LGR2 = file.path(auxfiles_dir, "soilwater_auxfile_lgr2_goflux.csv"),
  LGR3 = file.path(auxfiles_dir, "soilwater_auxfile_lgr3_goflux.csv"),
  Picarro = file.path(auxfiles_dir, "soilwater_auxfile_picarro_goflux.csv")
)

# Check which auxfiles exist
available_analyzers <- names(auxfile_paths)[sapply(auxfile_paths, file.exists)]

if (length(available_analyzers) == 0) {
  stop("No auxfiles found. Please run Step 3 first to create auxfiles.")
}

cat("\nAvailable analyzers with auxfiles:", paste(available_analyzers, collapse = ", "), "\n")

# Create output directory
output_dir <- "flux_code/soilwater_goflux_results"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# =============================================================================
# LOAD RAW GAS CONCENTRATION DATA
# =============================================================================

cat("\n=== LOADING RAW GAS CONCENTRATION DATA ===\n")

# Function to load raw data for each analyzer (following tree workflow patterns)
load_analyzer_data <- function(analyzer, data_path) {
  
  cat("Loading", analyzer, "data from", data_path, "\n")
  
  if (!dir.exists(data_path)) {
    cat("Warning: Data directory not found for", analyzer, "\n")
    return(NULL)
  }
  
  # Look for data files
  if (analyzer == "Picarro") {
    # Picarro G4301 files - look for .dat files
    dat_files <- list.files(data_path, pattern = "\\.dat$", recursive = TRUE, full.names = TRUE)
    data_files <- dat_files[file.size(dat_files) > 0]
    
    if (length(data_files) == 0) {
      cat("No .dat files found for Picarro\n")
      return(NULL)
    }
    
    # Create temporary directory for goFlux import
    temp_dir <- tempfile(paste0("picarro_temp_"))
    dir.create(temp_dir, recursive = TRUE)
    file.copy(data_files, temp_dir)
    
    cat("Found", length(data_files), ".dat files for Picarro\n")
    
    # Load Picarro data using goFlux import2RData
    picarro_data <- tryCatch({
      import2RData(
        path = temp_dir,
        instrument = "G4301",
        date.format = "ymd",
        timezone = "UTC",
        keep_all = FALSE,
        prec = c(0.025, 0.1, 10),  # CO2, CH4, H2O precision for G4301
        merge = TRUE
      )
    }, error = function(e) {
      cat("Error loading Picarro data:", e$message, "\n")
      return(NULL)
    })
    
    # Cleanup temporary directory
    unlink(temp_dir, recursive = TRUE)
    
    # Apply timestamp correction for Picarro (from tree workflows)
    if (!is.null(picarro_data) && "POSIX.time" %in% names(picarro_data)) {
      picarro_data <- picarro_data %>%
        mutate(POSIX.time = POSIX.time - 25220)
      cat("Applied timestamp correction (-25220 seconds) to Picarro data\n")
    }
    
    if (!is.null(picarro_data)) {
      cat("Picarro import complete:", nrow(picarro_data), "rows\n")
    }
    
    return(picarro_data)
    
  } else if (grepl("LGR", analyzer)) {
    # LGR files - extract zips and get txt files
    zip_files <- list.files(data_path, pattern = "\\.zip$", recursive = TRUE, full.names = TRUE)
    
    if (length(zip_files) > 0) {
      cat("Found", length(zip_files), "zip files to extract\n")
      for(zip_file in zip_files) {
        extract_dir <- dirname(zip_file)
        tryCatch({
          unzip(zip_file, exdir = extract_dir, overwrite = TRUE)
        }, error = function(e) {
          cat("Error extracting", basename(zip_file), "\n")
        })
      }
    }
    
    # Get all txt files
    txt_files <- list.files(data_path, pattern = "\\.txt$", recursive = TRUE, full.names = TRUE)
    # Remove problematic files (following tree workflow pattern)
    clean_txt_files <- txt_files[!grepl("\\._", txt_files)]
    data_files <- clean_txt_files[file.size(clean_txt_files) > 0]
    
    if (length(data_files) == 0) {
      cat("No .txt files found for", analyzer, "\n")
      return(NULL)
    }
    
    # Create temporary directory for goFlux import
    temp_dir <- tempfile(paste0(tolower(analyzer), "_temp_"))
    dir.create(temp_dir, recursive = TRUE)
    file.copy(data_files, temp_dir)
    
    cat("Found", length(data_files), ".txt files for", analyzer, "\n")
    
    # Load LGR data using goFlux import2RData (following tree workflow timezone)
    lgr_data <- tryCatch({
      import2RData(
        path = temp_dir,
        instrument = "UGGA",  # LGR UGGA instrument
        date.format = "mdy",
        timezone = "UTC",  # Use UTC like tree workflows
        keep_all = FALSE,
        prec = c(0.35, 0.9, 200),  # MGGA GLA131 precision
        merge = TRUE
      )
    }, error = function(e) {
      cat("Error loading", analyzer, "data:", e$message, "\n")
      return(NULL)
    })
    
    # Cleanup temporary directory
    unlink(temp_dir, recursive = TRUE)
    
    if (!is.null(lgr_data)) {
      cat(analyzer, "import complete:", nrow(lgr_data), "rows\n")
    }
    
    return(lgr_data)
  }
  
  return(NULL)
}

# Load data for all available analyzers
analyzer_data <- list()
for (analyzer in available_analyzers) {
  if (analyzer %in% names(analyzer_paths)) {
    analyzer_data[[analyzer]] <- load_analyzer_data(analyzer, analyzer_paths[[analyzer]])
  }
}

# Check loaded data (following tree workflow patterns)
cat("\nData loading summary:\n")
for (analyzer in names(analyzer_data)) {
  if (!is.null(analyzer_data[[analyzer]])) {
    cat("✓ ", analyzer, " data loaded:", nrow(analyzer_data[[analyzer]]), "rows\n")
    
    # Show timezone information (following tree workflow patterns)
    if ("POSIX.time" %in% names(analyzer_data[[analyzer]])) {
      data_tz <- attr(analyzer_data[[analyzer]]$POSIX.time, "tzone")
      if(is.null(data_tz)) data_tz <- "UTC (default)"
      cat("  Timezone:", data_tz, "\n")
      
      # Show date range using POSIX.time column
      tryCatch({
        date_range <- range(analyzer_data[[analyzer]]$POSIX.time, na.rm = TRUE)
        cat("  Date range:", format(date_range[1], "%Y-%m-%d"), "to", format(date_range[2], "%Y-%m-%d"), "\n")
      }, error = function(e) {
        cat("  Date range: unable to determine\n")
      })
    }
    
    # Show available gas columns
    gas_cols <- names(analyzer_data[[analyzer]])[grepl("CO2|CH4|H2O", names(analyzer_data[[analyzer]]))]
    if (length(gas_cols) > 0) {
      cat("  Gas columns:", paste(gas_cols, collapse = ", "), "\n")
    }
  } else {
    cat("✗ ", analyzer, " data: NULL (check data path and files)\n")
  }
}

# =============================================================================
# CREATE OBSERVATION WINDOWS FOR EACH ANALYZER
# =============================================================================

cat("\n=== CREATING OBSERVATION WINDOWS FOR EACH ANALYZER ===\n")

# Function to create observation windows for a single analyzer (following tree workflow)
create_obs_windows <- function(analyzer_name, raw_data, auxfile_path) {
  
  cat("\n--- Creating observation windows for", analyzer_name, "---\n")
  
  if (is.null(raw_data)) {
    cat("No raw data available for", analyzer_name, "\n")
    return(NULL)
  }
  
  if (!file.exists(auxfile_path)) {
    cat("Auxfile not found for", analyzer_name, ":", auxfile_path, "\n")
    return(NULL)
  }
  
  # Check timezone of imported data (following tree workflow patterns)
  data_tz <- attr(raw_data$POSIX.time, "tzone")
  if(is.null(data_tz)) data_tz <- ""
  cat(analyzer_name, "data timezone:", ifelse(data_tz == "", "UTC (default)", data_tz), "\n")
  
  # Load auxfile and match timezone (following tree workflow patterns)
  auxfile <- read_csv(auxfile_path, show_col_types = FALSE) %>%
    mutate(start.time = as.POSIXct(start.time, tz = ifelse(data_tz == "", "UTC", data_tz)))
  
  cat("Auxfile loaded:", nrow(auxfile), "measurements\n")
  cat("Auxfile timezone set to:", attr(auxfile$start.time, "tzone"), "\n")
  
  # Show date ranges for debugging (following tree workflow patterns)
  tryCatch({
    auxfile_dates <- range(auxfile$start.time, na.rm = TRUE)
    cat("Auxfile date range:", format(auxfile_dates[1], "%Y-%m-%d %H:%M"), "to", format(auxfile_dates[2], "%Y-%m-%d %H:%M"), "\n")
    
    data_dates <- range(raw_data$POSIX.time, na.rm = TRUE)
    cat("Raw data date range:", format(data_dates[1], "%Y-%m-%d %H:%M"), "to", format(data_dates[2], "%Y-%m-%d %H:%M"), "\n")
  }, error = function(e) {
    cat("Unable to display date ranges\n")
  })
  
  # Create observation windows (following tree workflow parameters)
  cat("Creating observation windows...\n")
  
  obs_win <- tryCatch({
    obs.win(
      inputfile = raw_data,
      auxfile = auxfile,
      gastype = "CO2dry_ppm",  # Use CO2 as primary gas type like tree workflows
      obs.length = 300,        # 5-minute observations
      shoulder = 300           # 5-minute shoulder (matching tree workflows)
    )
  }, error = function(e) {
    cat("Error creating observation windows:", e$message, "\n")
    cat("This might be due to timestamp misalignment between auxfile and raw data\n")
    return(NULL)
  })
  
  if (is.null(obs_win)) {
    cat("Failed to create observation windows for", analyzer_name, "\n")
    return(NULL)
  }
  
  # Check window quality (following tree workflow patterns)
  if (is.list(obs_win)) {
    window_sizes <- sapply(obs_win, nrow)
    good_windows <- which(window_sizes >= 30)  # At least 30 observations
    cat("Total observation windows:", length(obs_win), "\n")
    cat("Good windows (>=30 obs):", length(good_windows), "\n")
    
    if (length(good_windows) > 0) {
      cat("Window sizes (first 5):", window_sizes[1:min(5, length(window_sizes))], "\n")
      
      # Convert list of windows to single dataframe (following tree workflow patterns)
      obs_win_df <- do.call(rbind, obs_win)
      cat("Combined observation window data:", nrow(obs_win_df), "rows\n")
      cat("Unique measurements:", length(unique(obs_win_df$UniqueID)), "\n")
      
      # Save observation window data for use in Step 4B
      obswin_file <- file.path(output_dir, paste0(tolower(analyzer_name), "_soilwater_obswin.csv"))
      write_csv(obs_win_df, obswin_file)
      cat("Observation windows saved:", obswin_file, "\n")
      
      return(obs_win_df)
    } else {
      cat("No good observation windows found for", analyzer_name, "\n")
      cat("Check auxfile start times and data alignment\n")
      return(NULL)
    }
  } else {
    cat("Unexpected observation window format for", analyzer_name, "\n")
    return(NULL)
  }
}

# Create observation windows for all available analyzers
obswin_results <- list()

for (analyzer in available_analyzers) {
  if (analyzer %in% names(analyzer_data) && !is.null(analyzer_data[[analyzer]])) {
    obswin_result <- create_obs_windows(
      analyzer, 
      analyzer_data[[analyzer]], 
      auxfile_paths[[analyzer]]
    )
    
    if (!is.null(obswin_result)) {
      obswin_results[[analyzer]] <- obswin_result
    }
  }
}

# =============================================================================
# SUMMARY AND NEXT STEPS
# =============================================================================

cat("\n=== STEP 4A COMPLETE ===\n")
cat("Observation windows created for soil/water flux data\n\n")

cat("Summary:\n")
if (length(obswin_results) > 0) {
  for (analyzer in names(obswin_results)) {
    result <- obswin_results[[analyzer]]
    unique_measurements <- length(unique(result$UniqueID))
    cat("✓ ", analyzer, ": ", nrow(result), " rows, ", unique_measurements, " unique measurements\n", sep = "")
  }
} else {
  cat("⚠ No observation windows were successfully created\n")
  cat("This could be due to:\n")
  cat("- Timezone misalignment between auxfiles and raw data\n")
  cat("- Date range mismatch between auxfiles and raw data\n")
  cat("- Missing gas concentration columns\n")
}

cat("\nFiles created in", output_dir, ":\n")
obswin_files <- list.files(output_dir, pattern = "*_obswin.csv$", full.names = FALSE)
if (length(obswin_files) > 0) {
  for (file in obswin_files) {
    cat("✓ ", file, "\n")
  }
} else {
  cat("⚠ No observation window files were created\n")
}

if (length(obswin_results) > 0) {
  cat("\nNext Step:\n")
  cat("Run Step 4B (manual identification) for each analyzer separately.\n")
  cat("You'll need to manually click through each measurement to identify flux periods.\n\n")
  
  cat("Instructions for Step 4B:\n")
  cat("1. Open the manual identification script (Step 4B)\n")
  cat("2. Set ANALYZER_TO_PROCESS to one of: ", paste(names(obswin_results), collapse = ", "), "\n")
  cat("3. Run the script and click through measurements\n")
  cat("4. Repeat for each analyzer\n\n")
  
  # Show example for first available analyzer
  first_analyzer <- names(obswin_results)[1]
  cat("Example for", first_analyzer, ":\n")
  cat("# In Step 4B script, set:\n")
  cat("ANALYZER_TO_PROCESS <- \"", first_analyzer, "\"\n", sep = "")
  cat("# Then run the script\n")
} else {
  cat("\nTroubleshooting:\n")
  cat("1. Check that auxfile timestamps match raw data timestamps\n")
  cat("2. Verify timezone settings are consistent\n")
  cat("3. Ensure gas concentration columns are present in raw data\n")
  cat("4. Check that Step 3 (auxfile creation) completed successfully\n")
  cat("5. Verify that date ranges overlap between auxfiles and raw data\n")
}

cat("\nStep 4A complete!\n")
cat("==================================================================\n")