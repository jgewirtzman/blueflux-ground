# COMPLETE TREE FLUX PROCESSING WORKFLOW - ALL ANALYZERS (MANUAL WORKFLOW)
# This script handles EVERYTHING but uses the proper manual workflow for goFlux
# for LGR3 and Picarro in one comprehensive workflow

# Load required libraries
library(goFlux)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

cat("==================================================================\n")
cat("    COMPLETE TREE FLUX PROCESSING - MANUAL WORKFLOW\n")
cat("==================================================================\n\n")

# =============================================================================
# STEP 1: LOAD AND PREPARE THE TREE FLUX METADATA
# =============================================================================

cat("=== STEP 1: LOADING TREE FLUX METADATA ===\n")

# Read the additional tree flux data (corrected path)
tree_flux_data <- read_csv("field_notes/blueflux compiled tree fluxes_additional.csv")

# Clean up analyzer_id column (fix typos like LRG3 -> LGR3)
tree_flux_data <- tree_flux_data %>%
  mutate(analyzer_id = case_when(
    analyzer_id == "LRG3" ~ "LGR3",  # Fix typo
    TRUE ~ analyzer_id
  ))

cat("Tree flux metadata loaded:\n")
cat("- Rows:", nrow(tree_flux_data), "\n")
cat("- Columns:", ncol(tree_flux_data), "\n")
cat("- Date range:", min(tree_flux_data$date, na.rm = TRUE), "to", max(tree_flux_data$date, na.rm = TRUE), "\n")

# Check analyzer distribution
analyzer_counts <- table(tree_flux_data$analyzer_id, useNA = "ifany")
cat("\nAnalyzer distribution:\n")
print(analyzer_counts)

# =============================================================================
# STEP 2: FILL MISSING AIR TEMPERATURE DATA
# =============================================================================

cat("\n=== STEP 2: FILLING MISSING AIR TEMPERATURE DATA ===\n")

# Create intermediate directory
if (!dir.exists("flux_code/intermediate_files")) {
  dir.create("flux_code/intermediate_files", recursive = TRUE)
}

# Function to fill missing air temperature values
fill_missing_air_temp <- function(data) {
  # Create datetime column
  data$datetime <- mdy_hms(paste(data$date, data$start_time))
  
  # Try alternative formats if needed
  na_indices <- which(is.na(data$datetime))
  if (length(na_indices) > 0) {
    data$datetime[na_indices] <- dmy_hms(paste(data$date[na_indices], data$start_time[na_indices]))
    na_indices2 <- which(is.na(data$datetime))
    if (length(na_indices2) > 0) {
      data$datetime[na_indices2] <- ymd_hms(paste(data$date[na_indices2], data$start_time[na_indices2]))
    }
  }
  
  data$date_only <- as.Date(data$datetime)
  missing_indices <- which(is.na(data$air_temp))
  cat("Found", length(missing_indices), "missing air temperature values\n")
  
  filled_30min <- 0
  filled_same_day <- 0
  remaining_na <- 0
  
  for (i in missing_indices) {
    target_datetime <- data$datetime[i]
    target_date <- data$date_only[i]
    
    if (is.na(target_datetime) || is.na(target_date)) {
      remaining_na <- remaining_na + 1
      next
    }
    
    # Strategy 1: Find values within 30 minutes
    time_diff <- abs(difftime(data$datetime, target_datetime, units = "mins"))
    within_30min <- which(!is.na(data$air_temp) & 
                            data$date_only == target_date & 
                            time_diff <= 30 & time_diff > 0)
    
    if (length(within_30min) > 0) {
      data$air_temp[i] <- mean(data$air_temp[within_30min], na.rm = TRUE)
      filled_30min <- filled_30min + 1
    } else {
      # Strategy 2: Find nearest time from same day
      same_day_rows <- which(!is.na(data$air_temp) & data$date_only == target_date)
      if (length(same_day_rows) > 0) {
        time_diffs <- abs(difftime(data$datetime[same_day_rows], target_datetime, units = "mins"))
        nearest_idx <- same_day_rows[which.min(time_diffs)]
        data$air_temp[i] <- data$air_temp[nearest_idx]
        filled_same_day <- filled_same_day + 1
      } else {
        remaining_na <- remaining_na + 1
      }
    }
  }
  
  cat("Temperature filling summary:\n")
  cat("- Filled using 30-minute window:", filled_30min, "\n")
  cat("- Filled using nearest same-day value:", filled_same_day, "\n")
  cat("- Remaining as NA:", remaining_na, "\n")
  
  return(data)
}

# Apply temperature filling
tree_flux_filled <- fill_missing_air_temp(tree_flux_data)
write_csv(tree_flux_filled, "flux_code/intermediate_files/blueflux_trees_filled.csv")

# =============================================================================
# STEP 3: ASSIGN VOLUMES AND SURFACE AREAS
# =============================================================================

cat("\n=== STEP 3: ASSIGNING VOLUMES AND SURFACE AREAS ===\n")

# Run the volume and area assignment
source("flux_code/preprocess/assign_tree_vol_area.R")

# Load the complete tree data
tree_complete <- read_csv("flux_code/intermediate_files/main_trees_complete.csv")
cat("Complete tree data loaded:", nrow(tree_complete), "rows\n")

# =============================================================================
# STEP 4: CREATE AUXFILES FOR EACH ANALYZER
# =============================================================================

cat("\n=== STEP 4: CREATING AUXFILES ===\n")

if (!dir.exists("flux_code/auxfiles")) {
  dir.create("flux_code/auxfiles", recursive = TRUE)
}

# Function to create auxfile
create_auxfile <- function(data, analyzer_name) {
  analyzer_data <- data %>%
    filter(analyzer_id == analyzer_name) %>%
    filter(!is.na(total_system_volume_L), !is.na(surface_area_cm2), !is.na(air_temp))
  
  if (nrow(analyzer_data) == 0) return(NULL)
  
  auxfile <- analyzer_data %>%
    transmute(
      UniqueID = flux_id,
      DATE = case_when(
        grepl("/", date) ~ format(mdy(date), "%Y-%m-%d"),
        grepl("-", date) ~ format(ymd(date), "%Y-%m-%d"),
        TRUE ~ date
      ),
      TIME = format(start_time, "%H:%M:%S"),  # Ensure proper time format
      start.time = paste(DATE, TIME),
      Area = surface_area_cm2,
      offset = 0,
      Vcham = chamber_volume_cm3,
      Vtube = tubing_volume_cm3,
      Vinst = analyzer_cell_volume_cm3 + filter_volume_cm3,
      Vtot = total_system_volume_L,
      Tcham = air_temp,
      Pcham = 101.325
    ) %>%
    filter(!is.na(UniqueID), !is.na(Area), !is.na(Vtot), !is.na(Tcham)) %>%
    mutate(
      start.time = format(ymd_hms(start.time, tz = "America/New_York"), "%Y-%m-%d %H:%M:%S"),
      across(c(Area, offset, Vcham, Vtube, Vinst, Vtot, Tcham, Pcham), as.numeric)
    )
  
  return(auxfile)
}

# Create auxfiles for each analyzer
analyzers <- unique(tree_complete$analyzer_id)
analyzers <- analyzers[!is.na(analyzers)]
auxfiles <- list()

for (analyzer in analyzers) {
  auxfile <- create_auxfile(tree_complete, analyzer)
  if (!is.null(auxfile)) {
    filename <- paste0("tree_auxfile_", tolower(analyzer), "_complete.csv")
    filepath <- file.path("flux_code/auxfiles", filename)
    write_csv(auxfile, filepath)
    auxfiles[[analyzer]] <- list(path = filepath, data = auxfile)
    cat("Created auxfile for", analyzer, ":", nrow(auxfile), "measurements\n")
  }
}

# =============================================================================
# STEP 5: IMPORT RAW ANALYZER DATA
# =============================================================================

cat("\n=== STEP 5: IMPORTING RAW ANALYZER DATA ===\n")

# Function to import analyzer data (handles both LGR and Picarro)
import_analyzer_data <- function(analyzer_path, analyzer_name) {
  cat("Importing", analyzer_name, "data...\n")
  
  if (grepl("Picarro|PICARRO|picarro", analyzer_name, ignore.case = TRUE)) {
    # Picarro G4301 import
    dat_files <- list.files(analyzer_path, recursive = TRUE, pattern = "\\.dat$", full.names = TRUE)
    data_files <- dat_files[file.size(dat_files) > 0]
    
    if (length(data_files) == 0) {
      cat("No .dat files found for Picarro in", analyzer_path, "\n")
      return(NULL)
    }
    
    # Create temporary directory
    temp_dir <- tempfile(paste0("picarro_temp_"))
    dir.create(temp_dir, recursive = TRUE)
    file.copy(data_files, temp_dir)
    
    cat("Found", length(data_files), ".dat files for Picarro\n")
    
    # Import Picarro with goFlux
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
      cat("Error importing Picarro:", e$message, "\n")
      return(NULL)
    })
    
    # Cleanup
    unlink(temp_dir, recursive = TRUE)
    
    if (!is.null(picarro_data)) {
      cat("Picarro import complete:", nrow(picarro_data), "rows\n")
    }
    
    return(picarro_data)
    
  } else {
    # LGR import
    # Extract zip files if needed
    zip_files <- list.files(analyzer_path, recursive = TRUE, pattern = "\\.zip$", full.names = TRUE)
    if (length(zip_files) > 0) {
      cat("Extracting", length(zip_files), "zip files for", analyzer_name, "\n")
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
    txt_files <- list.files(analyzer_path, recursive = TRUE, pattern = "\\.txt$", full.names = TRUE)
    clean_txt_files <- txt_files[!grepl("/.*\\.txt/", txt_files)]
    data_files <- clean_txt_files[file.size(clean_txt_files) > 0]
    
    if (length(data_files) == 0) {
      cat("No .txt files found for", analyzer_name, "\n")
      return(NULL)
    }
    
    # Create temporary directory
    temp_dir <- tempfile(paste0(tolower(analyzer_name), "_temp_"))
    dir.create(temp_dir, recursive = TRUE)
    file.copy(data_files, temp_dir)
    
    cat("Found", length(data_files), ".txt files for", analyzer_name, "\n")
    
    # Import with goFlux (LGR UGGA)
    lgr_data <- tryCatch({
      import2RData(
        path = temp_dir,
        instrument = "UGGA",
        date.format = "mdy",
        timezone = "UTC",
        keep_all = FALSE,
        prec = c(0.35, 0.9, 200),  # MGGA GLA131 precision
        merge = TRUE
      )
    }, error = function(e) {
      cat("Error importing", analyzer_name, ":", e$message, "\n")
      return(NULL)
    })
    
    # Cleanup
    unlink(temp_dir, recursive = TRUE)
    
    if (!is.null(lgr_data)) {
      cat(analyzer_name, "import complete:", nrow(lgr_data), "rows\n")
    }
    
    return(lgr_data)
  }
}

# Define data paths based on your actual folder structure
# ADJUST THESE PATHS TO MATCH YOUR DIRECTORY STRUCTURE
data_paths <- list(
  LGR3 = "analyzer_data/LGR_GLA131/LGR3",
  Picarro = "analyzer_data/Picarro_G4301"  # Adjust this path as needed
)

# Import data for each analyzer
analyzer_data <- list()

for (analyzer in names(auxfiles)) {
  if (analyzer %in% names(data_paths)) {
    path <- data_paths[[analyzer]]
    if (dir.exists(path)) {
      analyzer_data[[analyzer]] <- import_analyzer_data(path, analyzer)
    } else {
      cat("Data path not found for", analyzer, ":", path, "\n")
      cat("Please check your folder structure and update data_paths\n")
    }
  } else {
    cat("No data path defined for", analyzer, "\n")
    cat("Available analyzers in auxfiles:", names(auxfiles), "\n")
    cat("Available paths:", names(data_paths), "\n")
  }
}
