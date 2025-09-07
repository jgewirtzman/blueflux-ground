# Load required libraries
library(dplyr)
library(readr)
library(lubridate)

# Create the directory structure if it doesn't exist
if (!dir.exists("flux_code")) {
  dir.create("flux_code")
}
if (!dir.exists("flux_code/auxfiles")) {
  dir.create("flux_code/auxfiles", recursive = TRUE)
}

# Read the tree data
tree_data <- read_csv("flux_code/intermediate_files/main_trees_complete.csv")

# Function to convert tree data to goFlux auxfile format
convert_tree_to_auxfile <- function(tree_data, default_pcham = 101.325) {
  
  # Convert tree data to auxfile format
  auxfile <- tree_data %>%
    # Select and rename relevant columns
    transmute(
      # UniqueID: Use flux_id as unique identifier
      UniqueID = flux_id,
      
      # DATE: Convert date to yyyy-mm-dd format
      DATE = case_when(
        # Handle different date formats that might exist
        grepl("/", date) ~ format(mdy(date), "%Y-%m-%d"),
        grepl("-", date) ~ format(ymd(date), "%Y-%m-%d"),
        TRUE ~ date  # fallback if already in correct format
      ),
      
      # TIME: Use start_time directly (already in hh:mm:ss format)
      TIME = start_time,
      
      # start.time: Combine DATE and TIME
      start.time = paste(DATE, TIME),
      
      # Area: Surface area inside the chamber (cm2)
      Area = surface_area_cm2,
      
      # offset: No offset needed for tree measurements
      offset = 0,
      
      # Vcham: Volume inside the chamber (cm3)
      Vcham = chamber_volume_cm3,
      
      # Vtube: Volume inside the tubing (cm3)
      Vtube = tubing_volume_cm3,
      
      # Vinst: Volume inside the instrument (cm3)
      # Using analyzer_cell_volume_cm3 + filter_volume_cm3
      Vinst = analyzer_cell_volume_cm3 + filter_volume_cm3,
      
      # Vtot: Total volume in liters (already calculated in your data)
      Vtot = total_system_volume_L,
      
      # Tcham: Air temperature inside the chamber (Celsius)
      Tcham = air_temp,
      
      # Pcham: Atmospheric pressure inside the chamber (kPa)
      # Set default value since not available in tree data
      Pcham = default_pcham
    ) %>%
    # Remove any rows with missing critical data
    filter(
      !is.na(UniqueID),
      !is.na(start.time),
      !is.na(Area),
      !is.na(Vtot),
      !is.na(Tcham)  # Also filter out rows with missing temperature
    ) %>%
    # Ensure proper data types
    mutate(
      start.time = ymd_hms(start.time, tz = "America/New_York"),  # Miami local time (EST/EDT)
      Area = as.numeric(Area),
      offset = as.numeric(offset),
      Vcham = as.numeric(Vcham),
      Vtube = as.numeric(Vtube),
      Vinst = as.numeric(Vinst),
      Vtot = as.numeric(Vtot),
      Tcham = as.numeric(Tcham),
      Pcham = as.numeric(Pcham)
    )
  
  return(auxfile)
}

# Convert the data
auxfile <- convert_tree_to_auxfile(tree_data)

# Create separate auxfiles for each instrument
instruments <- unique(tree_data$analyzer_id)
cat("Found instruments:", paste(instruments, collapse = ", "), "\n\n")

# Create auxfiles for each instrument
for(instrument in instruments) {
  # Filter data for this instrument
  instrument_data <- auxfile %>%
    filter(UniqueID %in% tree_data$flux_id[tree_data$analyzer_id == instrument])
  
  cat("Instrument:", instrument, "\n")
  cat("  Number of measurements:", nrow(instrument_data), "\n")
  
  if(nrow(instrument_data) > 0) {
    # Save standard format to flux_code/auxfiles folder
    filename_csv <- paste0("flux_code/auxfiles/tree_auxfile_", tolower(instrument), ".csv")
    write_csv(instrument_data, filename_csv)
    
    # Create goFlux-ready version
    instrument_goflux <- instrument_data %>%
      mutate(start.time = format(start.time, "%Y-%m-%d %H:%M:%S"))
    
    filename_goflux <- paste0("flux_code/auxfiles/tree_auxfile_", tolower(instrument), "_goflux.csv")
    write_csv(instrument_goflux, filename_goflux)
    
    cat("  Files saved:", filename_csv, "and", filename_goflux, "\n")
    
    # Display date range for this instrument
    cat("  Date range:", min(instrument_data$DATE), "to", max(instrument_data$DATE), "\n")
    cat("  Temperature range:", round(min(instrument_data$Tcham, na.rm = TRUE), 1), 
        "to", round(max(instrument_data$Tcham, na.rm = TRUE), 1), "°C\n\n")
  } else {
    cat("  No data found for this instrument\n\n")
  }
}

# Display summary of the overall conversion
cat("Overall Conversion Summary:\n")
cat("Original tree data rows:", nrow(tree_data), "\n")
cat("Total auxfile rows after conversion:", nrow(auxfile), "\n")
cat("Columns in auxfile:", ncol(auxfile), "\n\n")

# Display first few rows of the converted data
cat("First 6 rows of converted auxfile:\n")
print(head(auxfile))

# Check for any missing values in critical columns
missing_check <- auxfile %>%
  summarise(
    missing_UniqueID = sum(is.na(UniqueID)),
    missing_start_time = sum(is.na(start.time)),
    missing_Area = sum(is.na(Area)),
    missing_Vtot = sum(is.na(Vtot)),
    missing_Tcham = sum(is.na(Tcham))
  )

cat("\nMissing values check:\n")
print(missing_check)

# If there were any rows with missing air temperature, report how many were removed
rows_removed <- nrow(tree_data) - nrow(auxfile)
if(rows_removed > 0) {
  cat("\nNote:", rows_removed, "rows were removed due to missing critical data (likely missing air temperature)\n")
}

# Save the complete auxfile (all instruments combined) to flux_code/auxfiles folder
write_csv(auxfile, "flux_code/auxfiles/tree_auxfile_all_instruments.csv")
cat("Complete auxfile saved as 'flux_code/auxfiles/tree_auxfile_all_instruments.csv'\n")

# Create a version compatible with goFlux (with start.time as character)
auxfile_goflux <- auxfile %>%
  mutate(
    start.time = format(start.time, "%Y-%m-%d %H:%M:%S")
  )

# Save the goFlux-ready version to flux_code/auxfiles folder
write_csv(auxfile_goflux, "flux_code/auxfiles/tree_auxfile_all_instruments_goflux.csv")
cat("goFlux-ready auxfile (all instruments) saved as 'flux_code/auxfiles/tree_auxfile_all_instruments_goflux.csv'\n\n")

# Display overall data range and unique values for verification
cat("Overall Data Summary:\n")
cat("Date range:", min(auxfile$DATE), "to", max(auxfile$DATE), "\n")
cat("Total unique measurements:", length(unique(auxfile$UniqueID)), "\n")
cat("Instruments found:", paste(sort(unique(tree_data$analyzer_id[tree_data$flux_id %in% auxfile$UniqueID])), collapse = ", "), "\n")
cat("Temperature range (Tcham):", round(min(auxfile$Tcham, na.rm = TRUE), 1), 
    "to", round(max(auxfile$Tcham, na.rm = TRUE), 1), "°C\n")
cat("Area range:", round(min(auxfile$Area, na.rm = TRUE), 1), 
    "to", round(max(auxfile$Area, na.rm = TRUE), 1), "cm²\n\n")

cat("All auxfiles have been saved to the flux_code/auxfiles/ directory\n")