# STEP 3: CONVERT SOIL/WATER DATA TO GOFLUX AUXFILE FORMAT - REVISED
# Simple sequential numbering approach to create unique IDs

library(dplyr)
library(readr)
library(lubridate)

cat("==================================================================\n")
cat("    STEP 3: CONVERT SOIL/WATER DATA TO AUXFILE FORMAT\n")
cat("==================================================================\n\n")

# =============================================================================
# LOAD DATA
# =============================================================================

cat("=== LOADING COMPLETE SOIL/WATER DATA ===\n")

# Read the complete soil/water data from Step 2
soil_water_complete <- read_csv("flux_code/intermediate_files/main_soilwater_complete.csv", show_col_types = FALSE)

cat("Complete soil/water data loaded:", nrow(soil_water_complete), "rows\n")
cat("Date range:", min(soil_water_complete$date, na.rm = TRUE), "to", max(soil_water_complete$date, na.rm = TRUE), "\n")

# Check analyzer distribution
analyzer_counts <- table(soil_water_complete$analyzer_id, useNA = "ifany")
cat("\nAnalyzer distribution:\n")
print(analyzer_counts)

# =============================================================================
# CREATE AUXFILE DIRECTORY
# =============================================================================

if (!dir.exists("flux_code/auxfiles")) {
  dir.create("flux_code/auxfiles", recursive = TRUE)
}

# =============================================================================
# CONVERT TO AUXFILE FORMAT
# =============================================================================

cat("\n=== CONVERTING TO AUXFILE FORMAT ===\n")

# Function to convert soil/water data to goFlux auxfile format
convert_soilwater_to_auxfile <- function(soil_water_data, default_pcham = 101.325) {
  
  cat("Converting", nrow(soil_water_data), "soil/water measurements to auxfile format\n")
  
  # Filter to only complete records
  complete_data <- soil_water_data %>%
    filter(
      !is.na(total_system_volume_L),
      !is.na(surface_area_cm2),
      !is.na(air_temp),
      !is.na(datetime),
      !is.na(flux_id)
    )
  
  cat("Records with complete data:", nrow(complete_data), "\n")
  
  if (nrow(complete_data) == 0) {
    stop("No complete records found for auxfile conversion")
  }
  
  # Create unique flux_ids by simply adding row numbers
  complete_data_unique <- complete_data %>%
    arrange(datetime) %>%  # Sort by time to make numbering meaningful
    mutate(
      UniqueID = paste0("SW_", sprintf("%03d", row_number()))  # SW_001, SW_002, etc.
    )
  
  cat("Records with unique IDs:", nrow(complete_data_unique), "\n")
  
  # Convert to auxfile format and preserve analyzer info
  auxfile <- complete_data_unique %>%
    transmute(
      # UniqueID: Use the simple sequential ID
      UniqueID = UniqueID,
      
      # DATE: Convert date to yyyy-mm-d format
      DATE = case_when(
        # Handle different date formats that might exist
        grepl("/", date) ~ format(mdy(date), "%Y-%m-%d"),
        grepl("-", date) ~ format(ymd(date), "%Y-%m-%d"),
        TRUE ~ date  # fallback if already in correct format
      ),
      
      # TIME: Use start_time directly (should be in hh:mm:ss format)
      TIME = start_time,
      
      # start.time: Combine DATE and TIME as POSIXct
      start.time = ymd_hms(paste(DATE, TIME), tz = "America/New_York"),
      
      # Area: Surface area inside the chamber (cm2)
      Area = surface_area_cm2,
      
      # offset: No offset needed for soil/water measurements
      offset = 0,
      
      # Vcham: Volume inside the chamber (cm3) - chamber + collar
      Vcham = chamber_volume_cm3,
      
      # Vtube: Volume inside the tubing (cm3)
      Vtube = tubing_volume_cm3,
      
      # Vinst: Volume inside the instrument (cm3)
      # Using analyzer_cell_volume_cm3 + filter_volume_cm3
      Vinst = analyzer_cell_volume_cm3 + filter_volume_cm3,
      
      # Vtot: Total volume in liters (already calculated)
      Vtot = total_system_volume_L,
      
      # Tcham: Air temperature inside the chamber (Celsius)
      Tcham = air_temp,
      
      # Pcham: Atmospheric pressure inside the chamber (kPa)
      # Use default value since not directly measured
      Pcham = default_pcham,
      
      # Preserve key info for later use
      analyzer_id = analyzer_id,
      chamber_id = chamber_id,
      plot = plot,
      surface = surface
    ) %>%
    # Remove any rows with missing critical data
    filter(
      !is.na(UniqueID),
      !is.na(start.time),
      !is.na(Area),
      !is.na(Vtot),
      !is.na(Tcham)
    ) %>%
    # Ensure proper data types
    mutate(
      Area = as.numeric(Area),
      offset = as.numeric(offset),
      Vcham = as.numeric(Vcham),
      Vtube = as.numeric(Vtube),
      Vinst = as.numeric(Vinst),
      Vtot = as.numeric(Vtot),
      Tcham = as.numeric(Tcham),
      Pcham = as.numeric(Pcham)
    )
  
  # Check for any parsing failures
  datetime_failures <- sum(is.na(auxfile$start.time))
  if (datetime_failures > 0) {
    cat("Warning:", datetime_failures, "rows had datetime parsing failures\n")
  }
  
  # Remove rows with failed datetime parsing
  auxfile <- auxfile %>%
    filter(!is.na(start.time))
  
  cat("Final auxfile records:", nrow(auxfile), "\n")
  
  return(auxfile)
}

# Convert the data
auxfile <- convert_soilwater_to_auxfile(soil_water_complete)

# =============================================================================
# QUALITY CHECKS
# =============================================================================

cat("\n=== QUALITY CHECKS ===\n")

# Check data completeness
cat("Auxfile data summary:\n")
cat("- Total records:", nrow(auxfile), "\n")
cat("- Date range:", min(auxfile$DATE), "to", max(auxfile$DATE), "\n")
cat("- Unique measurements:", length(unique(auxfile$UniqueID)), "\n")

# Check for missing values in critical columns
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

# Value ranges
if (nrow(auxfile) > 0) {
  value_ranges <- auxfile %>%
    summarise(
      min_Area = round(min(Area, na.rm = TRUE), 1),
      max_Area = round(max(Area, na.rm = TRUE), 1),
      min_Vtot = round(min(Vtot, na.rm = TRUE), 3),
      max_Vtot = round(max(Vtot, na.rm = TRUE), 3),
      min_Tcham = round(min(Tcham, na.rm = TRUE), 1),
      max_Tcham = round(max(Tcham, na.rm = TRUE), 1)
    )
  
  cat("\nValue ranges:\n")
  cat("- Area:", value_ranges$min_Area, "to", value_ranges$max_Area, "cm²\n")
  cat("- Total volume:", value_ranges$min_Vtot, "to", value_ranges$max_Vtot, "L\n")
  cat("- Temperature:", value_ranges$min_Tcham, "to", value_ranges$max_Tcham, "°C\n")
}

# Summary by analyzer (using preserved info)
analyzer_summary <- auxfile %>%
  group_by(analyzer_id) %>%
  summarise(
    count = n(),
    date_range = paste(min(DATE), "to", max(DATE)),
    avg_area = round(mean(Area, na.rm = TRUE), 1),
    avg_volume = round(mean(Vtot, na.rm = TRUE), 3),
    avg_temp = round(mean(Tcham, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(analyzer_id)

cat("\nSummary by analyzer:\n")
print(analyzer_summary)

# Summary by chamber type (using preserved info)
chamber_summary <- auxfile %>%
  group_by(chamber_id, surface) %>%
  summarise(
    count = n(),
    analyzers = paste(sort(unique(analyzer_id)), collapse = ", "),
    avg_area = round(mean(Area, na.rm = TRUE), 1),
    avg_volume = round(mean(Vtot, na.rm = TRUE), 3),
    plots = paste(sort(unique(plot)), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(chamber_id)

cat("\nSummary by chamber type:\n")
print(chamber_summary)

# =============================================================================
# SAVE AUXFILES
# =============================================================================

cat("\n=== SAVING AUXFILES ===\n")

# Create clean auxfile with only the required columns for goFlux
auxfile_clean <- auxfile %>%
  select(UniqueID:Pcham)

# Save complete auxfile (all instruments combined)
write_csv(auxfile_clean, "flux_code/auxfiles/soilwater_auxfile_all_instruments.csv")
cat("Complete auxfile saved: flux_code/auxfiles/soilwater_auxfile_all_instruments.csv\n")

# Create goFlux-ready version (with start.time as character)
auxfile_goflux <- auxfile_clean %>%
  mutate(start.time = format(start.time, "%Y-%m-%d %H:%M:%S"))

write_csv(auxfile_goflux, "flux_code/auxfiles/soilwater_auxfile_all_instruments_goflux.csv")
cat("goFlux-ready auxfile saved: flux_code/auxfiles/soilwater_auxfile_all_instruments_goflux.csv\n")

# Create separate auxfiles for each analyzer (using preserved info)
analyzers <- unique(auxfile$analyzer_id)
cat("\nCreating separate auxfiles for each analyzer:", paste(analyzers, collapse = ", "), "\n")

for (analyzer in analyzers) {
  # Filter auxfile for this analyzer
  analyzer_auxfile <- auxfile %>%
    filter(analyzer_id == analyzer) %>%
    select(UniqueID:Pcham)  # Keep only the auxfile columns
  
  if (nrow(analyzer_auxfile) > 0) {
    # Save standard format
    analyzer_filename_csv <- paste0("flux_code/auxfiles/soilwater_auxfile_", tolower(analyzer), ".csv")
    write_csv(analyzer_auxfile, analyzer_filename_csv)
    
    # Create and save goFlux-ready version
    analyzer_auxfile_goflux <- analyzer_auxfile %>%
      mutate(start.time = format(start.time, "%Y-%m-%d %H:%M:%S"))
    
    analyzer_filename_goflux <- paste0("flux_code/auxfiles/soilwater_auxfile_", tolower(analyzer), "_goflux.csv")
    write_csv(analyzer_auxfile_goflux, analyzer_filename_goflux)
    
    cat("- ", analyzer, ":", nrow(analyzer_auxfile), "measurements saved\n")
    cat("  Files:", analyzer_filename_csv, "and", analyzer_filename_goflux, "\n")
    
    # Display date range for this analyzer
    cat("  Date range:", min(analyzer_auxfile$DATE), "to", max(analyzer_auxfile$DATE), "\n")
    cat("  Temperature range:", round(min(analyzer_auxfile$Tcham, na.rm = TRUE), 1), 
        "to", round(max(analyzer_auxfile$Tcham, na.rm = TRUE), 1), "°C\n\n")
  } else {
    cat("- ", analyzer, ": No data found\n\n")
  }
}

# =============================================================================
# DISPLAY SAMPLE DATA
# =============================================================================

cat("=== SAMPLE AUXFILE DATA ===\n")

# Display first few rows of the converted data
cat("First 6 rows of converted auxfile:\n")
print(head(auxfile_clean))

# Display sample from each analyzer if available
cat("\nSample data by analyzer:\n")
for (analyzer in analyzers) {
  analyzer_sample <- auxfile %>%
    filter(analyzer_id == analyzer) %>%
    select(UniqueID:Pcham) %>%
    slice_head(n = 3)
  
  if (nrow(analyzer_sample) > 0) {
    cat("\n", analyzer, "sample (first 3 rows):\n")
    print(analyzer_sample)
  }
}

# =============================================================================
# SUMMARY AND NEXT STEPS
# =============================================================================

cat("\n=== STEP 3 COMPLETE ===\n")
cat("Soil/water auxfiles created successfully!\n\n")

cat("Summary:\n")
cat("- Original soil/water data:", nrow(soil_water_complete), "rows\n")
cat("- Auxfile records created:", nrow(auxfile_clean), "rows\n")
cat("- Retention rate:", round(nrow(auxfile_clean)/nrow(soil_water_complete) * 100, 1), "%\n")
cat("- Analyzers processed:", paste(analyzers, collapse = ", "), "\n")
cat("- Date range:", min(auxfile_clean$DATE), "to", max(auxfile_clean$DATE), "\n\n")

cat("Files created in flux_code/auxfiles/:\n")
cat("- soilwater_auxfile_all_instruments.csv (all analyzers combined)\n")
cat("- soilwater_auxfile_all_instruments_goflux.csv (goFlux-ready format)\n")
for (analyzer in analyzers) {
  cat("- soilwater_auxfile_", tolower(analyzer), ".csv (", analyzer, " only)\n", sep = "")
  cat("- soilwater_auxfile_", tolower(analyzer), "_goflux.csv (", analyzer, " goFlux-ready)\n", sep = "")
}

cat("\nNext Steps:\n")
cat("1. Process raw gas concentration files for each analyzer\n")
cat("2. Use goFlux import functions with these auxfiles\n")
cat("3. Calculate fluxes using goFlux workflow\n")
cat("4. Follow same pattern as tree flux processing\n")

cat("\nReady for goFlux processing!\n")
cat("==================================================================\n")
