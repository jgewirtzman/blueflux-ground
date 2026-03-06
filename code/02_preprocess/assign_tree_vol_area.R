# R Script to Assign Volumes and Surface Areas to Measurements
# FINAL WORKING VERSION - Fixed surface area processing

library(dplyr)
library(readr)
library(ggplot2)

assign_volumes_and_areas <- function(simplified_vol_file, additional_vol_file, surface_area_file, blueflux_file, mangrove_leaf_file) {
  
  cat("=== Loading Data Files ===\n")
  
  # Read all input files
  simplified_vol <- read_csv(simplified_vol_file, show_col_types = FALSE)
  additional_vol <- read_csv(additional_vol_file, show_col_types = FALSE)
  surface_area <- read_csv(surface_area_file, show_col_types = FALSE)
  blueflux <- read_csv(blueflux_file, show_col_types = FALSE)
  mangrove_leaf <- read_csv(mangrove_leaf_file, show_col_types = FALSE)
  
  cat("Files loaded successfully.\n")
  cat("Original blueflux data:", nrow(blueflux), "rows\n")
  
  # Clean column names in simplified_vol (remove newlines and spaces)
  names(simplified_vol) <- gsub("\\n|\\r", "_", names(simplified_vol))
  names(simplified_vol) <- gsub("\\s+", "_", names(simplified_vol))
  names(simplified_vol) <- gsub("\\(|\\)", "", names(simplified_vol))
  
  cat("\n=== Processing Chamber Volume Data ===\n")
  
  # Get analyzer volumes by instrument type
  analyzer_lookup <- additional_vol %>%
    mutate(
      analyzer_name = case_when(
        tolower(instrument) == "lgr_mgga" ~ "LGR",
        tolower(instrument) == "picarro" ~ "Picarro",
        TRUE ~ toupper(instrument)
      )
    )
  
  print(analyzer_lookup)
  
  # Calculate base chamber volumes for A, B, C, D series
  # For A, B, C, D chambers in simplified_volume: Total = Analyzer Cell + Tubing + Chamber (NO drierite)
  # So pure chamber volume = Total - Analyzer Cell - Tubing
  lgr_tubing <- analyzer_lookup$tubing[analyzer_lookup$analyzer_name == "LGR"]
  lgr_analyzer_cell <- analyzer_lookup$analyzer_cell[analyzer_lookup$analyzer_name == "LGR"]
  small_drierite <- analyzer_lookup$drierite_small[1]  # Same for all instruments
  large_drierite <- analyzer_lookup$drierite_large[1]  # Same for all instruments
  
  # Use correct column names after cleaning
  base_chamber_volumes <- simplified_vol %>%
    filter(!is.na(`Chamber_Alt_ID`) & `Chamber_Alt_ID` %in% c("A", "B", "C", "D")) %>%
    mutate(
      total_vol_cm3 = `Total_Volume_mL`,  # mL = cm3
      # CORRECTED: For A,B,C,D chambers, subtract tubing and analyzer cell to get pure chamber volume
      chamber_volume_cm3 = total_vol_cm3 - lgr_tubing - lgr_analyzer_cell
    ) %>%
    group_by(`Chamber_Alt_ID`) %>%
    summarise(
      avg_chamber_volume_cm3 = mean(chamber_volume_cm3, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    rename(chamber_class = `Chamber_Alt_ID`)
  
  cat("Pure chamber volumes (A, B, C, D):\n")
  print(base_chamber_volumes)
  
  cat("\n=== Processing Surface Area Data ===\n")
  
  # Debug: Let's see what R is actually reading
  cat("Debug: Raw surface_area data:\n")
  print(surface_area)
  cat("Debug: Column names:\n")
  print(names(surface_area))
  cat("Debug: Number of rows:", nrow(surface_area), "\n")
  
  # FIXED: Extract surface area data with proper column names
  surface_area_data <- surface_area %>%
    filter(!is.na(`Chamber ID`) & `Chamber ID` != "") %>%
    select(`Chamber ID`, `a (inch)`, `SA cm2`) %>%
    setNames(c("chamber_series", "a_inch", "surface_area_cm2")) %>%
    mutate(
      chamber_class = case_when(
        grepl("^A", chamber_series, ignore.case = TRUE) ~ "A",
        grepl("^B", chamber_series, ignore.case = TRUE) ~ "B",
        grepl("^C", chamber_series, ignore.case = TRUE) ~ "C", 
        grepl("^D", chamber_series, ignore.case = TRUE) ~ "D",
        TRUE ~ NA_character_
      ),
      a_inch = as.numeric(a_inch),
      surface_area_cm2 = as.numeric(surface_area_cm2)
    ) %>%
    filter(!is.na(chamber_class))
  
  cat("Processed surface area data:\n")
  print(surface_area_data)
  
  # Check if we have all chamber classes
  missing_chambers <- setdiff(c("A", "B", "C", "D"), surface_area_data$chamber_class)
  if (length(missing_chambers) > 0) {
    cat("WARNING: Missing chamber classes in surface area data:", paste(missing_chambers, collapse = ", "), "\n")
  }
  
  cat("\n=== Processing Mangrove Leaf Data ===\n")
  
  # Process mangrove leaf surface area data
  mangrove_leaf_data <- mangrove_leaf %>%
    rename(
      species = Species,
      forest_type = `Forest Type`,
      leaf_area_cm2 = `1-sided leaf area for 15 leaves (cm2)`
    ) %>%
    mutate(
      # Create abbreviated species codes to match blueflux data
      species_code = case_when(
        species == "Rhizophora mangle" ~ "RHMA",
        species == "Laguncularia racemosa" ~ "LARA", 
        species == "Avicennia germinans" ~ "AVGE",
        TRUE ~ species
      )
    ) %>%
    select(species, species_code, forest_type, leaf_area_cm2)
  
  cat("Mangrove leaf data:\n")
  print(mangrove_leaf_data)
  
  cat("\n=== Processing Measurement Data ===\n")
  
  # Process all measurements with FIXED date parsing
  result <- blueflux %>%
    mutate(
      # FIXED: Parse dates with 2-digit year format (3/10/23 format)
      date_parsed = as.Date(date, format = "%m/%d/%y"),  # Changed from %Y to %y
      year = format(date_parsed, "%Y"),
      month = format(date_parsed, "%m"),
      time_period = case_when(
        year == "2022" & month == "03" ~ "March_2022",
        year == "2022" & month == "10" ~ "Oct_2022",
        year == "2023" & month == "03" ~ "March_2023", 
        TRUE ~ "Other"
      )
    ) %>%
    # Join with analyzer volumes
    left_join(analyzer_lookup, by = c("analyzer" = "analyzer_name")) %>%
    # Assign component volumes based on chamber class
    mutate(
      # Analyzer cell and tubing volumes (instrument-specific)
      analyzer_cell_volume_cm3 = analyzer_cell,
      tubing_volume_cm3 = tubing,
      
      # Filter volume assignment with proper handling (drierite IS the filter)
      filter_volume_cm3 = case_when(
        time_period == "March_2022" ~ as.numeric(drierite_small),
        time_period %in% c("Oct_2022", "March_2023") ~ as.numeric(drierite_large),
        TRUE ~ NA_real_
      ),
      
      # Chamber volume calculations with safe lookups
      chamber_volume_cm3 = case_when(
        # Standard A, B, C, D series - use pre-calculated pure chamber volumes
        chamber_class == "A" ~ {
          vol <- base_chamber_volumes$avg_chamber_volume_cm3[base_chamber_volumes$chamber_class == "A"]
          if (length(vol) > 0) vol else NA_real_
        },
        chamber_class == "B" ~ {
          vol <- base_chamber_volumes$avg_chamber_volume_cm3[base_chamber_volumes$chamber_class == "B"]
          if (length(vol) > 0) vol else NA_real_
        },
        chamber_class == "C" ~ {
          vol <- base_chamber_volumes$avg_chamber_volume_cm3[base_chamber_volumes$chamber_class == "C"]
          if (length(vol) > 0) vol else NA_real_
        },
        chamber_class == "D" ~ {
          vol <- base_chamber_volumes$avg_chamber_volume_cm3[base_chamber_volumes$chamber_class == "D"]
          if (length(vol) > 0) vol else NA_real_
        },
        
        # HA: A series chamber volume minus cylinder volume
        chamber_class == "HA" ~ {
          a_vol <- base_chamber_volumes$avg_chamber_volume_cm3[base_chamber_volumes$chamber_class == "A"]
          a_dim <- surface_area_data$a_inch[surface_area_data$chamber_class == "A"]
          
          if (length(a_vol) > 0 && length(a_dim) > 0) {
            cylinder_radius_cm <- (diameter / 2) * 2.54  # inches to cm
            cylinder_height_cm <- a_dim * 2.54  # inches to cm  
            cylinder_vol_cm3 <- pi * cylinder_radius_cm^2 * cylinder_height_cm
            a_vol - cylinder_vol_cm3
          } else {
            NA_real_
          }
        },
        
        # HB: B series chamber volume minus cylinder volume
        chamber_class == "HB" ~ {
          b_vol <- base_chamber_volumes$avg_chamber_volume_cm3[base_chamber_volumes$chamber_class == "B"]
          b_dim <- surface_area_data$a_inch[surface_area_data$chamber_class == "B"]
          
          if (length(b_vol) > 0 && length(b_dim) > 0) {
            cylinder_radius_cm <- (diameter / 2) * 2.54  # inches to cm
            cylinder_height_cm <- b_dim * 2.54  # inches to cm
            cylinder_vol_cm3 <- pi * cylinder_radius_cm^2 * cylinder_height_cm
            b_vol - cylinder_vol_cm3
          } else {
            NA_real_
          }
        },
        
        # LB: equivalent to D series chamber volume
        chamber_class == "LB" ~ {
          vol <- base_chamber_volumes$avg_chamber_volume_cm3[base_chamber_volumes$chamber_class == "D"]
          if (length(vol) > 0) vol else NA_real_
        },
        
        TRUE ~ NA_real_
      ),
      
      # Surface area calculations with safe lookups
      surface_area_cm2 = case_when(
        # For leaf/leaves components, use mangrove leaf surface area
        component %in% c("leaf", "leaves") ~ {
          # Determine forest type based on site
          forest_type_to_use <- ifelse(plot == "SE1", "Scrub", "Fringe")
          
          # Create lookup table using species_code
          leaf_lookup <- mangrove_leaf_data %>%
            mutate(lookup_key = paste(species_code, forest_type, sep = "_")) %>%
            select(lookup_key, leaf_area_cm2)
          
          # Create lookup key for current row
          lookup_key <- paste(species, forest_type_to_use, sep = "_")
          
          # Match with lookup table
          matched_area <- leaf_lookup$leaf_area_cm2[match(lookup_key, leaf_lookup$lookup_key)]
          matched_area
        },
        
        # Standard A, B, C, D series - lookup table values
        chamber_class == "A" ~ {
          area <- surface_area_data$surface_area_cm2[surface_area_data$chamber_class == "A"]
          if (length(area) > 0) area else NA_real_
        },
        chamber_class == "B" ~ {
          area <- surface_area_data$surface_area_cm2[surface_area_data$chamber_class == "B"]
          if (length(area) > 0) area else NA_real_
        },
        chamber_class == "C" ~ {
          area <- surface_area_data$surface_area_cm2[surface_area_data$chamber_class == "C"]
          if (length(area) > 0) area else NA_real_
        },
        chamber_class == "D" ~ {
          area <- surface_area_data$surface_area_cm2[surface_area_data$chamber_class == "D"]
          if (length(area) > 0) area else NA_real_
        },
        
        # HA: lateral surface area of cylinder (2πrh)
        chamber_class == "HA" ~ {
          a_dim <- surface_area_data$a_inch[surface_area_data$chamber_class == "A"]
          if (length(a_dim) > 0) {
            cylinder_radius_cm <- (diameter / 2) * 2.54  # inches to cm
            cylinder_height_cm <- a_dim * 2.54  # inches to cm
            2 * pi * cylinder_radius_cm * cylinder_height_cm
          } else {
            NA_real_
          }
        },
        
        # HB: lateral surface area of cylinder (2πrh)  
        chamber_class == "HB" ~ {
          b_dim <- surface_area_data$a_inch[surface_area_data$chamber_class == "B"]
          if (length(b_dim) > 0) {
            cylinder_radius_cm <- (diameter / 2) * 2.54  # inches to cm
            cylinder_height_cm <- b_dim * 2.54  # inches to cm
            2 * pi * cylinder_radius_cm * cylinder_height_cm
          } else {
            NA_real_
          }
        },
        
        # LB: NA as specified
        chamber_class == "LB" ~ NA_real_,
        
        TRUE ~ NA_real_
      ),
      
      # Total system volume calculation with proper NA handling
      total_system_volume_cm3 = case_when(
        # Only calculate if all components are available
        !is.na(analyzer_cell_volume_cm3) & !is.na(tubing_volume_cm3) & 
          !is.na(chamber_volume_cm3) & !is.na(filter_volume_cm3) ~ 
          analyzer_cell_volume_cm3 + tubing_volume_cm3 + chamber_volume_cm3 + filter_volume_cm3,
        TRUE ~ NA_real_
      ),
      
      # Convert all volumes to liters (divide cm3 by 1000)
      analyzer_cell_volume_L = analyzer_cell_volume_cm3 / 1000,
      tubing_volume_L = tubing_volume_cm3 / 1000,
      chamber_volume_L = chamber_volume_cm3 / 1000,
      filter_volume_L = filter_volume_cm3 / 1000,
      total_system_volume_L = total_system_volume_cm3 / 1000
    ) %>%
    # Clean up temporary columns
    select(-instrument, -param, -analyzer_cell, -tubing, -drierite_large, -drierite_small,
           -date_parsed, -year, -month)
  
  cat("\n=== Summary Results ===\n")
  cat("Total measurements processed:", nrow(result), "\n")
  
  # Chamber class distribution  
  chamber_counts <- table(result$chamber_class, useNA = "ifany")
  cat("\nChamber class distribution:\n")
  print(chamber_counts)
  
  # Analyzer distribution
  analyzer_counts <- table(result$analyzer, useNA = "ifany") 
  cat("\nAnalyzer distribution:\n")
  print(analyzer_counts)
  
  # Time period distribution (this should now show March_2023 instead of Other)
  time_counts <- table(result$time_period, useNA = "ifany")
  cat("\nTime period distribution:\n")
  print(time_counts)
  
  # Check completeness
  complete_volumes <- sum(complete.cases(result[c("analyzer_cell_volume_cm3", "tubing_volume_cm3", 
                                                  "chamber_volume_cm3", "filter_volume_cm3")]))
  cat("\nMeasurements with complete volume data:", complete_volumes, "of", nrow(result), "\n")
  
  complete_surface <- sum(!is.na(result$surface_area_cm2))
  cat("Measurements with surface area data:", complete_surface, "of", nrow(result), "\n")
  
  # Component distribution
  component_counts <- table(result$component, useNA = "ifany")
  cat("\nComponent distribution:\n")
  print(component_counts)
  
  # Check drierite (filter) assignments
  drierite_counts <- table(result$filter_volume_cm3, result$time_period, useNA = "ifany")
  cat("\nDrierite (filter) volume assignments by time period:\n")
  print(drierite_counts)
  
  # Check leaf surface area assignments
  leaf_measurements <- result[result$component %in% c("leaf", "leaves") & !is.na(result$component), ]
  if (nrow(leaf_measurements) > 0) {
    cat("\nLeaf measurements by species and site:\n")
    leaf_summary <- leaf_measurements %>%
      group_by(species, plot) %>%
      summarise(
        count = n(),
        avg_surface_area = mean(surface_area_cm2, na.rm = TRUE),
        .groups = 'drop'
      )
    print(leaf_summary)
  }
  
  # Show examples by chamber class and component type
  cat("\n=== Sample Results by Chamber Class ===\n")
  sample_cols <- c("flux_id", "component", "species", "plot", "chamber_class", "analyzer", "diameter", "time_period",
                   "analyzer_cell_volume_cm3", "analyzer_cell_volume_L",
                   "tubing_volume_cm3", "tubing_volume_L", 
                   "chamber_volume_cm3", "chamber_volume_L",
                   "filter_volume_cm3", "filter_volume_L",
                   "surface_area_cm2", 
                   "total_system_volume_cm3", "total_system_volume_L")
  
  # Show regular chamber examples
  for (chamber in c("A", "B", "C", "D", "HA", "HB", "LB")) {
    chamber_data <- result[result$chamber_class == chamber & !is.na(result$chamber_class), ]
    if (nrow(chamber_data) > 0) {
      cat("\n", chamber, "chambers (first 2 examples):\n")
      print(chamber_data[1:min(2, nrow(chamber_data)), sample_cols])
    }
  }
  
  # Show leaf examples separately
  if (nrow(leaf_measurements) > 0) {
    cat("\nLeaf measurements (first 3 examples):\n")
    print(leaf_measurements[1:min(3, nrow(leaf_measurements)), sample_cols])
  }
  
  return(result)
}

# Execute the function with error handling
tryCatch({
  cat("Starting volume and surface area assignment...\n")
  
  result <- assign_volumes_and_areas(
    simplified_vol_file = "data/field_notes/dimension_csvs/simplified_volume.csv",
    additional_vol_file = "data/field_notes/dimension_csvs/additional_vol.csv",
    surface_area_file = "data/field_notes/dimension_csvs/surface_area.csv",
    mangrove_leaf_file = "data/field_notes/dimension_csvs/mangrove_leaf_data.csv",
    blueflux_file = "intermediate/blueflux_trees_filled.csv"
  )
  
  # Save results
  write_csv(result, "intermediate/main_trees_complete.csv")
  cat("\nResults saved to: intermediate/main_trees_complete.csv\n")
  
  # Create visualizations only if we have valid data
  if (sum(!is.na(result$total_system_volume_L)) > 0) {
    p1 <- ggplot(result, aes(x = chamber_class, y = total_system_volume_L)) +
      geom_boxplot() +
      labs(title = "Total System Volume by Chamber Class",
           x = "Chamber Class", 
           y = "Total System Volume (L)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(p1)
  }
  
  if (sum(!is.na(result$surface_area_cm2)) > 0) {
    p2 <- ggplot(result, aes(x = chamber_class, y = surface_area_cm2)) +
      geom_boxplot() +
      labs(title = "Surface Area by Chamber Class",
           x = "Chamber Class",
           y = "Surface Area (cm²)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(p2)
  }
  
  cat("\nScript completed successfully!\n")
  
}, error = function(e) {
  cat("Error occurred:", e$message, "\n")
  cat("Check your input files and make sure they're in the correct format.\n")
  print(traceback())
})