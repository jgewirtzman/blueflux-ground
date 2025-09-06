# R Script to Assign Volumes and Surface Areas to Measurements
# Complete rewrite from scratch

library(dplyr)
library(readr)

assign_volumes_and_areas <- function(simplified_vol_file, additional_vol_file, surface_area_file, blueflux_file) {
  
  cat("=== Loading Data Files ===\n")
  
  # Read all input files
  simplified_vol <- read_csv(simplified_vol_file, show_col_types = FALSE)
  additional_vol <- read_csv(additional_vol_file, show_col_types = FALSE)
  surface_area <- read_csv(surface_area_file, show_col_types = FALSE)
  blueflux <- read_csv(blueflux_file, show_col_types = FALSE)
  
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
  # Total volume includes tubing + filter, so subtract those to get pure chamber volume
  lgr_tubing <- analyzer_lookup$tubing[analyzer_lookup$analyzer_name == "LGR"]
  small_drierite <- analyzer_lookup$drierite_small[1]  # Same for all instruments
  large_drierite <- analyzer_lookup$drierite_large[1]  # Same for all instruments
  
  base_chamber_volumes <- simplified_vol %>%
    filter(!is.na(`Chamber_Alt_ID`) & `Chamber_Alt_ID` %in% c("A", "B", "C", "D")) %>%
    mutate(
      total_vol_cm3 = `Total_Volume_mL`,  # mL = cm3
      # Subtract tubing and small drierite (baseline) to get chamber-only volume
      chamber_only_vol = total_vol_cm3 - lgr_tubing - small_drierite
    ) %>%
    group_by(`Chamber_Alt_ID`) %>%
    summarise(
      avg_chamber_volume_cm3 = mean(chamber_only_vol, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    rename(chamber_class = `Chamber_Alt_ID`)
  
  cat("Base chamber volumes (A, B, C, D):\n")
  print(base_chamber_volumes)
  
  cat("\n=== Processing Surface Area Data ===\n")
  
  # Extract surface area data (skip header rows with formula)
  surface_area_data <- surface_area %>%
    slice(4:n()) %>%  # Skip first 3 rows (formula, blank, headers)
    select(1, 2, 6) %>%  # Chamber ID, a dimension, surface area
    setNames(c("chamber_series", "a_inch", "surface_area_cm2")) %>%
    filter(!is.na(chamber_series)) %>%
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
  
  print(surface_area_data)
  
  cat("\n=== Processing Measurement Data ===\n")
  
  # Process all measurements - no filtering by chamber class
  result <- blueflux %>%
    mutate(
      # Parse dates for time period assignment
      date_parsed = as.Date(date, format = "%m/%d/%Y"),
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
      
      # Filter volume (time period-specific)
      filter_volume_cm3 = case_when(
        time_period == "March_2022" ~ small_drierite,
        time_period %in% c("Oct_2022", "March_2023") ~ large_drierite,
        TRUE ~ NA_real_
      ),
      
      # Chamber volume calculations
      chamber_volume_cm3 = case_when(
        # Standard A, B, C, D series - use pre-calculated averages
        chamber_class == "A" ~ base_chamber_volumes$avg_chamber_volume_cm3[base_chamber_volumes$chamber_class == "A"],
        chamber_class == "B" ~ base_chamber_volumes$avg_chamber_volume_cm3[base_chamber_volumes$chamber_class == "B"],
        chamber_class == "C" ~ base_chamber_volumes$avg_chamber_volume_cm3[base_chamber_volumes$chamber_class == "C"],
        chamber_class == "D" ~ base_chamber_volumes$avg_chamber_volume_cm3[base_chamber_volumes$chamber_class == "D"],
        
        # HA: A series volume minus cylinder volume
        chamber_class == "HA" ~ {
          a_vol <- base_chamber_volumes$avg_chamber_volume_cm3[base_chamber_volumes$chamber_class == "A"]
          a_dim_inch <- surface_area_data$a_inch[surface_area_data$chamber_class == "A"]
          cylinder_radius_cm <- (diameter / 2) * 2.54  # inches to cm
          cylinder_height_cm <- a_dim_inch * 2.54  # inches to cm  
          cylinder_vol_cm3 <- pi * cylinder_radius_cm^2 * cylinder_height_cm
          a_vol - cylinder_vol_cm3
        },
        
        # HB: B series volume minus cylinder volume
        chamber_class == "HB" ~ {
          b_vol <- base_chamber_volumes$avg_chamber_volume_cm3[base_chamber_volumes$chamber_class == "B"]
          b_dim_inch <- surface_area_data$a_inch[surface_area_data$chamber_class == "B"]
          cylinder_radius_cm <- (diameter / 2) * 2.54  # inches to cm
          cylinder_height_cm <- b_dim_inch * 2.54  # inches to cm
          cylinder_vol_cm3 <- pi * cylinder_radius_cm^2 * cylinder_height_cm
          b_vol - cylinder_vol_cm3
        },
        
        # LB: equivalent to D series volume
        chamber_class == "LB" ~ base_chamber_volumes$avg_chamber_volume_cm3[base_chamber_volumes$chamber_class == "D"],
        
        TRUE ~ NA_real_
      ),
      
      # Surface area calculations
      surface_area_cm2 = case_when(
        # Standard A, B, C, D series - lookup table values
        chamber_class == "A" ~ surface_area_data$surface_area_cm2[surface_area_data$chamber_class == "A"],
        chamber_class == "B" ~ surface_area_data$surface_area_cm2[surface_area_data$chamber_class == "B"],
        chamber_class == "C" ~ surface_area_data$surface_area_cm2[surface_area_data$chamber_class == "C"],
        chamber_class == "D" ~ surface_area_data$surface_area_cm2[surface_area_data$chamber_class == "D"],
        
        # HA: lateral surface area of cylinder (2πrh)
        chamber_class == "HA" ~ {
          a_dim_inch <- surface_area_data$a_inch[surface_area_data$chamber_class == "A"]
          cylinder_radius_cm <- (diameter / 2) * 2.54  # inches to cm
          cylinder_height_cm <- a_dim_inch * 2.54  # inches to cm
          2 * pi * cylinder_radius_cm * cylinder_height_cm
        },
        
        # HB: lateral surface area of cylinder (2πrh)  
        chamber_class == "HB" ~ {
          b_dim_inch <- surface_area_data$a_inch[surface_area_data$chamber_class == "B"]
          cylinder_radius_cm <- (diameter / 2) * 2.54  # inches to cm
          cylinder_height_cm <- b_dim_inch * 2.54  # inches to cm
          2 * pi * cylinder_radius_cm * cylinder_height_cm
        },
        
        # LB: NA as specified
        chamber_class == "LB" ~ NA_real_,
        
        TRUE ~ NA_real_
      ),
      
      # Total system volume in cm3
      total_system_volume_cm3 = analyzer_cell_volume_cm3 + tubing_volume_cm3 + 
        chamber_volume_cm3 + filter_volume_cm3,
      
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
  
  # Time period distribution
  time_counts <- table(result$time_period, useNA = "ifany")
  cat("\nTime period distribution:\n")
  print(time_counts)
  
  # Check completeness
  complete_volumes <- sum(complete.cases(result[c("analyzer_cell_volume_cm3", "tubing_volume_cm3", 
                                                  "chamber_volume_cm3", "filter_volume_cm3")]))
  cat("\nMeasurements with complete volume data:", complete_volumes, "of", nrow(result), "\n")
  
  complete_surface <- sum(!is.na(result$surface_area_cm2))
  cat("Measurements with surface area data:", complete_surface, "of", nrow(result), "\n")
  
  # Show examples by chamber class
  cat("\n=== Sample Results by Chamber Class ===\n")
  sample_cols <- c("flux_id", "chamber_class", "analyzer", "diameter", "time_period",
                   "analyzer_cell_volume_cm3", "analyzer_cell_volume_L",
                   "tubing_volume_cm3", "tubing_volume_L", 
                   "chamber_volume_cm3", "chamber_volume_L",
                   "filter_volume_cm3", "filter_volume_L",
                   "surface_area_cm2", 
                   "total_system_volume_cm3", "total_system_volume_L")
  
  for (chamber in c("A", "B", "C", "D", "HA", "HB", "LB")) {
    chamber_data <- result[result$chamber_class == chamber & !is.na(result$chamber_class), ]
    if (nrow(chamber_data) > 0) {
      cat("\n", chamber, "chambers (first 2 examples):\n")
      print(chamber_data[1:min(2, nrow(chamber_data)), sample_cols])
    }
  }
  
  return(result)
}




# Example usage:
# Uncomment and modify the file paths as needed
result <- assign_volumes_and_areas(
  simplified_vol_file = "field_notes/dimension_csvs/simplified_volume.csv",
  additional_vol_file = "field_notes/dimension_csvs/additional_vol.csv",
  surface_area_file = "field_notes/dimension_csvs/surface_area.csv",
  blueflux_file = "field_notes/blueflux compiled tree fluxes.csv"
)


# Save results
# write_csv(result, "blueflux_with_volumes_and_areas.csv")

ggplot(result, aes(x=chamber_class, y=total_system_volume_L))+
  geom_boxplot()
