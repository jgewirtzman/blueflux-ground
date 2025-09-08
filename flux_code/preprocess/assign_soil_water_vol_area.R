# STEP 2: ASSIGN VOLUMES AND SURFACE AREAS FOR SOIL/WATER DATA
# This follows the same logic as assign_tree_vol_area.R but for soil/water chambers
# Reads from actual files and includes drierite with time-period assignments

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

cat("==================================================================\n")
cat("    STEP 2: ASSIGN VOLUMES AND SURFACE AREAS - SOIL/WATER\n")
cat("==================================================================\n\n")

# =============================================================================
# LOAD DATA FILES
# =============================================================================

cat("=== LOADING DATA FILES ===\n")

# Read the filled soil/water data from Step 1
soil_water_data <- read_csv("flux_code/intermediate_files/blueflux_soilwater_filled.csv", show_col_types = FALSE)

# Read chamber dimensions specific to soil/water
chamber_dims <- read_csv("field_notes/dimension_csvs/soil_water_dims.csv", show_col_types = FALSE)

# Read analyzer volumes (same file as trees)
additional_vol <- read_csv("field_notes/dimension_csvs/additional_vol.csv", show_col_types = FALSE)

cat("Files loaded successfully:\n")
cat("- Soil/water data:", nrow(soil_water_data), "rows\n")
cat("- Chamber dimensions:", nrow(chamber_dims), "chambers\n")
cat("- Analyzer volume data:", nrow(additional_vol), "instruments\n")

# Display chamber information
cat("\nAvailable chambers:\n")
print(chamber_dims[, c("Chamber", "Chamber+Collar_Volume_L", "Ground_Surface_Area_cm2")])

# Display analyzer volume information
cat("\nAnalyzer volume data:\n")
print(additional_vol)

# =============================================================================
# PROCESS ANALYZER VOLUME DATA
# =============================================================================

cat("\n=== PROCESSING ANALYZER VOLUME DATA ===\n")

# Create analyzer lookup table (following tree workflow pattern)
analyzer_lookup <- additional_vol %>%
  mutate(
    analyzer_name = case_when(
      tolower(instrument) == "lgr_mgga" ~ "LGR",
      tolower(instrument) == "picarro" ~ "Picarro",
      TRUE ~ toupper(instrument)
    )
  )

cat("Analyzer lookup table:\n")
print(analyzer_lookup)

# Extract volume values for calculations
lgr_analyzer_cell <- analyzer_lookup$analyzer_cell[analyzer_lookup$analyzer_name == "LGR"]
lgr_tubing <- analyzer_lookup$tubing[analyzer_lookup$analyzer_name == "LGR"]
picarro_analyzer_cell <- analyzer_lookup$analyzer_cell[analyzer_lookup$analyzer_name == "Picarro"]
picarro_tubing <- analyzer_lookup$tubing[analyzer_lookup$analyzer_name == "Picarro"]
drierite_large <- analyzer_lookup$drierite_large[1]  # Same for all instruments
drierite_small <- analyzer_lookup$drierite_small[1]  # Same for all instruments

cat("\nExtracted volume values:\n")
cat("LGR: analyzer_cell =", lgr_analyzer_cell, "cm³, tubing =", lgr_tubing, "cm³\n")
cat("Picarro: analyzer_cell =", picarro_analyzer_cell, "cm³, tubing =", picarro_tubing, "cm³\n")
cat("Drierite: large =", drierite_large, "cm³, small =", drierite_small, "cm³\n")

# =============================================================================
# ASSIGN VOLUMES AND SURFACE AREAS
# =============================================================================

cat("\n=== ASSIGNING VOLUMES AND SURFACE AREAS ===\n")

# Function to assign volumes and areas for soil/water chambers
assign_soilwater_volumes_and_areas <- function(soil_water_data, chamber_dims, analyzer_lookup) {
  
  cat("Processing", nrow(soil_water_data), "soil/water measurements\n")
  
  # Parse dates and determine time periods (following tree workflow logic)
  result <- soil_water_data %>%
    mutate(
      # Parse dates more robustly
      date_parsed = tryCatch({
        case_when(
          # Try m/d/y format first (3/18/22)
          grepl("^\\d{1,2}/\\d{1,2}/\\d{2}$", date) ~ mdy(date),
          # Try m/d/Y format (3/18/2022) 
          grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", date) ~ mdy(date),
          # Try other common formats
          grepl("^\\d{4}-\\d{2}-\\d{2}$", date) ~ ymd(date),
          TRUE ~ mdy(date)  # Default attempt
        )
      }, error = function(e) {
        # If case_when fails, try direct parsing
        mdy(date)
      }),
      year = format(date_parsed, "%Y"),
      month = format(date_parsed, "%m"),
      # Determine time period for drierite assignment (same logic as trees)
      time_period = case_when(
        year == "2022" & month == "03" ~ "March_2022",
        year == "2022" & month == "10" ~ "Oct_2022",
        year == "2023" & month == "03" ~ "March_2023",
        TRUE ~ "Other"
      )
    )
  
  cat("Time period distribution:\n")
  time_counts <- table(result$time_period, useNA = "ifany")
  print(time_counts)
  
  # Merge with chamber dimensions
  result <- result %>%
    left_join(chamber_dims, by = c("chamber_id" = "Chamber"))
  
  # Check for missing chamber matches
  missing_chambers <- sum(is.na(result$`Chamber+Collar_Volume_L`))
  if (missing_chambers > 0) {
    cat("Warning:", missing_chambers, "measurements have missing chamber dimension data\n")
    missing_chamber_ids <- unique(result$chamber_id[is.na(result$`Chamber+Collar_Volume_L`)])
    cat("Missing chamber IDs:", paste(missing_chamber_ids, collapse = ", "), "\n")
  }
  
  # Assign analyzer-specific volumes
  result <- result %>%
    mutate(
      # Analyzer cell volume (instrument-specific)
      analyzer_cell_volume_cm3 = case_when(
        analyzer_id %in% c("LGR1", "LGR2", "LGR3") ~ lgr_analyzer_cell,
        analyzer_id == "Picarro" ~ picarro_analyzer_cell,
        TRUE ~ NA_real_
      ),
      
      # Tubing volume (instrument-specific)
      tubing_volume_cm3 = case_when(
        analyzer_id %in% c("LGR1", "LGR2", "LGR3") ~ lgr_tubing,
        analyzer_id == "Picarro" ~ picarro_tubing,
        TRUE ~ NA_real_
      ),
      
      # Filter volume assignment with time-period logic (same as trees)
      filter_volume_cm3 = case_when(
        time_period == "March_2022" ~ as.numeric(drierite_small),
        time_period %in% c("Oct_2022", "March_2023") ~ as.numeric(drierite_large),
        TRUE ~ NA_real_
      ),
      
      # Chamber volume (direct from dimensions file - already includes collar)
      chamber_volume_L = `Chamber+Collar_Volume_L`,
      chamber_volume_cm3 = chamber_volume_L * 1000,
      
      # Surface area (direct from dimensions file)
      surface_area_cm2 = Ground_Surface_Area_cm2,
      
      # Convert analyzer volumes to liters for consistency
      analyzer_cell_volume_L = analyzer_cell_volume_cm3 / 1000,
      tubing_volume_L = tubing_volume_cm3 / 1000,
      filter_volume_L = filter_volume_cm3 / 1000,
      
      # Total system volume calculation (following tree logic)
      total_system_volume_cm3 = case_when(
        # Only calculate if all components are available
        !is.na(analyzer_cell_volume_cm3) & !is.na(tubing_volume_cm3) & 
          !is.na(chamber_volume_cm3) & !is.na(filter_volume_cm3) ~ 
          analyzer_cell_volume_cm3 + tubing_volume_cm3 + chamber_volume_cm3 + filter_volume_cm3,
        TRUE ~ NA_real_
      ),
      total_system_volume_L = total_system_volume_cm3 / 1000
    )
  
  # Check for missing analyzer matches
  missing_analyzers <- sum(is.na(result$analyzer_cell_volume_cm3))
  if (missing_analyzers > 0) {
    cat("Warning:", missing_analyzers, "measurements have missing analyzer data\n")
    missing_analyzer_ids <- unique(result$analyzer_id[is.na(result$analyzer_cell_volume_cm3)])
    cat("Missing analyzer IDs:", paste(missing_analyzer_ids, collapse = ", "), "\n")
  }
  
  # Check for missing time period assignments
  missing_time_periods <- sum(is.na(result$filter_volume_cm3))
  if (missing_time_periods > 0) {
    cat("Warning:", missing_time_periods, "measurements have missing time period assignments\n")
    unknown_periods <- unique(result$time_period[is.na(result$filter_volume_cm3)])
    cat("Unknown time periods:", paste(unknown_periods, collapse = ", "), "\n")
  }
  
  return(result)
}

# Apply the function
soil_water_complete <- assign_soilwater_volumes_and_areas(soil_water_data, chamber_dims, analyzer_lookup)

# =============================================================================
# QUALITY CHECKS AND SUMMARY
# =============================================================================

cat("\n=== QUALITY CHECKS AND SUMMARY ===\n")

# Check data completeness
complete_data <- soil_water_complete %>%
  filter(!is.na(total_system_volume_L), !is.na(surface_area_cm2), !is.na(air_temp))

cat("Data completeness:\n")
cat("- Original measurements:", nrow(soil_water_complete), "\n")
cat("- Complete measurements (all components):", nrow(complete_data), "\n")
cat("- Retention rate:", round(nrow(complete_data)/nrow(soil_water_complete) * 100, 1), "%\n")

# Component completeness
component_summary <- soil_water_complete %>%
  summarise(
    total = n(),
    has_chamber_volume = sum(!is.na(chamber_volume_L)),
    has_analyzer_volume = sum(!is.na(analyzer_cell_volume_cm3)),
    has_filter_volume = sum(!is.na(filter_volume_cm3)),
    has_surface_area = sum(!is.na(surface_area_cm2)),
    has_air_temp = sum(!is.na(air_temp)),
    complete_system = sum(!is.na(total_system_volume_L))
  )

cat("\nComponent completeness:\n")
print(component_summary)

# Volume and area ranges
if (nrow(complete_data) > 0) {
  volume_area_summary <- complete_data %>%
    summarise(
      min_total_volume_L = round(min(total_system_volume_L, na.rm = TRUE), 3),
      max_total_volume_L = round(max(total_system_volume_L, na.rm = TRUE), 1),
      mean_total_volume_L = round(mean(total_system_volume_L, na.rm = TRUE), 2),
      min_chamber_volume_L = round(min(chamber_volume_L, na.rm = TRUE), 3),
      max_chamber_volume_L = round(max(chamber_volume_L, na.rm = TRUE), 1),
      min_surface_area_cm2 = round(min(surface_area_cm2, na.rm = TRUE), 1),
      max_surface_area_cm2 = round(max(surface_area_cm2, na.rm = TRUE), 1)
    )
  
  cat("\nVolume and area ranges:\n")
  cat("- Total system volume:", volume_area_summary$min_total_volume_L, "to", volume_area_summary$max_total_volume_L, "L\n")
  cat("- Chamber volume:", volume_area_summary$min_chamber_volume_L, "to", volume_area_summary$max_chamber_volume_L, "L\n")
  cat("- Surface area:", volume_area_summary$min_surface_area_cm2, "to", volume_area_summary$max_surface_area_cm2, "cm²\n")
}

# Summary by chamber and analyzer
chamber_analyzer_summary <- soil_water_complete %>%
  filter(!is.na(total_system_volume_L)) %>%
  group_by(chamber_id, analyzer_id) %>%
  summarise(
    count = n(),
    avg_total_volume_L = round(mean(total_system_volume_L, na.rm = TRUE), 3),
    avg_chamber_volume_L = round(mean(chamber_volume_L, na.rm = TRUE), 3),
    surface_area_cm2 = first(surface_area_cm2),
    time_periods = paste(sort(unique(time_period)), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(chamber_id, analyzer_id)

cat("\nSummary by chamber and analyzer:\n")
print(chamber_analyzer_summary)

# Drierite (filter) assignment check
drierite_summary <- soil_water_complete %>%
  filter(!is.na(filter_volume_cm3)) %>%
  group_by(time_period, analyzer_id) %>%
  summarise(
    count = n(),
    filter_volume_cm3 = first(filter_volume_cm3),
    .groups = "drop"
  ) %>%
  arrange(time_period, analyzer_id)

cat("\nDrierite (filter) assignments:\n")
print(drierite_summary)



# =============================================================================
# CREATE VISUALIZATIONS
# =============================================================================

cat("\n=== CREATING VISUALIZATIONS ===\n")

if (nrow(complete_data) > 0) {
  tryCatch({
    # Total system volume by chamber and analyzer
    p1 <- complete_data %>%
      ggplot(aes(x = chamber_id, y = total_system_volume_L, fill = analyzer_id)) +
      geom_boxplot(position = position_dodge(width = 0.8)) +
      geom_point(aes(color = analyzer_id), position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.2), alpha = 0.7) +
      labs(
        title = "Total System Volume by Chamber Type and Analyzer",
        x = "Chamber Type", 
        y = "Total System Volume (L)",
        fill = "Analyzer",
        color = "Analyzer"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_log10()
    
    print(p1)
    
    # Surface area by chamber (should be constant within chamber type)
    p2 <- complete_data %>%
      ggplot(aes(x = chamber_id, y = surface_area_cm2, fill = chamber_id)) +
      geom_boxplot() +
      geom_point(alpha = 0.7) +
      labs(
        title = "Surface Area by Chamber Type",
        x = "Chamber Type",
        y = "Surface Area (cm²)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    
    print(p2)
    
    # Volume components breakdown
    p3 <- complete_data %>%
      select(flux_id, chamber_id, analyzer_id, chamber_volume_L, analyzer_cell_volume_L, tubing_volume_L, filter_volume_L) %>%
      tidyr::pivot_longer(cols = ends_with("_volume_L"), names_to = "component", values_to = "volume_L") %>%
      mutate(
        component = gsub("_volume_L", "", component),
        component = factor(component, levels = c("chamber", "filter", "analyzer_cell", "tubing"))
      ) %>%
      ggplot(aes(x = chamber_id, y = volume_L, fill = component)) +
      geom_col(position = "stack") +
      facet_wrap(~ analyzer_id) +
      labs(
        title = "Volume Components by Chamber Type and Analyzer",
        x = "Chamber Type",
        y = "Volume (L)",
        fill = "Component"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(type = "qual", palette = "Set2")
    
    print(p3)
    
    # Save plots
    ggsave("flux_code/soilwater_volume_by_chamber_analyzer.png", p1, width = 12, height = 8, dpi = 300)
    ggsave("flux_code/soilwater_surface_area_by_chamber.png", p2, width = 10, height = 6, dpi = 300)
    ggsave("flux_code/soilwater_volume_components.png", p3, width = 14, height = 10, dpi = 300)
    
    cat("Plots saved:\n")
    cat("- flux_code/soilwater_volume_by_chamber_analyzer.png\n")
    cat("- flux_code/soilwater_surface_area_by_chamber.png\n")
    cat("- flux_code/soilwater_volume_components.png\n")
    
  }, error = function(e) {
    cat("Could not create plots:", e$message, "\n")
  })
}

# Get unique combinations only - don't sum multiple measurements
p3_fixed <- complete_data %>%
  select(chamber_id, analyzer_id, time_period, 
         chamber_volume_L, analyzer_cell_volume_L, tubing_volume_L, filter_volume_L) %>%
  # Get unique combinations only
  distinct(chamber_id, analyzer_id, time_period, .keep_all = TRUE) %>%
  pivot_longer(cols = ends_with("_volume_L"), 
               names_to = "component", 
               values_to = "volume_L") %>%
  mutate(
    component = gsub("_volume_L", "", component),
    component = factor(component, levels = c("chamber", "filter", "analyzer_cell", "tubing"))
  ) %>%
  ggplot(aes(x = chamber_id, y = volume_L, fill = component)) +
  geom_col(position = "stack") +
  facet_grid(time_period ~ analyzer_id, scales = "free_x") +
  labs(
    title = "Volume Components by Chamber Type, Analyzer, and Time Period",
    subtitle = "Individual component volumes (not summed across measurements)",
    x = "Chamber Type",
    y = "Volume (L)",
    fill = "Component"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 8)
  ) +
  scale_fill_brewer(type = "qual", palette = "Set2")

print(p3_fixed)


# =============================================================================
# SAVE RESULTS
# =============================================================================

cat("\n=== SAVING RESULTS ===\n")

# Save complete data (following tree workflow pattern)
write_csv(soil_water_complete, "flux_code/intermediate_files/main_soilwater_complete.csv")

cat("Results saved to: flux_code/intermediate_files/main_soilwater_complete.csv\n")

cat("\n=== STEP 2 COMPLETE ===\n")
cat("Complete soil/water data saved with volumes and surface areas assigned\n")
cat("- Chamber volumes from soil_water_dims.csv\n")
cat("- Analyzer volumes from additional_vol.csv\n") 
cat("- Drierite assignments based on time periods\n")
cat("- Total system volume = chamber + analyzer + tubing + drierite\n")
cat("Ready for Step 3: convert_soilwater_to_auxfile.R\n")
cat("==================================================================\n")