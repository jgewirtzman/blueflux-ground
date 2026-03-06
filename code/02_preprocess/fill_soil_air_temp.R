# STEP 1: FILL AIR TEMPERATURE FOR SOIL/WATER DATA FROM TREE FLUX FILES
# This script loads soil/water data and fills air temperature using the exact same
# hierarchical approach as fill_air_temp.R but pulling from tree flux files

library(dplyr)
library(readr)
library(lubridate)

cat("==================================================================\n")
cat("    STEP 1: FILL AIR TEMPERATURE FOR SOIL/WATER DATA\n")
cat("==================================================================\n\n")

# =============================================================================
# LOAD AND PREPARE SOIL/WATER DATA
# =============================================================================

cat("=== LOADING SOIL/WATER DATA ===\n")

# Read the soil/water flux data
soil_water_data <- read_csv("data/field_notes/BlueFlux Dataset_soils_water.csv", show_col_types = FALSE)

cat("Original soil/water data loaded:", nrow(soil_water_data), "rows\n")

# Clean and standardize column names, filter out empty rows
soil_water_data <- soil_water_data %>%
  # Remove rows where essential columns are all NA
  filter(
    !is.na(`Date`) & !is.na(`Gas Analyzer`) & !is.na(`Chamber ID`)
  ) %>%
  rename(
    date = `Date`,
    plot = `Plot`,
    surface = `Surface`,
    collar_notes = `Collar Notes`,
    collar_location_notes = `Collar Location Notes`,
    analyzer_id = `Gas Analyzer`,
    chamber_id = `Chamber ID`,
    soil_temp_c = `Soil Temp C`,
    water_temp_c = `Water Temp C`,
    water_depth_cm = `Water depth cm`,
    start_time = `Flux Start Time`,  # Rename to match tree workflow
    end_time = `Flux End Time`,
    temp_start = `Temp start`,
    temp_end = `Temp end`,
    pressure_start = `Pressure start`,
    pressure_end = `Pressure end`,
    rh_start = `RH start`,
    rh_end = `RH end`,
    pneumatophore_count = `Pneumatophore_Count`,
    notes_1 = `Notes 1`,
    notes_2 = `Notes 2`
  ) %>%
  # Remove empty columns
  select(-any_of(c("", "_1", "_2", "_3", "_4", "_5")))

cat("After cleaning:", nrow(soil_water_data), "rows\n")

# Create datetime column (following fill_air_temp.R logic exactly)
soil_water_data$datetime <- mdy_hms(paste(soil_water_data$date, soil_water_data$start_time))

# Try alternative formats if needed (same logic as fill_air_temp.R)
na_indices <- which(is.na(soil_water_data$datetime))
if (length(na_indices) > 0) {
  cat("Trying alternative date format for", length(na_indices), "rows\n")
  soil_water_data$datetime[na_indices] <- dmy_hms(paste(soil_water_data$date[na_indices], soil_water_data$start_time[na_indices]))
  
  # If still some failures, try ymd_hms
  na_indices2 <- which(is.na(soil_water_data$datetime))
  if (length(na_indices2) > 0) {
    soil_water_data$datetime[na_indices2] <- ymd_hms(paste(soil_water_data$date[na_indices2], soil_water_data$start_time[na_indices2]))
  }
}

# Create date-only column for same-day matching (same as fill_air_temp.R)
soil_water_data$date_only <- as.Date(soil_water_data$datetime)

cat("Data processing complete:\n")
cat("- Date range:", min(soil_water_data$date, na.rm = TRUE), "to", max(soil_water_data$date, na.rm = TRUE), "\n")
cat("- Analyzers:", paste(sort(unique(soil_water_data$analyzer_id)), collapse = ", "), "\n")
cat("- Chambers:", paste(sort(unique(soil_water_data$chamber_id)), collapse = ", "), "\n")
cat("- Datetime parsing failures:", sum(is.na(soil_water_data$datetime)), "\n")

# =============================================================================
# LOAD TREE FLUX DATA FOR AIR TEMPERATURE
# =============================================================================

cat("\n=== LOADING TREE FLUX DATA FOR AIR TEMPERATURE ===\n")

# Read both tree flux files (same as your tree workflow)
tree_files <- c(
  "data/field_notes/blueflux compiled tree fluxes.csv",
  "data/field_notes/blueflux compiled tree fluxes_additional.csv"
)

tree_temp_data <- data.frame()

for (file in tree_files) {
  if (file.exists(file)) {
    cat("Reading", file, "\n")
    
    tryCatch({
      temp_data <- read_csv(file, show_col_types = FALSE)
      
      # Only keep rows with air_temp data
      if ("air_temp" %in% names(temp_data)) {
        temp_subset <- temp_data %>%
          select(any_of(c("date", "start_time", "air_temp"))) %>%
          filter(!is.na(air_temp))
        
        tree_temp_data <- bind_rows(tree_temp_data, temp_subset)
        cat("Added", nrow(temp_subset), "temperature records\n")
      } else {
        cat("No air_temp column found in", file, "\n")
      }
      
    }, error = function(e) {
      cat("Error reading", file, ":", e$message, "\n")
    })
  } else {
    cat("File not found:", file, "\n")
  }
}

if (nrow(tree_temp_data) == 0) {
  stop("No air temperature data found in tree flux files. Please check file paths and column names.")
}

cat("Total tree temperature records loaded:", nrow(tree_temp_data), "\n")

# Create datetime for tree data (same logic as fill_air_temp.R)
tree_temp_data$datetime <- mdy_hms(paste(tree_temp_data$date, tree_temp_data$start_time))

# Try alternative formats if needed
na_tree_indices <- which(is.na(tree_temp_data$datetime))
if (length(na_tree_indices) > 0) {
  tree_temp_data$datetime[na_tree_indices] <- dmy_hms(paste(tree_temp_data$date[na_tree_indices], tree_temp_data$start_time[na_tree_indices]))
  
  na_tree_indices2 <- which(is.na(tree_temp_data$datetime))
  if (length(na_tree_indices2) > 0) {
    tree_temp_data$datetime[na_tree_indices2] <- ymd_hms(paste(tree_temp_data$date[na_tree_indices2], tree_temp_data$start_time[na_tree_indices2]))
  }
}

tree_temp_data$date_only <- as.Date(tree_temp_data$datetime)

# Remove rows with failed datetime parsing
tree_temp_data <- tree_temp_data %>%
  filter(!is.na(datetime), !is.na(date_only), !is.na(air_temp))

cat("Tree temperature data ready:", nrow(tree_temp_data), "valid records\n")
cat("Tree data date range:", min(tree_temp_data$date_only, na.rm = TRUE), "to", max(tree_temp_data$date_only, na.rm = TRUE), "\n")

# =============================================================================
# WEATHER STATION DATA FUNCTIONS
# =============================================================================

# Function to get weather station data for missing dates
get_weather_station_data <- function(missing_dates, site_coords) {
  
  # Check if worldmet is available
  if (!requireNamespace("worldmet", quietly = TRUE)) {
    cat("worldmet package not available. Skipping weather station fallback.\n")
    return(data.frame())
  }
  
  library(worldmet)
  
  cat("Attempting to get weather station data for", length(missing_dates), "dates\n")
  
  weather_data <- data.frame()
  
  tryCatch({
    # Get nearby weather stations based on site coordinates
    stations <- getMeta(lat = site_coords$lat, lon = site_coords$lon, n = 5)
    
    if (nrow(stations) == 0) {
      cat("No weather stations found near coordinates\n")
      return(data.frame())
    }
    
    cat("Found", nrow(stations), "nearby weather stations\n")
    
    # Get unique years from missing dates
    missing_years <- unique(year(missing_dates))
    
    # Try to get data from the first available station
    for (station_idx in 1:min(3, nrow(stations))) {  # Try up to 3 stations
      station_code <- stations$code[station_idx]
      
      cat("Trying station:", station_code, "\n")
      
      for (yr in missing_years) {
        tryCatch({
          station_data <- importNOAA(
            code = station_code,
            year = yr,
            hourly = TRUE
          )
          
          if (nrow(station_data) > 0) {
            # Process and filter for our missing dates
            station_processed <- station_data %>%
              mutate(
                date_only = as.Date(date),
                hour = hour(date)
              ) %>%
              filter(date_only %in% missing_dates, !is.na(air_temp)) %>%
              select(date, date_only, hour, air_temp, station = code)
            
            weather_data <- bind_rows(weather_data, station_processed)
            cat("Got", nrow(station_processed), "records for year", yr, "\n")
          }
          
        }, error = function(e) {
          cat("Error getting data for station", station_code, "year", yr, ":", e$message, "\n")
        })
      }
      
      # If we got some data, break
      if (nrow(weather_data) > 0) break
    }
    
  }, error = function(e) {
    cat("Error accessing weather data:", e$message, "\n")
  })
  
  return(weather_data)
}

# =============================================================================
# FILL AIR TEMPERATURE USING HIERARCHICAL APPROACH
# =============================================================================

cat("\n=== FILLING AIR TEMPERATURE USING HIERARCHICAL APPROACH ===\n")
cat("Using the same logic as fill_air_temp.R:\n")
cat("1. Mean of tree values within 30 minutes (same date)\n")
cat("2. Nearest tree value from the same day\n")
cat("3. Weather station data (NEW!)\n")
cat("4. Leave as NA if all strategies fail\n\n")

# Function to fill air temperature with weather station fallback
fill_soilwater_air_temp_from_trees <- function(soil_data, tree_data) {
  
  # Initialize air_temp column as NA (soil/water data has no air temp)
  soil_data$air_temp <- NA
  soil_data$temp_source <- NA_character_  # Track source of temperature
  
  # Find all rows that need air temperature (all of them)
  missing_indices <- 1:nrow(soil_data)
  
  cat("Found", length(missing_indices), "soil/water measurements needing air temperature\n")
  
  # Initialize counters for each filling method
  filled_30min <- 0
  filled_same_day <- 0
  filled_weather <- 0
  remaining_na <- 0
  
  for (i in missing_indices) {
    target_datetime <- soil_data$datetime[i]
    target_date <- soil_data$date_only[i]
    
    # Skip if datetime parsing failed
    if (is.na(target_datetime) || is.na(target_date)) {
      cat("Warning: Could not parse datetime for row", i, "\n")
      soil_data$temp_source[i] <- "Failed datetime"
      remaining_na <- remaining_na + 1
      next
    }
    
    # Strategy 1: Find tree values within 30 minutes on the same date
    time_diff <- abs(difftime(tree_data$datetime, target_datetime, units = "mins"))
    within_30min <- which(!is.na(tree_data$air_temp) & 
                            tree_data$date_only == target_date & 
                            time_diff <= 30)
    
    if (length(within_30min) > 0) {
      # Use mean of tree values within 30 minutes
      soil_data$air_temp[i] <- mean(tree_data$air_temp[within_30min], na.rm = TRUE)
      soil_data$temp_source[i] <- "Tree 30min"
      filled_30min <- filled_30min + 1
      if (i <= 5) {
        cat("Row", i, ": Filled with mean of", length(within_30min), "tree values within 30 minutes\n")
      }
    } else {
      # Strategy 2: Find nearest tree value from the same day
      same_day_rows <- which(!is.na(tree_data$air_temp) & 
                               tree_data$date_only == target_date)
      
      if (length(same_day_rows) > 0) {
        # Calculate time differences for same day
        time_diffs <- abs(difftime(tree_data$datetime[same_day_rows], target_datetime, units = "mins"))
        nearest_idx <- same_day_rows[which.min(time_diffs)]
        
        soil_data$air_temp[i] <- tree_data$air_temp[nearest_idx]
        soil_data$temp_source[i] <- "Tree same-day"
        filled_same_day <- filled_same_day + 1
        if (i <= 5) {
          cat("Row", i, ": Filled with nearest same-day tree value (", 
              round(min(time_diffs), 1), "minutes away)\n")
        }
      } else {
        # Mark for weather station filling
        soil_data$air_temp[i] <- NA
        soil_data$temp_source[i] <- "Pending weather"
      }
    }
  }
  
  # Strategy 3: Fill remaining NAs with weather station data
  still_missing <- which(is.na(soil_data$air_temp))
  
  if (length(still_missing) > 0) {
    cat("\nStrategy 3: Attempting weather station data for", length(still_missing), "remaining measurements\n")
    
    # Get unique missing dates
    missing_dates <- unique(soil_data$date_only[still_missing])
    
    # Site coordinates (adjust based on your study sites)
    site_coords <- list(
      lat = ifelse(any(grepl("MI", soil_data$plot[still_missing])), 25.937, 25.157),  # Marco Island or Flamingo
      lon = ifelse(any(grepl("MI", soil_data$plot[still_missing])), -81.760, -80.9232)
    )
    
    # Get weather station data
    weather_data <- get_weather_station_data(missing_dates, site_coords)
    
    if (nrow(weather_data) > 0) {
      cat("Retrieved weather data for", length(unique(weather_data$date_only)), "dates\n")
      
      # Store weather data in parent environment for plotting
      assign("weather_data", weather_data, envir = parent.frame())
      
      # Fill remaining missing values with weather data
      for (i in still_missing) {
        target_datetime <- soil_data$datetime[i]
        target_date <- soil_data$date_only[i]
        
        if (!is.na(target_datetime) && !is.na(target_date)) {
          # Find weather data for the same date
          same_date_weather <- weather_data %>%
            filter(date_only == target_date, !is.na(air_temp))
          
          if (nrow(same_date_weather) > 0) {
            # Find closest hour
            target_hour <- hour(target_datetime)
            hour_diffs <- abs(same_date_weather$hour - target_hour)
            closest_weather_idx <- which.min(hour_diffs)
            
            soil_data$air_temp[i] <- same_date_weather$air_temp[closest_weather_idx]
            soil_data$temp_source[i] <- "Weather station"
            filled_weather <- filled_weather + 1
            
            if (i <= still_missing[5]) {  # Show details for first few
              cat("Row", i, ": Filled with weather station data (", 
                  hour_diffs[closest_weather_idx], "hours away)\n")
            }
          } else {
            soil_data$temp_source[i] <- "No data available"
            remaining_na <- remaining_na + 1
          }
        } else {
          soil_data$temp_source[i] <- "Failed datetime"
          remaining_na <- remaining_na + 1
        }
      }
    } else {
      cat("No weather station data available\n")
      # Mark remaining as no weather data available
      for (i in still_missing) {
        soil_data$temp_source[i] <- "No weather data"
      }
      remaining_na <- remaining_na + length(still_missing)
    }
  }
  
  # Print summary
  cat("\nTemperature filling summary:\n")
  cat("- Filled using 30-minute window:", filled_30min, "\n")
  cat("- Filled using nearest same-day value:", filled_same_day, "\n")
  cat("- Filled using weather station data:", filled_weather, "\n")
  cat("- Remaining as NA:", remaining_na, "\n")
  cat("- Total originally missing:", length(missing_indices), "\n")
  
  return(soil_data)
}

# Apply the filling function
soil_water_filled <- fill_soilwater_air_temp_from_trees(soil_water_data, tree_temp_data)

# Compare before and after
cat("\nBefore filling - Missing air_temp values:", sum(is.na(soil_water_data$air_temp)), "\n")
cat("After filling - Missing air_temp values:", sum(is.na(soil_water_filled$air_temp)), "\n")

# Summary of filled air temperatures
if (sum(!is.na(soil_water_filled$air_temp)) > 0) {
  temp_summary <- soil_water_filled %>%
    filter(!is.na(air_temp)) %>%
    summarise(
      min_temp = round(min(air_temp), 1),
      max_temp = round(max(air_temp), 1),
      mean_temp = round(mean(air_temp), 1),
      median_temp = round(median(air_temp), 1)
    )
  
  cat("Air temperature summary (filled values):\n")
  cat("- Range:", temp_summary$min_temp, "to", temp_summary$max_temp, "°C\n")
  cat("- Mean:", temp_summary$mean_temp, "°C\n")
  cat("- Median:", temp_summary$median_temp, "°C\n")
}

# =============================================================================
# CREATE VISUALIZATION OF TEMPERATURE SOURCES
# =============================================================================

cat("\n=== CREATING TEMPERATURE SOURCE VISUALIZATION ===\n")

# Create source tracking for visualization
create_temp_source_plot <- function(soil_data, tree_data, weather_data = NULL) {
  
  library(ggplot2)
  
  # Create a combined dataset showing all temperature sources
  plot_data <- data.frame()
  
  # Add tree flux data (original measurements)
  if (nrow(tree_data) > 0) {
    tree_plot_data <- tree_data %>%
      filter(!is.na(air_temp), !is.na(datetime)) %>%
      mutate(
        source = "Tree flux (original)",
        measurement_type = "Available data",
        dataset = "Tree flux measurements"
      ) %>%
      select(datetime, air_temp, source, measurement_type, dataset)
    
    plot_data <- bind_rows(plot_data, tree_plot_data)
  }
  
  # Add weather station data if available
  if (!is.null(weather_data) && nrow(weather_data) > 0) {
    weather_plot_data <- weather_data %>%
      filter(!is.na(air_temp), !is.na(date)) %>%
      mutate(
        datetime = date,
        source = "Weather station",
        measurement_type = "Available data",
        dataset = paste("Weather station", station)
      ) %>%
      select(datetime, air_temp, source, measurement_type, dataset)
    
    plot_data <- bind_rows(plot_data, weather_plot_data)
  }
  
  # Add soil/water measurements with source attribution
  soil_plot_data <- soil_data %>%
    filter(!is.na(air_temp), !is.na(datetime)) %>%
    mutate(
      source = case_when(
        temp_source == "Tree 30min" ~ "Tree flux (30-min window)",
        temp_source == "Tree same-day" ~ "Tree flux (same-day nearest)",
        temp_source == "Weather station" ~ "Weather station (filled)",
        !is.na(temp_source) ~ paste("Other:", temp_source),
        TRUE ~ "Unknown source"
      ),
      measurement_type = "Soil/Water flux",
      dataset = paste(plot, surface, chamber_id, sep = " - ")
    ) %>%
    select(datetime, air_temp, source, measurement_type, dataset)
  
  plot_data <- bind_rows(plot_data, soil_plot_data)
  
  # Create the main time series plot
  if (nrow(plot_data) > 0) {
    p1 <- ggplot(plot_data, aes(x = datetime, y = air_temp, color = source)) +
      geom_point(alpha = 0.7, size = 1.5) +
      scale_color_manual(
        values = c(
          "Tree flux (original)" = "forestgreen",
          "Tree flux (30-min window)" = "darkgreen",
          "Tree flux (same-day nearest)" = "green", 
          "Weather station" = "red",
          "Weather station (filled)" = "darkred",
          "Unknown source" = "gray"
        ),
        name = "Temperature Source"
      ) +
      labs(
        title = "Air Temperature Data Sources Over Time",
        subtitle = "Soil/Water measurements filled using hierarchical approach",
        x = "Date and Time",
        y = "Air Temperature (°C)"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12)
      ) +
      facet_wrap(~ measurement_type, scales = "free_x", ncol = 1)
    
    print(p1)
    
    # Create a detailed daily view
    p2 <- plot_data %>%
      filter(measurement_type == "Soil/Water flux") %>%
      mutate(date_only = as.Date(datetime)) %>%
      ggplot(aes(x = datetime, y = air_temp, color = source, shape = source)) +
      geom_point(size = 2, alpha = 0.8) +
      geom_line(aes(group = dataset), alpha = 0.3, color = "gray") +
      scale_color_manual(
        values = c(
          "Tree flux (30-min window)" = "darkgreen",
          "Tree flux (same-day nearest)" = "green", 
          "Weather station (filled)" = "darkred",
          "Unknown source" = "gray"
        ),
        name = "Filling Method"
      ) +
      scale_shape_manual(
        values = c(
          "Tree flux (30-min window)" = 16,  # circle
          "Tree flux (same-day nearest)" = 17,  # triangle
          "Weather station (filled)" = 15,  # square
          "Unknown source" = 4  # cross
        ),
        name = "Filling Method"
      ) +
      labs(
        title = "Soil/Water Flux Air Temperature Filling Methods",
        subtitle = "Detailed view of how each measurement was filled",
        x = "Date and Time",
        y = "Air Temperature (°C)"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold")
      ) +
      facet_wrap(~ date_only, scales = "free_x")
    
    print(p2)
    
    # Save both plots
    ggsave("intermediate/air_temperature_sources_overview.png", p1, 
           width = 14, height = 10, dpi = 300)
    ggsave("intermediate/air_temperature_filling_methods.png", p2, 
           width = 16, height = 12, dpi = 300)
    
    cat("Temperature plots saved:\n")
    cat("- intermediate/air_temperature_sources_overview.png (overview)\n")
    cat("- intermediate/air_temperature_filling_methods.png (detailed filling methods)\n")
    
    # Create summary by source
    source_summary <- plot_data %>%
      group_by(source, measurement_type) %>%
      summarise(
        count = n(),
        min_temp = round(min(air_temp, na.rm = TRUE), 1),
        max_temp = round(max(air_temp, na.rm = TRUE), 1),
        mean_temp = round(mean(air_temp, na.rm = TRUE), 1),
        date_range = paste(min(as.Date(datetime), na.rm = TRUE), 
                           "to", 
                           max(as.Date(datetime), na.rm = TRUE)),
        .groups = "drop"
      )
    
    cat("\nTemperature source summary:\n")
    print(source_summary)
    
    # Create filling method summary for soil/water data only
    if (any(plot_data$measurement_type == "Soil/Water flux")) {
      filling_summary <- plot_data %>%
        filter(measurement_type == "Soil/Water flux") %>%
        count(source, name = "count") %>%
        mutate(
          percentage = round(count / sum(count) * 100, 1)
        ) %>%
        arrange(desc(count))
      
      cat("\nSoil/Water filling method summary:\n")
      print(filling_summary)
    }
    
    return(list(overview = p1, detailed = p2))
  } else {
    cat("No data available for plotting\n")
    return(NULL)
  }
}

# Create the plot (note: weather_data variable may not exist if weather station filling wasn't used)
tryCatch({
  temp_plot <- create_temp_source_plot(soil_water_filled, tree_temp_data, 
                                       if(exists("weather_data")) weather_data else NULL)
}, error = function(e) {
  cat("Could not create temperature source plot:", e$message, "\n")
})

# =============================================================================
# SAVE RESULTS
# =============================================================================

cat("\n=== SAVING RESULTS ===\n")

# Create intermediate directory (following tree workflow structure)
if (!dir.exists("intermediate/intermediate_files")) {
  dir.create("intermediate/intermediate_files", recursive = TRUE)
}

# Save filled data (following same naming pattern as tree workflow)
write_csv(soil_water_filled, "intermediate/blueflux_soilwater_filled.csv")

cat("Filled soil/water data saved to: intermediate/blueflux_soilwater_filled.csv\n")

# Optional: Show some examples of filled values (same as fill_air_temp.R)
filled_examples <- soil_water_filled %>%
  filter(!is.na(air_temp)) %>%
  slice_head(n = 10) %>%
  select(flux_id, date, start_time, air_temp, temp_source, analyzer_id, chamber_id)

if (nrow(filled_examples) > 0) {
  cat("\nExample of filled values (first 10):\n")
  print(filled_examples)
}

cat("\n=== STEP 1 COMPLETE ===\n")
cat("Air temperature successfully filled from tree flux data using hierarchical approach\n")
cat("Ready for Step 2: assign_soilwater_vol_area.R\n")
cat("==================================================================\n")