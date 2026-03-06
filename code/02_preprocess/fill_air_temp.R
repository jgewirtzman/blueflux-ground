# Air Temperature Missing Value Imputation
# This script fills missing air temperature observations using a hierarchical approach:
# 1. Mean of values within 30 minutes (same date and time)
# 2. Nearest time from the same day if no values within 30 minutes
# 3. NA if no values available on the same day

library(dplyr)
library(lubridate)

# Read the CSV file
df <- read.csv("data/field_notes/blueflux compiled tree fluxes.csv", stringsAsFactors = FALSE)

# Create datetime column by combining date and time strings
df$datetime <- mdy_hms(paste(df$date, df$start_time))

# If that fails for some rows, try alternative formats
na_indices <- which(is.na(df$datetime))
if (length(na_indices) > 0) {
  # Try dmy_hms format for failed rows
  df$datetime[na_indices] <- dmy_hms(paste(df$date[na_indices], df$start_time[na_indices]))
  
  # If still some failures, try ymd_hms
  na_indices2 <- which(is.na(df$datetime))
  if (length(na_indices2) > 0) {
    df$datetime[na_indices2] <- ymd_hms(paste(df$date[na_indices2], df$start_time[na_indices2]))
  }
}

# Create date-only column for same-day matching
df$date_only <- as.Date(df$datetime)

# Function to fill missing air temperature values
fill_missing_air_temp <- function(data) {
  # Create a copy of the data to modify
  filled_data <- data
  
  # Find rows with missing air_temp
  missing_indices <- which(is.na(filled_data$air_temp))
  
  cat("Found", length(missing_indices), "missing air temperature values\n")
  
  # Initialize counters for each filling method
  filled_30min <- 0
  filled_same_day <- 0
  remaining_na <- 0
  
  for (i in missing_indices) {
    target_datetime <- filled_data$datetime[i]
    target_date <- filled_data$date_only[i]
    
    # Skip if datetime parsing failed
    if (is.na(target_datetime) || is.na(target_date)) {
      cat("Warning: Could not parse datetime for row", i, "\n")
      remaining_na <- remaining_na + 1
      next
    }
    
    # Strategy 1: Find values within 30 minutes on the same date
    time_diff <- abs(difftime(filled_data$datetime, target_datetime, units = "mins"))
    within_30min <- which(!is.na(filled_data$air_temp) & 
                            filled_data$date_only == target_date & 
                            time_diff <= 30 & 
                            time_diff > 0)  # Exclude the target row itself
    
    if (length(within_30min) > 0) {
      # Use mean of values within 30 minutes
      filled_data$air_temp[i] <- mean(filled_data$air_temp[within_30min], na.rm = TRUE)
      filled_30min <- filled_30min + 1
      cat("Row", i, ": Filled with mean of", length(within_30min), "values within 30 minutes\n")
    } else {
      # Strategy 2: Find nearest time from the same day
      same_day_rows <- which(!is.na(filled_data$air_temp) & 
                               filled_data$date_only == target_date)
      
      if (length(same_day_rows) > 0) {
        # Calculate time differences for same day
        time_diffs <- abs(difftime(filled_data$datetime[same_day_rows], target_datetime, units = "mins"))
        nearest_idx <- same_day_rows[which.min(time_diffs)]
        
        filled_data$air_temp[i] <- filled_data$air_temp[nearest_idx]
        filled_same_day <- filled_same_day + 1
        cat("Row", i, ": Filled with nearest same-day value (", 
            round(min(time_diffs), 1), "minutes away)\n")
      } else {
        # Strategy 3: No values available on the same day - leave as NA
        remaining_na <- remaining_na + 1
        cat("Row", i, ": No values available on same day - leaving as NA\n")
      }
    }
  }
  
  # Print summary
  cat("\nFilling Summary:\n")
  cat("- Filled using 30-minute window:", filled_30min, "\n")
  cat("- Filled using nearest same-day value:", filled_same_day, "\n")
  cat("- Remaining as NA:", remaining_na, "\n")
  cat("- Total originally missing:", length(missing_indices), "\n")
  
  return(filled_data)
}

# Apply the filling function
df_filled <- fill_missing_air_temp(df)

# Compare before and after
cat("\nBefore filling - Missing air_temp values:", sum(is.na(df$air_temp)), "\n")
cat("After filling - Missing air_temp values:", sum(is.na(df_filled$air_temp)), "\n")

# Save the filled dataset
write.csv(df_filled, "intermediate/blueflux_trees_filled.csv", row.names = FALSE)
#cat("\nFilled dataset saved as 'blueflux_filled_air_temp.csv'\n")

# Optional: Show some examples of filled values
missing_original <- which(is.na(df$air_temp))
if (length(missing_original) > 0) {
  cat("\nExample of filled values (first 10):\n")
  examples <- head(missing_original, 10)
  comparison <- data.frame(
    Row = examples,
    Date = df$date[examples],
    Time = df$start_time[examples],
    Original = df$air_temp[examples],
    Filled = df_filled$air_temp[examples]
  )
  print(comparison)
}

# Create visualization of air temperature by time, faceted by date
library(ggplot2)

# Prepare data for plotting
plot_data <- df_filled %>%
  filter(!is.na(datetime) & !is.na(air_temp)) %>%
  mutate(
    time_of_day = format(datetime, "%H:%M"),
    date_label = format(date_only, "%m/%d/%Y"),
    was_filled = row_number() %in% missing_original & !is.na(air_temp)
  )

# Create the plot
air_temp_plot <- ggplot(plot_data, aes(x = datetime, y = air_temp)) +
  geom_point(aes(color = was_filled), alpha = 0.7, size = 1.5) +
  geom_line(alpha = 0.5, color = "gray60") +
  facet_wrap(~ date_label, scales = "free_x", ncol = 3) +
  scale_color_manual(
    values = c("FALSE" = "steelblue", "TRUE" = "red"),
    labels = c("Original", "Filled"),
    name = "Data Type"
  ) +
  labs(
    title = "Air Temperature by Time",
    subtitle = "Faceted by Date (Red points indicate filled missing values)",
    x = "Time",
    y = "Air Temperature (°C)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(size = 9),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  ) +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hours")

# Display the plot
print(air_temp_plot)
