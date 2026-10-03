# CLEAN LGR3 DATA: Remove corrupted measurements

library(dplyr)
library(readr)

cat("=== CLEANING LGR3 DATA ===\n")

# Load LGR3 data
output_dir <- "intermediate/soilwater_results"
lgr3_obswin_file <- file.path(output_dir, "lgr3_soilwater_obswin.csv")
lgr3_data <- read_csv(lgr3_obswin_file, show_col_types = FALSE)

cat("Original LGR3 data:", nrow(lgr3_data), "rows,", length(unique(lgr3_data$UniqueID)), "measurements\n")

# Remove the corrupted measurements (SW_166 through SW_175)
corrupted_measurements <- c("SW_166", "SW_167", "SW_168", "SW_169", "SW_170", "SW_171", "SW_172", "SW_173", "SW_174", "SW_175")

lgr3_data_clean <- lgr3_data %>%
  filter(!UniqueID %in% corrupted_measurements)

cat("After cleaning:", nrow(lgr3_data_clean), "rows,", length(unique(lgr3_data_clean$UniqueID)), "measurements\n")

# Save cleaned data
lgr3_clean_file <- file.path(output_dir, "lgr3_soilwater_obswin_clean.csv")
write_csv(lgr3_data_clean, lgr3_clean_file)

cat("Cleaned LGR3 data saved to:", lgr3_clean_file, "\n")

# Now run manual identification for LGR3
cat("\n=== STARTING LGR3 MANUAL IDENTIFICATION ===\n")

# Convert to list format
obswin_list <- split(lgr3_data_clean, lgr3_data_clean$UniqueID)
names(obswin_list) <- unique(lgr3_data_clean$UniqueID)

cat("Converted to list of", length(obswin_list), "observation windows\n")

# Set up graphics
graphics.off()
default.device <- getOption("device")

if (Sys.info()["sysname"] == "Darwin") {
  options(device = function() quartz(width = 10, height = 6))
} else if (Sys.info()["sysname"] == "Windows") {
  options(device = function() windows(width = 10, height = 6))
} else {
  options(device = function() X11(width = 10, height = 6))
}

cat("Starting LGR3 manual identification with 96 clean measurements...\n")

# Run manual identification
manID_result <- click.peak2(
  obswin_list,
  gastype = "CO2dry_ppm",
  sleep = 3,
  plot.lim = c(300, 1500),
  warn.length = 60,
  save.plots = "intermediate/soilwater_lgr3_manual_plots_clean"
)

# Revert graphics device
options(device = default.device)

# Save results
if (!is.null(manID_result)) {
  manid_file <- file.path(output_dir, "lgr3_soilwater_manual_identification_clean.csv")
  write_csv(manID_result, manid_file)
  
  cat("LGR3 manual identification complete!\n")
  cat("Results saved to:", manid_file, "\n")
  cat("Processed", length(unique(manID_result$UniqueID)), "measurements\n")
}