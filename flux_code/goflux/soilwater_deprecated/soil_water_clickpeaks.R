# STEP 4B: MANUAL IDENTIFICATION FOR SOIL/WATER DATA

library(goFlux)
library(dplyr)
library(readr)

# Set the analyzer you want to process
ANALYZER_TO_PROCESS <- "LGR2"  # Change to: LGR1, LGR2, LGR3, or Picarro

cat("Processing", ANALYZER_TO_PROCESS, "\n")

# Load observation window data
output_dir <- "flux_code/soilwater_goflux_results"
obswin_file <- file.path(output_dir, paste0(tolower(ANALYZER_TO_PROCESS), "_soilwater_obswin.csv"))
obswin_data <- read_csv(obswin_file, show_col_types = FALSE)

cat("Loaded", nrow(obswin_data), "rows,", length(unique(obswin_data$UniqueID)), "measurements\n")

# Convert combined data.frame back to list format for click.peak2()
obswin_list <- split(obswin_data, obswin_data$UniqueID)
names(obswin_list) <- unique(obswin_data$UniqueID)

cat("Converted to list of", length(obswin_list), "observation windows\n")

# Set up graphics device
graphics.off()
default.device <- getOption("device")

if (Sys.info()["sysname"] == "Darwin") {
  options(device = function() quartz(width = 10, height = 6))
} else if (Sys.info()["sysname"] == "Windows") {
  options(device = function() windows(width = 10, height = 6))
} else {
  options(device = function() X11(width = 10, height = 6))
}

# Manual identification with click.peak2()
cat("Starting manual identification...\n")
cat("Click on START and END points for each measurement\n")

manID_result <- click.peak2(
  obswin_list,
  gastype = "CO2dry_ppm",
  sleep = 3,
  plot.lim = c(300, 1500),    # Adjusted for soil/water CO2 range
  warn.length = 60,           # Minimum observations threshold
  save.plots = paste0("flux_code/soilwater_", tolower(ANALYZER_TO_PROCESS), "_manual_plots")
)

# Revert graphics device
options(device = default.device)

# Save results
manid_file <- file.path(output_dir, paste0(tolower(ANALYZER_TO_PROCESS), "_soilwater_manual_identification.csv"))
write_csv(manID_result, manid_file)

cat("Manual identification complete!\n")
cat("Processed", nrow(manID_result), "rows\n")
cat("Unique measurements:", length(unique(manID_result$UniqueID)), "\n")
cat("Results saved to:", manid_file, "\n")

# Show completion status
completed_files <- list.files(output_dir, pattern = "*_manual_identification.csv$")
remaining_analyzers <- setdiff(c("lgr1", "lgr2", "lgr3", "picarro"), 
                               gsub("_soilwater_manual_identification.csv", "", completed_files))

cat("\nProgress:\n")
cat("Completed:", paste(toupper(gsub("_soilwater_manual_identification.csv", "", completed_files)), collapse = ", "), "\n")
if (length(remaining_analyzers) > 0) {
  cat("Remaining:", paste(toupper(remaining_analyzers), collapse = ", "), "\n")
  cat("Next: Change ANALYZER_TO_PROCESS to", paste0('"', toupper(remaining_analyzers[1]), '"'), "and run again\n")
} else {
  cat("All analyzers complete! Ready for Step 4C\n")
}
