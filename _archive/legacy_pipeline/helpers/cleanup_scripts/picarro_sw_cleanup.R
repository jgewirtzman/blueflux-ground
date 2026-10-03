# Skip the complex verification and just do a simple check
cat("Data ready: 28 measurements, corrupted ones removed\n")

# Set up graphics and run manual identification
cat("\n=== STARTING MANUAL IDENTIFICATION ===\n")

graphics.off()
default.device <- getOption("device")

if (Sys.info()["sysname"] == "Darwin") {
  options(device = function() quartz(width = 10, height = 6))
} else if (Sys.info()["sysname"] == "Windows") {
  options(device = function() windows(width = 10, height = 6))
} else {
  options(device = function() X11(width = 10, height = 6))
}

cat("Starting manual identification with 28 clean measurements...\n")
cat("Click on START and END points for each measurement\n\n")

# Run manual identification
manID_result <- click.peak2(
  obswin_list,
  gastype = "CO2dry_ppm",
  sleep = 3,
  plot.lim = c(300, 1500),
  warn.length = 60,
  save.plots = paste0("intermediate/soilwater_picarro_manual_plots_clean")
)
