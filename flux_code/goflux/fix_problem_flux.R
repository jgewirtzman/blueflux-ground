# Fix a single measurement by re-clicking start/end points

# Step 1: Identify the UniqueID you want to fix
# Replace "YOUR_UNIQUE_ID" with the actual UniqueID from your plot review
problematic_id <- "YOUR_UNIQUE_ID"  # e.g. "Mar_23_3_A_1_LGR1"

# Step 2: Find the index of this measurement in your observation windows list
measurement_index <- which(names(ow.lgr1) == problematic_id)

if(length(measurement_index) == 0) {
  cat("UniqueID not found. Available UniqueIDs:\n")
  cat(paste(names(ow.lgr1), collapse = ", "), "\n")
  stop("Please check the UniqueID")
}

cat("Found measurement at index:", measurement_index, "\n")
cat("UniqueID:", names(ow.lgr1)[measurement_index], "\n")

# Step 3: Set up graphics device
default.device <- getOption("device")

if (Sys.info()["sysname"] == "Darwin") {
  options(device = function() quartz(width = 10, height = 6))
} else if (Sys.info()["sysname"] == "Windows") {
  options(device = function() windows(width = 10, height = 6))
} else {
  options(device = function() X11(width = 10, height = 6))
}

# Step 4: Re-click just this one measurement
cat("Re-clicking measurement:", problematic_id, "\n")
cat("Click on the NEW start and end points for this measurement\n")

corrected_measurement <- click.peak2(
  ow.lgr1,
  seq = measurement_index,          # Only process this specific measurement
  gastype = "CO2dry_ppm",
  sleep = NULL,                     # Don't auto-close plot so you can review
  plot.lim = c(200, 1000),
  save.plots = paste0("flux_code/corrected_", problematic_id, "_plot")
)

# Revert graphics device
options(device = default.device)

# Step 5: Update your main dataset
# Find the row(s) in manID.lgr1 that correspond to this UniqueID
rows_to_update <- which(manID.lgr1$UniqueID == problematic_id)

if(length(rows_to_update) > 0) {
  # Replace the old data with corrected data
  manID.lgr1[rows_to_update, ] <- corrected_measurement
  
  cat("Updated", length(rows_to_update), "rows for UniqueID:", problematic_id, "\n")
  
  # Save the updated manual identification results
  write_csv(manID.lgr1, "flux_code/lgr1_manual_identification_results_updated.csv")
  cat("Updated results saved to: flux_code/lgr1_manual_identification_results_updated.csv\n")
  
} else {
  cat("Warning: No rows found to update in manID.lgr1\n")
}

# Step 6: Re-run flux calculations for this measurement only
cat("Re-calculating fluxes for corrected measurement...\n")

# Filter just this measurement for flux calculation
single_measurement <- manID.lgr1[manID.lgr1$UniqueID == problematic_id, ]

# Calculate new flux
corrected_CO2_flux <- goFlux(
  dataframe = single_measurement,
  gastype = "CO2dry_ppm",
  H2O_col = "H2O_ppm",
  prec = 0.4,
  warn.length = 60
)

# Run best.flux on the corrected measurement
corrected_CO2_best <- best.flux(
  flux.result = corrected_CO2_flux,
  criteria = c("MAE", "RMSE", "AICc", "SE", "g.factor", "kappa", "MDF", "nb.obs", "intercept", "p-value"),
  g.limit = 2,
  p.val = 0.05,
  k.ratio = 1,
  warn.length = 60
)

# Step 7: Update your main results
# Remove old result and add corrected result
CO2_best_lgr1_updated <- CO2_best_lgr1[CO2_best_lgr1$UniqueID != problematic_id, ]
CO2_best_lgr1_updated <- rbind(CO2_best_lgr1_updated, corrected_CO2_best)

# Sort by UniqueID to maintain order
CO2_best_lgr1_updated <- CO2_best_lgr1_updated[order(CO2_best_lgr1_updated$UniqueID), ]

# Save updated results
write_csv(CO2_best_lgr1_updated, "flux_code/CO2_best_flux_lgr1_results_updated.csv")

# Step 8: Create new plot for verification
corrected_plot <- flux.plot(
  flux.results = corrected_CO2_best,
  dataframe = single_measurement,
  gastype = "CO2dry_ppm",
  shoulder = 30,
  plot.legend = c("MAE", "AICc", "k.ratio", "g.factor"),
  plot.display = c("MDF", "prec", "nb.obs", "flux.term"),
  quality.check = TRUE,
  best.model = TRUE,
  p.val.disp = "round"
)

# Save corrected plot
flux2pdf(
  plot.list = corrected_plot,
  outfile = paste0("flux_code/", problematic_id, "_corrected_plot.pdf"),
  width = 11.6,
  height = 8.2
)

# Step 9: Display results
cat("\n=== CORRECTION SUMMARY ===\n")
cat("Corrected UniqueID:", problematic_id, "\n")
cat("New flux estimate:", round(corrected_CO2_best$best.flux, 4), "µmol·m⁻²·s⁻¹\n")
cat("Model selected:", corrected_CO2_best$model, "\n")
cat("Quality check:", corrected_CO2_best$quality.check, "\n")

cat("\nFiles created:\n")
cat("- flux_code/lgr1_manual_identification_results_updated.csv\n")
cat("- flux_code/CO2_best_flux_lgr1_results_updated.csv\n")
cat("- flux_code/", problematic_id, "_corrected_plot.pdf\n")

cat("\nTo use updated results going forward:\n")
cat("manID.lgr1 <- read_csv('flux_code/lgr1_manual_identification_results_updated.csv')\n")
cat("CO2_best_lgr1 <- read_csv('flux_code/CO2_best_flux_lgr1_results_updated.csv')\n")









