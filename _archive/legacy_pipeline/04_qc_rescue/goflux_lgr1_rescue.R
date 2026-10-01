# Load required libraries
library(goFlux)
library(dplyr)
library(readr)

# Step 1: Check timezone of your imported data
cat("Checking timezones...\n")
lgr_data_tz <- attr(lgr_data$POSIX.time, "tzone")
if(is.null(lgr_data_tz)) lgr_data_tz <- ""
cat("LGR data timezone:", ifelse(lgr_data_tz == "", "UTC (default)", lgr_data_tz), "\n")

# Load the LGR1 auxfile and match the timezone
lgr1_auxfile <- read_csv("intermediate/auxfiles/lgr1_rescue_auxfile.csv")  %>%
  mutate(start.time = as.POSIXct(start.time, tz = ifelse(lgr_data_tz == "", "UTC", lgr_data_tz)))

cat("Auxfile timezone set to:", attr(lgr1_auxfile$start.time, "tzone"), "\n")

# Check the auxfile structure
cat("LGR1 Auxfile Summary:\n")
cat("Number of measurements:", nrow(lgr1_auxfile), "\n")
cat("Date range:", min(as.Date(lgr1_auxfile$start.time)), "to", max(as.Date(lgr1_auxfile$start.time)), "\n")
cat("Time range:", format(min(lgr1_auxfile$start.time), "%H:%M:%S"), "to", format(max(lgr1_auxfile$start.time), "%H:%M:%S"), "\n\n")

# Display first few rows
print("First 6 rows of LGR1 auxfile:")
print(head(lgr1_auxfile))

# Step 2: Create observation windows
# Adjust these parameters based on your measurement protocol:
measurement_length <- 300  # Chamber closure time in seconds (5 minutes)
buffer_time <- 300         # Buffer time before/after measurement (5 minutes)

# Replace 'lgr_data' with your actual imported LGR1 data object name
ow.lgr1 <- obs.win(
  inputfile = lgr_data,  # Your imported LGR data
  auxfile = lgr1_auxfile,
  gastype = "CO2dry_ppm",  # or "CH4dry_ppb" depending on what you're measuring
  obs.length = measurement_length,
  shoulder = buffer_time
)

cat("Observation windows created for", length(ow.lgr1), "measurements\n")

# Close all open graphics devices
graphics.off()

# Set graphics device options for RStudio
default.device <- getOption("device")

# For Mac in RStudio, use quartz with explicit parameters
if (Sys.info()["sysname"] == "Darwin") {
  options(device = function() quartz(width = 10, height = 6))
} else if (Sys.info()["sysname"] == "Windows") {
  options(device = function() windows(width = 10, height = 6))
} else {
  options(device = function() X11(width = 10, height = 6))
}

# Run manual identification for all measurements
cat("Starting manual identification for LGR1...\n")
cat("You will need to click on the start and end points of each measurement\n")

manID.lgr1 <- click.peak2(
  ow.lgr1,
  gastype = "CO2dry_ppm",     # Adjust based on your target gas
  sleep = 3,                   # 3 second delay between plots
  plot.lim = c(200, 1200),    # Adjust Y-axis limits for CO2 (ppm)
  save.plots = "intermediate/lgr1_flux_plots"  # Save plots for review
)

# Revert graphics device
options(device = default.device)

cat("Manual identification complete!\n")
cat("Check the saved plots in intermediate/lgr1_flux_plots.pdf\n")

# Save the results
write_csv(manID.lgr1, "intermediate/lgr1_manual_identification_results_rescue.csv")
cat("Results saved to intermediate/lgr1_manual_identification_results.csv\n")

# Display results summary
cat("\nLGR1 Processing Results Summary:\n")
cat("Successfully processed:", nrow(manID.lgr1), "measurements\n")
cat("UniqueIDs processed:", length(unique(manID.lgr1$UniqueID)), "unique measurements\n")
cat("Date range:", min(as.Date(manID.lgr1$start.time_corr)), "to", max(as.Date(manID.lgr1$start.time_corr)), "\n")









# Load required libraries
library(goFlux)
library(dplyr)
library(readr)

# Step 1: Load your manually identified LGR1 data
# Assuming you saved it as 'manID.lgr1' from the previous step
# If you saved it to CSV, load it back:
# manID.lgr1 <- read_csv("intermediate/lgr1_manual_identification_results.csv")

# Check the structure of your data
cat("Manual ID data summary:\n")
cat("Number of measurements:", nrow(manID.lgr1), "\n")
cat("Columns available:", paste(names(manID.lgr1), collapse = ", "), "\n\n")

# Display first few rows to verify structure
print("First 3 rows of manual ID data:")
print(head(manID.lgr1, 3))

# Step 2: Calculate CO2 fluxes
cat("\nCalculating CO2 fluxes...\n")
CO2_flux_lgr1 <- goFlux(
  dataframe = manID.lgr1,
  gastype = "CO2dry_ppm",
  H2O_col = "H2O_ppm",          # Water vapor column
  #prec = 0.4,                    # Instrument precision (adjust based on your LGR specs)
  warn.length = 60               # Minimum number of observations
)

# Step 3: Calculate CH4 fluxes (if available in your data)
if("CH4dry_ppb" %in% names(manID.lgr1)) {
  cat("Calculating CH4 fluxes...\n")
  CH4_flux_lgr1 <- goFlux(
    dataframe = manID.lgr1,
    gastype = "CH4dry_ppb",
    H2O_col = "H2O_ppm",
#    prec = 2,                    # Adjust precision for CH4 (typically higher than CO2)
    warn.length = 60
  )
} else {
  cat("CH4dry_ppb column not found - skipping CH4 flux calculation\n")
}

# Step 4: Display results summary
cat("\n=== CO2 Flux Results Summary ===\n")
cat("Total measurements processed:", nrow(CO2_flux_lgr1), "\n")
cat("Date range:", min(as.Date(CO2_flux_lgr1$UniqueID)), "to", max(as.Date(CO2_flux_lgr1$UniqueID)), "\n")

# Summary statistics for CO2 fluxes
co2_summary <- CO2_flux_lgr1 %>%
  summarise(
    mean_LM_flux = round(mean(LM.flux, na.rm = TRUE), 3),
    median_LM_flux = round(median(LM.flux, na.rm = TRUE), 3),
    mean_HM_flux = round(mean(HM.flux, na.rm = TRUE), 3),
    median_HM_flux = round(median(HM.flux, na.rm = TRUE), 3),
    mean_r2_LM = round(mean(LM.r2, na.rm = TRUE), 3),
    mean_r2_HM = round(mean(HM.r2, na.rm = TRUE), 3)
  )

print(co2_summary)

# Step 5: Save results
write_csv(CO2_flux_lgr1, "intermediate/CO2_flux_lgr1_results_rescue.csv")
cat("\nCO2 flux results saved to: intermediate/CO2_flux_lgr1_results.csv\n")

if(exists("CH4_flux_lgr1")) {
  write_csv(CH4_flux_lgr1, "intermediate/CH4_flux_lgr1_results_rescue.csv")
  cat("CH4 flux results saved to: intermediate/CH4_flux_lgr1_results.csv\n")
}

# Step 6: Quality check - identify potential issues
cat("\n=== Quality Check ===\n")

# Check for low R² values (indicating poor fits)
low_r2_LM <- CO2_flux_lgr1 %>% filter(LM.r2 < 0.7)
low_r2_HM <- CO2_flux_lgr1 %>% filter(HM.r2 < 0.7)

cat("Measurements with LM R² < 0.7:", nrow(low_r2_LM), "\n")
cat("Measurements with HM R² < 0.7:", nrow(low_r2_HM), "\n")

# Check for extreme flux values (potential outliers)
q95_LM <- quantile(CO2_flux_lgr1$LM.flux, 0.95, na.rm = TRUE)
q05_LM <- quantile(CO2_flux_lgr1$LM.flux, 0.05, na.rm = TRUE)
extreme_LM <- CO2_flux_lgr1 %>% filter(LM.flux > q95_LM | LM.flux < q05_LM)

cat("Potential outliers (extreme 5% of flux values):", nrow(extreme_LM), "\n")

if(nrow(extreme_LM) > 0) {
  cat("Extreme flux UniqueIDs:", paste(extreme_LM$UniqueID, collapse = ", "), "\n")
}

# Step 7: Compare Linear vs Non-linear models
cat("\n=== Model Comparison ===\n")
model_comparison <- CO2_flux_lgr1 %>%
  summarise(
    correlation_LM_HM = round(cor(LM.flux, HM.flux, use = "complete.obs"), 3),
    mean_diff_LM_HM = round(mean(LM.flux - HM.flux, na.rm = TRUE), 3),
    better_HM_AICc = sum(HM.AICc < LM.AICc, na.rm = TRUE),
    total_comparisons = sum(!is.na(HM.AICc) & !is.na(LM.AICc))
  )

cat("Correlation between LM and HM fluxes:", model_comparison$correlation_LM_HM, "\n")
cat("Mean difference (LM - HM):", model_comparison$mean_diff_LM_HM, "µmol·m⁻²·s⁻¹\n")
cat("Measurements where HM model fits better (lower AICc):", model_comparison$better_HM_AICc, 
    "out of", model_comparison$total_comparisons, "\n")

cat("\nFlux calculation complete! Check the saved CSV files for detailed results.\n")














# Load required libraries
library(goFlux)
library(dplyr)
library(readr)

# Step 1: Run best.flux on CO2 results
cat("Running best.flux analysis for CO2...\n")
CO2_best_lgr1 <- best.flux(
  flux.result = CO2_flux_lgr1,
  criteria = c("MAE", "RMSE", "AICc", "SE", "g.factor", "kappa", "MDF", "nb.obs", "intercept", "p-value"),
  intercept.lim = NULL,        # Auto-calculate from data
  g.limit = 2,                 # Medium threshold for g-factor
  p.val = 0.05,               # Statistical significance threshold
  k.ratio = 1,                # Kappa ratio threshold
  warn.length = 60            # Minimum observation count
)

# Step 2: Run best.flux on CH4 results (if available)
if(exists("CH4_flux_lgr1")) {
  cat("Running best.flux analysis for CH4...\n")
  CH4_best_lgr1 <- best.flux(
    flux.result = CH4_flux_lgr1,
    criteria = c("MAE", "RMSE", "AICc", "SE", "g.factor", "kappa", "MDF", "nb.obs", "intercept", "p-value"),
    intercept.lim = NULL,
    g.limit = 2,
    p.val = 0.05,
    k.ratio = 1,
    warn.length = 60
  )
}

# Step 3: Analyze results and quality flags
cat("\n=== BEST FLUX RESULTS SUMMARY ===\n")
cat("CO2 Analysis:\n")
cat("Total measurements:", nrow(CO2_best_lgr1), "\n")

# Model selection summary
model_selection <- CO2_best_lgr1 %>%
  count(model) %>%
  mutate(percentage = round(n/sum(n)*100, 1))

print("Model selection:")
print(model_selection)

# Quality flags summary
quality_summary <- CO2_best_lgr1 %>%
  summarise(
    clean_measurements = sum(quality.check == "OK", na.rm = TRUE),
    flagged_measurements = sum(quality.check != "OK", na.rm = TRUE),
    below_MDF = sum(grepl("MDF", quality.check), na.rm = TRUE),
    high_g_factor = sum(grepl("g.factor", quality.check), na.rm = TRUE),
    poor_fit = sum(grepl("MAE|RMSE|SE", quality.check), na.rm = TRUE),
    intercept_issues = sum(grepl("intercept", quality.check), na.rm = TRUE),
    low_significance = sum(grepl("p-value", quality.check), na.rm = TRUE),
    short_measurements = sum(grepl("nb.obs", quality.check), na.rm = TRUE)
  )

cat("\nQuality Assessment:\n")
cat("Clean measurements (OK):", quality_summary$clean_measurements, "\n")
cat("Flagged measurements:", quality_summary$flagged_measurements, "\n")
cat("  - Below detection limit (MDF):", quality_summary$below_MDF, "\n")
cat("  - High g-factor (overestimation):", quality_summary$high_g_factor, "\n")
cat("  - Poor model fit (MAE/RMSE/SE):", quality_summary$poor_fit, "\n")
cat("  - Intercept out of bounds:", quality_summary$intercept_issues, "\n")
cat("  - Low statistical significance:", quality_summary$low_significance, "\n")
cat("  - Too few observations:", quality_summary$short_measurements, "\n")

# Step 4: Best flux statistics
best_flux_stats <- CO2_best_lgr1 %>%
  summarise(
    mean_best_flux = round(mean(best.flux, na.rm = TRUE), 3),
    median_best_flux = round(median(best.flux, na.rm = TRUE), 3),
    sd_best_flux = round(sd(best.flux, na.rm = TRUE), 3),
    min_best_flux = round(min(best.flux, na.rm = TRUE), 3),
    max_best_flux = round(max(best.flux, na.rm = TRUE), 3),
    mean_HM_score = round(mean(HM.score, na.rm = TRUE), 2),
    mean_LM_score = round(mean(LM.score, na.rm = TRUE), 2)
  )

cat("\nBest Flux Statistics (µmol·m⁻²·s⁻¹):\n")
print(best_flux_stats)

# Step 5: Identify problematic measurements
cat("\n=== FLAGGED MEASUREMENTS ===\n")
flagged_measurements <- CO2_best_lgr1 %>%
  filter(quality.check != "OK") %>%
  select(UniqueID, model, best.flux, quality.check, HM.score, LM.score)

if(nrow(flagged_measurements) > 0) {
  cat("Measurements requiring attention:\n")
  print(flagged_measurements)
} else {
  cat("No measurements flagged - all passed quality checks!\n")
}

# Step 6: Save results
write_csv(CO2_best_lgr1, "intermediate/CO2_best_flux_lgr1_results.csv")
cat("\nCO2 best flux results saved to: intermediate/CO2_best_flux_lgr1_results.csv\n")

if(exists("CH4_best_lgr1")) {
  write_csv(CH4_best_lgr1, "intermediate/CH4_best_flux_lgr1_results.csv")
  cat("CH4 best flux results saved to: intermediate/CH4_best_flux_lgr1_results.csv\n")
}

# Step 7: Create summary report
summary_report <- CO2_best_lgr1 %>%
  select(UniqueID, best.flux, model, quality.check, 
         LM.flux, HM.flux, LM.r2, HM.r2, 
         LM.p.val, g.fact, HM.k, nb.obs) %>%
  arrange(desc(abs(best.flux)))

write_csv(summary_report, "intermediate/CO2_summary_report_lgr1.csv")
cat("Summary report saved to: intermediate/CO2_summary_report_lgr1.csv\n")

# Step 8: Display top and bottom fluxes
cat("\n=== TOP 5 HIGHEST FLUXES ===\n")
top_fluxes <- summary_report %>% 
  slice_head(n = 5) %>%
  select(UniqueID, best.flux, model, quality.check)
print(top_fluxes)

cat("\n=== 5 LOWEST FLUXES ===\n")
bottom_fluxes <- summary_report %>% 
  slice_tail(n = 5) %>%
  select(UniqueID, best.flux, model, quality.check)
print(bottom_fluxes)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Key files created:\n")
cat("- intermediate/CO2_best_flux_lgr1_results.csv (complete results)\n")
cat("- intermediate/CO2_summary_report_lgr1.csv (key metrics only)\n")
cat("\nReview flagged measurements and consider:\n")
cat("1. Re-examining manual identification for poor fits\n")
cat("2. Excluding measurements below detection limit\n")
cat("3. Investigating extreme flux values\n")










# Load required libraries
library(goFlux)
library(dplyr)
library(readr)

# Step 1: Create flux plots for CO2
cat("Creating CO2 flux plots...\n")
CO2_plots_lgr1 <- flux.plot(
  flux.results = CO2_best_lgr1,
  dataframe = manID.lgr1,           # Your manual identification data
  gastype = "CO2dry_ppm",
  shoulder = 30,                    # Show 30 seconds before/after measurement
  plot.legend = c("MAE", "AICc", "k.ratio", "g.factor"),  # Display model fit metrics
  plot.display = c("MDF", "prec", "nb.obs", "flux.term"), # Display detection limits & parameters
  quality.check = TRUE,             # Show quality flags
  best.model = TRUE,               # Mark best model with star
  p.val.disp = "round"             # Round p-values (p < 0.001, etc.)
)

cat("CO2 plots created for", length(CO2_plots_lgr1), "measurements\n")

# Step 2: Create flux plots for CH4 (if available)
if(exists("CH4_best_lgr1")) {
  cat("Creating CH4 flux plots...\n")
  CH4_plots_lgr1 <- flux.plot(
    flux.results = CH4_best_lgr1,
    dataframe = manID.lgr1,
    gastype = "CH4dry_ppb",
    shoulder = 30,
    plot.legend = c("MAE", "AICc", "k.ratio", "g.factor"),
    plot.display = c("MDF", "prec", "nb.obs", "flux.term"),
    quality.check = TRUE,
    best.model = TRUE,
    p.val.disp = "round"
  )
  cat("CH4 plots created for", length(CH4_plots_lgr1), "measurements\n")
}

# Step 3: Combine all plots into one list
if(exists("CH4_plots_lgr1")) {
  # Combine CO2 and CH4 plots
  all_plots_lgr1 <- c(CO2_plots_lgr1, CH4_plots_lgr1)
  cat("Combined", length(CO2_plots_lgr1), "CO2 plots and", length(CH4_plots_lgr1), "CH4 plots\n")
} else {
  # Only CO2 plots
  all_plots_lgr1 <- CO2_plots_lgr1
  cat("Using", length(CO2_plots_lgr1), "CO2 plots only\n")
}

# Step 4: Save plots to PDF
cat("Saving plots to PDF...\n")
flux2pdf(
  plot.list = all_plots_lgr1,
  outfile = "intermediate/LGR1_flux_plots_complete.pdf",
  width = 11.6,                    # Standard page width
  height = 8.2                     # Standard page height
)

cat("All plots saved to: intermediate/LGR1_flux_plots_complete.pdf\n")

# Step 5: Create separate PDFs for CO2 and CH4 (optional)
flux2pdf(
  plot.list = CO2_plots_lgr1,
  outfile = "intermediate/LGR1_CO2_flux_plots.pdf",
  width = 11.6,
  height = 8.2
)
cat("CO2 plots saved to: intermediate/LGR1_CO2_flux_plots.pdf\n")

if(exists("CH4_plots_lgr1")) {
  flux2pdf(
    plot.list = CH4_plots_lgr1,
    outfile = "intermediate/LGR1_CH4_flux_plots.pdf",
    width = 11.6,
    height = 8.2
  )
  cat("CH4 plots saved to: intermediate/LGR1_CH4_flux_plots.pdf\n")
}








# Simple code to create final dataset with original data + flux results

library(dplyr)
library(readr)

# Load original tree data
original_data <- read_csv("intermediate/main_trees_complete.csv")

# Filter for LGR1 only
lgr1_original <- original_data %>% filter(analyzer_id == "LGR1")

# Add CO2_ prefix to all flux columns except UniqueID
co2_results <- CO2_best_lgr1 %>%
  rename_with(~ paste0("CO2_", .), -UniqueID)

# Add CH4 results if they exist
if(exists("CH4_best_lgr1")) {
  ch4_results <- CH4_best_lgr1 %>%
    rename_with(~ paste0("CH4_", .), -UniqueID)
}

# Merge everything together
final_dataset <- lgr1_original %>%
  left_join(co2_results, by = c("flux_id" = "UniqueID"))

# Add CH4 if it exists
if(exists("ch4_results")) {
  final_dataset <- final_dataset %>%
    left_join(ch4_results, by = c("flux_id" = "UniqueID"))
}

# Save the final dataset
write_csv(final_dataset, "intermediate/LGR1_final_complete_dataset.csv")

# Print summary
cat("Final dataset created with", nrow(final_dataset), "rows and", ncol(final_dataset), "columns\n")
cat("Saved to: intermediate/LGR1_final_complete_dataset.csv\n")




