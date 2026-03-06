# Complete LGR2 Flux Processing Workflow - Start to Finish
# Load required libraries
library(goFlux)
library(dplyr)
library(readr)

# =============================================================================
# STEP 1: LOAD AND PREPARE LGR2 DATA
# =============================================================================

cat("=== STEP 1: LOADING LGR2 DATA ===\n")

# Set data path
data_path <- "data/analyzer/LGR_GLA131/LGR2"

# Check for zip files that need extraction
zip_files <- list.files(data_path, recursive = TRUE, pattern = "\\.zip$", full.names = TRUE)
cat("Found", length(zip_files), "zip files\n")

# Extract zip files to temporary directory
temp_extract_dir <- tempfile("lgr2_extract_")
dir.create(temp_extract_dir, recursive = TRUE)

cat("Extracting zip files...\n")
for(zip_file in zip_files) {
  tryCatch({
    unzip(zip_file, exdir = temp_extract_dir, overwrite = TRUE)
    cat("Extracted:", basename(zip_file), "\n")
  }, error = function(e) {
    cat("Error with", basename(zip_file), ":", e$message, "\n")
  })
}

# Combine existing and extracted files
existing_data_files <- list.files(data_path, recursive = TRUE, pattern = "\\.txt$", full.names = TRUE)
existing_data_files <- existing_data_files[file.size(existing_data_files) > 0]

new_txt_files <- list.files(temp_extract_dir, recursive = TRUE, pattern = "\\.txt$", full.names = TRUE)

# Create complete data directory
complete_lgr2_dir <- tempfile("lgr2_complete_")
dir.create(complete_lgr2_dir, recursive = TRUE)

# Copy all files
file.copy(existing_data_files, complete_lgr2_dir)
file.copy(new_txt_files, complete_lgr2_dir)

cat("Total files prepared:", length(list.files(complete_lgr2_dir, pattern = "\\.txt$")), "\n")

# Import complete LGR2 dataset
cat("Importing LGR2 data...\n")
lgr2_data_complete <- import2RData(
  path = complete_lgr2_dir,
  instrument = "UGGA",
  date.format = "mdy",
  timezone = "UTC",
  keep_all = FALSE,
  prec = c(0.35, 0.9, 200),
  merge = TRUE
)

# Clean up temporary directories
unlink(temp_extract_dir, recursive = TRUE)
unlink(complete_lgr2_dir, recursive = TRUE)

cat("LGR2 data loaded:", dim(lgr2_data_complete), "\n")

# =============================================================================
# STEP 2: PREPARE AUXFILE AND CREATE OBSERVATION WINDOWS
# =============================================================================

cat("\n=== STEP 2: PREPARING AUXFILE ===\n")

# Check timezone and load auxfile
lgr2_data_tz <- attr(lgr2_data_complete$POSIX.time, "tzone")
if(is.null(lgr2_data_tz)) lgr2_data_tz <- ""
cat("LGR2 data timezone:", ifelse(lgr2_data_tz == "", "UTC (default)", lgr2_data_tz), "\n")

# Load LGR2 auxfile
lgr2_auxfile <- read_csv("intermediate/auxfiles/lgr2_rescue_auxfile.csv") %>%
  mutate(start.time = as.POSIXct(start.time, tz = ifelse(lgr2_data_tz == "", "UTC", lgr2_data_tz)))

cat("Auxfile loaded:", nrow(lgr2_auxfile), "measurements\n")

# Create observation windows
cat("Creating observation windows...\n")
ow.lgr2.complete <- obs.win(
  inputfile = lgr2_data_complete,
  auxfile = lgr2_auxfile,
  gastype = "CO2dry_ppm",
  obs.length = 300,
  shoulder = 300
)

# Check window quality
window_sizes <- sapply(ow.lgr2.complete, nrow)
good_windows <- which(window_sizes >= 60)

# Assign names
unique_ids <- sapply(ow.lgr2.complete, function(x) unique(x$UniqueID)[1])
names(ow.lgr2.complete) <- unique_ids

cat("Total observation windows:", length(ow.lgr2.complete), "\n")
cat("Good windows (>=60 obs):", length(good_windows), "\n")

# =============================================================================
# STEP 3: MANUAL IDENTIFICATION IN BATCHES
# =============================================================================

cat("\n=== STEP 3: MANUAL IDENTIFICATION ===\n")

# Process in batches of 20
batch_size <- 20
total_measurements <- length(good_windows)
num_batches <- ceiling(total_measurements / batch_size)

cat("Processing", total_measurements, "measurements in", num_batches, "batches\n")

manID_batches <- list()

for(batch_num in 1:num_batches) {
  start_idx <- (batch_num - 1) * batch_size + 1
  end_idx <- min(batch_num * batch_size, total_measurements)
  batch_windows <- good_windows[start_idx:end_idx]
  
  cat("\n=== Processing Batch", batch_num, "of", num_batches, "===\n")
  cat("Measurements", start_idx, "to", end_idx, "\n")
  
  # Process this batch
  manID_batch <- click.peak2(
    ow.lgr2.complete,
    seq = batch_windows,
    gastype = "CO2dry_ppm",
    sleep = 3,
    plot.lim = c(200, 1600),
    save.plots = paste0("intermediate/lgr2_batch_", batch_num, "_plots")
  )
  
  # Store the batch result
  manID_batches[[batch_num]] <- manID_batch
  
  # Force close graphics devices
  while(dev.cur() > 1) dev.off()
  
  cat("Batch", batch_num, "complete. Processed", nrow(manID_batch), "measurements\n")
  cat("Press Enter to continue to next batch...\n")
  readline()
}

# Combine all batches
manID.lgr2 <- do.call(rbind, manID_batches)

cat("\nManual identification complete! Total:", nrow(manID.lgr2), "measurements\n")

# Save manual identification results
write_csv(manID.lgr2, "intermediate/lgr2_manual_identification_results.csv")

# =============================================================================
# STEP 4: FLUX CALCULATIONS
# =============================================================================

cat("\n=== STEP 4: FLUX CALCULATIONS ===\n")

# Calculate CO2 fluxes
cat("Calculating CO2 fluxes...\n")
CO2_flux_lgr2 <- goFlux(
  dataframe = manID.lgr2,
  gastype = "CO2dry_ppm",
  H2O_col = "H2O_ppm",
  warn.length = 60
)

# Calculate CH4 fluxes if available
if("CH4dry_ppb" %in% names(manID.lgr2)) {
  cat("Calculating CH4 fluxes...\n")
  CH4_flux_lgr2 <- goFlux(
    dataframe = manID.lgr2,
    gastype = "CH4dry_ppb",
    H2O_col = "H2O_ppm",
    warn.length = 60
  )
} else {
  cat("CH4dry_ppb column not found - skipping CH4 flux calculation\n")
}

# Save flux results
write_csv(CO2_flux_lgr2, "intermediate/CO2_flux_lgr2_rescue.csv")
if(exists("CH4_flux_lgr2")) {
  write_csv(CH4_flux_lgr2, "intermediate/CH4_flux_lgr2_rescue.csv")
}

cat("CO2 flux calculation complete:", nrow(CO2_flux_lgr2), "measurements\n")

# =============================================================================
# STEP 5: BEST FLUX ANALYSIS
# =============================================================================

cat("\n=== STEP 5: BEST FLUX ANALYSIS ===\n")

# Run best.flux on CO2 results
CO2_best_lgr2 <- best.flux(
  flux.result = CO2_flux_lgr2,
  criteria = c("MAE", "RMSE", "AICc", "SE", "g.factor", "kappa", "MDF", "nb.obs", "intercept", "p-value"),
  intercept.lim = NULL,
  g.limit = 2,
  p.val = 0.05,
  k.ratio = 1,
  warn.length = 60
)

# Run best.flux on CH4 results if available
if(exists("CH4_flux_lgr2")) {
  CH4_best_lgr2 <- best.flux(
    flux.result = CH4_flux_lgr2,
    criteria = c("MAE", "RMSE", "AICc", "SE", "g.factor", "kappa", "MDF", "nb.obs", "intercept", "p-value"),
    intercept.lim = NULL,
    g.limit = 2,
    p.val = 0.05,
    k.ratio = 1,
    warn.length = 60
  )
}

# Quality summary
quality_summary <- CO2_best_lgr2 %>%
  summarise(
    clean_measurements = sum(quality.check == "OK", na.rm = TRUE),
    flagged_measurements = sum(quality.check != "OK", na.rm = TRUE),
    below_MDF = sum(grepl("MDF", quality.check), na.rm = TRUE),
    high_g_factor = sum(grepl("g.factor", quality.check), na.rm = TRUE),
    poor_fit = sum(grepl("MAE|RMSE|SE", quality.check), na.rm = TRUE)
  )

cat("Quality Assessment:\n")
cat("Clean measurements:", quality_summary$clean_measurements, "\n")
cat("Flagged measurements:", quality_summary$flagged_measurements, "\n")

# Save best flux results
write_csv(CO2_best_lgr2, "intermediate/CO2_best_flux_lgr2_rescue.csv")
if(exists("CH4_best_lgr2")) {
  write_csv(CH4_best_lgr2, "intermediate/CH4_best_flux_lgr2_rescue.csv")
}

# =============================================================================
# STEP 6: CREATE PLOTS
# =============================================================================

cat("\n=== STEP 6: CREATING PLOTS ===\n")

# Create CO2 flux plots
CO2_plots_lgr2 <- flux.plot(
  flux.results = CO2_best_lgr2,
  dataframe = manID.lgr2,
  gastype = "CO2dry_ppm",
  shoulder = 30,
  plot.legend = c("MAE", "AICc", "k.ratio", "g.factor"),
  plot.display = c("MDF", "prec", "nb.obs", "flux.term"),
  quality.check = TRUE,
  best.model = TRUE,
  p.val.disp = "round"
)

# Create CH4 plots if available
if(exists("CH4_best_lgr2")) {
  CH4_plots_lgr2 <- flux.plot(
    flux.results = CH4_best_lgr2,
    dataframe = manID.lgr2,
    gastype = "CH4dry_ppb",
    shoulder = 30,
    plot.legend = c("MAE", "AICc", "k.ratio", "g.factor"),
    plot.display = c("MDF", "prec", "nb.obs", "flux.term"),
    quality.check = TRUE,
    best.model = TRUE,
    p.val.disp = "round"
  )
  
  # Combine plots
  all_plots_lgr2 <- c(CO2_plots_lgr2, CH4_plots_lgr2)
} else {
  all_plots_lgr2 <- CO2_plots_lgr2
}

# Save plots to PDF
flux2pdf(
  plot.list = all_plots_lgr2,
  outfile = "intermediate/LGR2_flux_plots_rescue.pdf",
  width = 11.6,
  height = 8.2
)

flux2pdf(
  plot.list = CO2_plots_lgr2,
  outfile = "intermediate/LGR2_CO2_flux_plots.pdf",
  width = 11.6,
  height = 8.2
)

if(exists("CH4_plots_lgr2")) {
  flux2pdf(
    plot.list = CH4_plots_lgr2,
    outfile = "intermediate/LGR2_CH4_flux_plots.pdf",
    width = 11.6,
    height = 8.2
  )
}

cat("Plots saved successfully\n")

# =============================================================================
# STEP 7: CREATE FINAL DATASET
# =============================================================================

cat("\n=== STEP 7: CREATING FINAL DATASET ===\n")

# Load original tree data
original_data <- read_csv("output/combined_gas_flux_dataset_with_month_year.csv")

# Filter for LGR2 only
lgr2_original <- original_data %>% filter(analyzer_id == "LGR2")

# Add CO2_ prefix to all flux columns except UniqueID
co2_results <- CO2_best_lgr2 %>%
  rename_with(~ paste0("CO2_", .), -UniqueID)

# Add CH4 results if they exist
if(exists("CH4_best_lgr2")) {
  ch4_results <- CH4_best_lgr2 %>%
    rename_with(~ paste0("CH4_", .), -UniqueID)
}

# Merge everything together
final_dataset <- lgr2_original %>%
  left_join(co2_results, by = c("flux_id" = "UniqueID"))

# Add CH4 if it exists
if(exists("ch4_results")) {
  final_dataset <- final_dataset %>%
    left_join(ch4_results, by = c("flux_id" = "UniqueID"))
}

# Save the final dataset
write_csv(final_dataset, "intermediate/final_complete_dataset_rescue.csv")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=== LGR2 PROCESSING COMPLETE ===\n")
cat("Final dataset created with", nrow(final_dataset), "rows and", ncol(final_dataset), "columns\n")
cat("\nFiles created:\n")
cat("- intermediate/LGR2_final_complete_dataset.csv (complete final dataset)\n")
cat("- intermediate/CO2_best_flux_lgr2_results.csv (CO2 flux results)\n")
if(exists("CH4_best_lgr2")) {
  cat("- intermediate/CH4_best_flux_lgr2_results.csv (CH4 flux results)\n")
}
cat("- intermediate/LGR2_flux_plots_complete.pdf (all plots)\n")
cat("- Multiple batch plot files for manual review\n")

cat("\nSummary statistics:\n")
cat("Total measurements processed:", nrow(final_dataset), "\n")
cat("Clean measurements:", quality_summary$clean_measurements, "\n")
cat("Flagged measurements:", quality_summary$flagged_measurements, "\n")

cat("\nLGR2 flux processing workflow complete!\n")