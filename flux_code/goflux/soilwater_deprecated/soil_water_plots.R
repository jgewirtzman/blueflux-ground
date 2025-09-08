# STEP 5: CREATE PLOTS AND FINAL DATASETS FOR SOIL/WATER DATA
library(goFlux)
library(dplyr)
library(readr)

# Turn off graphics for faster processing
options(device = "pdf")
pdf(NULL)

# Set which analyzer to process
ANALYZER_TO_PROCESS <- "LGR3"  # Change to: LGR1, LGR2, LGR3, or Picarro

# CLEAR ENVIRONMENT OF OLD VARIABLES TO PREVENT PERSISTENCE
rm(list = ls(pattern = "^(CO2_|CH4_|manID_data|CO2_best|CH4_best|CO2_plots|CH4_plots)"))

cat("=== PROCESSING ANALYZER:", ANALYZER_TO_PROCESS, "===\n")

# Set paths
results_dir <- "flux_code/soilwater_goflux_results"
analyzer_lower <- tolower(ANALYZER_TO_PROCESS)

# Load manual identification results (contains the concentration data for plotting)
manid_file <- file.path(results_dir, paste0(analyzer_lower, "_soilwater_manual_identification.csv"))
# Load manual identification results (contains the concentration data for plotting)
#manid_file <- file.path(results_dir, paste0(analyzer_lower, "_soilwater_manual_identification_FINAL.csv"))

if (!file.exists(manid_file)) {
  stop("Manual identification file not found: ", manid_file)
}

cat("Loading manual identification data from:", manid_file, "\n")
manID_data <- read_csv(manid_file, show_col_types = FALSE)
cat("Manual ID data loaded:", nrow(manID_data), "rows\n")

# Check that Etime column exists (should already be there from goFlux workflow)
if (!"Etime" %in% names(manID_data)) {
  stop("Etime column missing from manual identification data")
}

cat("Using existing Etime column. Range:", min(manID_data$Etime), "to", max(manID_data$Etime), "seconds\n")

# Load flux calculation results with better error checking
co2_results_file <- file.path(results_dir, paste0(analyzer_lower, "_soilwater_CO2_flux_results.csv"))
ch4_results_file <- file.path(results_dir, paste0(analyzer_lower, "_soilwater_CH4_flux_results.csv"))

cat("Checking for CO2 results file:", co2_results_file, "\n")
co2_exists <- file.exists(co2_results_file)
cat("CO2 file exists:", co2_exists, "\n")

cat("Checking for CH4 results file:", ch4_results_file, "\n")
ch4_exists <- file.exists(ch4_results_file)
cat("CH4 file exists:", ch4_exists, "\n")

# Initialize variables to NULL to prevent using old data
CO2_best <- NULL
CH4_best <- NULL
CO2_plots <- NULL
CH4_plots <- NULL

if (co2_exists) {
  cat("Loading CO2 flux results...\n")
  CO2_best <- read_csv(co2_results_file, show_col_types = FALSE)
  cat("CO2 results loaded:", nrow(CO2_best), "measurements\n")
} else {
  cat("No CO2 results file found for", ANALYZER_TO_PROCESS, "\n")
}

if (ch4_exists) {
  cat("Loading CH4 flux results...\n")
  CH4_best <- read_csv(ch4_results_file, show_col_types = FALSE)
  cat("CH4 results loaded:", nrow(CH4_best), "measurements\n")
} else {
  cat("No CH4 results file found for", ANALYZER_TO_PROCESS, "\n")
}

# CREATE FLUX PLOTS
cat("\n=== CREATING FLUX PLOTS ===\n")

if (co2_exists && !is.null(CO2_best)) {
  cat("Creating CO2 flux plots...\n")
  CO2_plots <- flux.plot(
    flux.results = CO2_best,
    dataframe = manID_data,
    gastype = "CO2dry_ppm",
    shoulder = 300,
    plot.legend = c("MAE", "AICc", "k.ratio", "g.factor"),
    plot.display = c("MDF", "prec", "nb.obs", "flux.term"),
    quality.check = TRUE,
    best.model = TRUE,
    p.val.disp = "round"
  )
  cat("CO2 plots created:", length(CO2_plots), "plots\n")
}

if (ch4_exists && !is.null(CH4_best)) {
  cat("Creating CH4 flux plots...\n")
  CH4_plots <- flux.plot(
    flux.results = CH4_best,
    dataframe = manID_data,
    gastype = "CH4dry_ppb",
    shoulder = 300,
    plot.legend = c("MAE", "AICc", "k.ratio", "g.factor"),
    plot.display = c("MDF", "prec", "nb.obs", "flux.term"),
    quality.check = TRUE,
    best.model = TRUE,
    p.val.disp = "round"
  )
  cat("CH4 plots created:", length(CH4_plots), "plots\n")
}

# SAVE PLOTS TO PDF
cat("\n=== SAVING PLOTS TO PDF ===\n")
all_plots <- list()

if (exists("CO2_plots") && !is.null(CO2_plots)) {
  all_plots <- c(all_plots, CO2_plots)
  cat("Added", length(CO2_plots), "CO2 plots to collection\n")
}

if (exists("CH4_plots") && !is.null(CH4_plots)) {
  all_plots <- c(all_plots, CH4_plots)
  cat("Added", length(CH4_plots), "CH4 plots to collection\n")
}

if (length(all_plots) > 0) {
  combined_pdf <- paste0("flux_code/", ANALYZER_TO_PROCESS, "_soilwater_flux_plots_complete.pdf")
  cat("Saving", length(all_plots), "plots to:", combined_pdf, "\n")
  
  flux2pdf(
    plot.list = all_plots,
    outfile = combined_pdf,
    width = 11.6,
    height = 8.2
  )
  cat("PDF saved successfully!\n")
} else {
  cat("No plots to save for", ANALYZER_TO_PROCESS, "\n")
}

# CREATE FINAL DATASET
cat("\n=== CREATING FINAL DATASET ===\n")

original_data_file <- "flux_code/intermediate_files/main_soilwater_complete.csv"
if (!file.exists(original_data_file)) {
  stop("Original data file not found: ", original_data_file)
}

cat("Loading original data from:", original_data_file, "\n")
original_data <- read_csv(original_data_file, show_col_types = FALSE)

# Filter for the specific analyzer
analyzer_original <- original_data %>% 
  filter(analyzer_id == ANALYZER_TO_PROCESS)

cat("Filtered data for", ANALYZER_TO_PROCESS, ":", nrow(analyzer_original), "rows\n")

if (nrow(analyzer_original) == 0) {
  stop("No data found for analyzer: ", ANALYZER_TO_PROCESS)
}

# Start with the original analyzer data
flux_results_combined <- analyzer_original

# Join CO2 results if available
if (co2_exists && !is.null(CO2_best)) {
  cat("Joining CO2 flux results...\n")
  co2_results <- CO2_best %>%
    rename_with(~ paste0("CO2_", .), -UniqueID)
  
  flux_results_combined <- flux_results_combined %>%
    left_join(co2_results, by = c("flux_id" = "UniqueID"))
  
  cat("CO2 results joined successfully\n")
}

# Join CH4 results if available
if (ch4_exists && !is.null(CH4_best)) {
  cat("Joining CH4 flux results...\n")
  ch4_results <- CH4_best %>%
    rename_with(~ paste0("CH4_", .), -UniqueID)
  
  flux_results_combined <- flux_results_combined %>%
    left_join(ch4_results, by = c("flux_id" = "UniqueID"))
  
  cat("CH4 results joined successfully\n")
}

# Save final dataset
final_dataset_file <- paste0("flux_code/", ANALYZER_TO_PROCESS, "_soilwater_final_complete_dataset.csv")
cat("Saving final dataset to:", final_dataset_file, "\n")
write_csv(flux_results_combined, final_dataset_file)

cat("Final dataset saved with", nrow(flux_results_combined), "rows and", ncol(flux_results_combined), "columns\n")

# SUMMARY
cat("\n=== PROCESSING SUMMARY FOR", ANALYZER_TO_PROCESS, "===\n")
cat("✓ Manual identification data:", nrow(manID_data), "measurements\n")
if (co2_exists) cat("✓ CO2 flux results:", nrow(CO2_best), "measurements\n")
if (ch4_exists) cat("✓ CH4 flux results:", nrow(CH4_best), "measurements\n")
cat("✓ Plots created:", length(all_plots), "total plots\n")
cat("✓ Final dataset:", nrow(flux_results_combined), "rows,", ncol(flux_results_combined), "columns\n")
cat("✓ Files saved for analyzer:", ANALYZER_TO_PROCESS, "\n")

# Turn graphics back on
dev.off()
cat("Graphics device reset\n")

# Clear variables to prevent carryover to next run
rm(CO2_plots, CH4_plots, all_plots, CO2_best, CH4_best, manID_data)
cat("Variables cleared to prevent carryover\n")