# STEP 4C: FLUX CALCULATIONS FOR SOIL/WATER DATA
# This calculates fluxes from manual identification results and runs best.flux analysis

library(goFlux)
library(dplyr)
library(readr)

cat("==================================================================\n")
cat("    STEP 4C: FLUX CALCULATIONS - SOIL/WATER\n")
cat("==================================================================\n\n")

# =============================================================================
# CONFIGURATION - SET ANALYZER TO PROCESS
# =============================================================================

# Set the analyzer you want to process
ANALYZER_TO_PROCESS <- "LGR3"  # Change to: LGR1, LGR2, LGR3, or Picarro

cat("Processing flux calculations for", ANALYZER_TO_PROCESS, "\n")

# =============================================================================
# LOAD MANUAL IDENTIFICATION RESULTS
# =============================================================================

cat("\n=== LOADING MANUAL IDENTIFICATION RESULTS ===\n")

# Set paths
results_dir <- "flux_code/soilwater_goflux_results"
analyzer_lower <- tolower(ANALYZER_TO_PROCESS)
manid_file <- file.path(results_dir, paste0(analyzer_lower, "_soilwater_manual_identification.csv"))
#manid_file <- file.path(results_dir, paste0(analyzer_lower, "_soilwater_manual_identification_FINAL.csv"))

# Check if manual identification file exists
if (!file.exists(manid_file)) {
  stop("Manual identification file not found: ", manid_file, "\nPlease complete Step 4B first.")
}

# Load manual identification results
manID_data <- read_csv(manid_file, show_col_types = FALSE)

cat("Manual identification data loaded:", nrow(manID_data), "rows\n")
cat("Unique measurements:", length(unique(manID_data$UniqueID)), "\n")

# Check available gas columns
gas_columns <- names(manID_data)[grepl("CO2|CH4|H2O", names(manID_data))]
cat("Available gas columns:", paste(gas_columns, collapse = ", "), "\n")

# Check for required columns
has_co2 <- "CO2dry_ppm" %in% names(manID_data)
has_ch4 <- "CH4dry_ppb" %in% names(manID_data)
has_h2o <- "H2O_ppm" %in% names(manID_data)

cat("CO2 data available:", has_co2, "\n")
cat("CH4 data available:", has_ch4, "\n")
cat("H2O data available:", has_h2o, "\n")

if (!has_co2 && !has_ch4) {
  stop("No CO2 or CH4 data found in manual identification results")
}

# =============================================================================
# CO2 FLUX CALCULATIONS
# =============================================================================

if (has_co2) {
  cat("\n=== CALCULATING CO2 FLUXES ===\n")
  
  # Calculate CO2 fluxes using goFlux
  CO2_flux_results <- tryCatch({
    goFlux(
      dataframe = manID_data,
      gastype = "CO2dry_ppm",
      H2O_col = if(has_h2o) "H2O_ppm" else NULL,
      warn.length = 60  # Minimum number of observations for warning
    )
  }, error = function(e) {
    cat("Error calculating CO2 fluxes:", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(CO2_flux_results)) {
    cat("CO2 flux calculation complete:", nrow(CO2_flux_results), "measurements\n")
    
    # Quick summary of CO2 flux results
    co2_summary <- CO2_flux_results %>%
      summarise(
        total_measurements = n(),
        lm_successful = sum(!is.na(LM.flux)),
        hm_successful = sum(!is.na(HM.flux)),
        mean_lm_flux = round(mean(LM.flux, na.rm = TRUE), 3),
        mean_hm_flux = round(mean(HM.flux, na.rm = TRUE), 3),
        mean_lm_r2 = round(mean(LM.r2, na.rm = TRUE), 3),
        mean_hm_r2 = round(mean(HM.r2, na.rm = TRUE), 3)
      )
    
    cat("CO2 flux summary:\n")
    print(co2_summary)
    
    # Save raw flux results
    co2_flux_file <- file.path(results_dir, paste0(analyzer_lower, "_soilwater_CO2_raw_flux.csv"))
    write_csv(CO2_flux_results, co2_flux_file)
    cat("Raw CO2 flux results saved to:", co2_flux_file, "\n")
    
  } else {
    cat("CO2 flux calculation failed\n")
  }
}

# =============================================================================
# CH4 FLUX CALCULATIONS
# =============================================================================

if (has_ch4) {
  cat("\n=== CALCULATING CH4 FLUXES ===\n")
  
  # Calculate CH4 fluxes using goFlux
  CH4_flux_results <- tryCatch({
    goFlux(
      dataframe = manID_data,
      gastype = "CH4dry_ppb",
      H2O_col = if(has_h2o) "H2O_ppm" else NULL,
      warn.length = 60  # Minimum number of observations for warning
    )
  }, error = function(e) {
    cat("Error calculating CH4 fluxes:", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(CH4_flux_results)) {
    cat("CH4 flux calculation complete:", nrow(CH4_flux_results), "measurements\n")
    
    # Quick summary of CH4 flux results
    ch4_summary <- CH4_flux_results %>%
      summarise(
        total_measurements = n(),
        lm_successful = sum(!is.na(LM.flux)),
        hm_successful = sum(!is.na(HM.flux)),
        mean_lm_flux = round(mean(LM.flux, na.rm = TRUE), 3),
        mean_hm_flux = round(mean(HM.flux, na.rm = TRUE), 3),
        mean_lm_r2 = round(mean(LM.r2, na.rm = TRUE), 3),
        mean_hm_r2 = round(mean(HM.r2, na.rm = TRUE), 3)
      )
    
    cat("CH4 flux summary:\n")
    print(ch4_summary)
    
    # Save raw flux results
    ch4_flux_file <- file.path(results_dir, paste0(analyzer_lower, "_soilwater_CH4_raw_flux.csv"))
    write_csv(CH4_flux_results, ch4_flux_file)
    cat("Raw CH4 flux results saved to:", ch4_flux_file, "\n")
    
  } else {
    cat("CH4 flux calculation failed\n")
  }
}

# =============================================================================
# BEST FLUX ANALYSIS - CO2
# =============================================================================

if (exists("CO2_flux_results") && !is.null(CO2_flux_results)) {
  cat("\n=== BEST FLUX ANALYSIS - CO2 ===\n")
  
  # Run best.flux analysis on CO2 results
  CO2_best <- tryCatch({
    best.flux(
      flux.result = CO2_flux_results,
      criteria = c("MAE", "RMSE", "AICc", "SE", "g.factor", "kappa", "MDF", "nb.obs", "intercept", "p-value"),
      intercept.lim = NULL,
      g.limit = 2,        # g-factor limit for quality control
      p.val = 0.05,       # p-value threshold
      k.ratio = 1,        # kappa ratio for robust regression
      warn.length = 60    # minimum observations for warning
    )
  }, error = function(e) {
    cat("Error in CO2 best.flux analysis:", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(CO2_best)) {
    cat("CO2 best flux analysis complete:", nrow(CO2_best), "measurements\n")
    
    # Summary of best flux results
    co2_best_summary <- CO2_best %>%
      summarise(
        total_measurements = n(),
        successful_fluxes = sum(!is.na(best.flux)),
        success_rate = round(successful_fluxes / total_measurements * 100, 1),
        mean_flux = round(mean(best.flux, na.rm = TRUE), 3),
        median_flux = round(median(best.flux, na.rm = TRUE), 3),
        quality_ok = sum(quality.check == "OK", na.rm = TRUE),
        quality_rate = round(quality_ok / total_measurements * 100, 1)
      )
    
    cat("CO2 best flux summary:\n")
    cat("- Success rate:", co2_best_summary$success_rate, "%\n")
    cat("- Quality check pass rate:", co2_best_summary$quality_rate, "%\n")
    cat("- Mean flux:", co2_best_summary$mean_flux, "μmol/m²/s\n")
    cat("- Median flux:", co2_best_summary$median_flux, "μmol/m²/s\n")
    
    # Check for flagged measurements
    flagged_co2 <- CO2_best %>%
      filter(quality.check != "OK") %>%
      select(UniqueID, model, best.flux, quality.check, HM.score, LM.score)
    
    if (nrow(flagged_co2) > 0) {
      cat("Flagged CO2 measurements requiring attention:", nrow(flagged_co2), "\n")
      cat("First few flagged measurements:\n")
      print(head(flagged_co2, 5))
    } else {
      cat("All CO2 measurements passed quality checks!\n")
    }
    
    # Save CO2 best flux results
    co2_best_file <- file.path(results_dir, paste0(analyzer_lower, "_soilwater_CO2_flux_results.csv"))
    write_csv(CO2_best, co2_best_file)
    cat("CO2 best flux results saved to:", co2_best_file, "\n")
    
  } else {
    cat("CO2 best flux analysis failed\n")
  }
}

# =============================================================================
# BEST FLUX ANALYSIS - CH4
# =============================================================================

if (exists("CH4_flux_results") && !is.null(CH4_flux_results)) {
  cat("\n=== BEST FLUX ANALYSIS - CH4 ===\n")
  
  # Run best.flux analysis on CH4 results
  CH4_best <- tryCatch({
    best.flux(
      flux.result = CH4_flux_results,
      criteria = c("MAE", "RMSE", "AICc", "SE", "g.factor", "kappa", "MDF", "nb.obs", "intercept", "p-value"),
      intercept.lim = NULL,
      g.limit = 2,        # g-factor limit for quality control
      p.val = 0.05,       # p-value threshold
      k.ratio = 1,        # kappa ratio for robust regression
      warn.length = 60    # minimum observations for warning
    )
  }, error = function(e) {
    cat("Error in CH4 best.flux analysis:", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(CH4_best)) {
    cat("CH4 best flux analysis complete:", nrow(CH4_best), "measurements\n")
    
    # Summary of best flux results
    ch4_best_summary <- CH4_best %>%
      summarise(
        total_measurements = n(),
        successful_fluxes = sum(!is.na(best.flux)),
        success_rate = round(successful_fluxes / total_measurements * 100, 1),
        mean_flux = round(mean(best.flux, na.rm = TRUE), 3),
        median_flux = round(median(best.flux, na.rm = TRUE), 3),
        quality_ok = sum(quality.check == "OK", na.rm = TRUE),
        quality_rate = round(quality_ok / total_measurements * 100, 1)
      )
    
    cat("CH4 best flux summary:\n")
    cat("- Success rate:", ch4_best_summary$success_rate, "%\n")
    cat("- Quality check pass rate:", ch4_best_summary$quality_rate, "%\n")
    cat("- Mean flux:", ch4_best_summary$mean_flux, "nmol/m²/s\n")
    cat("- Median flux:", ch4_best_summary$median_flux, "nmol/m²/s\n")
    
    # Check for flagged measurements
    flagged_ch4 <- CH4_best %>%
      filter(quality.check != "OK") %>%
      select(UniqueID, model, best.flux, quality.check, HM.score, LM.score)
    
    if (nrow(flagged_ch4) > 0) {
      cat("Flagged CH4 measurements requiring attention:", nrow(flagged_ch4), "\n")
      cat("First few flagged measurements:\n")
      print(head(flagged_ch4, 5))
    } else {
      cat("All CH4 measurements passed quality checks!\n")
    }
    
    # Save CH4 best flux results
    ch4_best_file <- file.path(results_dir, paste0(analyzer_lower, "_soilwater_CH4_flux_results.csv"))
    write_csv(CH4_best, ch4_best_file)
    cat("CH4 best flux results saved to:", ch4_best_file, "\n")
    
  } else {
    cat("CH4 best flux analysis failed\n")
  }
}

# =============================================================================
# COMPLETION SUMMARY
# =============================================================================

cat("\n=== FLUX CALCULATION COMPLETE FOR", ANALYZER_TO_PROCESS, "===\n")

# Check which analyzers have been processed
completed_files <- list.files(results_dir, pattern = "*_flux_results.csv$")
completed_analyzers <- unique(gsub("(lgr[123]|picarro)_soilwater_(CO2|CH4)_flux_results.csv", "\\1", completed_files))
completed_analyzers <- toupper(completed_analyzers)

available_analyzers <- c("LGR1", "LGR2", "LGR3", "Picarro")
remaining_analyzers <- setdiff(available_analyzers, completed_analyzers)

cat("Progress for soil/water flux calculations:\n")
cat("Completed:", paste(completed_analyzers, collapse = ", "), "\n")
if (length(remaining_analyzers) > 0) {
  cat("Remaining:", paste(remaining_analyzers, collapse = ", "), "\n")
  cat("Next: Change ANALYZER_TO_PROCESS to", paste0('"', remaining_analyzers[1], '"'), "and run again\n")
} else {
  cat("All analyzers complete!\n")
}

cat("\nFiles created for", ANALYZER_TO_PROCESS, ":\n")
results_files <- list.files(results_dir, pattern = paste0("^", analyzer_lower, "_soilwater_.*flux.*\\.csv$"), full.names = FALSE)
for (file in results_files) {
  cat("✓", file, "\n")
}

if (length(completed_analyzers) == length(available_analyzers)) {
  cat("\nAll flux calculations complete! Ready for Step 5 (plotting and final datasets)\n")
} else {
  cat("\nNext: Complete flux calculations for remaining analyzers\n")
}

cat("==================================================================\n")
