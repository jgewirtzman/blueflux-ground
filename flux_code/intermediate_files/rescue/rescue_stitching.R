# COMPILE ALL RESCUE FLUX RESULTS WITH BEST.FLUX ANALYSIS
# Run best.flux on all rescue results and combine them

library(goFlux)
library(dplyr)
library(readr)

cat("=== COMPILING ALL RESCUE FLUX RESULTS ===\n")

# =============================================================================
# STEP 1: DEFINE ALL RESCUE FILES
# =============================================================================

rescue_files <- list(
  # CO2 files
  CO2_picarro = "rescue_CO2_picarro_results.csv",
  CO2_lgr1 = "CO2_flux_lgr1_results_rescue.csv", 
  CO2_lgr2 = "CO2_flux_lgr2_rescue.csv",
  CO2_lgr3 = "CO2_flux_lgr3_results_rescue.csv",
  
  # CH4 files  
  CH4_picarro = "rescue_CH4_picarro_results.csv",
  CH4_lgr1 = "CH4_flux_lgr1_results_rescue.csv",
  CH4_lgr2 = "CH4_flux_lgr2_rescue.csv", 
  CH4_lgr3 = "CH4_flux_lgr3_results_rescue.csv"
)

# Check which files exist
existing_files <- rescue_files[file.exists(paste0("flux_code/", rescue_files))]
cat("Found", length(existing_files), "rescue files:\n")
for(i in 1:length(existing_files)) {
  cat("-", names(existing_files)[i], ":", existing_files[[i]], "\n")
}

# =============================================================================
# STEP 2: FUNCTION TO RUN BEST.FLUX IF NEEDED
# =============================================================================

process_rescue_file <- function(file_path, gas_type) {
  
  cat("\nProcessing:", file_path, "\n")
  
  # Read the file
  flux_data <- read_csv(paste0("flux_code/", file_path), show_col_types = FALSE)
  cat("Loaded", nrow(flux_data), "measurements\n")
  
  # Check if this is already best.flux output (has best.flux column)
  if("best.flux" %in% names(flux_data)) {
    cat("File already contains best.flux results - using as is\n")
    return(flux_data)
  }
  
  # If not, run best.flux
  cat("Running best.flux analysis...\n")
  best_flux_result <- best.flux(
    flux.result = flux_data,
    criteria = c("MAE", "RMSE", "AICc", "SE", "g.factor", "kappa", "MDF", "nb.obs"),
    intercept.lim = NULL,
    g.limit = 2,
    p.val = 0.05,
    k.ratio = 1,
    warn.length = 60
  )
  
  cat("Best flux analysis complete:", nrow(best_flux_result), "measurements\n")
  return(best_flux_result)
}

# =============================================================================
# STEP 3: PROCESS ALL FILES
# =============================================================================

all_best_results <- list()

for(i in 1:length(existing_files)) {
  file_name <- names(existing_files)[i]
  file_path <- existing_files[[i]]
  
  # Determine gas type from filename
  gas_type <- ifelse(grepl("CO2", file_name), "CO2", "CH4")
  
  # Process the file
  best_result <- process_rescue_file(file_path, gas_type)
  
  # Add analyzer info to the results
  analyzer <- case_when(
    grepl("picarro", file_name) ~ "Picarro",
    grepl("lgr1", file_name) ~ "LGR1", 
    grepl("lgr2", file_name) ~ "LGR2",
    grepl("lgr3", file_name) ~ "LGR3",
    TRUE ~ "Unknown"
  )
  
  best_result$rescued_analyzer <- analyzer
  best_result$rescued_gas <- gas_type
  
  # Store result
  all_best_results[[file_name]] <- best_result
  
  # Save individual best.flux file
  output_name <- paste0("flux_code/BEST_", file_path)
  write_csv(best_result, output_name)
  cat("Saved:", output_name, "\n")
}

# =============================================================================
# STEP 4: COMBINE ALL CO2 AND CH4 RESULTS
# =============================================================================

cat("\n=== COMBINING RESULTS ===\n")

# Separate CO2 and CH4 results
co2_results <- all_best_results[grepl("CO2", names(all_best_results))]
ch4_results <- all_best_results[grepl("CH4", names(all_best_results))]

# Combine CO2 results
if(length(co2_results) > 0) {
  combined_co2 <- bind_rows(co2_results)
  write_csv(combined_co2, "flux_code/ALL_RESCUED_CO2_BEST_FLUX.csv")
  cat("Combined CO2 results:", nrow(combined_co2), "measurements\n")
  cat("Saved: flux_code/ALL_RESCUED_CO2_BEST_FLUX.csv\n")
}

# Combine CH4 results
if(length(ch4_results) > 0) {
  combined_ch4 <- bind_rows(ch4_results)
  write_csv(combined_ch4, "flux_code/ALL_RESCUED_CH4_BEST_FLUX.csv")
  cat("Combined CH4 results:", nrow(combined_ch4), "measurements\n")
  cat("Saved: flux_code/ALL_RESCUED_CH4_BEST_FLUX.csv\n")
}

# =============================================================================
# STEP 5: CREATE SUMMARY REPORT
# =============================================================================

cat("\n=== RESCUE SUMMARY REPORT ===\n")

total_rescued <- 0
if(exists("combined_co2")) total_rescued <- total_rescued + nrow(combined_co2)
if(exists("combined_ch4")) total_rescued <- total_rescued + nrow(combined_ch4)

cat("Total rescued flux calculations:", total_rescued, "\n")

# Summary by analyzer
if(exists("combined_co2") && exists("combined_ch4")) {
  all_rescued <- bind_rows(
    combined_co2 %>% select(UniqueID, rescued_analyzer, rescued_gas, best.flux, quality.check),
    combined_ch4 %>% select(UniqueID, rescued_analyzer, rescued_gas, best.flux, quality.check)
  )
  
  analyzer_summary <- all_rescued %>%
    group_by(rescued_analyzer, rescued_gas) %>%
    summarise(
      measurements = n(),
      clean = sum(quality.check == "clean", na.rm = TRUE),
      flagged = sum(quality.check != "clean", na.rm = TRUE),
      .groups = 'drop'
    )
  
  cat("\nRescue summary by analyzer:\n")
  print(analyzer_summary)
  
  # Overall quality summary
  quality_summary <- all_rescued %>%
    summarise(
      total_measurements = n(),
      clean_measurements = sum(quality.check == "clean", na.rm = TRUE),
      flagged_measurements = sum(quality.check != "clean", na.rm = TRUE),
      success_rate = round(sum(quality.check == "clean", na.rm = TRUE) / n() * 100, 1)
    )
  
  cat("\nOverall quality summary:\n")
  print(quality_summary)
  
  write_csv(all_rescued, "flux_code/ALL_RESCUED_SUMMARY.csv")
  cat("\nSaved complete summary: flux_code/ALL_RESCUED_SUMMARY.csv\n")
}

cat("\n=== FILES CREATED ===\n")
cat("Individual best.flux files: flux_code/BEST_*.csv\n")
cat("Combined results: flux_code/ALL_RESCUED_*_BEST_FLUX.csv\n") 
cat("Complete summary: flux_code/ALL_RESCUED_SUMMARY.csv\n")

cat("\n=== RESCUE OPERATION COMPLETE ===\n")
