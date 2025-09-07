# =============================================================================
# STEP 6: COMPLETE PICARRO PROCESSING WORKFLOW (MANUAL) - CH4 WITH CORRECTED TIMESTAMPS
# This continues from your existing code and adds all Picarro-specific processing
# =============================================================================

cat("\n=== STEP 6: STARTING PICARRO PROCESSING ===\n")

# Check if we have Picarro data
if ("Picarro" %in% names(analyzer_data) && !is.null(analyzer_data[["Picarro"]])) {
  
  cat("Processing Picarro data with", nrow(analyzer_data[["Picarro"]]), "rows\n")
  
  # Load the Picarro auxfile
  picarro_auxfile_path <- auxfiles[["Picarro"]]$path
  picarro_auxfile <- read_csv(picarro_auxfile_path)
  
  # Convert start.time to POSIXct for Picarro auxfile and apply timestamp correction
  picarro_auxfile_corrected <- picarro_auxfile %>%
    mutate(start.time = as.POSIXct(start.time, tz = "UTC") + 25220)
  
  # Apply timestamp correction to Picarro data DATE_TIME column
  if("DATE_TIME" %in% names(analyzer_data[["Picarro"]])) {
    picarro_data_corrected <- analyzer_data[["Picarro"]] %>%
      mutate(DATE_TIME = as.POSIXct(DATE_TIME, format = "%m-%d-%Y %H:%M:%OS", tz = "UTC") - 25220)
  } else {
    picarro_data_corrected <- analyzer_data[["Picarro"]]
  }
  
  cat("Picarro auxfile loaded:", nrow(picarro_auxfile_corrected), "measurements\n")
  cat("Date range (after timestamp correction):", min(as.Date(picarro_auxfile_corrected$start.time)), "to", max(as.Date(picarro_auxfile_corrected$start.time)), "\n")
  cat("Timestamp correction applied: -25220 seconds\n")
  
  # =============================================================================
  # STEP 6A: CREATE OBSERVATION WINDOWS FOR PICARRO
  # =============================================================================
  
  cat("\n=== STEP 6A: CREATING OBSERVATION WINDOWS FOR PICARRO ===\n")
  
  # Create observation windows for manual identification
  ow.picarro <- obs.win(
    inputfile = picarro_data_corrected,
    auxfile = picarro_auxfile_corrected,
    gastype = "CH4dry_ppb",      # Primary gas for tree flux measurements
    obs.length = 180,            # 3 minutes measurement time
    shoulder = 420               # 7 minutes buffer (total 10 min window)
  )
  
  # Check observation windows
  window_sizes <- sapply(ow.picarro, nrow)
  good_windows <- which(window_sizes >= 30)  # At least 30 observations
  
  # Assign names to observation windows
  unique_ids <- sapply(ow.picarro, function(x) unique(x$UniqueID)[1])
  names(ow.picarro) <- unique_ids
  
  cat("Total observation windows:", length(ow.picarro), "\n")
  cat("Good windows (>=30 obs):", length(good_windows), "\n")
  cat("Window sizes (first 5):", window_sizes[1:5], "\n")
  
  if (length(good_windows) == 0) {
    cat("ERROR: No good observation windows found for Picarro data\n")
    cat("Check your auxfile start times and data alignment\n")
    cat("Note: Timestamp correction of -25220 seconds has been applied\n")
    
    # Debug information
    cat("\nDEBUG INFO:\n")
    cat("Auxfile time range:", format(range(picarro_auxfile_corrected$start.time)), "\n")
    cat("Data time range:", format(range(picarro_data_corrected$DATE_TIME, na.rm = TRUE)), "\n")
    
  } else {
    
    # =============================================================================
    # STEP 6B: MANUAL IDENTIFICATION FOR PICARRO (IN BATCHES)
    # =============================================================================
    
    cat("\n=== STEP 6B: MANUAL IDENTIFICATION FOR PICARRO ===\n")
    
    # Set up graphics device for manual clicking
    default.device <- getOption("device")
    
    # Configure graphics device based on operating system
    if (Sys.info()["sysname"] == "Darwin") {
      options(device = function() quartz(width = 12, height = 8))
    } else if (Sys.info()["sysname"] == "Windows") {
      options(device = function() windows(width = 12, height = 8))
    } else {
      options(device = function() X11(width = 12, height = 8))
    }
    
    # Process in batches to avoid overwhelming the user
    batch_size <- 15  # Smaller batches for better management
    total_measurements <- length(good_windows)
    num_batches <- ceiling(total_measurements / batch_size)
    
    cat("Processing", total_measurements, "Picarro measurements in", num_batches, "batches\n")
    cat("You will need to click on the start and end points of each CH4 measurement\n")
    cat("Note: All timestamps have been corrected by -25220 seconds\n")
    cat("Press Enter when ready to begin...\n")
    readline()
    
    manID_picarro_batches <- list()
    
    for(batch_num in 1:num_batches) {
      start_idx <- (batch_num - 1) * batch_size + 1
      end_idx <- min(batch_num * batch_size, total_measurements)
      batch_windows <- good_windows[start_idx:end_idx]
      
      cat("\n=== Processing Picarro Batch", batch_num, "of", num_batches, "===\n")
      cat("Measurements", start_idx, "to", end_idx, "\n")
      cat("UniqueIDs in this batch:\n")
      batch_ids <- unique_ids[batch_windows]
      cat(paste(batch_ids, collapse = ", "), "\n")
      
      tryCatch({
        manID_batch <- click.peak2(
          ow.picarro,
          seq = batch_windows,
          gastype = "CH4dry_ppb",
          sleep = 3,                    # 3 second delay between plots
          plot.lim = c(1500, 10000),    # CH4 range for Picarro
          save.plots = paste0("flux_code/picarro_batch_", batch_num, "_plots"),
          warn.length = 60
        )
        
        manID_picarro_batches[[batch_num]] <- manID_batch
        cat("Batch", batch_num, "complete. Processed", nrow(manID_batch), "measurements\n")
        
      }, error = function(e) {
        cat("Error in Picarro batch", batch_num, ":", e$message, "\n")
        cat("Skipping problematic measurements and continuing...\n")
        manID_picarro_batches[[batch_num]] <- NULL
      })
      
      # Clean up graphics devices
      while(dev.cur() > 1) dev.off()
      
      if(batch_num < num_batches) {
        cat("Batch", batch_num, "complete. Press Enter to continue to next batch...\n")
        readline()
      }
    }
    
    # Revert graphics device
    options(device = default.device)
    
    # Combine all successful batches
    successful_picarro_batches <- manID_picarro_batches[!sapply(manID_picarro_batches, is.null)]
    
    if(length(successful_picarro_batches) > 0) {
      manID.picarro <- do.call(rbind, successful_picarro_batches)
      
      cat("\nPicarro manual identification complete!\n")
      cat("Total measurements processed:", nrow(manID.picarro), "\n")
      cat("Unique measurements:", length(unique(manID.picarro$UniqueID)), "\n")
      
      # Save manual identification results
      write_csv(manID.picarro, "flux_code/picarro_manual_identification_CH4_results.csv")
      cat("Results saved to: flux_code/picarro_manual_identification_CH4_results.csv\n")
      
      # =============================================================================
      # STEP 6C: FLUX CALCULATIONS FOR PICARRO
      # =============================================================================
      
      cat("\n=== STEP 6C: FLUX CALCULATIONS FOR PICARRO ===\n")
      
      # Calculate CH4 fluxes (primary focus)
      if("CH4dry_ppb" %in% names(manID.picarro)) {
        cat("Calculating Picarro CH4 fluxes...\n")
        CH4_flux_picarro <- goFlux(
          dataframe = manID.picarro,
          gastype = "CH4dry_ppb",
          H2O_col = "H2O_ppm",
          warn.length = 60
        )
      } else {
        cat("CH4dry_ppb column not found - cannot calculate CH4 fluxes\n")
        CH4_flux_picarro <- NULL
      }
      
      # Calculate CO2 fluxes (secondary)
      if("CO2dry_ppm" %in% names(manID.picarro)) {
        cat("Calculating Picarro CO2 fluxes...\n")
        CO2_flux_picarro <- goFlux(
          dataframe = manID.picarro,
          gastype = "CO2dry_ppm",
          H2O_col = "H2O_ppm",
          warn.length = 60
        )
      } else {
        cat("CO2dry_ppm column not found - skipping CO2 flux calculation\n")
        CO2_flux_picarro <- NULL
      }
      
      # Save flux results
      if(!is.null(CH4_flux_picarro)) {
        write_csv(CH4_flux_picarro, "flux_code/CH4_flux_picarro_results.csv")
        cat("Picarro CH4 flux calculation complete:", nrow(CH4_flux_picarro), "measurements\n")
      }
      
      if(!is.null(CO2_flux_picarro)) {
        write_csv(CO2_flux_picarro, "flux_code/CO2_flux_picarro_results.csv")
        cat("Picarro CO2 flux calculation complete:", nrow(CO2_flux_picarro), "measurements\n")
      }
      
      # =============================================================================
      # STEP 6D: BEST FLUX ANALYSIS FOR PICARRO
      # =============================================================================
      
      cat("\n=== STEP 6D: BEST FLUX ANALYSIS FOR PICARRO ===\n")
      
      # Run best.flux on CH4 results (primary)
      if(!is.null(CH4_flux_picarro)) {
        cat("Running best.flux analysis on Picarro CH4 data...\n")
        CH4_best_picarro <- best.flux(
          flux.result = CH4_flux_picarro,
          criteria = c("MAE", "RMSE", "AICc", "SE", "g.factor", "kappa", "MDF", "nb.obs", "intercept", "p-value"),
          intercept.lim = NULL,
          g.limit = 2,
          p.val = 0.05,
          k.ratio = 1,
          warn.length = 60
        )
      }
      
      # Run best.flux on CO2 results (secondary)
      if(!is.null(CO2_flux_picarro)) {
        cat("Running best.flux analysis on Picarro CO2 data...\n")
        CO2_best_picarro <- best.flux(
          flux.result = CO2_flux_picarro,
          criteria = c("MAE", "RMSE", "AICc", "SE", "g.factor", "kappa", "MDF", "nb.obs", "intercept", "p-value"),
          intercept.lim = NULL,
          g.limit = 2,
          p.val = 0.05,
          k.ratio = 1,
          warn.length = 60
        )
      }
      
      # Save best flux results
      if(exists("CH4_best_picarro")) {
        write_csv(CH4_best_picarro, "flux_code/CH4_best_flux_picarro_results.csv")
      }
      if(exists("CO2_best_picarro")) {
        write_csv(CO2_best_picarro, "flux_code/CO2_best_flux_picarro_results.csv")
      }
      
      cat("Picarro best flux analysis complete!\n")
      
      # =============================================================================
      # STEP 6E: QUALITY ASSESSMENT FOR PICARRO
      # =============================================================================
      
      cat("\n=== STEP 6E: QUALITY ASSESSMENT FOR PICARRO ===\n")
      
      # Analyze CH4 quality flags (primary)
      if(exists("CH4_best_picarro")) {
        ch4_quality_picarro <- CH4_best_picarro %>%
          count(quality.check, name = "count") %>%
          mutate(percentage = round(count / sum(count) * 100, 1))
        
        cat("Picarro CH4 Quality Distribution:\n")
        print(ch4_quality_picarro)
        
        # Summary statistics for CH4
        quality_summary_picarro <- list(
          total_measurements = nrow(CH4_best_picarro),
          clean_measurements = sum(CH4_best_picarro$quality.check == "ok", na.rm = TRUE),
          flagged_measurements = sum(CH4_best_picarro$quality.check != "ok", na.rm = TRUE),
          success_rate = round(sum(CH4_best_picarro$quality.check == "ok", na.rm = TRUE) / nrow(CH4_best_picarro) * 100, 1)
        )
      }
      
      # Analyze CO2 quality flags (secondary)
      if(exists("CO2_best_picarro")) {
        co2_quality_picarro <- CO2_best_picarro %>%
          count(quality.check, name = "count") %>%
          mutate(percentage = round(count / sum(count) * 100, 1))
        
        cat("\nPicarro CO2 Quality Distribution:\n")
        print(co2_quality_picarro)
      }
      
      cat("\nPicarro Processing Summary:\n")
      if(exists("quality_summary_picarro")) {
        cat("Total measurements:", quality_summary_picarro$total_measurements, "\n")
        cat("Clean measurements:", quality_summary_picarro$clean_measurements, "\n") 
        cat("Flagged measurements:", quality_summary_picarro$flagged_measurements, "\n")
        cat("Success rate:", quality_summary_picarro$success_rate, "%\n")
      }
      
      # =============================================================================
      # STEP 6F: CREATE COMPREHENSIVE PLOTS FOR PICARRO
      # =============================================================================
      
      cat("\n=== STEP 6F: CREATING COMPREHENSIVE PLOTS FOR PICARRO ===\n")
      
      # Create comprehensive plots
      pdf("flux_code/Picarro_CH4_flux_plots_complete.pdf", width = 12, height = 8)
      
      # Plot CH4 flux distribution (primary)
      if(exists("CH4_best_picarro")) {
        p1 <- ggplot(CH4_best_picarro, aes(x = best.flux.CH4dry_ppb)) +
          geom_histogram(bins = 30, fill = "lightcoral", alpha = 0.7) +
          labs(title = "Picarro CH4 Flux Distribution (Timestamp Corrected)", 
               x = "CH4 Flux (nmol/m²/s)", y = "Count") +
          theme_minimal()
        print(p1)
        
        p2 <- ggplot(CH4_best_picarro, aes(x = quality.check, y = best.flux.CH4dry_ppb)) +
          geom_boxplot(fill = "lightyellow", alpha = 0.7) +
          labs(title = "Picarro CH4 Flux by Quality Check", 
               x = "Quality Check", y = "CH4 Flux (nmol/m²/s)") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        print(p2)
      }
      
      # Plot CO2 flux distribution (secondary)
      if(exists("CO2_best_picarro")) {
        p3 <- ggplot(CO2_best_picarro, aes(x = best.flux.CO2dry_ppm)) +
          geom_histogram(bins = 30, fill = "lightblue", alpha = 0.7) +
          labs(title = "Picarro CO2 Flux Distribution (Timestamp Corrected)", 
               x = "CO2 Flux (µmol/m²/s)", y = "Count") +
          theme_minimal()
        print(p3)
        
        p4 <- ggplot(CO2_best_picarro, aes(x = quality.check, y = best.flux.CO2dry_ppm)) +
          geom_boxplot(fill = "lightgreen", alpha = 0.7) +
          labs(title = "Picarro CO2 Flux by Quality Check", 
               x = "Quality Check", y = "CO2 Flux (µmol/m²/s)") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        print(p4)
      }
      
      dev.off()
      cat("Comprehensive plots saved to: flux_code/Picarro_CH4_flux_plots_complete.pdf\n")
      
      # =============================================================================
      # STEP 6G: CREATE FINAL PICARRO DATASET
      # =============================================================================
      
      cat("\n=== STEP 6G: CREATING FINAL PICARRO DATASET ===\n")
      
      # Get original Picarro metadata
      picarro_original <- tree_complete %>%
        filter(analyzer_id == "Picarro") %>%
        select(-c(analyzer_id))
      
      # Prepare CH4 results for merging (primary)
      if(exists("CH4_best_picarro")) {
        ch4_results_picarro <- CH4_best_picarro %>%
          rename_with(~ paste0("CH4_", .), -UniqueID)
      }
      
      # Prepare CO2 results for merging (secondary)
      if(exists("CO2_best_picarro")) {
        co2_results_picarro <- CO2_best_picarro %>%
          rename_with(~ paste0("CO2_", .), -UniqueID)
      }
      
      # Merge everything together
      final_dataset_picarro <- picarro_original
      
      # Add CH4 if available (primary)
      if(exists("ch4_results_picarro")) {
        final_dataset_picarro <- final_dataset_picarro %>%
          left_join(ch4_results_picarro, by = c("flux_id" = "UniqueID"))
      }
      
      # Add CO2 if available (secondary)
      if(exists("co2_results_picarro")) {
        final_dataset_picarro <- final_dataset_picarro %>%
          left_join(co2_results_picarro, by = c("flux_id" = "UniqueID"))
      }
      
      # Save the final dataset
      write_csv(final_dataset_picarro, "flux_code/Picarro_CH4_final_complete_dataset.csv")
      
      # =============================================================================
      # PICARRO PROCESSING SUMMARY
      # =============================================================================
      
      cat("\n=== PICARRO PROCESSING COMPLETE ===\n")
      cat("Final dataset created with", nrow(final_dataset_picarro), "rows and", ncol(final_dataset_picarro), "columns\n")
      cat("Primary focus: CH4 flux measurements\n")
      cat("Timestamp correction: -25220 seconds applied\n")
      
      cat("\nFiles created:\n")
      cat("- flux_code/Picarro_CH4_final_complete_dataset.csv (complete final dataset)\n")
      if(exists("CH4_best_picarro")) {
        cat("- flux_code/CH4_best_flux_picarro_results.csv (CH4 flux results)\n")
      }
      if(exists("CO2_best_picarro")) {
        cat("- flux_code/CO2_best_flux_picarro_results.csv (CO2 flux results)\n")
      }
      cat("- flux_code/Picarro_CH4_flux_plots_complete.pdf (all plots)\n")
      cat("- Multiple batch plot files for manual review\n")
      
      if(exists("quality_summary_picarro")) {
        cat("\nSummary statistics:\n")
        cat("Total measurements processed:", quality_summary_picarro$total_measurements, "\n")
        cat("Clean measurements:", quality_summary_picarro$clean_measurements, "\n")
        cat("Flagged measurements:", quality_summary_picarro$flagged_measurements, "\n")
        cat("Success rate:", quality_summary_picarro$success_rate, "%\n")
      }
      
    } else {
      cat("ERROR: No successful batches processed for Picarro data\n")
    }
  }
  
} else {
  cat("No Picarro data found or data is NULL\n")
  cat("Available analyzers:", names(analyzer_data), "\n")
  if("Picarro" %in% names(analyzer_data)) {
    cat("Picarro data status: NULL\n")
  }
}

# =============================================================================
# WORKFLOW COMPLETION MESSAGE
# =============================================================================

cat("\n==================================================================\n")
cat("    COMPLETE TREE FLUX PROCESSING WORKFLOW FINISHED\n")
cat("    PICARRO CH4 FOCUS WITH TIMESTAMP CORRECTION\n")
cat("==================================================================\n\n")

cat("Processing completed for the following analyzers:\n")
for(analyzer in names(analyzer_data)) {
  if(!is.null(analyzer_data[[analyzer]])) {
    if(analyzer == "Picarro") {
      cat("✓", analyzer, "- Successfully processed (CH4 focus, timestamps corrected -25220 sec)\n")
    } else {
      cat("✓", analyzer, "- Successfully processed\n")
    }
  } else {
    cat("✗", analyzer, "- No data or processing failed\n")
  }
}

cat("\nCheck the flux_code/ directory for all output files and plots.\n")
cat("Manual identification plots have been saved for quality review.\n")
cat("All Picarro output files include 'CH4' prefix to indicate methane focus.\n")
cat("\nWorkflow complete!\n")




# Create CH4 plots using goFlux function
CH4_plots <- flux.plot(
  flux.results = CH4_best_picarro,
  dataframe = manID.picarro,
  gastype = "CH4dry_ppb",
  shoulder = 420,  # Your 7-minute buffer
  plot.legend = c("MAE", "RMSE", "AICc", "k.ratio", "g.factor"),
  plot.display = c("MDF", "prec", "nb.obs", "flux.term"),
  quality.check = TRUE
)

# Create CO2 plots using goFlux function
CO2_plots <- flux.plot(
  flux.results = CO2_best_picarro,
  dataframe = manID.picarro,
  gastype = "CO2dry_ppm", 
  shoulder = 420,
  plot.legend = c("MAE", "RMSE", "AICc", "k.ratio", "g.factor"),
  plot.display = c("MDF", "prec", "nb.obs", "flux.term"),
  quality.check = TRUE
)

# Combine both gas types into one list
plot.list <- c(CO2_plots, CH4_plots)

# Save to PDF using goFlux function
flux2pdf(plot.list, outfile = "flux_code/Picarro_CH4_flux_plots_complete.pdf", width = 12, height = 8)

cat("Comprehensive plots saved to: flux_code/Picarro_CH4_flux_plots_complete.pdf\n")









# =============================================================================
# STEP 6G: CREATE FINAL PICARRO DATASET
# =============================================================================

cat("\n=== STEP 6G: CREATING FINAL PICARRO DATASET ===\n")

# Load original tree data
original_data <- read_csv("flux_code/intermediate_files/main_trees_complete.csv")

# Filter for Picarro only
picarro_original <- original_data %>% 
  filter(analyzer_id == "Picarro") %>%
  select(-analyzer_id)  # Remove the analyzer_id column since it's redundant

# Prepare CH4 results for merging (primary)
if(exists("CH4_best_picarro")) {
  ch4_results <- CH4_best_picarro %>%
    rename_with(~ paste0("CH4_", .), -UniqueID)
}

# Prepare CO2 results for merging (secondary)
if(exists("CO2_best_picarro")) {
  co2_results <- CO2_best_picarro %>%
    rename_with(~ paste0("CO2_", .), -UniqueID)
}

# Merge everything together starting with original metadata
final_dataset_picarro <- picarro_original

# Add CH4 results if available (primary focus)
if(exists("ch4_results")) {
  final_dataset_picarro <- final_dataset_picarro %>%
    left_join(ch4_results, by = c("flux_id" = "UniqueID"))
}

# Add CO2 results if available (secondary)
if(exists("co2_results")) {
  final_dataset_picarro <- final_dataset_picarro %>%
    left_join(co2_results, by = c("flux_id" = "UniqueID"))
}

# Add processing metadata
final_dataset_picarro <- final_dataset_picarro %>%
  mutate(
    processing_date = Sys.Date(),
    timestamp_correction = "-25220 seconds",
    primary_gas = "CH4dry_ppb",
    notes = "Timestamps corrected by -25220 seconds, CH4 focus"
  )

# Save the final dataset
write_csv(final_dataset_picarro, "flux_code/Picarro_CH4_final_complete_dataset.csv")

cat("Final Picarro dataset saved to: flux_code/Picarro_CH4_final_complete_dataset.csv\n")
cat("Dataset contains", nrow(final_dataset_picarro), "measurements with", ncol(final_dataset_picarro), "columns\n")
