# MINIMAL LGR3 PROCESSING SCRIPT
# Process 50 LGR3 measurements with complete metadata

library(goFlux)
library(dplyr)
library(readr)

cat("=== MINIMAL LGR3 PROCESSING ===\n")

# Step 1: Import LGR3 raw data
cat("Importing LGR3 data...\n")

# Handle zip files if present
data_path <- "data/analyzer/LGR_GLA131/LGR3"
zip_files <- list.files(data_path, recursive = TRUE, pattern = "\\.zip$", full.names = TRUE)

if (length(zip_files) > 0) {
  cat("Found", length(zip_files), "zip files - extracting...\n")
  temp_extract_dir <- tempfile("lgr3_extract_")
  dir.create(temp_extract_dir, recursive = TRUE)
  
  for(zip_file in zip_files) {
    tryCatch({
      unzip(zip_file, exdir = temp_extract_dir, overwrite = TRUE)
    }, error = function(e) {
      cat("Error with", basename(zip_file), ":", e$message, "\n")
    })
  }
  
  # Combine existing and extracted files
  existing_files <- list.files(data_path, recursive = TRUE, pattern = "\\.txt$", full.names = TRUE)
  new_files <- list.files(temp_extract_dir, recursive = TRUE, pattern = "\\.txt$", full.names = TRUE)
  
  complete_data_dir <- tempfile("lgr3_complete_")
  dir.create(complete_data_dir, recursive = TRUE)
  
  if (length(existing_files) > 0) file.copy(existing_files, complete_data_dir)
  if (length(new_files) > 0) file.copy(new_files, complete_data_dir)
  
  import_path <- complete_data_dir
} else {
  import_path <- data_path
}

lgr3_data <- import2RData(
  path = import_path,
  instrument = "UGGA",
  date.format = "mdy",
  timezone = "UTC",
  keep_all = FALSE,
  prec = c(0.35, 0.9, 200),
  merge = TRUE
)

# Clean up temporary directories
if (exists("temp_extract_dir")) unlink(temp_extract_dir, recursive = TRUE)
if (exists("complete_data_dir")) unlink(complete_data_dir, recursive = TRUE)

cat("LGR3 data imported:", nrow(lgr3_data), "rows\n")

# Step 2: Load auxfile
cat("Loading auxfile...\n")
lgr3_auxfile <- read_csv("intermediate/auxfiles/lgr3_rescue_auxfile.csv") %>%
  mutate(start.time = as.POSIXct(start.time, tz = "UTC"))

cat("Auxfile loaded:", nrow(lgr3_auxfile), "measurements\n")

# Step 3: Create observation windows
cat("Creating observation windows...\n")
ow.lgr3 <- obs.win(
  inputfile = lgr3_data,
  auxfile = lgr3_auxfile,
  gastype = "CO2dry_ppm",
  obs.length = 300,
  shoulder = 300
)

cat("Observation windows created:", length(ow.lgr3), "\n")

# Step 4: Manual identification
cat("Running manual identification...\n")
cat("NOTE: You will need to click start/end points for", length(ow.lgr3), "measurements\n")
cat("Consider processing in batches if this is too many at once.\n")

manID.lgr3 <- click.peak2(
  ow.list = ow.lgr3,
  gastype = "CO2dry_ppm",
  plot.lim = c(380, 2000),
  sleep = 2
)

cat("Manual ID complete:", nrow(manID.lgr3), "rows\n")

# Save manual identification results
write_csv(manID.lgr3, "intermediate/lgr3_rescue_manual_identification.csv")

# Step 5: Calculate CO2 fluxes
cat("Calculating CO2 fluxes...\n")
CO2_flux_lgr3 <- goFlux(
  dataframe = manID.lgr3,
  gastype = "CO2dry_ppm",
  H2O_col = "H2O_ppm",
  warn.length = 60
)

# Step 6: Best flux analysis
cat("Running best flux analysis for CO2...\n")
CO2_best_lgr3 <- best.flux(
  flux.result = CO2_flux_lgr3,
  criteria = c("MAE", "RMSE", "AICc", "SE", "g.factor", "kappa", "MDF", "nb.obs"),
  g.limit = 2,
  p.val = 0.05,
  warn.length = 60
)

# Step 7: CH4 fluxes (if available)
if("CH4dry_ppb" %in% names(manID.lgr3)) {
  cat("Calculating CH4 fluxes...\n")
  CH4_flux_lgr3 <- goFlux(
    dataframe = manID.lgr3,
    gastype = "CH4dry_ppb",
    H2O_col = "H2O_ppm",
    warn.length = 60
  )
  
  cat("Running best flux analysis for CH4...\n")
  CH4_best_lgr3 <- best.flux(
    flux.result = CH4_flux_lgr3,
    criteria = c("MAE", "RMSE", "AICc", "SE", "g.factor", "kappa", "MDF", "nb.obs"),
    g.limit = 2,
    p.val = 0.05,
    warn.length = 60
  )
} else {
  cat("CH4 column not found\n")
  CH4_best_lgr3 <- NULL
}

# Step 8: Save results
write_csv(CO2_best_lgr3, "intermediate/rescue_CO2_lgr3_results.csv")
if(!is.null(CH4_best_lgr3)) {
  write_csv(CH4_best_lgr3, "intermediate/rescue_CH4_lgr3_results.csv")
}

# Step 9: Display results
cat("\n=== LGR3 RESCUE RESULTS ===\n")
cat("CO2 fluxes calculated:", nrow(CO2_best_lgr3), "\n")
if(!is.null(CH4_best_lgr3)) {
  cat("CH4 fluxes calculated:", nrow(CH4_best_lgr3), "\n")
}

# Show summary statistics
cat("\nCO2 Results Summary:\n")
co2_summary <- CO2_best_lgr3 %>%
  summarise(
    measurements = n(),
    clean = sum(quality.check == "clean", na.rm = TRUE),
    flagged = sum(quality.check != "clean", na.rm = TRUE),
    mean_flux = round(mean(best.flux, na.rm = TRUE), 4),
    median_flux = round(median(best.flux, na.rm = TRUE), 4)
  )
print(co2_summary)

if(!is.null(CH4_best_lgr3)) {
  cat("\nCH4 Results Summary:\n")
  ch4_summary <- CH4_best_lgr3 %>%
    summarise(
      measurements = n(),
      clean = sum(quality.check == "clean", na.rm = TRUE),
      flagged = sum(quality.check != "clean", na.rm = TRUE),
      mean_flux = round(mean(best.flux, na.rm = TRUE), 4),
      median_flux = round(median(best.flux, na.rm = TRUE), 4)
    )
  print(ch4_summary)
}

cat("\nFiles created:\n")
cat("- intermediate/lgr3_rescue_manual_identification.csv\n")
cat("- intermediate/rescue_CO2_lgr3_results.csv\n")
if(!is.null(CH4_best_lgr3)) {
  cat("- intermediate/rescue_CH4_lgr3_results.csv\n")
}

cat("\nLGR3 processing complete!\n")