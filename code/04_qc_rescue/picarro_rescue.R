# MINIMAL PICARRO PROCESSING SCRIPT
# Process 3 Picarro measurements with complete metadata

library(goFlux)
library(dplyr)
library(readr)

cat("=== MINIMAL PICARRO PROCESSING ===\n")

# Step 1: Import Picarro raw data
cat("Importing Picarro data...\n")
picarro_data <- import2RData(
  path = "data/analyzer/Picarro_G4301",
  instrument = "G4301",
  date.format = "mdy",
  timezone = "UTC",
  keep_all = FALSE,
  prec = c(0.12, 2e-3, 1e-4),
  merge = TRUE
)

cat("Picarro data imported:", nrow(picarro_data), "rows\n")

# Step 2: Load auxfile
cat("Loading auxfile...\n")
picarro_auxfile <- read_csv("intermediate/auxfiles/picarro_rescue_auxfile.csv") %>%
  mutate(start.time = as.POSIXct(start.time, tz = "UTC"))

cat("Auxfile loaded:", nrow(picarro_auxfile), "measurements\n")

# Step 3: Create observation windows
cat("Creating observation windows...\n")
ow.picarro <- obs.win(
  inputfile = picarro_data,
  auxfile = picarro_auxfile,
  gastype = "CO2dry_ppm",
  obs.length = 300,
  shoulder = 300
)

cat("Observation windows created:", length(ow.picarro), "\n")

# Step 4: Manual identification
cat("Running manual identification...\n")
manID.picarro <- click.peak2(
  ow.list = ow.picarro,  # CORRECTED: use ow.list instead of inputfile
  gastype = "CO2dry_ppm",
  plot.lim = c(-50, 5000)
)

cat("Manual ID complete:", nrow(manID.picarro), "rows\n")

# Step 5: Calculate CO2 fluxes
cat("Calculating CO2 fluxes...\n")
CO2_flux_picarro <- goFlux(
  dataframe = manID.picarro,
  gastype = "CO2dry_ppm",
  H2O_col = "H2O_ppm",
  warn.length = 60
)

# Step 6: Best flux analysis
cat("Running best flux analysis...\n")
CO2_best_picarro <- best.flux(
  flux.result = CO2_flux_picarro,
  criteria = c("MAE", "RMSE", "AICc", "SE", "g.factor", "kappa", "MDF", "nb.obs"),
  g.limit = 2,
  p.val = 0.05,
  warn.length = 60
)

# Step 7: CH4 fluxes (if available)
if("CH4dry_ppb" %in% names(manID.picarro)) {
  cat("Calculating CH4 fluxes...\n")
  CH4_flux_picarro <- goFlux(
    dataframe = manID.picarro,
    gastype = "CH4dry_ppb",
    H2O_col = "H2O_ppm",
    warn.length = 60
  )
  
  CH4_best_picarro <- best.flux(
    flux.result = CH4_flux_picarro,
    criteria = c("MAE", "RMSE", "AICc", "SE", "g.factor", "kappa", "MDF", "nb.obs"),
    g.limit = 2,
    p.val = 0.05,
    warn.length = 60
  )
} else {
  cat("CH4 column not found\n")
  CH4_best_picarro <- NULL
}

# Step 8: Save results
write_csv(CO2_best_picarro, "intermediate/rescue_CO2_picarro_results.csv")
if(!is.null(CH4_best_picarro)) {
  write_csv(CH4_best_picarro, "intermediate/rescue_CH4_picarro_results.csv")
}

# Step 9: Display results
cat("\n=== RESULTS ===\n")
cat("CO2 fluxes calculated:", nrow(CO2_best_picarro), "\n")
if(!is.null(CH4_best_picarro)) {
  cat("CH4 fluxes calculated:", nrow(CH4_best_picarro), "\n")
}

# Show the results
cat("\nCO2 Results:\n")
print(CO2_best_picarro %>% select(UniqueID, best.flux, model, quality.check))

if(!is.null(CH4_best_picarro)) {
  cat("\nCH4 Results:\n")
  print(CH4_best_picarro %>% select(UniqueID, best.flux, model, quality.check))
}

cat("\nFiles saved:\n")
cat("- intermediate/rescue_CO2_picarro_results.csv\n")
if(!is.null(CH4_best_picarro)) {
  cat("- intermediate/rescue_CH4_picarro_results.csv\n")
}

cat("\nProcessing complete!\n")