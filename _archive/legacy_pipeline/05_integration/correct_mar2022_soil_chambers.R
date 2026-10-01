#!/usr/bin/env Rscript
# correct_mar2022_soil_chambers.R
# The March 2022 campaign soil measurements at BL60, FLM30 and MI were recorded
# with the analyzer logging every ~5 s (not 1 Hz) and were mistakenly assigned
# the 8-inch soil chamber geometry (area 324.3 cm2) when a 6-inch dome + collar
# was actually used. They are corrected here to:
#   - 6-inch geometry: dome 765.6 cm3, area 182.4 cm2, collar height 2 cm
#     (collar volume 364.8 cm3), preserving each measurement's analyzer-specific
#     plumbing (tubing + cell + filter) volume;
#   - the linear (LM) flux estimate, because the sparse (~5 s) traces make the
#     Hutchinson-Mosier nonlinear fit unstable and upward-biased.
# Flux scales with total-system-volume / area (V/A), so corrected flux =
#   LM_flux * (V/A)_6in-2cm / (V/A)_as-processed.
#
# Must run AFTER: assemble_clean_dataset.R (idempotent: only acts on records
# still carrying the 8-inch area, so re-running is safe).
suppressMessages({library(readr); library(dplyr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

dataset_file <- "output/data_products/combined_gas_flux_dataset.csv"
df <- read_csv(dataset_file, show_col_types = FALSE)

DOME6 <- 765.6; AREA6 <- 182.4; COLLAR_H <- 2         # cm
COLLAR6 <- AREA6 * COLLAR_H                            # 364.8 cm3
CHCOLLAR6 <- DOME6 + COLLAR6                           # 1130.4 cm3

target <- with(df, component == "soil" & year == 2022 & month == 3 &
                 !is.na(surface_area_cm2) & abs(surface_area_cm2 - 324.3) < 1)
cat("=== Mar 2022 soil chamber correction (6-inch, 2 cm collar, LM) ===\n")
cat("Records targeted:", sum(target), "\n")
if (sum(target) == 0) { cat("Nothing to correct (already applied?). Exiting.\n"); quit(status = 0) }

idx <- which(target)
extras   <- df$total_system_volume_cm3[idx] - df$chamber_volume_cm3[idx]   # plumbing
Vtot_new <- extras + CHCOLLAR6
VA_old   <- df$total_system_volume_cm3[idx] / df$surface_area_cm2[idx]
VA_new   <- Vtot_new / AREA6
factor   <- VA_new / VA_old

log <- data.frame(flux_id = df$flux_id[idx], plot = df$plot[idx],
                  CH4_old = df$CH4_best.flux[idx], CH4_new = df$CH4_LM.flux[idx] * factor,
                  CO2_old = df$CO2_best.flux[idx], CO2_new = df$CO2_LM.flux[idx] * factor,
                  va_factor = round(factor, 3))

# apply: best flux -> LM * V/A factor; model -> LM; update geometry
df$CH4_best.flux[idx] <- df$CH4_LM.flux[idx] * factor
df$CO2_best.flux[idx] <- df$CO2_LM.flux[idx] * factor
if ("CH4_model" %in% names(df)) df$CH4_model[idx] <- "LM"
if ("CO2_model" %in% names(df)) df$CO2_model[idx] <- "LM"
df$surface_area_cm2[idx]        <- AREA6
df$chamber_volume_cm3[idx]      <- CHCOLLAR6
df$collar_volume_cm3[idx]       <- COLLAR6
df$total_system_volume_cm3[idx] <- Vtot_new
df$total_system_volume_L[idx]   <- Vtot_new / 1000

write_csv(df, dataset_file)
dir.create("intermediate", showWarnings = FALSE)
write_csv(log, "intermediate/mar2022_soil_correction_log.csv")
cat("Applied. Mean V/A factor:", round(mean(factor), 3), "\n")
cat("Per-site corrected means:\n")
print(log %>% group_by(plot) %>% summarise(n = n(),
        CH4_old = round(mean(CH4_old), 1), CH4_new = round(mean(CH4_new), 1),
        CO2_old = round(mean(CO2_old, na.rm = TRUE), 1),
        CO2_new = round(mean(CO2_new, na.rm = TRUE), 1), .groups = "drop") %>% as.data.frame())
cat("Log -> intermediate/mar2022_soil_correction_log.csv ;  dataset updated.\n")
