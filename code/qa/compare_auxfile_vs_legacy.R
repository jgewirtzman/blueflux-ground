# =============================================================================
# QA: auxfile geometry vs the legacy pipeline (needs gitignored intermediate/).
# Area and Vtot must match the legacy final dataset for every row that had
# geometry (the HA/HB and Mar 2022 rows included); Tcham differences are
# expected (tower air temperature, Jon 2026-10-01).
# Writes output/qa/auxfile_vs_legacy.csv.
# =============================================================================
suppressMessages({library(dplyr); library(readr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
rd <- function(f, ...) read_csv(f, show_col_types = FALSE, ...)
aux <- rd("output/flux/01_metadata/auxfile.csv")

# ---- Verify against the legacy pipeline ------------------------------------------------
# (a) legacy preprocessing tables (before the dataset-level patches)
# (b) legacy final dataset (after HA/HB, Mar 2022 and volume patches)
leg_pre <- bind_rows(
  rd("intermediate/main_trees_complete.csv", col_types = cols(.default = col_character())),
  rd("intermediate/main_trees_complete_additional.csv", col_types = cols(.default = col_character())),
  rd("intermediate/main_soilwater_complete.csv", col_types = cols(.default = col_character()))) %>%
  distinct(flux_id, .keep_all = TRUE) %>%
  transmute(UniqueID = flux_id, pre_Area = as.numeric(surface_area_cm2),
            pre_Vtot_cm3 = as.numeric(total_system_volume_cm3), pre_Tcham = as.numeric(air_temp))
leg_final <- rd("output/qa/baseline/output__data_products__combined_gas_flux_dataset.csv") %>%
  filter(data_source != "ebullition_reprocessing") %>%
  transmute(UniqueID = flux_id, final_Area = surface_area_cm2, final_Vtot_cm3 = total_system_volume_cm3,
            final_Tcham = air_temp, final_date = as.Date(date))
rel <- function(a, b) abs(a - b) / pmax(abs(b), 1e-9)
cmp <- aux %>% select(UniqueID, measurement_type, component, plot, analyzer, chamber_id, excluded,
                      Area, Vtot, Tcham, date) %>%
  left_join(leg_pre, by = "UniqueID") %>% left_join(leg_final, by = "UniqueID") %>%
  mutate(Vtot_cm3 = Vtot * 1000,
         d_pre_Area = rel(Area, pre_Area), d_pre_Vtot = rel(Vtot_cm3, pre_Vtot_cm3),
         d_pre_T = abs(Tcham - pre_Tcham),
         d_final_Area = rel(Area, final_Area), d_final_Vtot = rel(Vtot_cm3, final_Vtot_cm3),
         d_final_T = abs(Tcham - final_Tcham),
         date_matches_final = is.na(final_date) | (!is.na(date) & date == final_date))
tol <- 1e-6
flag <- function(x) !is.na(x) & x > tol
cmp <- cmp %>% mutate(
  geometry_mismatch_vs_final = flag(d_final_Area) | flag(d_final_Vtot) | !date_matches_final,
  Tcham_changed = flag(d_final_T) | (is.na(Tcham) != is.na(final_Tcham)))
write_csv(cmp, "output/qa/auxfile_vs_legacy.csv")
n_final <- sum(!is.na(cmp$final_Area) | !is.na(cmp$final_Vtot_cm3))
cat("\nComparison with the legacy final dataset (", n_final, "rows with geometry):\n")
cat("  geometry/date mismatches:", sum(cmp$geometry_mismatch_vs_final), "(must be 0)\n")
stopifnot(sum(cmp$geometry_mismatch_vs_final) == 0)
cat("  Tcham changed (intended: tower air temperature for all):", sum(cmp$Tcham_changed), "\n")
print(as.data.frame(cmp %>% filter(Tcham_changed) %>% left_join(aux %>% select(UniqueID, Tcham_source), by = "UniqueID") %>%
  group_by(Tcham_source) %>%
  summarise(n = n(), median_dT = median(Tcham - final_Tcham, na.rm = TRUE),
            max_abs_dT = max(abs(Tcham - final_Tcham), na.rm = TRUE),
            now_filled = sum(!is.na(Tcham) & is.na(final_Tcham)), .groups = "drop")), row.names = FALSE)
cat("\nTcham source:\n"); print(table(aux$Tcham_source, useNA = "ifany"))
cat("\nPcham source:\n"); print(table(aux$Pcham_source, useNA = "ifany"))
cat("Pcham range (kPa):", round(range(aux$Pcham), 3), "\n")
