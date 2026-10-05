# =============================================================================
# Clean flux inputs: one corrected table per closure, read by every later stage.
#
# Stages 01-02 reconcile the field sheets, scanned data sheets, clock and date
# corrections, chamber geometry and curated windows (data/field_notes,
# data/flux_metadata). This step writes what they settle on as flat files, and
# stages 03 onward read only these files plus the analyzer records:
#   data/inputs/closures.csv            one row per logged closure
#   data/inputs/unlogged_placements.csv floating-chamber placements found in the
#                                       analyzer record but not on the field sheet
#   data/inputs/chamber_dimensions_{tree,soil_water}.csv  chamber classes and collars
# Times: field_start / field_end are local field time as logged (after date and
# time corrections); window_start / window_end are the fit window on the
# analyzer clock (= field time + clock_offset_s), the clock of the analyzer
# records. Heights are as recorded after transcription fixes (height_corrections.csv);
# the root-crown datum is applied in 05_dataset/01_compile_datasets.R.
# =============================================================================
suppressMessages({library(dplyr); library(readr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
rd <- function(f, ...) read_csv(f, show_col_types = FALSE, ...)
chr <- function(f) rd(f, col_types = cols(.default = col_character()))
num <- function(x) suppressWarnings(as.numeric(x))
out <- "data/inputs"; dir.create(out, showWarnings = FALSE)

aux  <- rd("output/flux/01_metadata/auxfile.csv", col_types = cols(start.time = col_character(), end.time = col_character(), .default = col_guess()))
win  <- rd("output/flux/02_windows/windows.csv", col_types = cols(start = col_character(), end = col_character(),
                                                                   field_start = col_character(), field_end = col_character(), .default = col_guess()))
excl <- rd("data/flux_metadata/excluded_measurements.csv")
hfix <- rd("data/flux_metadata/height_corrections.csv")
dims <- rd("data/field_notes/dimension_csvs/soil_water_dims.csv")
instr <- rd("data/field_notes/dimension_csvs/additional_vol.csv")

# descriptive columns from the field sheets
trees <- bind_rows(chr("data/field_notes/blueflux compiled tree fluxes.csv"),
                   chr("data/field_notes/blueflux compiled tree fluxes_additional.csv")) %>%
  filter(!is.na(flux_id)) %>%
  transmute(flux_id, sheet_index = index, species, status, height_cm = num(height), diameter_cm = num(diameter),
            lenticels = tolower(lenticels), height_above = tolower(above), chamber_class,
            stem_temp_C = num(stem_temp), soil_temp_C = num(soil_temp), water_depth_cm = num(water_depth))
sw_raw <- chr("data/field_notes/BlueFlux Dataset_soils_water.csv"); names(sw_raw)[1] <- "index"
sw <- sw_raw %>% filter(!is.na(flux_id)) %>%
  transmute(flux_id, sheet_index = index, surface_type = tolower(Surface), collar_id = `Collar Notes`,
            collar_location = `Collar Location Notes`, soil_temp_C = num(`Soil Temp C`),
            water_temp_C = num(`Water Temp C`), water_depth_cm = num(`Water depth cm`),
            pressure_start = num(`Pressure start`), rh_start = num(`RH start`),
            pneumatophore_count = num(Pneumatophore_Count), notes = coalesce(`Notes 1`, `Notes 2`))
field <- bind_rows(trees, sw)
stopifnot(!anyDuplicated(field$flux_id))
field <- field %>% left_join(hfix %>% select(flux_id, height_fix = height), by = "flux_id") %>%
  mutate(height_cm = coalesce(height_fix, height_cm)) %>% select(-height_fix)

cell <- setNames(instr$analyzer_cell, instr$instrument)
closures <- aux %>%
  transmute(flux_id = UniqueID, site = plot, date, measurement_type, component, analyzer,
            field_start = start.time, field_end = end.time, end_time_repair, date_corrected, analyzer_corrected, time_corrected,
            chamber_id, geometry_rule, area_cm2 = Area, chamber_volume_cm3 = Vcham, tubing_volume_cm3 = Vtube,
            instrument_volume_cm3 = Vinst, total_volume_L = Vtot,
            analyzer_cell_volume_cm3 = unname(ifelse(grepl("^LGR", analyzer), cell["lgr_mgga"], cell["picarro"])),
            air_temp_C = Tcham, air_temp_source = Tcham_source, air_temp_handheld_C = Tcham_handheld,
            air_temp_handheld_source = Tcham_handheld_source, pressure_kPa = Pcham, pressure_source = Pcham_source) %>%
  left_join(dims %>% select(chamber_id = Chamber, collar_offset_cm = Offset_cm, collar_volume_cm3 = Collar_Volume_cm3), by = "chamber_id") %>%
  left_join(win %>% select(flux_id = UniqueID, campaign, window_source, window_start = start, window_end = end,
                           clock_offset_s = offset_s, clock_offset_source = offset_source), by = "flux_id") %>%
  left_join(field, by = "flux_id") %>%
  left_join(excl %>% rename(exclusion_reason = reason), by = "flux_id") %>%
  arrange(date, analyzer, field_start, flux_id)
stopifnot(!anyDuplicated(closures$flux_id))
write_csv(closures, file.path(out, "closures.csv"), na = "")

unl <- rd("data/flux_metadata/unlogged_placements.csv") %>% filter(decision == "add") %>%
  transmute(placement_id, analyzer, site, date, component, start_analyzer_clock = format(start_analyzer_clock, "%Y-%m-%d %H:%M:%S"),
            end_analyzer_clock = format(end_analyzer_clock, "%Y-%m-%d %H:%M:%S"), geometry_from, reason)
write_csv(unl, file.path(out, "unlogged_placements.csv"), na = "")
# chamber dimension tables (tree chamber classes; soil collars and floating chambers)
write_csv(rd("data/field_notes/dimension_csvs/surface_area.csv"), file.path(out, "chamber_dimensions_tree.csv"), na = "")
write_csv(dims, file.path(out, "chamber_dimensions_soil_water.csv"), na = "")
cat("closures.csv:", nrow(closures), "closures (", sum(!is.na(closures$window_start)), "with a fit window ) | unlogged placements:", nrow(unl), "\n")
