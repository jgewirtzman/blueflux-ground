# =============================================================================
# Compile the measurement datasets (pipeline stage 05). Written once; no later
# script modifies them.
#
# Inputs (all produced by earlier stages or tracked):
#   data/field_notes/*.csv                        field sheets (descriptive columns)
#   data/flux_metadata/*.csv                      curated corrections / exclusions
#   output/flux/01_metadata/auxfile.csv           geometry, Tcham, Pcham, dates, analyzers
#   output/flux/02_windows/windows.csv            fit window per closure
#   output/flux/03_fit/{CH4,CO2}/fluxes.csv       goFlux + fluxqc results
#   output/qa/baseline/...combined_gas_flux_dataset.csv
#       legacy values, kept side by side (legacy_* columns) and, until stage 04
#       (ebullition) is rebuilt, the legacy ebullition partitioning: CH4 totals
#       for the matched water closures and the added placements.
#
# Data hygiene, in order (nothing is deleted; every decision is a column):
#   1. field metadata cleaning      auxfile: date / analyzer corrections, end-time
#                                   repairs, chamber overrides (end_time_repair, ...)
#   2. fit-level QC                 MDF_emp / det_class_emp (1.96 sigma_MAD / t),
#                                   qc_* screens, hm_min_obs_rule
#   3. measurement exclusions       excluded + exclusion_reason (curated list, no raw
#                                   data in window, no chamber geometry)
#   4. analysis rule                use_in_analysis + analysis_note: every closure
#                                   that is not excluded and has a flux. Below-MDF
#                                   fluxes keep their measured value; QC flags are
#                                   carried, not applied (Jon, 2026-10-01).
#
# Writes
#   output/data_products/flux_measurements_all.csv   every closure + added placements
#   output/data_products/combined_gas_flux_dataset.csv  analysis set (use_in_analysis),
#       legacy-compatible column names; read by stages 06-08
#   output/data_products/data_dictionary.csv
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(lubridate); library(stringr); library(tidyr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

rd <- function(f, ...) read_csv(f, show_col_types = FALSE, ...)
chr <- function(f) rd(f, col_types = cols(.default = col_character()))
num <- function(x) suppressWarnings(as.numeric(x))

aux  <- rd("output/flux/01_metadata/auxfile.csv")
win  <- rd("output/flux/02_windows/windows.csv")
excl <- rd("data/flux_metadata/excluded_measurements.csv")
dims <- rd("data/field_notes/dimension_csvs/soil_water_dims.csv")
instr <- rd("data/field_notes/dimension_csvs/additional_vol.csv")
legacy <- rd("output/qa/baseline/output__data_products__combined_gas_flux_dataset.csv",
             col_types = cols(start_time = col_character(), end_time = col_character(), .default = col_guess()))

# ---- 1. Descriptive columns from the field sheets -----------------------------------------
trees <- bind_rows(chr("data/field_notes/blueflux compiled tree fluxes.csv"),
                   chr("data/field_notes/blueflux compiled tree fluxes_additional.csv")) %>%
  filter(!is.na(flux_id)) %>%
  transmute(flux_id, index, species, status, height = num(height), diameter = num(diameter),
            lenticels = tolower(lenticels), above = tolower(above), chamber_class,
            stem_temp = num(stem_temp), soil_temp = num(soil_temp), water_depth = num(water_depth),
            notes = NA_character_)
sw_raw <- chr("data/field_notes/BlueFlux Dataset_soils_water.csv"); names(sw_raw)[1] <- "index"
sw <- sw_raw %>% filter(!is.na(flux_id)) %>%
  transmute(flux_id, index, surface_type = tolower(Surface), collar_id = `Collar Notes`,
            collar_location = `Collar Location Notes`, soil_temp = num(`Soil Temp C`),
            water_temp = num(`Water Temp C`), water_depth = num(`Water depth cm`),
            pressure_start = num(`Pressure start`), rh_start = num(`RH start`),
            pneumatophore_count = num(Pneumatophore_Count),
            notes = coalesce(`Notes 1`, `Notes 2`))
field <- bind_rows(trees, sw)
stopifnot(!anyDuplicated(field$flux_id))

# ---- 2. Fit results ------------------------------------------------------------------------
fit_cols <- c("best.flux", "model", "quality.check", "LM.flux", "LM.SE", "LM.r2", "LM.p.val",
              "HM.flux", "HM.SE", "HM.r2", "MDF", "prec", "nb.obs", "flux.term", "LM.diagnose",
              "HM.diagnose", "LM.score", "HM.score", "g.fact", "k.ratio.lim", "MDF.lim", "warn.nb.obs",
              "sigma_emp", "MDF_emp", "MDF_emp_method", "below_MDF_emp", "det_class_emp",
              "hm_min_obs_rule", "qc_c0", "qc_co2_tracer", "qc_convex", "qc_min_window", "qc_noisy", "qc_any")
read_fit <- function(gas) {
  f <- rd(file.path("output/flux/03_fit", gas, "fluxes.csv"))
  f <- f[, intersect(c("UniqueID", fit_cols), names(f))]
  names(f)[-1] <- paste0(gas, "_", names(f)[-1]); f
}
fit <- full_join(read_fit("CH4"), read_fit("CO2"), by = "UniqueID") %>% rename(flux_id = UniqueID)

# ---- 3. Assemble the closure table ---------------------------------------------------------
cell_of <- function(an) ifelse(grepl("^LGR", an), instr$analyzer_cell[instr$instrument == "lgr_mgga"],
                               instr$analyzer_cell[instr$instrument == "picarro"])
d <- aux %>%
  transmute(flux_id = UniqueID, plot, date, start_time = substr(start.time, 12, 19), end_time = substr(end.time, 12, 19),
            measurement_type, component = tolower(component), analyzer_source = analyzer,
            chamber_id, geometry_rule, chamber_volume_cm3 = Vcham, surface_area_cm2 = Area,
            total_system_volume_cm3 = Vtot * 1000, total_system_volume_L = Vtot, tubing_volume_cm3 = Vtube,
            analyzer_cell_volume_cm3 = cell_of(analyzer), air_temp = Tcham, air_temp_source = Tcham_source,
            air_temp_handheld = Tcham_handheld, pressure_kPa = Pcham, pressure_source = Pcham_source,
            end_time_repair, date_corrected, analyzer_corrected, excluded) %>%
  left_join(field, by = "flux_id") %>%
  left_join(dims %>% select(chamber_id = Chamber, collar_offset_cm = Offset_cm, collar_volume_cm3 = Collar_Volume_cm3),
            by = "chamber_id") %>%
  left_join(win %>% select(flux_id = UniqueID, window_source, window_start = start, window_end = end,
                           clock_offset_s = offset_s, clock_offset_source = offset_source), by = "flux_id") %>%
  left_join(fit, by = "flux_id") %>%
  mutate(data_source = "rebuild fit (stage 03)")

# ---- 4. Cleaning rules carried over from the legacy assembly -------------------------------
d <- d %>% mutate(
  status = case_when(tolower(status) == "alive" ~ "alive", tolower(status) == "dead" ~ "dead",
                     toupper(status) == "CWD" ~ "CWD", TRUE ~ status),
  # negative heights are submerged roots
  component = if_else(!is.na(height) & height < 0, "root", component),
  height_corrected = pmax(height, 0),
  height_corrected = if_else(!is.na(above) & above == "sediment" & !is.na(water_depth) & water_depth > 0 &
                               !is.na(height_corrected) & component %in% c("stem", "root"),
                             height_corrected - water_depth, height_corrected),
  chamber_class = coalesce(chamber_class, if_else(measurement_type == "tree", chamber_id, NA_character_)),
  chamber_id = if_else(measurement_type == "tree", NA_character_, chamber_id),
  year = year(date), month = month(date), month_year = format(date, "%Y-%m"),
  season = case_when(month %in% c(3, 12) ~ "dry", month == 10 ~ "wet"),
  disturbance_level = case_when(plot %in% c("SRS5", "SRS6", "RB10") ~ "healthy", plot == "BL60" ~ "regenerating",
                                plot %in% c("CP40", "FLM30", "MI") ~ "ghost", plot == "SE1" ~ "scrub"),
  pneumatophore_density = if_else(!is.na(pneumatophore_count) & surface_area_cm2 > 0,
                                  pneumatophore_count / (surface_area_cm2 / 1e4), NA_real_))

# ---- 5. Legacy-compatible flux columns (fluxqc conventions) ---------------------------------
for (g in c("CH4", "CO2")) {
  bf <- d[[paste0(g, "_best.flux")]]; mdl <- d[[paste0(g, "_model")]]
  se <- ifelse(mdl == "HM", d[[paste0(g, "_HM.SE")]], d[[paste0(g, "_LM.SE")]])
  d[[paste0(g, "_flux_status")]] <- ifelse(is.na(bf), "no_data", "valid")
  d[[paste0(g, "_below_MDF")]]  <- d[[paste0(g, "_below_MDF_emp")]] %in% TRUE   # lab convention MDF
  d[[paste0(g, "_flagged")]]    <- d[[paste0(g, "_qc_any")]] %in% TRUE          # fluxqc screens
  d[[paste0(g, "_SNR")]]        <- ifelse(!is.na(bf) & !is.na(se) & se > 0, abs(bf) / se, NA_real_)
}
d <- d %>% mutate(flux_status = if_else(CH4_flux_status == "valid" | CO2_flux_status == "valid", "valid", "no_data"))

# ---- 6. Ebullition (legacy partitioning until stage 04 is rebuilt) --------------------------
eb_cols <- c("CH4_ebull_flux", "CH4_diffusive_flux", "CH4_ebullitive_fraction", "CH4_n_ebull_events", "ebullition_reprocessed")
leg_eb <- legacy %>% filter(ebullition_reprocessed %in% TRUE, data_source != "ebullition_reprocessing") %>%
  select(flux_id, legacy_total = CH4_best.flux, all_of(eb_cols))
d <- d %>% left_join(leg_eb, by = "flux_id") %>%
  mutate(ebullition_source = if_else(!is.na(legacy_total), "legacy partitioning (pending stage 04)", NA_character_),
         CH4_diffusive_flux = if_else(!is.na(legacy_total), CH4_diffusive_flux,
                                      if_else(component == "water", CH4_best.flux, NA_real_)),
         CH4_best.flux = if_else(!is.na(legacy_total), legacy_total, CH4_best.flux),
         CH4_ebull_flux = coalesce(CH4_ebull_flux, 0), CH4_ebullitive_fraction = coalesce(CH4_ebullitive_fraction, 0),
         CH4_n_ebull_events = coalesce(CH4_n_ebull_events, 0), ebullition_reprocessed = coalesce(ebullition_reprocessed, FALSE)) %>%
  select(-legacy_total)
added <- legacy %>% filter(data_source == "ebullition_reprocessing") %>%
  mutate(data_source = "legacy ebullition placement (pending stage 04)",
         ebullition_source = "legacy partitioning (pending stage 04)", excluded = FALSE,
         date = as.Date(date), window_source = "ebullition placement")

# ---- 7. Exclusions and the analysis rule -----------------------------------------------------
d <- d %>% left_join(excl %>% rename(exclusion_reason = reason), by = "flux_id") %>%
  mutate(exclusion_reason = case_when(!is.na(exclusion_reason) ~ exclusion_reason,
                                      is.na(surface_area_cm2) | is.na(total_system_volume_cm3) ~ "no chamber geometry",
                                      is.na(window_source) | window_source == "none" ~ "no closure time on the field sheet",
                                      flux_status == "no_data" ~ "no raw analyzer data in the window",
                                      TRUE ~ NA_character_),
         excluded = !is.na(exclusion_reason))
all_rows <- bind_rows(d, added %>% select(any_of(names(d)))) %>%
  left_join(legacy %>% filter(data_source != "ebullition_reprocessing") %>%
              select(flux_id, legacy_data_source = data_source, legacy_CH4_best.flux = CH4_best.flux,
                     legacy_CO2_best.flux = CO2_best.flux), by = "flux_id") %>%
  mutate(legacy_CH4_best.flux = if_else(data_source == "legacy ebullition placement (pending stage 04)", CH4_best.flux, legacy_CH4_best.flux),
         use_in_analysis = !excluded & flux_status == "valid",
         analysis_note = case_when(excluded ~ paste("excluded:", exclusion_reason),
                                   CH4_below_MDF & CH4_flagged ~ "kept; CH4 below MDF and QC-flagged",
                                   CH4_below_MDF ~ "kept; CH4 below MDF (measured value retained)",
                                   CH4_flagged ~ "kept; CH4 QC-flagged",
                                   TRUE ~ "kept")) %>%
  arrange(date, analyzer_source, start_time, flux_id)
stopifnot(!anyDuplicated(all_rows$flux_id))

write_csv(all_rows, "output/data_products/flux_measurements_all.csv")
analysis <- all_rows %>% filter(use_in_analysis)
write_csv(analysis, "output/data_products/combined_gas_flux_dataset.csv")

cat("flux_measurements_all.csv:", nrow(all_rows), "rows (", sum(all_rows$excluded), "excluded )\n")
cat("combined_gas_flux_dataset.csv (analysis set):", nrow(analysis), "rows\n")
print(count(all_rows, excluded, exclusion_reason))
print(count(analysis, measurement_type, component))
