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
#   output/flux/04_ebullition/partition.csv       floating-chamber placements: diffusive /
#                                                 ebullitive CH4 and CO2 per placement
#   output/qa/baseline/...combined_gas_flux_dataset.csv
#       legacy values, kept side by side (legacy_* columns)
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
#   output/data_products/flux_measurements_all.csv   every closure + unlogged placements
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
part <- rd("output/flux/04_ebullition/partition.csv", col_types = cols(placement_start = col_character(),
           placement_end = col_character(), diffusive_start = col_character(), diffusive_end = col_character(), .default = col_guess()))
unl_meta <- rd("data/flux_metadata/unlogged_placements.csv")
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
  # negative heights are prop roots measured down from the root crown
  component = if_else(!is.na(height) & height < 0, "root", component),
  chamber_class = coalesce(chamber_class, if_else(measurement_type == "tree", chamber_id, NA_character_)),
  chamber_id = if_else(measurement_type == "tree", NA_character_, chamber_id),
  year = year(date), month = month(date), month_year = format(date, "%Y-%m"),
  season = case_when(month %in% c(3, 12) ~ "dry", month == 10 ~ "wet"),
  disturbance_level = case_when(plot %in% c("SRS5", "SRS6", "RB10") ~ "healthy", plot == "BL60" ~ "regenerating",
                                plot %in% c("CP40", "FLM30", "MI") ~ "ghost", plot == "SE1" ~ "scrub"),
  pneumatophore_density = if_else(!is.na(pneumatophore_count) & surface_area_cm2 > 0,
                                  pneumatophore_count / (surface_area_cm2 / 1e4), NA_real_))

# ---- 4a. Chamber heights on one datum: height above the sediment ------------------------------
# R. mangle in the 2022 campaigns was chambered from the root crown (stems at
# nominal 0/50/100 cm, prop roots at -25/-50 cm); 00_lib/rhizophora_crown.R
# estimates the crown height per site. Otherwise heights are as labelled:
# from the sediment, or from the water surface (+ water depth).
# height_sediment: height above the sediment; height_corrected: height above
# the water surface where water stood (as before), else above the sediment.
source("code/00_lib/rhizophora_crown.R")
crown <- crown_heights(d)
cat("R. mangle root-crown height (cm, ", attr(crown, "mode"), "): ",
    paste(names(crown), round(crown), sep = " ", collapse = "; "), "\n", sep = "")
d <- d %>% mutate(
  from_crown = attr(crown, "mode") != "none" & species %in% "RHMA" & month_year %in% c("2022-03", "2022-10") &
    component %in% c("stem", "root") & !is.na(height) & height %in% c(-50, -25, 0, 25, 50, 100, 150, 170) &
    plot %in% names(crown),
  depth0 = if_else(!is.na(water_depth) & water_depth > 0, water_depth, 0),
  height_datum = case_when(is.na(height) ~ NA_character_, from_crown ~ "root_crown",
                           above %in% "water" ~ "water_surface", TRUE ~ "sediment_surface"),
  height_sediment = case_when(
    is.na(height) ~ NA_real_,
    from_crown ~ pmax(0, unname(crown[plot]) + height),
    above %in% "water" ~ height + depth0,
    TRUE ~ pmax(height, 0)),
  height_corrected = if_else(component %in% c("stem", "root") & !is.na(height_sediment),
                             height_sediment - depth0, pmax(height, 0))) %>%
  select(-from_crown, -depth0)

# ---- 4b. Floating-chamber placements (stage 04) ---------------------------------------------
# One flux per placement (Jon, 2026-10-01). Water rows take the stage-04 values:
# CH4 = diffusive (goAquaFlux fork, de-ebulliated) + ebullitive (whole
# placement); CO2 = stage-04 diffusive window (stage-03 fit where it failed).
# The stage-03 window fit stays in *_stage03_best.flux; the fit diagnostics
# (LM/HM, MDF, qc_*) describe that window. Unlogged placements
# (unlogged_placements.csv, decision add) become rows of their own, with the
# descriptive and geometry columns of the same-day closure in geometry_from.
fit_names <- setdiff(names(fit), "flux_id")
unl_rows <- part %>% filter(!logged) %>%
  select(placement_id, geometry_from, placement_start, placement_end) %>%
  inner_join(d, by = c("geometry_from" = "flux_id")) %>%
  left_join(unl_meta %>% select(placement_id, unlogged_reason = reason), by = "placement_id") %>%
  mutate(flux_id = placement_id, start_time = substr(placement_start, 12, 19), end_time = substr(placement_end, 12, 19),
         window_source = "placement (unlogged)", window_start = as.POSIXct(placement_start, tz = "UTC"),
         window_end = as.POSIXct(placement_end, tz = "UTC"),
         clock_offset_s = 0, clock_offset_source = "analyzer clock (no field log)",
         index = NA_character_, water_depth = NA_real_, water_temp = NA_real_, collar_id = NA_character_,
         notes = paste("unlogged placement:", unlogged_reason), data_source = "unlogged placement (stage 04)",
         end_time_repair = NA_character_, date_corrected = FALSE, analyzer_corrected = FALSE, excluded = FALSE) %>%
  mutate(across(all_of(fit_names), ~ NA)) %>%
  select(all_of(names(d)))
d <- bind_rows(d, unl_rows)
pw <- part %>% transmute(flux_id = placement_id, placement_start, placement_end, placement_duration_s = duration_s,
                         placement_end_by = end_by, diffusive_rule, diffusive_window_start = diffusive_start,
                         diffusive_window_end = diffusive_end, p_CH4_total = CH4_total, p_CH4_diffusive = CH4_diffusive,
                         p_CH4_ebull = CH4_ebullitive, p_n_bubbles = n_bubbles, p_frac = CH4_ebullitive_fraction,
                         p_CO2 = CO2_flux, ebullition_flag)
d <- d %>% left_join(pw, by = "flux_id") %>%
  mutate(in_placement = component == "water" & !is.na(placement_start),
         CH4_stage03_best.flux = if_else(component == "water", CH4_best.flux, NA_real_),
         CO2_stage03_best.flux = if_else(component == "water", CO2_best.flux, NA_real_),
         CH4_best.flux = if_else(in_placement, p_CH4_total, CH4_best.flux),
         CO2_source = case_when(in_placement & !is.na(p_CO2) ~ "stage 04 diffusive window",
                                component == "water" & !is.na(CO2_best.flux) ~ "stage 03 fit window",
                                TRUE ~ NA_character_),
         CO2_best.flux = if_else(in_placement & !is.na(p_CO2), p_CO2, CO2_best.flux),
         CH4_diffusive_flux = if_else(component == "water", if_else(in_placement, p_CH4_diffusive, CH4_best.flux), NA_real_),
         CH4_ebull_flux = if_else(in_placement, coalesce(p_CH4_ebull, 0), 0),
         CH4_n_ebull_events = if_else(in_placement, coalesce(p_n_bubbles, 0), 0),
         CH4_ebullitive_fraction = if_else(in_placement, coalesce(p_frac, 0), 0),
         ebullition_reprocessed = in_placement,
         ebullition_source = if_else(in_placement, "stage 04: goAquaFlux fork 2ed7224, de-ebulliated diffusive + whole-placement ebullition", NA_character_)) %>%
  select(-starts_with("p_"), -in_placement)

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

# ---- 6. Ebullition: from stage 04 (section 4b) -------------------------------------------------

# ---- 7. Exclusions and the analysis rule -----------------------------------------------------
d <- d %>% left_join(excl %>% rename(exclusion_reason = reason), by = "flux_id") %>%
  mutate(exclusion_reason = case_when(!is.na(exclusion_reason) ~ exclusion_reason,
                                      is.na(surface_area_cm2) | is.na(total_system_volume_cm3) ~ "no chamber geometry",
                                      is.na(window_source) | window_source == "none" ~ "no closure time on the field sheet",
                                      data_source == "unlogged placement (stage 04)" & flux_status == "no_data" ~
                                        "stage 04: placement too short for goAquaFlux (< 30 observations)",
                                      flux_status == "no_data" ~ "no raw analyzer data in the window",
                                      TRUE ~ NA_character_),
         excluded = !is.na(exclusion_reason))
all_rows <- d %>%
  left_join(legacy %>% select(flux_id, legacy_data_source = data_source, legacy_CH4_best.flux = CH4_best.flux,
                              legacy_CO2_best.flux = CO2_best.flux), by = "flux_id") %>%
  mutate(use_in_analysis = !excluded & flux_status == "valid",
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
