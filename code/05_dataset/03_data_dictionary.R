# =============================================================================
# Data dictionary for output/data_products/flux_measurements_all.csv (and the
# analysis subset combined_gas_flux_dataset.csv, same columns). Stops if a
# column has no description, so the dictionary cannot fall behind the data.
# Writes output/data_products/data_dictionary.csv.
# =============================================================================
suppressMessages({library(dplyr); library(readr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

d <- read_csv("output/data_products/flux_measurements_all.csv", show_col_types = FALSE, guess_max = 5000, n_max = 5)

desc <- c(
  flux_id = "Measurement ID (closure); placements added by ebullition processing use their placement ID",
  index = "Row index on the field sheet", plot = "Site / plot code",
  date = "Measurement date (after date_corrections.csv)",
  start_time = "Closure start, field-log clock (HH:MM:SS)", end_time = "Closure end, field-log clock (after end-time repair)",
  measurement_type = "tree (stem, root, CWD, leaves chambers) or surface (soil, water)",
  component = "stem, root, cwd, leaves, soil, water (negative stem heights recoded to root)",
  analyzer_source = "Analyzer unit (LGR1-3, Picarro) after analyzer_corrections.csv",
  chamber_id = "Surface chamber (soil_water_dims.csv name) after chamber_overrides.csv; NA for tree chambers",
  chamber_class = "Tree chamber class (A-D, HA, HB, LB) or chamber from scanned sheets",
  geometry_rule = "How chamber volume and area were derived",
  chamber_volume_cm3 = "Chamber (+ collar / float) volume", surface_area_cm2 = "Enclosed surface area",
  total_system_volume_cm3 = "Chamber + tubing + analyzer cell + Drierite", total_system_volume_L = "Same, litres",
  tubing_volume_cm3 = "Tubing volume", analyzer_cell_volume_cm3 = "Analyzer cell volume (LGR GLA131 28, Picarro 35)",
  collar_offset_cm = "Collar height (soil chambers, table default; Mar 2022 6-inch override 2 cm in geometry_rule)",
  collar_volume_cm3 = "Collar volume from soil_water_dims.csv",
  air_temp = "Chamber air temperature used in the flux (US-Skr tower TA_1_1_1; C)",
  air_temp_source = "Source of air_temp", air_temp_handheld = "Handheld / sheet-based air temperature (comparison only; C)",
  pressure_kPa = "Chamber pressure used in the flux", pressure_source = "Tower PA, or 101.325 kPa where none",
  end_time_repair = "Field-log end time repair note (wrong hour corrected / dropped)",
  date_corrected = "Date changed by date_corrections.csv", analyzer_corrected = "Analyzer changed by analyzer_corrections.csv",
  excluded = "TRUE if excluded from analysis (see exclusion_reason)", exclusion_reason = "Why the closure is excluded",
  species = "Tree species code", status = "alive / dead / CWD", height = "Chamber height on the stem as recorded (cm)",
  height_corrected = "Height above sediment, negative set to 0, minus water depth where measured from the water surface (cm)",
  diameter = "Stem / root diameter (cm)", lenticels = "Lenticels present (field sheet)", above = "Height reference (sediment / water)",
  stem_temp = "Stem temperature (C)", soil_temp = "Soil temperature (C)", water_temp = "Water temperature (C)",
  water_depth = "Water depth (cm)", notes = "Field-sheet notes", surface_type = "soil / water (surface chambers)",
  collar_id = "Collar ID / note", collar_location = "Collar location note",
  pressure_start = "Field-sheet pressure (mixed units; not used)", rh_start = "Field-sheet relative humidity (not used)",
  pneumatophore_count = "Pneumatophores inside the collar", pneumatophore_density = "Pneumatophores per m2",
  window_source = "Fit window: trimmed (curated), saved manual window, scripted (field log + offset), ebullition placement",
  window_start = "Fit window start (analyzer clock)", window_end = "Fit window end (analyzer clock)",
  clock_offset_s = "Analyzer clock minus field-log clock (s)", clock_offset_source = "How the offset was obtained",
  data_source = "rebuild fit (stage 03) or legacy ebullition placement (pending stage 04)",
  year = "Year", month = "Month", month_year = "YYYY-MM", season = "dry (Mar, Dec) / wet (Oct)",
  disturbance_level = "healthy (SRS5, SRS6, RB10), regenerating (BL60), ghost (CP40, FLM30, MI), scrub (SE1)",
  flux_status = "valid if either gas has a flux", ebullition_source = "Origin of the ebullition partitioning",
  CH4_ebull_flux = "Ebullitive CH4 flux (nmol m-2 s-1)", CH4_diffusive_flux = "Diffusive CH4 flux (nmol m-2 s-1)",
  CH4_ebullitive_fraction = "Ebullitive share of total water CH4 flux", CH4_n_ebull_events = "Bubble events in the trace",
  ebullition_reprocessed = "CH4_best.flux is the total (diffusive + ebullitive) from ebullition processing",
  legacy_data_source = "Legacy flux source (original / rescued)",
  legacy_CH4_best.flux = "Legacy pipeline CH4 flux (comparison)", legacy_CO2_best.flux = "Legacy pipeline CO2 flux (comparison)",
  use_in_analysis = "Analysis rule: not excluded and has a flux (QC flags and below-MDF retained)",
  analysis_note = "Why the closure is or is not in the analysis set, with its flags"
)
gas_desc <- c(
  best.flux = "Selected flux (CH4 nmol m-2 s-1; CO2 umol m-2 s-1); for ebullition-processed water, total flux",
  model = "Model selected by best.flux (LM / HM; HM only with >= 30 points)", quality.check = "goFlux quality flags",
  LM.flux = "Linear-model flux", LM.SE = "Linear-model flux SE", LM.r2 = "Linear-model R2", LM.p.val = "Linear-model p value",
  HM.flux = "Hutchinson-Mosier flux", HM.SE = "HM flux SE", HM.r2 = "HM R2", MDF = "goFlux datasheet-precision MDF",
  prec = "Instrument precision given to goFlux", nb.obs = "Points in the fit window", flux.term = "goFlux flux term",
  LM.diagnose = "goFlux LM diagnostics", HM.diagnose = "goFlux HM diagnostics", LM.score = "goFlux LM score",
  HM.score = "goFlux HM score", g.fact = "g factor (HM / LM)", k.ratio.lim = "goFlux kappa limit", MDF.lim = "goFlux MDF limit",
  warn.nb.obs = "goFlux observation-count warning",
  sigma_emp = "Empirical precision: MAD of first differences / sqrt(2), per analyzer x campaign",
  MDF_emp = "Minimum detectable flux, 1.96 sigma_emp / t x flux.term (t = closure seconds)",
  MDF_emp_method = "MDF method string", below_MDF_emp = "|best.flux| <= MDF_emp",
  det_class_emp = "emission / uptake / below detection", hm_min_obs_rule = "HM replaced by LM (fewer than 30 points)",
  qc_c0 = "QC: starting concentration > 1.5 x group median", qc_co2_tracer = "QC: no CO2 rise on live tissue (NA for water, leaves, CWD)",
  qc_convex = "QC: concave-down trace (saturation / leak)", qc_min_window = "QC: window shorter than 60 s",
  qc_noisy = "QC: closure noise > 1.5 x campaign sigma", qc_any = "Any QC screen fired",
  flux_status = "valid / no_data", below_MDF = "Below detection (= below_MDF_emp, lab convention)",
  flagged = "QC-flagged (= qc_any)", SNR = "|best.flux| / SE of the selected model")
for (g in c("CH4", "CO2")) desc <- c(desc, setNames(paste(g, gas_desc), paste0(g, "_", names(gas_desc))))

missing <- setdiff(names(d), names(desc))
if (length(missing)) stop("Columns without a description: ", paste(missing, collapse = ", "))
dict <- tibble(column = names(d), description = unname(desc[names(d)]),
               type = vapply(d, function(v) class(v)[1], ""))
write_csv(dict, "output/data_products/data_dictionary.csv")
cat("data_dictionary.csv:", nrow(dict), "columns described\n")
