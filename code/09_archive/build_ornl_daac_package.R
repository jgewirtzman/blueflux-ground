# =============================================================================
# DRAFT ORNL DAAC data package: BlueFlux ground chamber fluxes (measured only).
# Per-measurement CH4 and CO2 fluxes from stem, prop-root, downed-wood, leaf,
# soil (incl. pneumatophores) and water-surface chambers, 2022-2023, with
# chamber geometry, height and datum, species, site coordinates, times (local
# and UTC), detection limits and QC flags. Upscaled products are NOT included
# (they depend on the TLS work and come later).
#
# Follows ORNL DAAC conventions: one CSV per table, snake_case names, units in
# the data dictionary, missing = -9999, ISO 8601 dates, UTC date-times.
# Nothing here is submitted; Jon submits.
#
# Inputs : output/data_products/combined_gas_flux_dataset.csv (stage 05/06)
#          data/sites/site_metadata.csv
#          data/field_notes/dimension_csvs/{surface_area,soil_water_dims}.csv
# Outputs: output/archive/ornl_daac/
#            BlueFlux_ground_chamber_fluxes_2022_2023.csv
#            BlueFlux_ground_sites.csv
#            BlueFlux_ground_chamber_geometry.csv
#            BlueFlux_ground_data_dictionary.csv
#            README_dataset_guide.md
#            OPEN_QUESTIONS.md
# =============================================================================
suppressMessages({library(dplyr); library(readr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
out <- "output/archive/ornl_daac"; dir.create(out, recursive = TRUE, showWarnings = FALSE)
MISS <- -9999
TZ <- "America/New_York"   # field-log clock: local civil time (EST/EDT)

d <- read_csv("output/data_products/combined_gas_flux_dataset.csv", show_col_types = FALSE, guess_max = 5000,
              col_types = cols(start_time = col_character(), end_time = col_character(), .default = col_guess())) %>%
  filter(use_in_analysis)
sites <- read_csv("data/sites/site_metadata.csv", show_col_types = FALSE)

species_names <- c(RHMA = "Rhizophora mangle", AVGE = "Avicennia germinans", LARA = "Laguncularia racemosa",
                   COER = "Conocarpus erectus", COPE = NA, Cyprus = "Taxodium distichum",
                   Mahogany = "Swietenia mahagoni", `Slash Pine` = "Pinus elliottii", UNKN = "unknown", CWD = NA)
component_names <- c(stem = "stem", root = "prop_root", cwd = "downed_wood", leaves = "leaf", soil = "soil", water = "water_surface")
area_basis <- c(stem = "enclosed bark surface", prop_root = "enclosed root bark surface", downed_wood = "enclosed wood surface",
                leaf = "one-sided leaf area (15 leaves x mean leaf area for species and forest type)",
                soil = "ground area inside the collar (pneumatophores within the footprint included)",
                water_surface = "water surface inside the floating chamber")

loc <- function(date, time) {
  time <- ifelse(grepl("^\\d{1,2}:\\d{2}$", time), paste0(time, ":00"), time)
  as.POSIXct(ifelse(is.na(time), NA, paste(date, time)), format = "%Y-%m-%d %H:%M:%S", tz = TZ)
}
fmt_utc <- function(x) ifelse(is.na(x), NA, format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
se_of <- function(model, lm, hm) ifelse(model == "HM", hm, lm)
r2_of <- function(model, lm, hm) ifelse(model == "HM", hm, lm)

f <- d %>%
  mutate(t0 = loc(date, start_time), t1 = loc(date, end_time),
         component = unname(component_names[component]),
         is_tree = component %in% c("stem", "prop_root", "downed_wood", "leaf")) %>%
  left_join(sites %>% select(site_id, latitude, longitude), by = c("plot" = "site_id")) %>%
  transmute(
    measurement_id = flux_id,
    site_id = plot,
    latitude, longitude,
    disturbance_class = disturbance_level,
    campaign = month_year,
    date_local = format(as.Date(date)),
    start_time_local = start_time, end_time_local = end_time,
    utc_offset = format(t0, "%z"),
    start_datetime_utc = fmt_utc(t0), end_datetime_utc = fmt_utc(t1),
    component,
    area_basis = unname(area_basis[component]),
    species_code = ifelse(is_tree, species, NA),
    species_name = ifelse(is_tree, unname(species_names[species]), NA),
    tissue_status = ifelse(is_tree, status, NA),
    diameter_cm = ifelse(is_tree, diameter, NA),
    chamber_height_cm = ifelse(is_tree, height, NA),
    height_datum = ifelse(is_tree, recode(above, sediment = "sediment_surface", water = "water_surface"), NA),
    height_above_sediment_cm = ifelse(is_tree, ifelse(above %in% "water" & !is.na(water_depth), height + water_depth, height), NA),
    height_above_water_cm = ifelse(is_tree & !is.na(water_depth) & water_depth > 0,
                                   ifelse(above %in% "water", height, height - water_depth), NA),
    water_depth_cm = water_depth,
    pneumatophore_count = ifelse(component == "soil", pneumatophore_count, NA),
    pneumatophore_density_m2 = ifelse(component == "soil", pneumatophore_density, NA),
    chamber_type = ifelse(is_tree, chamber_class, chamber_id),
    chamber_area_cm2 = surface_area_cm2,
    system_volume_L = total_system_volume_cm3 / 1000,
    analyzer = sub("[0-9]+$", "", analyzer_source),
    analyzer_unit = analyzer_source,
    air_temp_C = air_temp, pressure_kPa,
    placement_duration_s = ifelse(component == "water_surface", placement_duration_s, NA),
    CH4_flux = CH4_best.flux,
    CH4_flux_model = CH4_model,
    CH4_flux_se = se_of(CH4_model, CH4_LM.SE, CH4_HM.SE),
    CH4_r2 = r2_of(CH4_model, CH4_LM.r2, CH4_HM.r2),
    CH4_MDF = CH4_MDF_emp,
    CH4_below_MDF = as.integer(CH4_below_MDF_emp),
    CH4_detection_class = CH4_det_class_emp,
    CH4_qc_flag = as.integer(CH4_qc_any),
    CH4_diffusive_flux = ifelse(component == "water_surface", CH4_diffusive_flux, NA),
    CH4_ebullitive_flux = ifelse(component == "water_surface", CH4_ebull_flux, NA),
    CO2_flux = CO2_best.flux,
    CO2_flux_model = CO2_model,
    CO2_flux_se = se_of(CO2_model, CO2_LM.SE, CO2_HM.SE),
    CO2_r2 = r2_of(CO2_model, CO2_LM.r2, CO2_HM.r2),
    CO2_MDF = CO2_MDF_emp,
    CO2_below_MDF = as.integer(CO2_below_MDF_emp),
    CO2_detection_class = CO2_det_class_emp,
    CO2_qc_flag = as.integer(CO2_qc_any),
    CO2_flux_source = CO2_source
  ) %>%
  arrange(start_datetime_utc, measurement_id)

num <- vapply(f, is.numeric, logical(1))
f_out <- f; f_out[num] <- lapply(f_out[num], function(x) ifelse(is.na(x), MISS, x))
f_out[!num] <- lapply(f_out[!num], function(x) ifelse(is.na(x), as.character(MISS), x))
write_csv(f_out, file.path(out, "BlueFlux_ground_chamber_fluxes_2022_2023.csv"), na = "")

# sites
s_out <- sites %>% filter(site_id %in% f$site_id) %>%
  left_join(f %>% group_by(site_id) %>% summarise(n_measurements = n(), first_date = min(date_local), last_date = max(date_local),
                                                  disturbance_class = first(disturbance_class), .groups = "drop"), by = "site_id")
missing_sites <- setdiff(unique(f$site_id), sites$site_id)
s_out <- bind_rows(s_out, f %>% filter(site_id %in% missing_sites) %>% group_by(site_id) %>%
  summarise(n_measurements = n(), first_date = min(date_local), last_date = max(date_local),
            disturbance_class = first(disturbance_class), .groups = "drop"))
write_csv(s_out %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.x), MISS, .x))), file.path(out, "BlueFlux_ground_sites.csv"), na = "")

# chamber geometry
sa <- read_csv("data/field_notes/dimension_csvs/surface_area.csv", show_col_types = FALSE)
sw <- read_csv("data/field_notes/dimension_csvs/soil_water_dims.csv", show_col_types = FALSE)
geom <- bind_rows(
  sa %>% transmute(chamber_type = sub(" series", "", `Chamber ID`, ignore.case = TRUE), chamber_kind = "tree (elliptical, sealed to bark with clay)",
                   enclosed_area_cm2 = `SA cm2`, diameter_cm = NA_real_, chamber_height_cm = NA_real_, collar_offset_cm = NA_real_),
  sw %>% transmute(chamber_type = Chamber, chamber_kind = ifelse(grepl("Floating", Chamber), "floating (water surface)", "soil collar"),
                   enclosed_area_cm2 = Ground_Surface_Area_cm2, diameter_cm = Diameter_cm, chamber_height_cm = Height_cm,
                   collar_offset_cm = Offset_cm))
geom <- geom %>% left_join(f %>% group_by(chamber_type) %>% summarise(n_measurements = n(),
  system_volume_L_min = min(system_volume_L, na.rm = TRUE), system_volume_L_max = max(system_volume_L, na.rm = TRUE), .groups = "drop"),
  by = "chamber_type")
used_types <- setdiff(unique(f$chamber_type), geom$chamber_type)
geom <- bind_rows(geom, f %>% filter(chamber_type %in% used_types) %>% group_by(chamber_type) %>%
  summarise(chamber_kind = ifelse(first(component) == "leaf", "leaf (transparent, branch cluster)", "tree, whole small stem or root enclosed (A or B chamber; area from diameter, volume = chamber minus stem cylinder)"),
            enclosed_area_cm2 = first(chamber_area_cm2), n_measurements = n(),
            system_volume_L_min = min(system_volume_L, na.rm = TRUE), system_volume_L_max = max(system_volume_L, na.rm = TRUE), .groups = "drop"))
write_csv(geom %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.x) | !is.finite(.x), MISS, .x))),
          file.path(out, "BlueFlux_ground_chamber_geometry.csv"), na = "")

# data dictionary
dd <- tribble(~column, ~units, ~description,
  "measurement_id", "", "Unique measurement (closure or floating-chamber placement) ID",
  "site_id", "", "Site code (see BlueFlux_ground_sites.csv)",
  "latitude", "degrees_north", "Site latitude (WGS84; site level, not per tree)",
  "longitude", "degrees_east", "Site longitude (WGS84; site level, not per tree)",
  "disturbance_class", "", "healthy, regenerating, ghost or scrub mangrove",
  "campaign", "", "Campaign month (YYYY-MM)",
  "date_local", "", "Measurement date, local (America/New_York)",
  "start_time_local", "", "Closure start, local civil time (field log)",
  "end_time_local", "", "Closure end, local civil time (field log)",
  "utc_offset", "", "UTC offset of the local time (-0500 EST, -0400 EDT)",
  "start_datetime_utc", "", "Closure start, ISO 8601 UTC",
  "end_datetime_utc", "", "Closure end, ISO 8601 UTC",
  "component", "", "stem, prop_root, downed_wood, leaf, soil or water_surface",
  "area_basis", "", "Surface the flux is expressed per",
  "species_code", "", "Tree species code (RHMA, AVGE, LARA, COER, ...); trees only",
  "species_name", "", "Scientific name; trees only",
  "tissue_status", "", "alive or dead (trees, roots, downed wood)",
  "diameter_cm", "cm", "Stem, root or wood diameter at the chamber",
  "chamber_height_cm", "cm", "Chamber height as recorded, measured from height_datum",
  "height_datum", "", "Reference surface for chamber_height_cm: sediment_surface or water_surface",
  "height_above_sediment_cm", "cm", "Chamber height above the sediment: chamber_height_cm, plus water_depth_cm where measured from the water surface",
  "height_above_water_cm", "cm", "Chamber height above the standing-water surface where water was present (negative = chamber below the water line)",
  "water_depth_cm", "cm", "Standing-water depth at the measurement",
  "pneumatophore_count", "count", "Pneumatophores inside the soil collar",
  "pneumatophore_density_m2", "count m-2", "Pneumatophore density inside the soil collar",
  "chamber_type", "", "Tree chamber class (A-D, HA, HB, LB) or soil/floating chamber (see BlueFlux_ground_chamber_geometry.csv)",
  "chamber_area_cm2", "cm2", "Enclosed area used in the flux",
  "system_volume_L", "L", "Total system volume (chamber + collar + tubing + desiccant + analyzer cell)",
  "analyzer", "", "LGR (ABB/LGR GLA131, off-axis ICOS) or Picarro (G4301, cavity ring-down)",
  "analyzer_unit", "", "Analyzer unit (LGR1-3, Picarro)",
  "air_temp_C", "degC", "Air temperature used in the flux (US-Skr tower TA at the measurement time)",
  "pressure_kPa", "kPa", "Pressure used in the flux (US-Skr tower; 101.325 where unavailable)",
  "placement_duration_s", "s", "Floating-chamber placement duration (water only)",
  "CH4_flux", "nmol m-2 s-1", "CH4 flux, positive = emission to the atmosphere; water = diffusive + ebullitive",
  "CH4_flux_model", "", "Model of the reported flux: LM (linear) or HM (Hutchinson-Mosier)",
  "CH4_flux_se", "nmol m-2 s-1", "Standard error of the reported CH4 flux fit",
  "CH4_r2", "1", "R2 of the reported CH4 fit",
  "CH4_MDF", "nmol m-2 s-1", "Minimum detectable CH4 flux, 1.96 sigma / t x flux term (sigma: MAD of within-window first differences, pooled by analyzer and campaign)",
  "CH4_below_MDF", "1", "1 if |CH4_flux| < CH4_MDF (flux retained at its measured value)",
  "CH4_detection_class", "", "emission, uptake or below detection",
  "CH4_qc_flag", "1", "1 if any fluxqc screen fired (flux retained)",
  "CH4_diffusive_flux", "nmol m-2 s-1", "Water only: diffusive CH4 flux (goAquaFlux, de-ebulliated window)",
  "CH4_ebullitive_flux", "nmol m-2 s-1", "Water only: ebullitive CH4 flux over the placement",
  "CO2_flux", "umol m-2 s-1", "CO2 flux, positive = emission to the atmosphere",
  "CO2_flux_model", "", "LM or HM",
  "CO2_flux_se", "umol m-2 s-1", "Standard error of the reported CO2 flux fit",
  "CO2_r2", "1", "R2 of the reported CO2 fit",
  "CO2_MDF", "umol m-2 s-1", "Minimum detectable CO2 flux (as CH4_MDF)",
  "CO2_below_MDF", "1", "1 if |CO2_flux| < CO2_MDF",
  "CO2_detection_class", "", "emission, uptake or below detection",
  "CO2_qc_flag", "1", "1 if any fluxqc screen fired",
  "CO2_flux_source", "", "How the CO2 flux was obtained (chamber fit, or dissolved gas x k600 for wet-season intact water)")
stopifnot(setequal(dd$column, names(f)))
write_csv(dd, file.path(out, "BlueFlux_ground_data_dictionary.csv"))

# summary numbers for the guide
n_comp <- f %>% count(component) %>% mutate(s = paste0(component, " ", n)) %>% pull(s) %>% paste(collapse = ", ")
n_site <- f %>% count(site_id) %>% mutate(s = paste0(site_id, " ", n)) %>% pull(s) %>% paste(collapse = ", ")
bbox <- sites %>% filter(site_id %in% f$site_id)
guide <- c(
"# BlueFlux: Ground-Based Chamber CH4 and CO2 Fluxes from Mangrove Components, Florida Everglades, 2022-2023",
"",
"**DRAFT dataset guide (ORNL DAAC format). Not submitted.** Generated by `code/09_archive/build_ornl_daac_package.R`.",
"",
"## 1. Dataset Overview",
"",
"Per-measurement methane (CH4) and carbon dioxide (CO2) fluxes measured with closed dynamic chambers on the components of mangrove forests along a hurricane-disturbance gradient (intact, regenerating, ghost and scrub mangrove) in and near Everglades National Park, Florida, USA, as part of NASA BlueFlux. Components: tree stems, prop roots, downed coarse woody debris, leaves, soil (including pneumatophores within the collar) and water surfaces. Each record carries the flux, its fit diagnostics, the minimum detectable flux and QC flags, chamber geometry and height (with the reference surface), species and live/dead status for tree components, site coordinates, and local and UTC times. Stand-level (upscaled) budgets are not included.",
"",
"**Project:** NASA BlueFlux (Carbon Monitoring System).",
"",
"**Investigators:** [to complete: Gewirtzman, J. and co-authors].",
"",
"**Related publications:** [manuscript in preparation; preprint DOI to add].",
"",
"**Acknowledgements:** [to complete: NASA CMS grant number; Everglades National Park research permit].",
"",
"## 2. Data Characteristics",
"",
sprintf("- **Spatial coverage:** %d sites, %.3f to %.3f N, %.3f to %.3f W. Coordinates are site level (no per-tree positions); SRS5 and SRS6 from FCE LTER, others from the BlueFlux site list (`coord_source`). Three single-visit non-mangrove comparison sites (Cypress Boardwalk, Long Pine Key, Mahogany Hammock; October 2022, 3 stem closures each) have no coordinates yet.",
        n_distinct(f$site_id), min(bbox$latitude), max(bbox$latitude), -max(bbox$longitude), -min(bbox$longitude)),
sprintf("- **Temporal coverage:** %s to %s; campaigns March 2022, October 2022, March 2023.", min(f$date_local), max(f$date_local)),
"- **Temporal resolution:** one value per chamber closure (fit windows typically 3-3.5 min) or per floating-chamber placement (median ~6.5 min).",
sprintf("- **Records:** %d (%s).", nrow(f), n_comp),
sprintf("- **By site:** %s.", n_site),
"- **Times:** field clocks were local civil time (America/New_York; EDT from 2023-03-12, inside the March 2023 campaign). `utc_offset` and the UTC columns account for this.",
"- **Trees:** trees were not tagged, so there is no tree ID. Within a campaign no stem position was remeasured; some trees may have been resampled in a later campaign, but they cannot be linked.",
"- **Scope:** all valid fluxes at all sites and campaigns (core, context and non-mangrove comparison sites; March 2022 included).",
"- **Sign convention:** positive = emission to the atmosphere. Units: CH4 nmol m-2 s-1; CO2 umol m-2 s-1, per the surface given in `area_basis`.",
"- **Missing values:** -9999.",
"",
"### Data files",
"",
"| File | Content |",
"|---|---|",
"| `BlueFlux_ground_chamber_fluxes_2022_2023.csv` | One row per measurement (columns in the data dictionary) |",
"| `BlueFlux_ground_sites.csv` | Site codes, names, coordinates, ecosystem type, dominant species, record counts and dates |",
"| `BlueFlux_ground_chamber_geometry.csv` | Chamber classes: enclosed area, dimensions, collar offset, system-volume range |",
"| `BlueFlux_ground_data_dictionary.csv` | Column names, units and definitions |",
"",
"## 3. Application and Derivation",
"",
"Component-resolved fluxes for scaling ecosystem CH4 and CO2 exchange with structural (TLS) data, for comparison with airborne (CARAFE) and tower (US-Skr) fluxes, and for syntheses of tree-stem and woody-surface CH4 emission. Stem fluxes at 0, 50, 100 and (where possible) 150 cm resolve vertical gradients diagnostic of soil-origin gas transport.",
"",
"## 4. Quality Assessment",
"",
"- Minimum detectable flux per measurement: 1.96 sigma / t x the flux term, sigma = median absolute deviation of within-window first differences, centred per closure and pooled by analyzer and campaign. Below-detection fluxes are retained at their measured values and flagged (`*_below_MDF`, `*_detection_class`).",
"- QC screens (fluxqc 0.2.3: initial concentration, CO2 tracer, curvature, minimum window, noise) are reported as `*_qc_flag`; flagged fluxes are retained.",
"- Not in the file: closures with no analyzer record in the window, analyzer artefacts, duplicate data entries, three pilot chamber designs and March 2022 chambers without recorded dimensions.",
"- Fit uncertainty: `*_flux_se` and `*_r2` of the reported model.",
"",
"## 5. Data Acquisition, Materials, and Methods",
"",
"**Analyzers.** Three ABB/LGR GLA131 microportable greenhouse-gas analyzers (off-axis ICOS; 1 Hz, 0.1 Hz in March 2022) and one Picarro G4301 cavity ring-down analyzer (~0.2 Hz; CH4 and CO2 on alternate logged rows, only fresh readings of each gas used), recording dry mole fractions.",
"",
"**Chambers.** Four elliptical stem-chamber classes (A-D; enclosed 40-462 cm2) and HA/HB classes sealed to bark with modelling clay at 0, 50, 100 and 150 cm above the sediment or water surface (`height_datum`); prop-root chambers on individual *Rhizophora mangle* aerial roots; chambers on downed wood; a transparent leaf chamber (LB) on a branch cluster of 15 leaves (direction and approximate magnitude, not a controlled physiological rate); open-bottom acrylic soil cylinders (23.5 cm diameter) in the wet season and PVC soil collars (14.3 or 19.4 cm) in the dry season, with pneumatophores within the footprint counted; floating chambers (19.4 cm) on water. Closed-loop tubing with inline desiccant; system volumes per chamber-analyzer combination (0.8-39.6 L).",
"",
"**Flux calculation.** goFlux (R): linear and Hutchinson-Mosier models fitted to each concentration series; the reported flux follows the goFlux best.flux criteria (HM considered only with >= 30 points). Ideal-gas conversion with air temperature and pressure from the co-located AmeriFlux US-Skr tower at each measurement time. Floating-chamber placements were identified in the analyzer record (one flux per placement); ebullition was separated with goAquaFlux (de-ebulliated diffusive window; first 10 min for placements > 12 min; ebullition = summed bubble steps over the placement). Picarro water placements (5 s readings) do not resolve bubbles; their total is the two-point flux over the placement. Wet-season water CO2 at the intact sites, where no floating-chamber record exists, comes from dissolved gas and a calibrated gas-transfer velocity (`CO2_flux_source`).",
"",
"## 6. Data Access",
"",
"[ORNL DAAC to assign DOI.]",
"",
"## 7. References",
"",
"Delaria, E. R. et al. (2024) [CARAFE BlueFlux airborne fluxes].",
"Rheault, K. et al. goFlux: a user-friendly way to calculate GHG fluxes yourself, regardless of user experience. R package.",
"[Manuscript reference to add.]"
)
writeLines(guide, file.path(out, "README_dataset_guide.md"))

# open questions (data-derived parts)
cope_sites <- paste(unique(f$site_id[f$species_code %in% "COPE"]), collapse = ", ")
noco <- paste(missing_sites, collapse = ", ")
q <- c(
"# Open questions for Jon (ORNL DAAC draft)",
"",
"Resolved 2026-10-02: no tree IDs (not tagged); field clocks were local civil time; include all fluxes and all sites; SRS5/SRS6 coordinates from FCE LTER.",
"",
sprintf("1. **Species code COPE**: %d stem measurements at BL60, March 2023 (chambers A, B, C). BL60 stems were coded COER (*Conocarpus erectus*) in October 2022 and COPE in March 2023, with no COER in March, so COPE is probably *Conocarpus erectus* under another code. Confirm? Codes Cyprus / Mahogany / Slash Pine are mapped to *Taxodium distichum*, *Swietenia mahagoni* and *Pinus elliottii*: confirm.", sum(f$species_code %in% "COPE")),
"2. **Chamber classes HA and HB**: 16 measurements, all March 2023 (BL60: 3 roots, 2 stems; SE1: 6 roots, 5 stems). The geometry rule is 'A (or B) chamber minus stem cylinder', with area from the stem diameter, which reads as the A or B chamber fitted around the whole circumference of a small stem or root. One record is written 'Hollow HB'; if H means 'hollow' (open-ended chamber around a small stem or root), the rule fits. Correct?",
"3. **Coordinates** for BL60, CP40, FLM30, MI, RB10 and SE1 come from the BlueFlux site list (3-4 decimals); better GPS values? Cypress Boardwalk, Long Pine Key and Mahogany Hammock have none.",
"4. **Pneumatophores** are counted inside soil collars (count, density), not chambered separately. Fine as is?",
"5. **Leaf area basis**: 15 leaves x literature mean leaf area (Lin & Sternberg 1992), not measured. Fine as is?",
"6. **Raw analyzer records** (~1 GB) as a second granule?",
"7. **Authors, funding, permits**: see the proposal below.",
"8. **Non-mangrove comparison sites.** Cypress Boardwalk stems (*Taxodium*) were measured from the water surface over 34 cm of standing water, so 'upland' is wrong for it; the package now says 'non-mangrove comparison sites'. What habitat label should each of Cypress Boardwalk, Long Pine Key and Mahogany Hammock carry?",
"9. **BL60 Mar_23_26/27/28** (2023-03-19): component stem, but status recorded as 'CWD' (two COPE, one RHMA; 9-12.5 cm diameter, 30-60 cm height). Standing dead stems, or downed wood?",
"10. **Negative prop-root heights**: Oct_22_80 (FLM30), Oct_22_119/125/130/137 (SRS5) were entered as stems at -25 or -50 cm 'from the water surface' with 3-20 cm water, and are treated as roots with height above sediment 0. What does the negative height mean (distance down a prop root from the stem? below the water line?)",
"11. **Downed wood without status**: 7 downed-wood closures have no tissue status (Mar_23_199-202, 207 at SRS5; Mar_23_150-151 at CP40). Should they be labelled dead?",
"12. **Height datum check** (raised by the compilation): height_above_sediment_cm is now computed as chamber height + water depth where the datum is the water surface (it previously repeated chamber_height_cm). Some records then give chambers below the water line (height_above_water_cm < 0), e.g. sediment-referenced 14 cm with 11 cm water. Confirm the datum convention in `above`.",
"",
"## Proposed dataset metadata (approve or correct)",
"",
"- **Title:** BlueFlux: Ground-Based Chamber CH4 and CO2 Fluxes from Mangrove Components, Florida Everglades, 2022-2023",
"- **Authors (data producers, proposed order):** Gewirtzman, J.; Adams, F.; Charles, S.; Peterman, J.; Lindquist, A.; Carruthers, L.; Colwell, A.; Powell, E.; Stovall, A.; Malone, S.; Lagomasino, D.; Poulter, B.; Raymond, P. A. [field and lab people from the manuscript list; add or remove]",
"- **Contact:** Jonathan Gewirtzman (jongewirtzman@gmail.com)",
"- **Project:** NASA Carbon Monitoring System (CMS), BlueFlux. [CMS award number(s): to fill]",
"- **Funding:** NASA CMS (BlueFlux); NSF Graduate Research Fellowship (J.G.); NASA Connecticut Space Grant Graduate Fellowship (J.G.). [Other co-author awards: to fill]",
"- **Permits:** Everglades National Park research permit [number]; Rookery Bay NERR [permit/access]; [Marco Island site access].",
"- **Acknowledgements:** field team (N. Bendavid, K. Blumenthal, R. D'Ascanio, A. Stemberger, Q. Ying, J. Hirsch); FCE LTER; Everglades National Park; Rookery Bay NERR; Yale Analytical and Stable Isotope Center.",
"- **Related data:** CARAFE airborne fluxes (Delaria et al., ORNL DAAC [DOI]); AmeriFlux US-Skr (doi:10.17190/AMF/1246105); FCE LTER water levels (knb-lter-fce.1168).",
"- **Related publication:** Gewirtzman et al., Hurricane-induced mortality switches mangroves from carbon sink to methane source [preprint DOI].",
"- **Keywords:** methane; carbon dioxide; mangrove; tree stem flux; ebullition; chamber; Everglades; hurricane; ghost forest; blue carbon."
)
writeLines(q, file.path(out, "OPEN_QUESTIONS.md"))
cat("ORNL DAAC draft package:", nrow(f), "measurements ->", out, "\n")
