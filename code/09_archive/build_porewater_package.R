# =============================================================================
# DRAFT ORNL DAAC data package 2: BlueFlux ground porewater and surface-water
# biogeochemistry at the chamber-flux sites, 2022-2025.
#
# Only data not archived elsewhere: our plot samples. The BlueFlux tidal-river
# and creek survey (Vaughn & Raymond, ORNL DAAC 2333) is cited, not repeated.
#
# Tables (one CSV each; missing = -9999; ISO 8601 dates):
#   BlueFlux_porewater_dissolved_gas.csv     one row per vial: dissolved CH4, CO2 (and d13C-CH4
#                                            for 2025) in porewater and plot surface water
#   BlueFlux_porewater_field_2022_2023.csv   one row per sample: salinity, conductivity, pH, DO
#   BlueFlux_porewater_profiles_2025.csv     one row per site x depth: field sonde, sulfide, iron,
#                                            anions, alkalinity, DOC, inorganic N
#   BlueFlux_porewater_sites.csv, BlueFlux_porewater_data_dictionary.csv, README_dataset_guide.md
# Specific conductance (2025) is not included: the sheet mixes units (surface water in mS cm-1,
# porewater apparently in units of 10 uS cm-1); salinity carries the same information.
# d13C-CO2 from the Picarro runs is not included: H2S in the headspace interferes with the
# 13CO2 measurement (values of -100 to -7000 permil).
# Inputs: data/porewater/raw (Picarro runs), the GC workbook, Blueflux Salinity.xlsx (via
# 05_dataset/00_dissolved_gas.R and 00_porewater_2025.R outputs), porewater_all_parameters.csv.
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(readxl); library(tidyr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
out <- "data/deposit/porewater_biogeochemistry"; dir.create(out, recursive = TRUE, showWarnings = FALSE)
MISS <- -9999
SITES <- c("SRS5", "SRS6", "BL60", "CP40", "FLM30", "SE1")
miss <- function(d) { n <- vapply(d, is.numeric, TRUE)
  d[n] <- lapply(d[n], function(x) ifelse(is.na(x) | !is.finite(x), MISS, x))
  d[!n] <- lapply(d[!n], function(x) ifelse(is.na(x), as.character(MISS), as.character(x))); d }
camp <- function(d) format(as.Date(d), "%Y-%m")

# ---- dissolved gas, one row per vial ----------------------------------------------------------
gc <- read_csv("data/environmental/dissolved_gas/dissolved_gas_all_observations.csv", show_col_types = FALSE) %>%
  filter(source == "GC") %>%
  transmute(site_id = site, campaign = camp(real_date), date = format(real_date), sample_type, depth_cm = NA_real_,
            sample_label = sample_id, method = "headspace equilibration, gas chromatography",
            CH4_uM, CO2_uM, d13C_CH4_permil = NA_real_, CH4_above_standard = CH4_above_std,
            CO2_above_standard = CO2_above_std, failed_vial)
pic <- read_csv("data/porewater/porewater_gas_samples_2025.csv", show_col_types = FALSE) %>%
  transmute(site_id = site, campaign = "2025-10", date = NA_character_, sample_type,
            depth_cm = suppressWarnings(as.numeric(ifelse(depth_cm == "Surface", NA, depth_cm))),
            sample_label = sample_name, method = "headspace equilibration, Picarro G2201-i (1:5 dilution)",
            CH4_uM, CO2_uM, d13C_CH4_permil = d13C_CH4, CH4_above_standard = FALSE, CO2_above_standard = FALSE, failed_vial)
gas <- bind_rows(gc, pic) %>% filter(site_id %in% SITES) %>% arrange(campaign, site_id, sample_type, depth_cm, sample_label)
write_csv(miss(gas), file.path(out, "BlueFlux_porewater_dissolved_gas.csv"), na = "")

# ---- field measurements 2022-2023, one row per sample ------------------------------------------
fld <- suppressMessages(read_excel("data/environmental/salinity/Blueflux Salinity.xlsx", sheet = "Terrestrial Data (Jon)")) %>%
  filter(Location %in% SITES) %>%
  transmute(site_id = Location, campaign = camp(Date), date = format(as.Date(Date)),
            sample_type = ifelse(grepl("Pore", `Sample Type`), "porewater", "surface_water"),
            depth_cm = suppressWarnings(as.numeric(`Depth (cm)`)), replicate = suppressWarnings(as.integer(Replicate)),
            specific_conductance_uS_cm = suppressWarnings(as.numeric(`Specific Conductivity (uS/cm)`)),
            salinity_PSU = suppressWarnings(as.numeric(`Salinity (PSU) - Final`)),
            pH = suppressWarnings(as.numeric(pH)), DO_mg_L = suppressWarnings(as.numeric(`HDO (mg/L)`)),
            DO_percent_sat = suppressWarnings(as.numeric(`HDO (%)`))) %>%
  arrange(campaign, site_id, sample_type, depth_cm, replicate)
write_csv(miss(fld), file.path(out, "BlueFlux_porewater_field_2022_2023.csv"), na = "")

# ---- October 2025 profiles, one row per site x depth --------------------------------------------
pw <- read_csv("output/data_products/porewater_all_parameters.csv", show_col_types = FALSE) %>%
  transmute(site_id = Site, campaign = "2025-10", sample_type = ifelse(Depth_cm == "Surface", "surface_water", "porewater"),
            depth_cm = ifelse(Depth_cm == "Surface", NA, suppressWarnings(as.numeric(Depth_cm))),
            temperature_C = TempC, pH, ORP_mV = ORP, DO_mg_L = ppmDO, DO_percent_sat = `%DO`,
            salinity_PSU = PSU,
            sulfide_total_dissolved_mg_L = Sulfide, iron_total_dissolved_mg_L = `Total Iron`,
            chloride_mg_L = Cl_ppm, sulfate_mg_L = SO4_ppm, bromide_mg_L = Br_ppm, fluoride_mg_L = F_ppm,
            nitrite_N_mg_L = NO2_N_ppm, phosphate_P_mg_L = PO4_P_ppm,
            nitrate_N_mg_L = NO3_N_mgL, ammonium_N_mg_L = NH4_N_mgL,
            nitrate_below_detection = as.integer(NO3_N_bdl), ammonium_below_detection = as.integer(NH4_N_bdl),
            alkalinity_uM = Alkalinity_uM, DOC_mg_L,
            CH4_uM_mean = CH4_mean_uM, CH4_uM_sd = CH4_sd_uM, d13C_CH4_permil_mean = d13C_CH4_mean,
            CO2_uM_mean = CO2_mean_uM, CO2_uM_sd = CO2_sd_uM, n_gas_vials = n_replicates) %>%
  arrange(site_id, !is.na(depth_cm), depth_cm)
write_csv(miss(pw), file.path(out, "BlueFlux_porewater_profiles_2025.csv"), na = "")

# ---- sites -----------------------------------------------------------------------------------
s25 <- read_csv("data/porewater/Porewater - Sites.csv", show_col_types = FALSE) %>%
  transmute(site_id = Site, latitude_2025 = Lat, longitude_2025 = Long)
sites <- read_csv("data/sites/site_metadata.csv", show_col_types = FALSE) %>% filter(site_id %in% SITES) %>%
  transmute(site_id, site_name, latitude, longitude, ecosystem_type) %>% left_join(s25, by = "site_id")
write_csv(miss(sites), file.path(out, "BlueFlux_porewater_sites.csv"), na = "")

# ---- dictionary --------------------------------------------------------------------------------
dd <- tribble(~table, ~column, ~units, ~description,
  "all", "site_id", "", "Site code (BlueFlux_porewater_sites.csv; the chamber-flux sites)",
  "all", "campaign", "", "Sampling campaign (YYYY-MM)",
  "all", "sample_type", "", "porewater or surface_water (standing water on the plot)",
  "all", "depth_cm", "cm", "Porewater sampling depth below the sediment surface; -9999 for surface water or where not recorded (2022-2023 porewater mostly ~40 cm)",
  "dissolved_gas", "date", "", "Sampling date (2022-2023); -9999 for 2025 (campaign 2025-10)",
  "dissolved_gas", "sample_label", "", "Vial label as written",
  "dissolved_gas", "method", "", "Headspace method and instrument",
  "dissolved_gas", "CH4_uM", "umol L-1", "Dissolved CH4 (headspace: 180 mL water, 20 mL ambient-air headspace, 25 C; air CH4 1.95 ppm subtracted; KH 1.4e-3 mol L-1 atm-1). 2022-2023: GC peak areas calibrated per run against gravimetric standards (power law)",
  "dissolved_gas", "CO2_uM", "umol L-1", "Dissolved CO2 (as CH4; air CO2 420 ppm subtracted; KH 3.4e-2 mol L-1 atm-1). 2022-2023: weighted quadratic calibration",
  "dissolved_gas", "d13C_CH4_permil", "permil VPDB", "d13C of CH4 (2025 only)",
  "dissolved_gas", "CH4_above_standard", "", "TRUE if the GC peak area exceeds the highest CH4 standard (5029 ppm): value extrapolated",
  "dissolved_gas", "CO2_above_standard", "", "TRUE if the GC peak area exceeds the highest CO2 standard (10080 ppm): value extrapolated linearly, indicative only",
  "dissolved_gas", "failed_vial", "", "TRUE if below 30% of the median of its replicate set (>= 3 vials); excluded from all means",
  "field_2022_2023", "date", "", "Sampling date",
  "field_2022_2023", "replicate", "", "Replicate number",
  "field_2022_2023", "specific_conductance_uS_cm", "uS cm-1", "Specific conductance at 25 C",
  "field_2022_2023", "salinity_PSU", "PSU", "Salinity",
  "field_2022_2023", "pH", "", "pH",
  "field_2022_2023", "DO_mg_L", "mg L-1", "Dissolved oxygen",
  "field_2022_2023", "DO_percent_sat", "percent", "Dissolved oxygen saturation",
  "profiles_2025", "temperature_C", "degC", "Water temperature (Hanna HI98494)",
  "profiles_2025", "pH", "", "pH (Hanna HI98494)",
  "profiles_2025", "ORP_mV", "mV", "Oxidation-reduction potential",
  "profiles_2025", "DO_mg_L", "mg L-1", "Dissolved oxygen, sonde floor removed (see DO_percent_sat)",
  "profiles_2025", "DO_percent_sat", "percent", "Dissolved oxygen saturation; the sonde's constant 30% floor (read in anoxic porewater and surface water) subtracted. SRS5/SRS6 surface water: transect sonde at the adjacent river station, 22 Oct 2025, no offset",
  "profiles_2025", "salinity_PSU", "PSU", "Salinity",
  "profiles_2025", "sulfide_total_dissolved_mg_L", "mg L-1", "Total dissolved sulfide, methylene blue (Hach DR900)",
  "profiles_2025", "iron_total_dissolved_mg_L", "mg L-1", "Total dissolved iron, FerroVer (Hach DR900)",
  "profiles_2025", "chloride_mg_L", "mg L-1", "Chloride (ion chromatography)",
  "profiles_2025", "sulfate_mg_L", "mg L-1", "Sulfate (ion chromatography)",
  "profiles_2025", "bromide_mg_L", "mg L-1", "Bromide (ion chromatography)",
  "profiles_2025", "fluoride_mg_L", "mg L-1", "Fluoride (ion chromatography)",
  "profiles_2025", "nitrite_N_mg_L", "mg N L-1", "Nitrite-N (ion chromatography)",
  "profiles_2025", "phosphate_P_mg_L", "mg P L-1", "Phosphate-P (ion chromatography)",
  "profiles_2025", "nitrate_N_mg_L", "mg N L-1", "Nitrate-N (0 where at or below detection)",
  "profiles_2025", "ammonium_N_mg_L", "mg N L-1", "Ammonium-N (0 where at or below detection)",
  "profiles_2025", "nitrate_below_detection", "", "1 = nitrate at or below detection",
  "profiles_2025", "ammonium_below_detection", "", "1 = ammonium at or below detection",
  "profiles_2025", "alkalinity_uM", "umol L-1", "Total alkalinity (titration)",
  "profiles_2025", "DOC_mg_L", "mg C L-1", "Dissolved organic carbon (Shimadzu TOC)",
  "profiles_2025", "CH4_uM_mean", "umol L-1", "Mean dissolved CH4 of the vials (BlueFlux_porewater_dissolved_gas.csv)",
  "profiles_2025", "CH4_uM_sd", "umol L-1", "SD of dissolved CH4 across vials",
  "profiles_2025", "d13C_CH4_permil_mean", "permil VPDB", "Mean d13C of CH4",
  "profiles_2025", "CO2_uM_mean", "umol L-1", "Mean dissolved CO2",
  "profiles_2025", "CO2_uM_sd", "umol L-1", "SD of dissolved CO2",
  "profiles_2025", "n_gas_vials", "", "Number of gas vials",
  "sites", "site_name", "", "Site name",
  "sites", "latitude", "degrees_north", "Site latitude (WGS84; as in the chamber-flux dataset)",
  "sites", "longitude", "degrees_east", "Site longitude (WGS84)",
  "sites", "ecosystem_type", "", "Mangrove condition class",
  "sites", "latitude_2025", "degrees_north", "October 2025 profile location",
  "sites", "longitude_2025", "degrees_east", "October 2025 profile location")
write_csv(dd, file.path(out, "BlueFlux_porewater_data_dictionary.csv"), na = "")

writeLines(c(
"# BlueFlux: Porewater and Surface-Water Biogeochemistry at Mangrove Chamber-Flux Sites, Florida Everglades, 2022-2025",
"",
"DRAFT dataset guide (ORNL DAAC). Companion to the BlueFlux ground chamber-flux dataset.",
"",
"## Summary",
"",
sprintf("Dissolved CH4 and CO2 in porewater and plot surface water at the BlueFlux chamber-flux sites (%s): %d vials from October 2022, March 2023 and October 2025; field salinity, conductivity, pH and dissolved oxygen for %d samples (2022-2023); and October 2025 depth profiles (surface water, 0, 15, 45 and 90 cm) at SRS5, SRS6, BL60 and CP40 with redox, sulfide, iron, anions, inorganic nitrogen, alkalinity, DOC and d13C-CH4.",
        paste(SITES, collapse = ", "), nrow(gas), nrow(fld)),
"",
"River, estuary and tidal-creek samples from the BlueFlux aquatic survey are archived separately (Vaughn and Raymond, ORNL DAAC 2333) and are not repeated here.",
"",
"## Methods (brief)",
"",
"2022-2023: porewater (mostly ~40 cm) and surface water; dissolved gases by headspace equilibration (180 mL water, 20 mL headspace, 25 C) and gas chromatography; vials the run sheet marks as not run are omitted. Salinity, conductivity, pH and DO measured on separate aliquots.",
"",
"October 2025: MHE PushPoint samplers; field sonde (Hanna HI98494; dissolved O2 corrected for a constant 30% saturation floor of the sensor; SRS5/SRS6 surface water from the transect sonde at the adjacent river station, 22 Oct 2025); sulfide (methylene blue) and iron (FerroVer) on a Hach DR900; dissolved CH4, CO2 and d13C-CH4 by headspace equilibration (1:5 dilution) on a Picarro G2201-i with SAM autosampler (re-run samples: the second run); DOC, anions and alkalinity at the Yale Analytical and Stable Isotope Center; inorganic N at Yale (values at or below zero set to zero and flagged).",
"",
"d13C-CO2 is not reported: H2S in the headspace interferes with the 13CO2 measurement.",
"",
"Dissolved gas (umol L-1) = (n_headspace + n_aqueous) / V_water from the headspace mole fraction; KH 1.4e-3 (CH4) and 3.4e-2 (CO2) mol L-1 atm-1 at 25 C.",
"",
"Missing values: -9999."), file.path(out, "README_dataset_guide.md"))
cat("porewater package:", nrow(gas), "vials,", nrow(fld), "field samples,", nrow(pw), "profile rows ->", out, "\n")
