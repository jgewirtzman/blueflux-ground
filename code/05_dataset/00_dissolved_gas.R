# =============================================================================
# Dissolved CH4 and CO2 in plot surface water and porewater, all rounds.
#   Oct 2022 and Mar 2023: headspace GC (Yale, December 2023 run; peak areas calibrated
#     against the run's standards in code/00_lib/gc_dec2023.R); headspace equilibration
#     180 mL water + 20 mL ambient-air headspace at 25 C (air CH4 and CO2 subtracted);
#     KH 1.4e-3 (CH4) and 3.4e-2 (CO2) mol L-1 atm-1 (code/00_lib/gc_dec2023.R).
#     Samples at the flux sites only (SRS5, SRS6, BL60, CP40, FLM30, SE1); vials the
#     run sheet marks "NO RUN" are dropped. Campaign from the sample date.
#   Oct 2025: Picarro site x depth means (00_porewater_2025.R, gas_summary_for_merge.csv).
# Outputs:
#   data/environmental/dissolved_gas/dissolved_gas_all_observations.csv (one row per vial;
#     2025 as site x depth means, as used by the analyses)
#   data/environmental/site_characterization_salinity_ch4.csv (site x round x sample type
#     means of CH4, CO2 and salinity; salinity from Blueflux Salinity.xlsx,
#     "Terrestrial Data (Jon)", and the 2025 porewater sonde; SRS5/SRS6 surface water without a
#     plot reading takes the adjacent river-survey station, ORNL DAAC 2333)
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(readxl); library(stringr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/00_lib/gc_dec2023.R")   # vial -> raw file by sequence position; per-run standard curve
gc <- gc_dec2023() %>% filter(!grepl("NO RUN", notes, ignore.case = TRUE)) %>%
  transmute(sample_id, real_date = date, CH4_below_lod, CH4_above_std, CO2_above_std,
            CH4_uM = headspace_dissolved_uM(CH4_ppm, "CH4"), CO2_uM = headspace_dissolved_uM(CO2_ppm, "CO2")) %>%
  mutate(site = case_when(grepl("BL.?60", sample_id, ignore.case = TRUE) ~ "BL60", grepl("^CP|CP.?4", sample_id, ignore.case = TRUE) ~ "CP40",
                          grepl("FLM|FML", sample_id, ignore.case = TRUE) ~ "FLM30", grepl("SRS.?5", sample_id) ~ "SRS5",
                          grepl("SRS.?6", sample_id) ~ "SRS6", grepl("SE.?1", sample_id) ~ "SE1"),
         sample_type = case_when(grepl("pore|pour", sample_id, ignore.case = TRUE) ~ "porewater",
                                 grepl("surface", sample_id, ignore.case = TRUE) ~ "surface_water",
                                 sample_id == "CP 40" ~ "porewater", sample_id == "FML 30" ~ "surface_water"),
         season = ifelse(real_date < as.Date("2023-01-01"), "wet (Oct 2022)", "dry (Mar 2023)"), source = "GC") %>%
  filter(!is.na(site), !is.na(sample_type), is.finite(CH4_uM)) %>%
  group_by(site, season, sample_id) %>% mutate(failed_vial = n() >= 3 & CH4_uM < 0.3 * median(CH4_uM)) %>% ungroup()   # as REP_MIN_FRAC (03_fit/02)
pic <- read_csv("data/porewater/gas_summary_for_merge.csv", show_col_types = FALSE) %>%
  transmute(site = Site, season = "Oct 2025", sample_type = ifelse(Depth_cm == "Surface", "surface_water", "porewater"),
            source = "Picarro", real_date = as.Date(NA), CH4_uM = CH4_mean_uM, CO2_uM = CO2_mean_uM, sample_id = paste(Site, Depth_cm))
obs <- bind_rows(gc %>% select(site, season, sample_type, source, real_date, CH4_uM, CO2_uM, sample_id, CH4_above_std, CO2_above_std, failed_vial), pic %>% mutate(failed_vial = FALSE))
write_csv(obs, "data/environmental/dissolved_gas/dissolved_gas_all_observations.csv")

sal <- suppressMessages(read_excel("data/environmental/salinity/Blueflux Salinity.xlsx", sheet = "Terrestrial Data (Jon)")) %>%
  transmute(site = Location, PSU = as.numeric(`Salinity (PSU) - Final`),
            sample_type = ifelse(grepl("Pore", `Sample Type`), "porewater", "surface_water"),
            season = ifelse(grepl("2022", as.character(Date)), "wet (Oct 2022)", "dry (Mar 2023)")) %>%
  filter(site %in% unique(obs$site), is.finite(PSU)) %>%
  bind_rows(read_csv("data/porewater/merged_porewater_all_parameters.csv", show_col_types = FALSE) %>%
              transmute(site = Site, PSU, sample_type = ifelse(Depth_cm == "Surface", "surface_water", "porewater"), season = "Oct 2025") %>%
              filter(is.finite(PSU)))
# surface-water salinity at SRS5 / SRS6 where the plot has none: the BlueFlux river-survey station
# beside the plot on the campaign (Vaughn & Raymond 2024, ORNL DAAC 2333; cited, not redeposited)
tr <- read_csv("data/environmental/aquatic/ORNL_DAAC_2333_BLUEFLUX_Transect_Shark_Haney_Rivers_TarponBay.csv",
               show_col_types = FALSE, na = "-9999") %>%
  filter(site %in% c("SRS 5", "SRS 6"), is.finite(salinity)) %>%
  transmute(site = sub(" ", "", site), PSU = salinity, sample_type = "surface_water",
            season = ifelse(substr(date, 1, 7) == "2022-10", "wet (Oct 2022)", ifelse(substr(date, 1, 7) == "2023-03", "dry (Mar 2023)", NA))) %>%
  filter(!is.na(season)) %>% anti_join(sal, by = c("site", "season", "sample_type"))
sal <- bind_rows(sal, tr)
sc <- obs %>% filter(!failed_vial) %>% group_by(site, season, sample_type, source) %>%
  summarise(n = n(), CH4_uM_mean = mean(CH4_uM), CH4_uM_sd = sd(CH4_uM), CO2_uM_mean = mean(CO2_uM), CO2_uM_sd = sd(CO2_uM), .groups = "drop") %>%
  left_join(sal %>% group_by(site, season, sample_type) %>% summarise(n_sal = n(), PSU_mean = mean(PSU), PSU_sd = sd(PSU), .groups = "drop"),
            by = c("site", "season", "sample_type"))
write_csv(sc, "data/environmental/site_characterization_salinity_ch4.csv")
cat("observations:", nrow(obs), " summary rows:", nrow(sc), "\n")
