# =============================================================================
# October 2025 porewater and surface-water chemistry at SRS5, SRS6, CP40 and BL60,
# built from the laboratory and field files (ported from Blueflux/microbes/
# gas_depths.R and merge.R, which produced the earlier merged tables).
#
# Inputs (data/porewater/):
#   raw/picarro_porewater_run1_20251111.csv, raw/picarro_porewater_run2.csv
#       Picarro G2201-i headspace runs (CH4, CO2 and their 13C). Samples were
#       diluted 1:5; headspace equilibration 180 mL water + 20 mL gas at 25 C.
#       Re-run samples: the second run is kept.
#   Porewater - Values.csv, Porewater - Sites.csv   field sonde, sulfide, iron; sites
#   surface_water_2025_river_stations.csv           SRS5/SRS6 surface sonde (river station, 22 Oct 2025)
#   Anions_251121.csv                               ion chromatography (10x dilution)
#   Shark River alkalinity(Sheet1).csv              total alkalinity (uM)
#   SRS_November_2025_DOC_full_curve.xlsx           DOC (10x dilution)
# Dissolved gas (uM) = (n_gas + n_aq - n_air) / Vw from the headspace partial pressure,
#   KH 1.4e-3 (CH4) and 3.4e-2 (CO2) mol L-1 atm-1 at 25 C; 12C + 13C isotopologues.
# Outputs:
#   data/porewater/porewater_gas_samples_2025.csv   one row per vial (deposit table)
#   data/porewater/gas_summary_for_merge.csv        site x depth means
#   data/porewater/merged_porewater_all_parameters.csv, porewater_key_parameters.csv
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(readxl); library(stringr); library(tidyr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
P <- "data/porewater"

# 180 mL water + 20 mL ambient-air headspace; the equilibrated headspace was diluted 1:5 for
# the Picarro. The air brought into the vial (x_air) is subtracted from the mass balance.
headspace_uM <- function(ppm_diluted, KH, x_air = 0, Vw = 0.180, Vg = 0.020, T = 25, P_atm = 1, dil = 5) {
  p <- ppm_diluted * dil / 1e6 * P_atm; RT <- 0.082057 * (T + 273.15)
  (p * (Vg / RT + KH * Vw) - x_air / 1e6 * P_atm * Vg / RT) / Vw * 1e6
}
runs <- bind_rows(read_csv(file.path(P, "raw/picarro_porewater_run1_20251111.csv"), show_col_types = FALSE) %>% mutate(source = "first"),
                  read_csv(file.path(P, "raw/picarro_porewater_run2.csv"), show_col_types = FALSE) %>% mutate(source = "second"))
pat <- "-0cm-|-15cm-|-45cm-|-90cm-|-Surf-|^SRS5_10_22$|^SRS6_10_22$"
vials <- runs %>% filter(grepl(pat, SampleName)) %>%
  mutate(site = ifelse(grepl("^SRS[56]_10_22$", SampleName), substr(SampleName, 1, 4), str_extract(SampleName, "^[^-]+")),
         depth = case_when(grepl("-Surf-|^SRS[56]_10_22$", SampleName) ~ "Surface",
                           TRUE ~ str_remove(str_extract(SampleName, "(?<=-)[0-9]+cm(?=-)"), "cm"))) %>%
  group_by(SampleName, source) %>% mutate(replicate_num = row_number()) %>% ungroup() %>%
  group_by(SampleName, replicate_num) %>% arrange(desc(source == "second"), .by_group = TRUE) %>% slice(1) %>% ungroup() %>%
  mutate(CH4_uM = headspace_uM(HR_12CH4_dry_mean + HR_13CH4_mean, 1.4e-3, x_air = 1.95),
         CO2_uM = headspace_uM(`12CO2_mean` + `13CO2_mean`, 3.4e-2, x_air = 420),
         d13C_CH4 = HR_Delta_iCH4_Raw_mean, d13C_CO2 = Delta_Raw_iCO2_mean,
         sample_type = ifelse(depth == "Surface", "surface_water", "porewater"),
         run_date = as.Date(as.character(Rundate), "%Y%m%d")) %>%
  arrange(site, depth, SampleName, replicate_num) %>%
  group_by(site, depth) %>% mutate(failed_vial = n() >= 3 & CH4_uM < 0.3 * median(CH4_uM)) %>% ungroup()   # as REP_MIN_FRAC (03_fit/02)
write_csv(vials %>% transmute(sample_name = SampleName, site, sample_type, depth_cm = depth, replicate = replicate_num,
                              analysis_run = source, run_date, CH4_uM, d13C_CH4, CO2_uM, d13C_CO2, failed_vial),
          file.path(P, "porewater_gas_samples_2025.csv"))
gas <- vials %>% filter(!failed_vial) %>% group_by(Site = site, Depth_cm = depth) %>%
  summarise(CH4_mean_uM = mean(CH4_uM, na.rm = TRUE), CH4_sd_uM = sd(CH4_uM, na.rm = TRUE),
            d13C_CH4_mean = mean(d13C_CH4, na.rm = TRUE), d13C_CH4_sd = sd(d13C_CH4, na.rm = TRUE),
            CO2_mean_uM = mean(CO2_uM, na.rm = TRUE), CO2_sd_uM = sd(CO2_uM, na.rm = TRUE),
            d13C_CO2_mean = mean(d13C_CO2, na.rm = TRUE), d13C_CO2_sd = sd(d13C_CO2, na.rm = TRUE),
            n_replicates = n(), .groups = "drop")
write_csv(gas, file.path(P, "gas_summary_for_merge.csv"))

sites_meta <- read_csv(file.path(P, "Porewater - Sites.csv"), show_col_types = FALSE)
pv <- read_csv(file.path(P, "Porewater - Values.csv"), show_col_types = FALSE) %>% select(-Label) %>%
  mutate(Depth_cm = as.character(Value)) %>%
  select(Site, Depth_cm, ORP, pH, `%DO`, ppmDO, SpCond, `Tds ppt`, PSU, TempC, Sulfide, `Total Iron`)
# Dissolved O2: the 2025 sonde read a constant floor of ~30% saturation in anoxic, sulfidic
# porewater and in anoxic BL60 surface water (the 2022 sonde read 0.6-2.9% in the same waters),
# i.e. a zero offset in % saturation. Subtract the lowest porewater reading from every reading
# (% saturation) and rescale mg L-1 by the same factor; raw values kept as *_raw.
DO_FLOOR <- min(pv$`%DO`[pv$Depth_cm != "Surface"], na.rm = TRUE)
pv <- pv %>% mutate(DO_pct_raw = `%DO`, ppmDO_raw = ppmDO,
                    `%DO` = pmax(DO_pct_raw - DO_FLOOR, 0), ppmDO = ppmDO_raw * `%DO` / DO_pct_raw)
# SRS5 / SRS6 surface water (no sonde reading at the profile): the transect sonde at the river
# station beside each plot, 22 Oct 2025 (surface_water_2025_river_stations.csv; no offset)
rs <- read_csv(file.path(P, "surface_water_2025_river_stations.csv"), show_col_types = FALSE)
for (i in seq_len(nrow(rs))) {
  k <- pv$Site == rs$Site[i] & pv$Depth_cm == "Surface"
  pv[k, c("TempC", "pH", "ppmDO", "%DO", "PSU")] <- rs[i, c("TempC", "pH", "ppmDO", "pct_DO", "PSU")]
}
cat("DO floor subtracted:", DO_FLOOR, "% saturation\n")
an <- read_csv(file.path(P, "Anions_251121.csv"), show_col_types = FALSE) %>%
  filter(`Sample type` == "Sample", !grepl("Blank|Spike|ch|ac", Ident, ignore.case = TRUE)) %>%
  mutate(Site = str_extract(Ident, "^[A-Za-z0-9]+"), Depth_cm = str_extract(Ident, "(?<=-)[A-Za-z0-9]+$")) %>%
  transmute(Site, Depth_cm, F_ppm = `Anions.F.Concentration`, Cl_ppm = `Anions.Cl.Concentration`,
            NO2_N_ppm = `Anions.NO2-N.Concentration`, Br_ppm = `Anions.Br.Concentration`,
            NO3_N_ppm = `Anions.NO3-N.Concentration`, PO4_P_ppm = `Anions.PO4-P.Concentration`, SO4_ppm = `Anions.SO4.Concentration`) %>%
  mutate(across(F_ppm:SO4_ppm, ~ .x * 10))
alk <- read_csv(file.path(P, "Shark River alkalinity(Sheet1).csv"), show_col_types = FALSE) %>%
  mutate(Site = str_trim(Sample),
         Site = case_when(Site == "SRS 5" ~ "SRS5", Site == "SRS 6" ~ "SRS6", Site == "BL" ~ "BL60", Site == "Cp" ~ "CP40", TRUE ~ Site),
         Depth_cm = case_when(`Depth (cm)` == "Surface water" ~ "Surface", `Depth (cm)` == "5" ~ "0", TRUE ~ `Depth (cm)`)) %>%
  filter(Site %in% c("SRS5", "SRS6", "BL60", "CP40")) %>% transmute(Site, Depth_cm, Alkalinity_uM = Alkalinty)
doc <- suppressMessages(read_excel(file.path(P, "SRS_November_2025_DOC_full_curve.xlsx"), skip = 13)) %>%
  filter(Type == "Unknown", !grepl("Exblank|empty|ppm|Spike", `Sample ID`, ignore.case = TRUE),
         !grepl("^SRS[0-9]_|^SRS[0-9]\\.[0-9]_", `Sample ID`)) %>%
  mutate(Site = str_extract(`Sample ID`, "^[A-Za-z0-9]+"), Depth_cm = str_extract(`Sample ID`, "(?<=-)[A-Za-z0-9]+$"),
         Depth_cm = ifelse(Depth_cm == "5", "0", Depth_cm), DOC_mg_L = as.numeric(`Mean Conc.`) * 10) %>%
  filter(!is.na(Site)) %>% select(Site, Depth_cm, DOC_mg_L)
merged <- expand.grid(Site = c("SRS5", "SRS6", "BL60", "CP40"), Depth_cm = c("Surface", "0", "15", "45", "90"), stringsAsFactors = FALSE) %>%
  left_join(sites_meta, by = "Site") %>% left_join(pv, by = c("Site", "Depth_cm")) %>% left_join(an, by = c("Site", "Depth_cm")) %>%
  left_join(alk, by = c("Site", "Depth_cm")) %>% left_join(doc, by = c("Site", "Depth_cm")) %>% left_join(gas, by = c("Site", "Depth_cm")) %>%
  mutate(Depth_numeric = c(Surface = -5, `0` = 0, `15` = 15, `45` = 45, `90` = 90)[Depth_cm]) %>% arrange(Site, Depth_numeric)
write_csv(merged, file.path(P, "merged_porewater_all_parameters.csv"))
write_csv(merged %>% select(Site, Depth_cm, Depth_numeric, Lat, Long, Description, ORP, pH, ppmDO, SpCond, PSU, TempC, Sulfide, `Total Iron`,
                            Cl_ppm, Br_ppm, SO4_ppm, NO3_N_ppm, PO4_P_ppm, F_ppm, Alkalinity_uM, DOC_mg_L,
                            CH4_mean_uM, CH4_sd_uM, d13C_CH4_mean, d13C_CH4_sd, CO2_mean_uM, CO2_sd_uM, d13C_CO2_mean, d13C_CO2_sd),
          file.path(P, "porewater_key_parameters.csv"))
cat("vials:", nrow(vials), " merged rows:", nrow(merged), "\n")
