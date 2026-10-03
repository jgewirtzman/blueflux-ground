# =============================================================================
# Water CH4 flux from dissolved CH4 where no chamber water flux exists.
#
# Intact sites SRS5 and SRS6 have no Oct 2022 water flux: the three SRS5
# floating-chamber closures were logged to the Picarro, which recorded nothing
# from 18 Oct (~12:25 local) to 24 Oct, and the legacy values for them were LGR3
# tree closures (see data/flux_metadata/excluded_measurements.csv). Estimate
#   F = k (Cw - Ceq),   Ceq from 1.95 ppm atmospheric CH4,
# with k (k600, Schmidt-scaled) calibrated on every plot x campaign that has
# both chamber water fluxes (rebuild fit) and surface-water CH4:
#   - own GC surface-water samples at the plots
#     (data/environmental/dissolved_gas/dissolved_gas_all_observations.csv), and
#   - the BlueFlux aquatic transect (Everglades Lateral C/GHG Dataset,
#     D. Vaughn; data/environmental/aquatic/, exported 2026-10-01), stations
#     "SRS 5" / "SRS 6" next to the SRS5 / SRS6 plots.
# Pairs implying k600 > K600_MAX are dropped as implausible (CP40 Oct 2022:
# GC surface water 0.025 uM against ~40 nmol m-2 s-1 chamber fluxes).
# Central estimate: median calibrated k600; interval: min-max of calibrated
# k600 (written as ci_lo / ci_hi for the upscaling Monte Carlo).
# Cw for Oct 2022: SRS5 from the plot GC sample (19 Oct); SRS6 from transect
# station SRS 6 (15 Oct). Solubility: Yamamoto et al. (1976) Bunsen
# coefficient (~4% above tabulated values at 25 C); Schmidt number:
# Wanninkhof (2014).
#
# Cross-check: for every plot x campaign with surface-water CH4 (and no
# standing-water flag against it), the dissolved estimate with k600 from the
# OTHER calibration pairs (leave-one-out, so a site's own chamber flux does not
# set its k) beside the chamber mean.
#
# CO2: the same k600 (gas-independent), with the CO2 Schmidt number (Wanninkhof
# 2014, freshwater) and solubility (Weiss 1974), atmospheric CO2 ATM_CO2_UATM;
# dissolved CO2 from the same sources (GC CO2_uM; transect pCO2_uatm x K0).
# Legacy's SRS5 Oct 2022 water CO2 came from the three excluded "Picarro" water
# closures (LGR tree closures), so without this the intact sites had none.
#
# Writes output/flux/03_fit/water_flux_estimates.csv (one row per site x
# campaign x gas, column gas = CH4 / CO2; read by
# code/07_upscaling/02_upscale_methane.R), output/flux/03_fit/water_k_calibration.csv
# and output/flux/03_fit/water_flux_dissolved_crosscheck.csv.
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(readxl)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

ATM_CH4_UATM <- 1.95; ATM_CO2_UATM <- 417; K600_MAX <- 50   # cm h-1
STATION <- c(SRS5 = "SRS 5", SRS6 = "SRS 6")

bunsen_ch4 <- function(T_C, S) {             # Yamamoto, Alcauskas & Crozier (1976)
  T <- T_C + 273.15
  exp(-67.1962 + 99.1624 * (100 / T) + 27.9015 * log(T / 100) +
        S * (-0.072909 + 0.041674 * (T / 100) - 0.0064603 * (T / 100)^2))
}
K0  <- function(T_C, S) bunsen_ch4(T_C, S) / 22.4136                       # mol L-1 atm-1
Ceq_nM <- function(T_C, S) ATM_CH4_UATM * 1e-6 * K0(T_C, S) * 1e9           # nmol L-1
sc_ch4 <- function(T_C) 1909.4 - 120.78 * T_C + 4.1555 * T_C^2 - 0.080578 * T_C^3 + 0.00065777 * T_C^4
k_from <- function(F, dC_nM, T_C) F / (dC_nM * 1e3) * 3600 * 100 * (sc_ch4(T_C) / 600)^0.5   # k600, cm h-1
F_from <- function(k600, dC_nM, T_C) k600 * (sc_ch4(T_C) / 600)^-0.5 / 3600 / 100 * dC_nM * 1e3

# CO2 (Weiss 1974 K0, mol L-1 atm-1; Wanninkhof 2014 freshwater Schmidt number)
K0_co2 <- function(T_C, S) { T <- T_C + 273.15
  exp(-58.0931 + 90.5069 * (100 / T) + 22.2940 * log(T / 100) + S * (0.027766 - 0.025888 * (T / 100) + 0.0050578 * (T / 100)^2)) }
sc_co2 <- function(T_C) 1923.6 - 125.06 * T_C + 4.3773 * T_C^2 - 0.085681 * T_C^3 + 0.00070284 * T_C^4
F_co2 <- function(k600, dC_uM, T_C) k600 * (sc_co2(T_C) / 600)^-0.5 / 3600 / 100 * dC_uM * 1e3   # umol m-2 s-1

camp_of <- function(d) ifelse(format(d, "%Y-%m") == "2022-10", "Oct 2022",
                              ifelse(format(d, "%Y-%m") == "2023-03", "Mar 2023", NA))

# ---- chamber water fluxes (rebuild fit) per plot x campaign ----------------------------
aux <- read_csv("output/flux/01_metadata/auxfile.csv", show_col_types = FALSE)
chamber <- read_csv("output/flux/03_fit/CH4/fluxes.csv", show_col_types = FALSE) %>%
  select(UniqueID, best.flux) %>%
  inner_join(aux %>% filter(component == "water") %>% select(UniqueID, plot, date), by = "UniqueID") %>%
  mutate(campaign = camp_of(date)) %>% filter(!is.na(campaign)) %>%
  group_by(plot, campaign) %>% summarise(n_chamber = n(), F_chamber = mean(best.flux), .groups = "drop")

# ---- dissolved CH4 -------------------------------------------------------------------------
gc <- read_csv("data/environmental/dissolved_gas/dissolved_gas_all_observations.csv", show_col_types = FALSE) %>%
  filter(sample_type == "surface_water", source == "GC") %>%
  mutate(campaign = ifelse(grepl("2022", season), "Oct 2022", "Mar 2023"),
         T_C = ifelse(campaign == "Oct 2022", 28, 26), S = 15) %>%       # no paired T/S: campaign typicals
  group_by(plot = site, campaign, T_C, S) %>%
  summarise(n_samples = n(), Cw_nM = mean(CH4_uM) * 1e3, .groups = "drop") %>%
  mutate(source = "own GC, plot surface water")
gc_co2 <- read_csv("data/environmental/dissolved_gas/dissolved_gas_all_observations.csv", show_col_types = FALSE) %>%
  filter(sample_type == "surface_water", source == "GC") %>%
  mutate(campaign = ifelse(grepl("2022", season), "Oct 2022", "Mar 2023"),
         T_C = ifelse(campaign == "Oct 2022", 28, 26), S = 15) %>%
  group_by(plot = site, campaign, T_C, S) %>%
  summarise(n_samples = n(), Cw_uM = mean(CO2_uM), .groups = "drop") %>%
  mutate(source = "own GC, plot surface water")
tr <- read_excel("data/environmental/aquatic/Everglades_Lateral_C_GHG_Dataset_export_2026-10-01.xlsx",
                 "BlueFlux Transect Data") %>%
  transmute(station = Site, date = as.Date(`Date Sampled`), S = suppressWarnings(as.numeric(Salinity)),
            T_C = suppressWarnings(as.numeric(Temp)), pCH4 = suppressWarnings(as.numeric(pCH4_uatm)),
            pCO2_uatm_raw = pCO2_uatm) %>%
  mutate(pCO2 = suppressWarnings(as.numeric(pCO2_uatm_raw))) %>%
  filter(station %in% STATION, !is.na(pCH4), !is.na(T_C), !is.na(S)) %>%
  mutate(plot = names(STATION)[match(station, STATION)], campaign = camp_of(date)) %>%
  filter(!is.na(campaign)) %>%
  transmute(plot, campaign, T_C, S, n_samples = 1L, Cw_nM = pCH4 * 1e-6 * K0(T_C, S) * 1e9,
            Cw_co2_uM = pCO2 * 1e-6 * K0_co2(T_C, S) * 1e6,
            source = paste0("aquatic transect, station ", station, " (", date, ")"))
dis_co2 <- bind_rows(gc_co2, tr %>% transmute(plot, campaign, T_C, S, n_samples, Cw_uM = Cw_co2_uM, source)) %>%
  filter(!is.na(Cw_uM)) %>% mutate(dC_uM = Cw_uM - ATM_CO2_UATM * 1e-6 * K0_co2(T_C, S) * 1e6)
tr <- tr %>% select(-Cw_co2_uM)
dis <- bind_rows(gc, tr) %>% mutate(dC_nM = Cw_nM - Ceq_nM(T_C, S))

# ---- k calibration -------------------------------------------------------------------------
cal <- dis %>% inner_join(chamber, by = c("plot", "campaign")) %>%
  mutate(k600_cm_h = k_from(F_chamber, dC_nM, T_C),
         used = k600_cm_h > 0 & k600_cm_h <= K600_MAX)
write_csv(cal, "output/flux/03_fit/water_k_calibration.csv")
k_use <- cal$k600_cm_h[cal$used]
k_mid <- median(k_use); k_lo <- min(k_use); k_hi <- max(k_use)

# ---- estimates for plot x campaign without chamber water flux ------------------------------
targets <- tibble(plot = c("SRS5", "SRS6"), campaign = "Oct 2022",
                  prefer = c("own GC, plot surface water", "aquatic transect"))
est <- targets %>% inner_join(dis, by = c("plot", "campaign")) %>%
  filter(startsWith(source, prefer)) %>%
  group_by(plot, campaign) %>% slice(1) %>% ungroup() %>%
  anti_join(chamber, by = c("plot", "campaign")) %>%
  transmute(site = plot, campaign, component = "water",
            flux_rate = F_from(k_mid, dC_nM, T_C), ci_lo = F_from(k_lo, dC_nM, T_C), ci_hi = F_from(k_hi, dC_nM, T_C),
            Cw_nM = round(Cw_nM, 1), dissolved_source = source,
            k600_median_cm_h = round(k_mid, 2), k600_range_cm_h = sprintf("%.2f-%.2f", k_lo, k_hi),
            n_k_pairs = length(k_use), method = "F = k600 (Sc/600)^-0.5 (Cw - Ceq); k from chamber/dissolved pairs")
# CO2 for the same plot x campaign targets and sources (no chamber water CO2 there either)
est_co2 <- targets %>% inner_join(dis_co2, by = c("plot", "campaign")) %>%
  filter(startsWith(source, prefer)) %>% group_by(plot, campaign) %>% slice(1) %>% ungroup() %>%
  transmute(site = plot, campaign, component = "water",
            flux_rate = F_co2(k_mid, dC_uM, T_C), ci_lo = F_co2(k_lo, dC_uM, T_C), ci_hi = F_co2(k_hi, dC_uM, T_C),
            Cw_nM = round(Cw_uM * 1e3, 1), dissolved_source = source,
            k600_median_cm_h = round(k_mid, 2), k600_range_cm_h = sprintf("%.2f-%.2f", k_lo, k_hi),
            n_k_pairs = length(k_use), method = "F = k600 (Sc_CO2/600)^-0.5 (Cw - Ceq), K0 Weiss 1974; k from CH4 chamber/dissolved pairs")
est <- bind_rows(est %>% mutate(gas = "CH4", unit = "nmol m-2 s-1"), est_co2 %>% mutate(gas = "CO2", unit = "umol m-2 s-1"))
write_csv(est, "output/flux/03_fit/water_flux_estimates.csv")

# ---- cross-check: every plot x campaign with dissolved CH4, leave-one-out k ------------------
xc <- dis %>% left_join(chamber, by = c("plot", "campaign")) %>% rowwise() %>%
  mutate(k_loo = median(cal$k600_cm_h[cal$used & !(cal$plot == plot & cal$campaign == campaign & cal$source == source)]),
         F_dissolved = F_from(k_loo, dC_nM, T_C),
         F_dissolved_lo = F_from(k_lo, dC_nM, T_C), F_dissolved_hi = F_from(k_hi, dC_nM, T_C)) %>% ungroup() %>%
  transmute(plot, campaign, dissolved_source = source, n_samples, Cw_nM = round(Cw_nM, 1), T_C, S,
            k600_loo_cm_h = round(k_loo, 2), F_dissolved, F_dissolved_lo, F_dissolved_hi,
            n_chamber, F_chamber, ratio_chamber_to_dissolved = F_chamber / F_dissolved)
write_csv(xc, "output/flux/03_fit/water_flux_dissolved_crosscheck.csv")

cat("k calibration (k600, cm h-1):\n")
print(as.data.frame(cal %>% transmute(plot, campaign, source = substr(source, 1, 40), Cw_nM = round(Cw_nM, 1),
                                      n_chamber, F_chamber = round(F_chamber, 2), k600 = round(k600_cm_h, 2), used)), row.names = FALSE)
cat(sprintf("\nk600 used: median %.2f, range %.2f-%.2f (n = %d)\n", k_mid, k_lo, k_hi, length(k_use)))
cat("\nCross-check, all plot x campaign with dissolved CH4 (nmol m-2 s-1):\n")
print(as.data.frame(xc %>% transmute(plot, campaign, src = substr(dissolved_source, 1, 22), Cw_nM, k_loo = k600_loo_cm_h,
                                     F_dis = round(F_dissolved, 2), n_chamber, F_ch = round(F_chamber, 2),
                                     ratio = round(ratio_chamber_to_dissolved, 2))), row.names = FALSE)
cat("\nEstimates (nmol m-2 s-1):\n")
print(as.data.frame(est %>% select(site, campaign, gas, flux_rate, ci_lo, ci_hi, Cw_nM, dissolved_source) %>%
                      mutate(across(c(flux_rate, ci_lo, ci_hi), ~ round(.x, 2)))), row.names = FALSE)
