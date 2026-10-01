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
# Writes output/flux/03_fit/water_flux_estimates.csv (read by
# code/08_upscaling/upscale_methane_to_plots.R) and
# output/flux/03_fit/water_k_calibration.csv.
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(readxl)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

ATM_CH4_UATM <- 1.95; K600_MAX <- 50   # cm h-1
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
tr <- read_excel("data/environmental/aquatic/Everglades_Lateral_C_GHG_Dataset_export_2026-10-01.xlsx",
                 "BlueFlux Transect Data") %>%
  transmute(station = Site, date = as.Date(`Date Sampled`), S = suppressWarnings(as.numeric(Salinity)),
            T_C = suppressWarnings(as.numeric(Temp)), pCH4 = suppressWarnings(as.numeric(pCH4_uatm))) %>%
  filter(station %in% STATION, !is.na(pCH4), !is.na(T_C), !is.na(S)) %>%
  mutate(plot = names(STATION)[match(station, STATION)], campaign = camp_of(date)) %>%
  filter(!is.na(campaign)) %>%
  transmute(plot, campaign, T_C, S, n_samples = 1L, Cw_nM = pCH4 * 1e-6 * K0(T_C, S) * 1e9,
            source = paste0("aquatic transect, station ", station, " (", date, ")"))
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
write_csv(est, "output/flux/03_fit/water_flux_estimates.csv")

cat("k calibration (k600, cm h-1):\n")
print(as.data.frame(cal %>% transmute(plot, campaign, source = substr(source, 1, 40), Cw_nM = round(Cw_nM, 1),
                                      n_chamber, F_chamber = round(F_chamber, 2), k600 = round(k600_cm_h, 2), used)), row.names = FALSE)
cat(sprintf("\nk600 used: median %.2f, range %.2f-%.2f (n = %d)\n", k_mid, k_lo, k_hi, length(k_use)))
cat("\nEstimates (nmol m-2 s-1):\n")
print(as.data.frame(est %>% select(site, campaign, flux_rate, ci_lo, ci_hi, Cw_nM, dissolved_source) %>%
                      mutate(across(c(flux_rate, ci_lo, ci_hi), ~ round(.x, 2)))), row.names = FALSE)
