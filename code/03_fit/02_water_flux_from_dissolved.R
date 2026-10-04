# =============================================================================
# Water CH4 flux from dissolved CH4 where no chamber water flux exists.
#
# Intact sites SRS5 and SRS6 have no Oct 2022 water flux: the three SRS5
# floating-chamber closures were logged to the Picarro, which recorded nothing
# from 18 Oct (~12:25 local) to 24 Oct, and the legacy values for them were LGR3
# tree closures (see data/flux_metadata/excluded_measurements.csv). Estimate
#   F = k600 (Sc/600)^-0.5 (Cw - Ceq),   Ceq from 1.95 ppm atmospheric CH4.
#
# Dissolved CH4:
#   - own GC surface-water samples at the plots (triplicates;
#     data/environmental/dissolved_gas/dissolved_gas_all_observations.csv). A
#     replicate below REP_MIN_FRAC of its triplicate median is a failed vial and
#     dropped (SRS5 Oct 2022 8 nM against 127-202; FLM30 Oct 2022 27 nM against
#     ~1,300; SE1 Mar 2023 12 nM, a vial the GC run sheet marks "NO RUN").
#   - BlueFlux tidal-river transect (Vaughn & Raymond 2024, ORNL DAAC 2333;
#     data/environmental/aquatic/ORNL_DAAC_2333_*.csv): river stations "SRS 5" /
#     "SRS 6" beside the plots (channel water) and the SRS6 tidal creek inside the
#     forest (forest water).
# Salinity and temperature: transect samples, their own sonde values; plot
# samples, surface-water salinity measured at the plot in that campaign
# (site_characterization_salinity_ch4.csv) and water temperature recorded at the
# water chambers or, failing that, on the plot water datasheet (18-35 C accepted),
# else the campaign typical (28 C Oct, 26 C Mar; salinity 15).
#
# k600 calibration: chamber water fluxes are grouped by plot x campaign x position
# (above the flooded forest floor, or channel / open water; code/00_lib/
# water_position.R) and paired with dissolved CH4 from the same position: plot
# samples and the tidal creek with floor chambers, river stations with channel
# chambers. Transect samples are taken from the date nearest the chambers (within
# 1 day). A chamber group with no sample from its position is paired with the
# site's other sample. Pairs implying k600 > K600_MAX are dropped as implausible
# (CP40 Oct 2022: GC surface water 0.025 uM against ~40 nmol m-2 s-1 chamber
# fluxes).
# k per site: median of that site's pairs. Estimates use the median of the site
# medians (each site weighted equally; 1.29 cm h-1 at the time of writing) for both
# gases; interval: range of the site medians. Sensitivities written alongside: the
# site's own k (SRS6 6.8 overshoots the US-Skr tower: CO2 respiration +50%) and the
# intact-class median.
# Cw for Oct 2022: SRS5 from the plot GC sample (19 Oct); SRS6 from transect
# station SRS 6 (15 Oct). Solubility: Yamamoto et al. (1976) Bunsen coefficient;
# Schmidt number: Wanninkhof (2014).
#
# Cross-check: for every plot x campaign with surface-water CH4, the dissolved
# estimate with k600 from the OTHER sites (leave-site-out median of site medians)
# beside the chamber mean.
#
# CO2: the same k600 (gas-independent), with the CO2 Schmidt number (Wanninkhof
# 2014, freshwater) and solubility (Weiss 1974), atmospheric CO2 ATM_CO2_UATM;
# dissolved CO2 from the same samples (GC CO2_uM; transect pCO2 x K0).
#
# Writes output/flux/03_fit/water_flux_estimates.csv (one row per site x
# campaign x gas, column gas = CH4 / CO2; read by
# code/07_upscaling/02_upscale_methane.R), output/flux/03_fit/water_k_calibration.csv
# and output/flux/03_fit/water_flux_dissolved_crosscheck.csv.
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(readxl)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

ATM_CH4_UATM <- 1.95; ATM_CO2_UATM <- 417; K600_MAX <- 50   # cm h-1
REP_MIN_FRAC <- 0.3                                           # failed-vial threshold (fraction of triplicate median)
source("code/00_lib/water_position.R")

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

# ---- chamber water fluxes (rebuild fit) per plot x campaign x position ------------------------
aux <- read_csv("data/inputs/closures.csv", show_col_types = FALSE) %>% rename(UniqueID = flux_id, plot = site)
sw <- aux %>% transmute(UniqueID, collar_location, T_water = water_temp_C)
clos <- read_csv("output/flux/03_fit/CH4/fluxes.csv", show_col_types = FALSE) %>% select(UniqueID, F_ch4 = best.flux) %>%
  full_join(read_csv("output/flux/03_fit/CO2/fluxes.csv", show_col_types = FALSE) %>% select(UniqueID, F_co2 = best.flux), by = "UniqueID") %>%
  inner_join(aux %>% filter(component == "water") %>% select(UniqueID, plot, date), by = "UniqueID") %>%
  left_join(sw, by = "UniqueID") %>%
  mutate(date = as.Date(date), campaign = camp_of(date), position = water_position(collar_location)) %>% filter(!is.na(campaign))
chamber <- clos %>% filter(!is.na(F_ch4)) %>% group_by(plot, campaign, position) %>%
  summarise(n_chamber = n(), F_chamber = mean(F_ch4), F_co2_chamber = mean(F_co2, na.rm = TRUE),
            chamber_date = median(date), T_chamber = mean(T_water, na.rm = TRUE), .groups = "drop")

# ---- salinity / temperature for the plot samples ----------------------------------------------
camp_T <- c("Oct 2022" = 28, "Mar 2023" = 26)
sal <- read.csv("data/environmental/site_characterization_salinity_ch4.csv") %>% filter(sample_type == "surface_water") %>%
  mutate(campaign = case_when(grepl("Oct 2022", season) ~ "Oct 2022", grepl("Mar 2023", season) ~ "Mar 2023")) %>%
  filter(!is.na(campaign), is.finite(PSU_mean)) %>% select(plot = site, campaign, S_meas = PSU_mean)
h2o <- read.csv("data/environmental/NASA BLueFlux Data Sheets_h2o.csv", check.names = FALSE, fileEncoding = "UTF-8-BOM")
names(h2o)[1] <- "Date"; h2o <- h2o[, names(h2o) != "" & !is.na(names(h2o))]
h2o <- h2o %>% filter(grepl("surf", Measurement, ignore.case = TRUE)) %>%
  transmute(plot = gsub(" ", "", Plot), date = as.Date(Date, "%m/%d/%y"), T_sheet = suppressWarnings(as.numeric(temp))) %>%
  filter(T_sheet >= 18, T_sheet <= 35) %>% mutate(campaign = camp_of(date)) %>%
  group_by(plot, campaign) %>% summarise(T_sheet = mean(T_sheet), .groups = "drop")
T_cham <- clos %>% group_by(plot, campaign) %>% summarise(T_ch = mean(T_water, na.rm = TRUE), .groups = "drop") %>% filter(is.finite(T_ch))

# ---- dissolved CH4 and CO2 ---------------------------------------------------------------------
gc_raw <- read_csv("data/environmental/dissolved_gas/dissolved_gas_all_observations.csv", show_col_types = FALSE) %>%
  filter(sample_type == "surface_water", source == "GC") %>%
  mutate(campaign = ifelse(grepl("2022", season), "Oct 2022", "Mar 2023")) %>%
  group_by(site, campaign) %>% mutate(failed = CH4_uM < REP_MIN_FRAC * median(CH4_uM)) %>% ungroup()
cat("Failed GC replicates dropped:\n"); print(as.data.frame(gc_raw %>% filter(failed) %>% select(site, campaign, real_date, CH4_uM)))
gc <- gc_raw %>% filter(!failed) %>%
  group_by(plot = site, campaign) %>%
  summarise(n_samples = n(), Cw_nM = mean(CH4_uM) * 1e3, Cw_co2_uM = mean(CO2_uM), sample_date = min(as.Date(real_date)), .groups = "drop") %>%
  left_join(sal, by = c("plot", "campaign")) %>% left_join(T_cham, by = c("plot", "campaign")) %>%
  left_join(h2o, by = c("plot", "campaign")) %>%
  mutate(S = coalesce(S_meas, 15), T_C = coalesce(T_ch, T_sheet, unname(camp_T[campaign])),
         TS_source = paste0(ifelse(is.na(S_meas), "S typical", "S measured"), ", ",
                            ifelse(!is.na(T_ch), "T chamber", ifelse(!is.na(T_sheet), "T datasheet", "T typical"))),
         position = "above forest floor", source = "own GC, plot surface water") %>%
  select(plot, campaign, position, source, sample_date, n_samples, T_C, S, TS_source, Cw_nM, Cw_co2_uM)
STATION <- c("SRS 5" = "SRS5", "SRS 6" = "SRS6", "SRS 6 Tidal Creek" = "SRS6", "SRS 6 Tidal Creek 2" = "SRS6", "SRS 6 Tidal Creek 3" = "SRS6")
tr_all <- read.csv("data/environmental/aquatic/ORNL_DAAC_2333_BLUEFLUX_Transect_Shark_Haney_Rivers_TarponBay.csv",
                   fileEncoding = "UTF-8-BOM", na.strings = "-9999") %>%
  filter(site %in% names(STATION), is.finite(pCH4)) %>%
  transmute(station = site, plot = unname(STATION[site]), sample_date = as.Date(date), campaign = camp_of(sample_date),
            position = ifelse(grepl("Creek", site), "above forest floor", "channel / open water"),
            S_raw = salinity, T_raw = temp, pCH4, pCO2) %>% filter(!is.na(campaign))
# creek samples without sonde values take the same-day river station's
tr_all <- tr_all %>% group_by(plot, sample_date) %>%
  mutate(S = coalesce(S_raw, first(na.omit(S_raw))), T_C = coalesce(T_raw, first(na.omit(T_raw)))) %>% ungroup() %>%
  group_by(plot, campaign) %>% mutate(S = coalesce(S, mean(S_raw, na.rm = TRUE)), T_C = coalesce(T_C, mean(T_raw, na.rm = TRUE))) %>% ungroup() %>%
  mutate(n_samples = 1L, Cw_nM = pCH4 * 1e-6 * K0(T_C, S) * 1e9, Cw_co2_uM = pCO2 * 1e-6 * K0_co2(T_C, S) * 1e6,
         TS_source = ifelse(is.na(S_raw), "S, T from same-day station", "sonde"),
         source = paste0("ORNL DAAC 2333, ", station, " (", sample_date, ")"))
dis_all <- bind_rows(gc, tr_all %>% select(names(gc))) %>%
  mutate(dC_nM = Cw_nM - Ceq_nM(T_C, S), dC_uM = Cw_co2_uM - ATM_CO2_UATM * 1e-6 * K0_co2(T_C, S) * 1e6)

# ---- k calibration: pair each chamber group with dissolved CH4 from its position ----------------
pick <- function(g) {                                    # g: one chamber group (plot, campaign, position, chamber_date)
  d <- dis_all %>% filter(plot == g$plot, campaign == g$campaign)
  if (!nrow(d)) return(NULL)
  same <- d %>% filter(position == g$position)
  matched <- nrow(same) > 0
  if (matched) d <- same
  d <- d %>% mutate(lag = abs(as.numeric(sample_date - g$chamber_date))) %>%
    filter(grepl("^own GC", source) | lag <= 1 | !any(lag <= 1)) %>% filter(lag == min(lag)) %>% slice(1)
  d %>% mutate(position_matched = matched, chamber_position = g$position)
}
cal <- bind_rows(lapply(seq_len(nrow(chamber)), function(i) { g <- chamber[i, ]
  p <- pick(g); if (is.null(p)) return(NULL)
  bind_cols(g %>% select(n_chamber, F_chamber, F_co2_chamber, chamber_date), p) })) %>%
  mutate(k600_cm_h = k_from(F_chamber, dC_nM, T_C), used = k600_cm_h > 0 & k600_cm_h <= K600_MAX) %>%
  select(plot, campaign, chamber_position, position_matched, source, sample_date, chamber_date, T_C, S, TS_source,
         n_samples, Cw_nM, dC_nM, n_chamber, F_chamber, F_co2_chamber, Cw_co2_uM, dC_uM, k600_cm_h, used)
write_csv(cal, "output/flux/03_fit/water_k_calibration.csv")
cls <- c(SRS5 = "intact", SRS6 = "intact", BL60 = "regenerating", CP40 = "ghost", FLM30 = "ghost", SE1 = "scrub")
k_site <- cal %>% filter(used) %>% group_by(plot) %>% summarise(k600 = median(k600_cm_h), n_pairs = n(), .groups = "drop") %>%
  mutate(class = cls[plot])
k_class <- k_site %>% group_by(class) %>% summarise(k600 = median(k600), .groups = "drop")
k_all <- median(k_site$k600); k_lo <- min(k_site$k600); k_hi <- max(k_site$k600)
write_csv(bind_rows(k_site %>% transmute(level = "site", name = plot, k600, n_pairs),
                    k_class %>% transmute(level = "class", name = class, k600),
                    tibble(level = "all", name = "median of site medians", k600 = k_all)),
          "output/flux/03_fit/water_k_by_site.csv")

# ---- estimates for plot x campaign without chamber water flux ------------------------------
targets <- tibble(plot = c("SRS5", "SRS6"), campaign = "Oct 2022",
                  prefer = c("own GC, plot surface water", "ORNL DAAC 2333, SRS 6 ("))
tg <- targets %>% inner_join(dis_all, by = c("plot", "campaign")) %>% filter(startsWith(source, prefer)) %>%
  group_by(plot, campaign) %>% slice(1) %>% ungroup() %>%
  anti_join(chamber %>% distinct(plot, campaign), by = c("plot", "campaign")) %>%
  left_join(k_site %>% select(plot, k_own = k600), by = "plot") %>%
  mutate(k_class = k_class$k600[match(cls[plot], k_class$class)], k_all = k_all)
mk <- function(gas, Ffun, dC, unit, conc, meth) tg %>%
  transmute(site = plot, campaign, component = "water",
            flux_rate = Ffun(k_all, .data[[dC]], T_C), ci_lo = Ffun(k_lo, .data[[dC]], T_C), ci_hi = Ffun(k_hi, .data[[dC]], T_C),
            flux_k_site = Ffun(k_own, .data[[dC]], T_C), flux_k_class = Ffun(k_class, .data[[dC]], T_C),
            Cw_nM = round(.data[[conc]], 1), dissolved_source = source, T_C, S,
            k600_used_cm_h = round(k_all, 2), k600_site_cm_h = round(k_own, 2), k600_class_cm_h = round(k_class, 2),
            k600_range_cm_h = sprintf("%.2f-%.2f", k_lo, k_hi), n_k_sites = nrow(k_site), method = meth,
            gas = gas, unit = unit)
est <- bind_rows(
  mk("CH4", F_from, "dC_nM", "nmol m-2 s-1", "Cw_nM", "F = k600 (Sc/600)^-0.5 (Cw - Ceq); k600 = median of site medians of position-matched chamber/dissolved pairs"),
  mk("CO2", F_co2, "dC_uM", "umol m-2 s-1", "Cw_co2_uM", "F = k600 (Sc_CO2/600)^-0.5 (Cw - Ceq), K0 Weiss 1974; k600 from CH4 pairs") %>%
    mutate(Cw_nM = Cw_nM * 1e3))
write_csv(est, "output/flux/03_fit/water_flux_estimates.csv")

# ---- cross-check: every plot x campaign with dissolved CH4, leave-site-out k ------------------
dis_pc <- dis_all %>% group_by(plot, campaign) %>% slice(1) %>% ungroup()
xc <- dis_all %>% left_join(chamber %>% group_by(plot, campaign) %>%
                              summarise(F_chamber = weighted.mean(F_chamber, n_chamber), n_chamber = sum(n_chamber), .groups = "drop"),
                            by = c("plot", "campaign")) %>% rowwise() %>%
  mutate(k_loo = median(k_site$k600[k_site$plot != plot]),
         F_dissolved = F_from(k_loo, dC_nM, T_C),
         F_dissolved_lo = F_from(k_lo, dC_nM, T_C), F_dissolved_hi = F_from(k_hi, dC_nM, T_C)) %>% ungroup() %>%
  transmute(plot, campaign, position, dissolved_source = source, n_samples, Cw_nM = round(Cw_nM, 1), T_C, S,
            k600_loo_cm_h = round(k_loo, 2), F_dissolved, F_dissolved_lo, F_dissolved_hi,
            n_chamber, F_chamber, ratio_chamber_to_dissolved = F_chamber / F_dissolved)
write_csv(xc, "output/flux/03_fit/water_flux_dissolved_crosscheck.csv")

cat("k calibration (k600, cm h-1):\n")
print(as.data.frame(cal %>% transmute(plot, campaign, pos = substr(chamber_position, 1, 7), matched = position_matched,
                                      source = substr(source, 1, 38), T_C = round(T_C, 1), S = round(S, 1), Cw_nM = round(Cw_nM, 1),
                                      n_chamber, F_chamber = round(F_chamber, 2), k600 = round(k600_cm_h, 2), used)), row.names = FALSE)
cat("\nk600 by site:\n"); print(as.data.frame(k_site %>% mutate(k600 = round(k600, 2))), row.names = FALSE)
cat("by class:\n"); print(as.data.frame(k_class %>% mutate(k600 = round(k600, 2))), row.names = FALSE)
cat(sprintf("median of site medians %.2f; range %.2f-%.2f\n", k_all, k_lo, k_hi))
cat("\nEstimates:\n")
print(as.data.frame(est %>% select(site, campaign, gas, flux_rate, ci_lo, ci_hi, flux_k_site, flux_k_class, Cw_nM, k600_used_cm_h, dissolved_source) %>%
                      mutate(across(c(flux_rate, ci_lo, ci_hi, flux_k_site, flux_k_class), ~ signif(.x, 3)))), row.names = FALSE)
