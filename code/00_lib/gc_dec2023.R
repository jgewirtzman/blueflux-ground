# =============================================================================
# Headspace mole fractions for the October 2022 and March 2023 dissolved-gas vials,
# from the Yale GC run of 18-19 December 2023 (GC Run_Dec_2023_Peterman_Gewirtzman.xlsx).
#   Each vial is read from the raw channel-1 file whose number equals its sequence
#   position in the run plan ("Run 1 Plan", "Run 2 Plan"). The workbook's "Compiled"
#   sheets are not used: in Run 2 they pair every position with the next file (the
#   blank that follows it), so standards and samples there read as blanks. Peak areas
#   are taken by analyte label (in vials with no CH4 peak, the CO2 peak sits in the
#   first analyte slot).
#   Calibration, per run, against the gravimetric standards SB1, SB3, SB4, SB5 and their
#   N2 dilutions, injected at the start and end of the run (drift between the two sets
#   <= 4% above SB1, no drift correction). CH4: power law, log(ppm) = a + b log(area),
#   chosen by leave-one-standard-out error (RMS 5.9%, vs 7.4% for the weighted-linear fit
#   used in the tree-methanogens pipeline and 7.0% for a weighted quadratic). CO2: weighted
#   quadratic, weights 1/(conc + 50)^2, as in the tree-methanogens pipeline (SB5_5:15,
#   off-curve, and N2 excluded: one Run 1 N2 injection carries a CO2 peak of area 58,
#   against 68 for SB1). No clamp: the detection limit is
#   3 SD of the back-predicted SB1 injections (both runs), and values are flagged, not
#   overwritten. Vials above the SB5 area (5029 ppm CH4, 10080 ppm CO2) rest on
#   extrapolation and are flagged; for CO2 (most porewater, 10-20x SB5) the extrapolation is
#   a straight line through the SB4 and SB5 means, not the quadratic, and is indicative only.
#   The instrument's own concentration column (an old factory calibration) is not used.
# gc_dec2023() returns one row per planned Everglades vial: run, seq, sample_id,
#   replicate, notes, date (sample date on the plan), area_CH4, area_CO2, CH4_ppm,
#   CO2_ppm, CH4_below_lod, CH4_above_std, CO2_above_std. attr(, "cal") holds the fits and LODs.
# =============================================================================
gc_dec2023 <- function(f = "data/environmental/porewater_gas/GC Run_Dec_2023_Peterman_Gewirtzman (1).xlsx") {
  suppressMessages({ library(readxl); library(dplyr) })
  r <- suppressMessages(read_excel(f, "Raw Channel 1", col_names = FALSE, col_types = "text"))
  num <- function(x) suppressWarnings(as.numeric(x))
  raw <- data.frame(seq = num(sub(".*s([0-9]+)[.]CHR$", "\\1", r[[1]])), a1 = r[[4]], area1 = num(r[[6]]),
                    a2 = r[[9]], area2 = num(r[[11]])) %>%
    filter(!is.na(seq)) %>%
    transmute(seq, area_CH4 = ifelse(a1 == "CH4", area1, 0),
              area_CO2 = ifelse(a1 == "CO2", area1, ifelse(a2 == "CO2", area2, NA)))
  plan <- bind_rows(lapply(c("Run 1 Plan", "Run 2 Plan"), function(s)
    suppressMessages(read_excel(f, s, col_types = "text")) %>% mutate(run = sub(" Plan", "", s)))) %>%
    transmute(run, seq = num(`Sequence Run`), sample_id = `Sample ID`, project = Project,
              replicate = Replicate, notes = Notes, date = as.Date(num(Date), origin = "1899-12-30")) %>%
    filter(!is.na(seq)) %>% left_join(raw, by = "seq")
  # standard mole fractions (ppm): stock x mL stock / 20 mL ("Standard Concentrations" sheet)
  std <- data.frame(sample_id = c("N2", "SB1", "SB3_5:15", "SB3_10:10", "SB3", "SB4_10:10", "SB4", "SB5_5:15", "SB5"),
                    CH4 = c(0, 0.9763, 5.16, 10.32, 20.64, 100.7, 201.4, 1257.25, 5029),
                    CO2 = c(0, 152.4, 503.25, 1006.5, 2013, 2483.5, 4967, NA, 10080))
  s <- plan %>% inner_join(std, by = "sample_id")
  fits <- lapply(split(s, s$run), function(d) list(
    ch4 = lm(log(CH4) ~ log(area_CH4), d[d$sample_id != "N2", ]),
    co2 = lm(CO2 ~ area_CO2 + I(area_CO2^2), d[!is.na(d$CO2) & d$sample_id != "N2", ], weights = 1 / (CO2 + 50)^2),
    top = max(d$area_CH4[d$sample_id == "SB5"]),
    top2 = d %>% filter(sample_id %in% c("SB4", "SB5")) %>% group_by(sample_id) %>%
      summarise(a = mean(area_CO2), c = first(CO2)) %>% arrange(c)))
  pr <- function(d, g) unlist(lapply(seq_len(nrow(d)), function(i) { v <- predict(fits[[d$run[i]]][[g]], d[i, ])
    if (g == "ch4") ifelse(d$area_CH4[i] > 0, exp(v), 0) else v }))
  sb1 <- s %>% filter(sample_id == "SB1")
  lod <- c(CH4 = 3 * sd(pr(sb1, "ch4")), CO2 = 3 * sd(pr(sb1, "co2")))
  out <- plan %>% filter(project == "Everglades") %>%
    mutate(CH4_ppm = pr(., "ch4"), CO2_ppm = pr(., "co2"),
           CO2_above_std = area_CO2 > sapply(run, function(x) fits[[x]]$top2$a[2]),
           # above the SB5 area the quadratic is not used: straight line through the SB4 and SB5 means
           CO2_ppm = ifelse(CO2_above_std, mapply(function(x, a) { t <- fits[[x]]$top2
             t$c[2] + (a - t$a[2]) * diff(t$c) / diff(t$a) }, run, area_CO2), CO2_ppm),
           CH4_below_lod = CH4_ppm < lod[["CH4"]],
           CH4_above_std = area_CH4 > sapply(run, function(x) fits[[x]]$top)) %>%
    select(run, seq, sample_id, replicate, notes, date, area_CH4, area_CO2, CH4_ppm, CO2_ppm, CH4_below_lod, CH4_above_std, CO2_above_std)
  attr(out, "cal") <- list(fits = fits, lod = lod,
                           check = s %>% filter(sample_id != "N2") %>%
                             mutate(CH4_pred = pr(., "ch4"), CO2_pred = pr(., "co2"),
                                    CH4_err = CH4_pred / CH4 - 1, CO2_err = CO2_pred / CO2 - 1))
  out
}

# Dissolved concentration (umol L-1) in the original water from the equilibrated headspace
# mole fraction: 180 mL water + 20 mL ambient-air headspace, shaken to equilibrium at 25 C
# and 1 atm. Mass balance: Cw Vw + x_air Vg/RT = x_eq (Vg/RT + KH Vw), so the CH4 and CO2
# the air brought into the vial (1.95 and 420 ppm) are subtracted.
# KH (mol L-1 atm-1): CH4 1.4e-3, CO2 3.4e-2.
headspace_dissolved_uM <- function(ppm, gas = c("CH4", "CO2"), Vw = 0.180, Vg = 0.020, T = 25, P = 1) {
  gas <- match.arg(gas)
  KH <- c(CH4 = 1.4e-3, CO2 = 3.4e-2)[[gas]]; x_air <- c(CH4 = 1.95, CO2 = 420)[[gas]]
  RT <- 0.082057 * (T + 273.15)
  ((ppm / 1e6 * P) * (Vg / RT + KH * Vw) - (x_air / 1e6 * P) * Vg / RT) / Vw * 1e6
}

# Failed vials: a replicate below `frac` of its location's median (locations with >= 3 vials),
# the same rule as the water-flux step (REP_MIN_FRAC in 03_fit/02_water_flux_from_dissolved.R).
drop_failed_vials <- function(d, ..., value = CH4_uM, frac = 0.3) {
  d %>% dplyr::group_by(...) %>% dplyr::filter(dplyr::n() < 3 | {{ value }} >= frac * stats::median({{ value }})) %>% dplyr::ungroup()
}
