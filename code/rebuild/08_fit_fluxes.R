# =============================================================================
# Fit CH4 and CO2 fluxes for every closure (handoff work plan, step 4).
#
# Inputs: output/rebuild/auxfile.csv (geometry, Tcham, Pcham), windows.csv
# (fit windows), raw analyzer records (lib_raw.R).
#
# Path per gas: fluxqc::process_fluxes() = goFlux::goFlux() -> best.flux() ->
# flag_detection() -> qc_screens(); outputs written with write_outputs().
# Conventions (Jon, 2026-10-01; ch4-data-filtering WORKLOG_2026-09):
#   - Tcham = tower air temperature, Pcham = tower pressure (auxfile);
#   - no H2O dilution correction: H2O_ppm set to 0 (LGR H2O channel reads
#     negative); fluxes are on the analyzers' dry mole fractions;
#   - instrument precision for goFlux's own MDF: LGR GLA131 0.35 ppm CO2 /
#     0.9 ppb CH4 (as legacy); Picarro G4301 goFlux defaults;
#   - empirical precision sigma = MAD(dx)/sqrt(2) per analyzer x campaign
#     ("group"), MDF = 1.96 sigma / t * flux.term, t = closure seconds;
#   - best.flux criteria as the legacy scripts (all ten, g.limit 2, p 0.05,
#     k.ratio 1);
#   - HM only with >= HM_MIN_OBS points in the window; otherwise the LM
#     estimate is used (replaces the legacy Mar 2022 forced-LM patch);
#   - CO2 fitted first and passed to the CH4 co2_tracer screen; that screen
#     is set to NA for water, leaves and dead wood (no respiratory CO2).
# Nothing is deleted; rows are flagged. Each closure is fitted on a padded
# trace (window +- PAD_S) so the MAD precision sees shoulders as well.
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(lubridate); library(purrr); library(fluxqc)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/rebuild/lib_raw.R")

PAD_S <- 300; HM_MIN_OBS <- 30
PREC <- list(LGR = c(CO2dry_ppm = 0.35, CH4dry_ppb = 0.9), Picarro = c(CO2dry_ppm = 0.025, CH4dry_ppb = 0.1))
BEST_ARGS <- list(criteria = c("MAE", "RMSE", "AICc", "SE", "g.factor", "kappa", "MDF", "nb.obs", "intercept", "p-value"),
                  intercept.lim = NULL, g.limit = 2, p.val = 0.05, k.ratio = 1, warn.length = 20)
NO_CO2_TRACER <- c("water", "leaves", "leaf", "cwd")
# ambient_start is off: it compares the window start with the pre-closure
# ambient at 3x the 1 Hz Allan noise, but BlueFlux windows start after a dead
# band by design and the shoulder before start.time often holds the previous
# closure's tail, so it fired on ~95% of CO2 closures without information.
QC <- list(c0 = TRUE, co2_tracer = TRUE, convex = TRUE, min_window = TRUE, ambient_start = FALSE, noisy = TRUE)

utc <- function(x) as.POSIXct(x, tz = "UTC")
aux <- read_csv("output/rebuild/auxfile.csv", show_col_types = FALSE)
win <- read_csv("output/rebuild/windows.csv", show_col_types = FALSE) %>%
  filter(window_source != "none") %>%
  left_join(aux %>% select(UniqueID, Area, Vcham, Vtot, Tcham, Pcham), by = "UniqueID") %>%
  filter(!is.na(Area), !is.na(Vtot)) %>%
  mutate(group = paste(analyzer, campaign), start = utc(start), end = utc(end),
         rec_start = utc(field_start) + offset_s,
         rec_end = if_else(!is.na(field_end), utc(field_end) + offset_s, end))

# ---- observation windows from the raw records ------------------------------------------
build_ow <- function(i) {
  w <- win[i, ]
  tr <- read_raw(w$analyzer, w$start - PAD_S, w$end + PAD_S)
  if (is.null(tr) || sum(tr$POSIX.time >= w$start & tr$POSIX.time <= w$end) < 5) return(NULL)
  inst <- if (grepl("^LGR", w$analyzer)) "LGR" else "Picarro"
  tr %>% mutate(UniqueID = w$UniqueID, H2O_ppm = 0,
                CO2_prec = PREC[[inst]][["CO2dry_ppm"]], CH4_prec = PREC[[inst]][["CH4dry_ppb"]], H2O_prec = 0,
                start.time = coalesce(w$rec_start, w$start), end.time = pmax(coalesce(w$rec_end, w$end), w$start + 1),
                Area = w$Area, offset = 0, Vtot = w$Vtot, Vcham = w$Vcham, Tcham = w$Tcham, Pcham = w$Pcham,
                group = w$group) %>%
    mutate(obs.length = as.numeric(end.time - start.time, units = "secs")) %>%
    select(-source_file) %>% as.data.frame()
}
cat("Building observation windows for", nrow(win), "closures...\n")
ow <- map(seq_len(nrow(win)), build_ow)
missing_raw <- win$UniqueID[vapply(ow, is.null, TRUE)]
ow <- compact(ow); names(ow) <- vapply(ow, function(d) d$UniqueID[1], "")
manID <- suppressWarnings(windows_from_table(ow, win %>% filter(UniqueID %in% names(ow)) %>%
                                               select(UniqueID, start, end), warn.length = 10))
cat("  closures with traces:", length(ow), "| no raw data in window:", length(missing_raw), "\n")

aux_grp <- win %>% select(UniqueID, group, component)

# ---- HM minimum points, then re-derive the detection flags -------------------------------------
apply_hm_min <- function(fx) {
  use_lm <- fx$model == "HM" & !is.na(fx$nb.obs) & fx$nb.obs < HM_MIN_OBS
  fx$hm_min_obs_rule <- use_lm
  fx$best.flux[use_lm] <- fx$LM.flux[use_lm]
  fx$model[use_lm] <- "LM"
  fx$below_MDF_emp <- abs(fx$best.flux) <= fx$MDF_emp
  fx$detected_emp <- !fx$below_MDF_emp
  fx$det_class_emp <- ifelse(fx$below_MDF_emp, "below detection", ifelse(fx$best.flux > 0, "emission", "uptake"))
  fx
}

fit_gas <- function(gas, co2 = NULL) {
  res <- suppressWarnings(process_fluxes(manID, aux = aux_grp, gastype = gas, group = "group", co2 = co2, qc = QC,
                                         H2O_col = "H2O_ppm", best.flux_args = BEST_ARGS))
  res$fluxes <- apply_hm_min(res$fluxes) %>%
    left_join(aux_grp %>% select(UniqueID, component), by = "UniqueID")
  if ("qc_co2_tracer" %in% names(res$fluxes)) {
    res$fluxes$qc_co2_tracer[tolower(res$fluxes$component) %in% NO_CO2_TRACER] <- NA
    qc_cols <- grep("^qc_(?!any)", names(res$fluxes), value = TRUE, perl = TRUE)
    qc_cols <- qc_cols[vapply(res$fluxes[qc_cols], is.logical, TRUE)]
    res$fluxes$qc_any <- Reduce(`|`, lapply(res$fluxes[qc_cols], function(v) coalesce(v, FALSE)))
  }
  res$settings$hm_min_obs <- HM_MIN_OBS
  res$settings$h2o_correction <- "none (H2O_ppm = 0; LGR H2O channel reads negative)"
  res$settings$instrument_prec <- PREC
  res$settings$co2_tracer_off_for <- NO_CO2_TRACER
  res$settings$window_pad_s <- PAD_S
  res$settings$qc_note <- "ambient_start off: windows start after a dead band by design"
  res$settings$no_raw_data <- missing_raw
  res
}

cat("Fitting CO2...\n"); co2 <- fit_gas("CO2dry_ppm")
write_outputs(co2, "output/rebuild/fit/CO2", plots = FALSE)
cat("Fitting CH4...\n"); ch4 <- fit_gas("CH4dry_ppb", co2 = co2$fluxes)
write_outputs(ch4, "output/rebuild/fit/CH4", plots = FALSE)

for (g in list(co2, ch4)) {
  f <- g$fluxes
  cat("\n==", g$gastype, ":", nrow(f), "closures\n")
  print(table(model = f$model, hm_min_rule = f$hm_min_obs_rule))
  print(table(f$det_class_emp, useNA = "ifany"))
  cat("qc_any:", sum(f$qc_any, na.rm = TRUE), "\n")
  print(colSums(f[grep("^qc_", names(f))] == TRUE, na.rm = TRUE))
}
