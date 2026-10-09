# =============================================================================
# Fit CH4 and CO2 fluxes for every closure (handoff work plan, step 4).
#
# Inputs: data/inputs/closures.csv (fit windows, geometry, Tcham, Pcham; written by
# hygiene/02_windows/03_export_inputs.R), analyzer records (lib_raw.R).
#
# Path per gas: goFlux::process.fluxes() = goFlux() -> best.flux() ->
# flux.class() -> qc.flags() (+ co2.tracer()); outputs written with
# write.outputs(). goFlux is the fork release v0.5.0.9002 (goFlux (Rheault et
# al. 2024) version 0.5.0.9002 with additions, doi:10.5281/zenodo.23256675),
# from a project library (code/00_lib/goflux_release.R); it replaces fluxqc.
# Conventions (Jon, 2026-10-01; ch4-data-filtering WORKLOG_2026-09):
#   - Tcham = tower air temperature, Pcham = tower pressure (auxfile);
#   - no H2O dilution correction: H2O_ppm set to 0 (LGR H2O channel reads
#     negative); fluxes are on the analyzers' dry mole fractions;
#   - instrument precision for goFlux's own MDF: LGR GLA131 0.35 ppm CO2 /
#     0.9 ppb CH4 (as legacy); Picarro G4301 goFlux defaults;
#   - empirical precision sigma = MAD(dx)/sqrt(2) per analyzer x campaign
#     ("group"), MDF = z sigma / t * flux.term with z = 1.96 (conf = 0.95; a
#     benchmark multiplier, not a calibrated 95% test) and t = closure.time()
#     (span of the window + one logging interval).
#     The first differences dx are taken within each closure's fit window and
#     centred on that closure's own median dx before pooling, so the closures'
#     different slopes do not enter sigma (fluxqc 0.2.3 pools uncentred dx,
#     which at 6-10 s steps inflates sigma ~1.4-2x: code/qa/sigma_pooling_check.R;
#     reported as jgewirtzman/fluxqc#1, the custom-sigma MDF grouping as #2).
#     that sigma is passed to flux.class() as prec, one value per group;
#   - best.flux criteria as the legacy scripts (all ten, g.limit 2, p 0.05,
#     k.ratio 1);
#   - HM only with >= HM_MIN_OBS points in the window; otherwise the LM
#     estimate is used (replaces the legacy Mar 2022 forced-LM patch);
#   - QC screens (flags only), all from qc.flags(): starting concentration
#     (C0 > 1.5 x group median), curvature (significant quadratic term of the
#     same sign as the net change, p < 0.05), minimum window (closure.time()
#     < 60 s) and noise (closure precision > 1.5 x group);
#   - CO2 fitted first; the CH4 co2_tracer screen fires when the CO2 flux is not
#     positive and significant (!co2.tracer()); NA for water, leaves and dead
#     wood (no respiratory CO2).
# Output columns keep the names read downstream (sigma_emp, MDF_emp,
# det_class_emp, qc_*); goFlux's det.* / qc.* columns are renamed to them.
# Nothing is deleted; rows are flagged. Each closure is fitted on a padded
# trace (window +- PAD_S) so the MAD precision sees shoulders as well.
# =============================================================================
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/00_lib/goflux_release.R"); goflux_release()
suppressMessages({library(dplyr); library(readr); library(lubridate); library(purrr); library(goFlux)})
source("code/00_lib/lib_raw.R")
OUT_DIR <- Sys.getenv("FIT_OUT", "output/flux/03_fit")

PAD_S <- 300; HM_MIN_OBS <- 30
PREC <- list(LGR = c(CO2dry_ppm = 0.35, CH4dry_ppb = 0.9), Picarro = c(CO2dry_ppm = 0.025, CH4dry_ppb = 0.1))
BEST_ARGS <- list(criteria = c("MAE", "RMSE", "AICc", "SE", "g.factor", "kappa", "MDF", "nb.obs", "intercept", "p-value"),
                  intercept.lim = NULL, g.limit = 2, p.val = 0.05, k.ratio = 1, warn.length = 20)
NO_CO2_TRACER <- c("water", "leaves", "leaf", "cwd")
# The ambient check is off: the shoulder before the recorded closure start often
# holds the previous closure's tail (back-to-back closures), so the pre-closure
# record is not ambient. min.obs (a count of points) is off; min.secs is used.
QC <- list(c0.mult = 1.5, min.obs = NULL, min.secs = 60, convex.p = 0.05, ambient.sigma = NULL, noisy.mult = 1.5)

utc <- function(x) as.POSIXct(x, tz = "UTC")
win <- read_csv("data/inputs/closures.csv", show_col_types = FALSE) %>%
  filter(window_source != "none") %>%
  transmute(UniqueID = flux_id, analyzer, campaign, component, start = window_start, end = window_end,
            field_start, field_end, offset_s = clock_offset_s, Area = area_cm2, Vcham = chamber_volume_cm3,
            Vtot = total_volume_L, Tcham = air_temp_C, Pcham = pressure_kPa) %>%
  filter(!is.na(Area), !is.na(Vtot)) %>%
  mutate(group = paste(analyzer, campaign), start = utc(start), end = utc(end),
         rec_start = utc(field_start) + offset_s,
         rec_end = if_else(!is.na(field_end), utc(field_end) + offset_s, end))

# ---- observation windows from the raw records ------------------------------------------
build_ow <- function(i, fresh = c("CO2", "CH4")) {
  fresh <- match.arg(fresh)
  w <- win[i, ]
  tr <- read_raw(w$analyzer, w$start - PAD_S, w$end + PAD_S)
  # Picarro: each gas's fresh readings only (alternate rows; lib_raw.R fresh_ch4 / fresh_co2)
  if (!is.null(tr)) tr <- tr[if (fresh == "CH4") fresh_ch4(tr, w$analyzer) else fresh_co2(tr, w$analyzer), ]
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
manID <- suppressWarnings(windows.from.table(ow, win %>% filter(UniqueID %in% names(ow)) %>%
                                               select(UniqueID, start, end), warn.length = 10))
ow_ch4 <- compact(map(seq_len(nrow(win)), build_ow, fresh = "CH4"))
names(ow_ch4) <- vapply(ow_ch4, function(d) d$UniqueID[1], "")
manID_ch4 <- suppressWarnings(windows.from.table(ow_ch4, win %>% filter(UniqueID %in% names(ow_ch4)) %>%
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

# centred empirical precision per group: dx within each closure's window, minus
# that closure's median dx, pooled over the group; MAD / sqrt(2)
centred_sigma <- function(traces, gas) {
  tr <- traces[!is.na(traces$flag) & traces$flag == 1, c("UniqueID", "group", gas)]
  by_id <- split(tr[[gas]], tr$UniqueID)
  dx <- lapply(by_id, function(v) { d <- diff(v); d - stats::median(d) })
  g <- tr$group[match(names(by_id), tr$UniqueID)]
  s <- tapply(unlist(dx), rep(g, lengths(dx)), function(v) stats::mad(v) / sqrt(2))
  data.frame(UniqueID = names(by_id), prec = as.numeric(s[g]), group = g)
}

fit_gas <- function(gas, co2 = NULL, man = manID) {
  sig <- centred_sigma(man, gas)
  res <- suppressWarnings(process.fluxes(man, gastype = gas, auxfile = aux_grp, by = "group",
                                         prec = sig[c("UniqueID", "prec")], conf = 0.95, qc = QC,
                                         best.flux.args = BEST_ARGS, H2O_col = "H2O_ppm"))
  fx <- res$fluxes
  fx <- fx %>% mutate(
    sigma_emp = det.prec, MDF_emp = det.MDF,
    MDF_emp_method = "z sigma / t * flux.term; z = 1.96 (conf 0.95); t = closure.time(); sigma = centred MAD(dx)/sqrt(2) per group",
    qc_c0_ratio = qc.c0.ratio, qc_c0 = qc.c0,
    qc_co2_tracer = if (is.null(co2)) NA else !co2.tracer(co2, fx),
    qc_convex = qc.convex, qc_min_window = qc.min.secs,
    qc_noisy_ratio = qc.noisy.ratio, qc_noisy = qc.noisy) %>%
    select(-starts_with("det."), -starts_with("qc."))
  fx <- apply_hm_min(fx) %>% left_join(aux_grp %>% select(UniqueID, component), by = "UniqueID")
  fx$qc_co2_tracer[tolower(fx$component) %in% NO_CO2_TRACER] <- NA
  qc_cols <- c("qc_c0", "qc_co2_tracer", "qc_convex", "qc_min_window", "qc_noisy")
  fx$qc_any <- Reduce(`|`, lapply(fx[qc_cols], function(v) coalesce(v, FALSE)))
  res$fluxes <- fx
  res$settings$sigma_method <- "custom: MAD/sqrt(2) of within-window first differences, centred per closure, pooled per analyzer x campaign"
  res$settings$sigma_by_group <- as.list(tapply(sig$prec, sig$group, function(v) v[1]))
  res$settings$co2_tracer_flag <- "!co2.tracer(): CO2 best.flux not > 0 with LM p < 0.05"
  res$settings$hm_min_obs <- HM_MIN_OBS
  res$settings$h2o_correction <- "none (H2O_ppm = 0; LGR H2O channel reads negative)"
  res$settings$instrument_prec <- PREC
  res$settings$co2_tracer_off_for <- NO_CO2_TRACER
  res$settings$window_pad_s <- PAD_S
  res$settings$picarro_rows <- if (gas == "CH4dry_ppb") paste0("fresh CH4 readings only (|dCH4| >= ", PICARRO_HELD_PPB, " ppb; every other row)") else "fresh CO2 readings only (the rows where CH4 is carried forward)"
  res$settings$qc_note <- "ambient check off: the pre-closure shoulder often holds the previous closure's tail"
  res$settings$no_raw_data <- missing_raw
  res
}

cat("Fitting CO2...\n"); co2 <- fit_gas("CO2dry_ppm")
write.outputs(co2, file.path(OUT_DIR, "CO2"), plots = FALSE)
cat("Fitting CH4...\n"); ch4 <- fit_gas("CH4dry_ppb", co2 = co2$fluxes, man = manID_ch4)
write.outputs(ch4, file.path(OUT_DIR, "CH4"), plots = FALSE)

for (g in list(co2, ch4)) {
  f <- g$fluxes
  cat("\n==", g$gastype, ":", nrow(f), "closures\n")
  print(table(model = f$model, hm_min_rule = f$hm_min_obs_rule))
  print(table(f$det_class_emp, useNA = "ifany"))
  cat("qc_any:", sum(f$qc_any, na.rm = TRUE), "\n")
  print(colSums(f[grep("^qc_", names(f))] == TRUE, na.rm = TRUE))
}
