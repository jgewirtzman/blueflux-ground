# =============================================================================
# Clock offset per analyzer x day (handoff work plan, step 3).
#
# Field-sheet closure start times are on a watch / phone / iPad clock; each
# analyzer stamps data with its own clock. The offset (analyzer time minus
# field-sheet time) is estimated per closure and summarised per analyzer-day:
#
#   1. prior: median offset of the saved manual windows for that analyzer-day,
#      else that analyzer-campaign, else 0 (LGR) / +25200 s (Picarro, ~7 h);
#   2. per closure: the CO2 rise is located with goFlux::find.rise() in the
#      analyzer record from (field start + prior - 2 min) to (field end +
#      prior + 2 min), with sample-count and gap limits scaled to the logging
#      interval; offset = rise start - field start;
#   3. per analyzer-day: median of the per-closure offsets; closures more than
#      120 s from it are flagged.
#
# goFlux::find.clock.offset() (one score over all closures of the day) was
# tried first: on most BlueFlux days its score curve is flat and the optimum
# lands 300-1900 s from the saved windows, because soil and water closures at
# high background and back-to-back closures give no clean onset.
#
# Writes output/flux/02_windows/rise_detection_closures.csv (per closure) and
# output/flux/02_windows/rise_detection_days.csv (per analyzer-day, with checks against the
# saved windows and the clock notes on the scanned sheets).
# =============================================================================
# (Run originally with fluxqc 0.2.3, now retired; ported to the goFlux fork
# release v0.5.0.9001, code/00_lib/goflux_release.R, whose find.rise() is the
# same function under a new name.)
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/00_lib/goflux_release.R"); goflux_release()
suppressMessages({library(dplyr); library(readr); library(lubridate); library(purrr); library(goFlux)})
source("code/00_lib/lib_raw.R")

PAD_S <- 120; MAX_DEV_S <- 120   # prior is good to ~30 s (LGR) / ~100 s (Picarro); wider pads catch the neighbouring closure

aux <- read_csv("output/flux/01_metadata/auxfile.csv", show_col_types = FALSE) %>%
  filter(!excluded, !is.na(start.time)) %>%
  mutate(start = as.POSIXct(start.time, tz = "UTC"), end = as.POSIXct(end.time, tz = "UTC"),
         campaign = recode(format(date, "%Y-%m"), "2022-03" = "Mar2022", "2022-10" = "Oct2022", "2023-03" = "Mar2023"))
saved_win <- read_csv("data/flux_metadata/saved_manual_windows.csv", show_col_types = FALSE)
notes <- read_csv("data/flux_metadata/clock_notes_from_scans.csv", show_col_types = FALSE)

saved_all <- saved_win %>% filter(!is.na(offset_from_fieldlog_s)) %>%
  select(UniqueID = flux_id, saved_minus_fieldlog_s = offset_from_fieldlog_s) %>%
  inner_join(aux %>% select(UniqueID, analyzer, date, campaign), by = "UniqueID")
prior_day  <- saved_all %>% group_by(analyzer, date) %>% summarise(p_day = median(saved_minus_fieldlog_s), .groups = "drop")
prior_camp <- saved_all %>% group_by(analyzer, campaign) %>% summarise(p_camp = median(saved_minus_fieldlog_s), .groups = "drop")
aux <- aux %>% left_join(prior_day, by = c("analyzer", "date")) %>%
  left_join(prior_camp, by = c("analyzer", "campaign")) %>%
  mutate(prior_s = coalesce(p_day, p_camp, if_else(analyzer == "Picarro", 25200, 0)))

detect <- function(unit, start, end, prior) {
  e <- if (is.na(end)) start + 600 else end
  tr <- read_raw(unit, start + prior - PAD_S, e + prior + PAD_S)
  if (is.null(tr) || nrow(tr) < 10) return(tibble(n_raw = 0L))
  dt <- median(diff(as.numeric(tr$POSIX.time)))
  try_gas <- function(gas, rise) {
    r <- find.rise(tr$POSIX.time, tr[[gas]], rise = rise, gap.secs = max(5, 2.5 * dt),
                   min.n = max(8, round(60 / dt)), min.dur = max(60, 6 * dt),
                   conc.range = if (gas == "CO2dry_ppm") c(300, 20000) else c(1000, 1e7))
    if (is.null(r)) NULL else tibble(gas = gas, rise_start = r$start, rise_end = r$end, rise_dconc = r$dconc)
  }
  r <- try_gas("CO2dry_ppm", 6)
  if (is.null(r)) r <- try_gas("CH4dry_ppb", 20)
  base <- tibble(n_raw = nrow(tr), dt_s = dt)
  if (is.null(r)) base else bind_cols(base, r)
}

cat("Detecting rises for", nrow(aux), "closures...\n")
det <- pmap_dfr(list(aux$analyzer, aux$start, aux$end, aux$prior_s), detect)
cl <- bind_cols(aux %>% select(UniqueID, analyzer, date, campaign, measurement_type, component, start, end, prior_s), det) %>%
  mutate(offset_closure_s = as.numeric(difftime(rise_start, start, units = "secs")))

day <- cl %>% group_by(analyzer, date) %>%
  summarise(n_closures = n(), n_detected = sum(!is.na(offset_closure_s)),
            prior_s = first(prior_s),
            offset_s = median(offset_closure_s, na.rm = TRUE),
            offset_mad_s = mad(offset_closure_s, na.rm = TRUE), .groups = "drop")
cl <- cl %>% left_join(day %>% select(analyzer, date, offset_day_s = offset_s), by = c("analyzer", "date")) %>%
  mutate(dev_from_day_s = offset_closure_s - offset_day_s,
         closure_flag = case_when(is.na(offset_closure_s) ~ "no rise detected",
                                  abs(dev_from_day_s) > MAX_DEV_S ~ "rise far from day offset",
                                  TRUE ~ NA_character_)) %>%
  left_join(saved_all %>% select(UniqueID, saved_minus_fieldlog_s), by = "UniqueID") %>%
  mutate(diff_vs_saved_s = offset_closure_s - saved_minus_fieldlog_s)
write_csv(cl, "output/flux/02_windows/rise_detection_closures.csv")

saved <- saved_all %>% group_by(analyzer, date) %>%
  summarise(n_saved = n(), saved_offset_median_s = median(saved_minus_fieldlog_s), .groups = "drop")
note_off <- notes %>% filter(!is.na(instrument_time), !is.na(real_time)) %>%
  mutate(real = hm(real_time, quiet = TRUE), inst = hm(instrument_time, quiet = TRUE),
         real_s = hour(real) * 3600 + minute(real) * 60, inst_s = hour(inst) * 3600 + minute(inst) * 60,
         real_s = if_else(real_s < 6 * 3600, real_s + 12 * 3600, real_s),   # "2:10" pm
         note_instrument_minus_real_s = inst_s - real_s) %>%
  group_by(analyzer, date = as.Date(date)) %>%
  summarise(note_instrument_minus_real_s = first(note_instrument_minus_real_s), .groups = "drop")
offsets <- day %>% left_join(saved, by = c("analyzer", "date")) %>% left_join(note_off, by = c("analyzer", "date")) %>%
  mutate(diff_vs_saved_s = offset_s - saved_offset_median_s,
         review = case_when(n_detected == 0 ~ "no rises detected",
                            n_detected < 3 ~ "fewer than 3 rises",
                            offset_mad_s > 60 ~ "closures disagree (MAD > 60 s)",
                            !is.na(diff_vs_saved_s) & abs(diff_vs_saved_s) > 60 ~ "disagrees with saved windows",
                            TRUE ~ NA_character_))
write_csv(offsets, "output/flux/02_windows/rise_detection_days.csv")

print(as.data.frame(offsets %>% transmute(analyzer, date, n = n_closures, det = n_detected, prior = round(prior_s),
                                          offset = round(offset_s), mad = round(offset_mad_s), saved = saved_offset_median_s,
                                          d_saved = round(diff_vs_saved_s), note = note_instrument_minus_real_s, review)),
      row.names = FALSE)
cat("\nClosures:", nrow(cl), "| rise detected:", sum(!is.na(cl$offset_closure_s)),
    "| flagged:", sum(!is.na(cl$closure_flag)), "\n")
cat("Per-closure offset minus saved-window offset (s), where both exist [10/50/90 pct]:",
    round(quantile(cl$diff_vs_saved_s, c(.1, .5, .9), na.rm = TRUE)), "\n")
