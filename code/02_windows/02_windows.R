# =============================================================================
# Fit windows for every included closure (handoff work plan, step 3).
#
# Clock offset per analyzer-day (analyzer time - field-sheet time):
#   1. median offset of the saved manual windows that day (checked by eye
#      against the data when they were clicked);
#   2. else the analyzer-campaign median (e.g. LGR2 trees share the clock with
#      the LGR2 soil/water closures of the same campaign);
#   3. else the per-closure rise detection median (06_clock_offsets.R);
#   4. else 0 (LGR) / 25200 s (Picarro).
#
# Window per closure (decision 3, Jon 2026-10-01: saved windows win):
#   - trimmed_windows.csv (curated) if present and not rejected;
#   - else the saved manual window, unless rejected (start.time_corr / end.time_corr, primary
#     copy, data/flux_metadata/saved_manual_windows.csv);
#   - else scripted: field start + offset + dead band to field end + offset,
#     the dead band being the median (saved start - field start - offset) of
#     that analyzer-campaign; field end missing -> start + the median saved
#     window length of that analyzer-campaign.
# Where a saved window exists the scripted window is built too and the two are
# compared (overlap, start/end differences); large disagreements are listed for
# review, the saved window is kept.
# Saved or trimmed windows shown to be wrong are listed in window_rejections.csv
# (the closure falls back to the next source). Windows that run across a chamber
# lift or start before the chamber goes on are cut at it (window_clips.csv; the
# lift / step is found in the raw CO2 trace). Finally no two closures on one
# analyzer may share more than OVERLAP_S of record: the step stops if they do.
#
# Writes output/flux/02_windows/windows.csv (the tracked window table the fit uses) and
# output/flux/02_windows/windows_disagreements.csv.
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(lubridate)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/00_lib/lib_raw.R")
OVERLAP_S <- 30

utc <- function(x) suppressWarnings(parse_date_time(x, c("Ymd HMS", "Ymd HM"), tz = "UTC"))
aux <- read_csv("output/flux/01_metadata/auxfile.csv", show_col_types = FALSE) %>% filter(!excluded)
saved_win <- read_csv("data/flux_metadata/saved_manual_windows.csv", show_col_types = FALSE)
det <- read_csv("output/flux/02_windows/rise_detection_days.csv", show_col_types = FALSE)
cl  <- read_csv("output/flux/02_windows/rise_detection_closures.csv", show_col_types = FALSE)
rejections <- read_csv("data/flux_metadata/window_rejections.csv", show_col_types = FALSE)
clips <- read_csv("data/flux_metadata/window_clips.csv", show_col_types = FALSE)
rejected <- rejections %>% filter(window == "saved")
trim <- read_csv("data/flux_metadata/trimmed_windows.csv", show_col_types = FALSE) %>%
  filter(!flux_id %in% rejections$flux_id[rejections$window == "trimmed"])

w <- aux %>%
  transmute(UniqueID, analyzer, date, measurement_type, component,
            field_start = utc(start.time), field_end = utc(end.time),
            campaign = recode(format(date, "%Y-%m"), "2022-03" = "Mar2022", "2022-10" = "Oct2022", "2023-03" = "Mar2023")) %>%
  left_join(saved_win %>% transmute(UniqueID = flux_id, saved_start = utc(start), saved_end = utc(end),
                                    saved_window_file = source_file, saved_offset = offset_from_fieldlog_s),
            by = "UniqueID") %>%
  # saved windows shown to be wrong (window_rejections.csv) are not used
  mutate(across(c(saved_start, saved_end), ~ if_else(UniqueID %in% rejected$flux_id, as.POSIXct(NA, tz = "UTC"), .x)),
         saved_offset = if_else(UniqueID %in% rejected$flux_id, NA_real_, saved_offset))

# ---- offsets ----------------------------------------------------------------------
off_day  <- w %>% filter(!is.na(saved_offset)) %>% group_by(analyzer, date) %>%
  summarise(o_day = median(saved_offset), n_day = n(), .groups = "drop")
off_camp <- w %>% filter(!is.na(saved_offset)) %>% group_by(analyzer, campaign) %>%
  summarise(o_camp = median(saved_offset), .groups = "drop")
w <- w %>% left_join(off_day, by = c("analyzer", "date")) %>% left_join(off_camp, by = c("analyzer", "campaign")) %>%
  left_join(det %>% select(analyzer, date, o_det = offset_s), by = c("analyzer", "date")) %>%
  mutate(offset_s = coalesce(o_day, o_camp, o_det, if_else(analyzer == "Picarro", 25200, 0)),
         offset_source = case_when(!is.na(o_day) ~ "saved windows, same day",
                                   !is.na(o_camp) ~ "saved windows, same analyzer-campaign",
                                   !is.na(o_det) ~ "rise detection", TRUE ~ "default"))

# ---- scripted windows ------------------------------------------------------------------
db <- w %>% filter(!is.na(saved_start), !is.na(field_start)) %>%
  mutate(dead = as.numeric(difftime(saved_start, field_start, units = "secs")) - offset_s,
         len = as.numeric(difftime(saved_end, saved_start, units = "secs"))) %>%
  group_by(analyzer, campaign) %>%
  summarise(deadband_s = median(pmax(dead, 0)), typical_len_s = median(len), .groups = "drop")
w <- w %>% left_join(db, by = c("analyzer", "campaign")) %>%
  mutate(deadband_s = coalesce(deadband_s, median(db$deadband_s)),
         typical_len_s = coalesce(typical_len_s, median(db$typical_len_s)),
         script_start = field_start + offset_s + deadband_s,
         script_end = if_else(!is.na(field_end), field_end + offset_s, script_start + typical_len_s))

# ---- choose -------------------------------------------------------------------------------
w <- w %>% left_join(trim %>% transmute(UniqueID = flux_id, trim_start = utc(window_start), trim_end = utc(window_end)),
                     by = "UniqueID") %>%
  mutate(window_source = case_when(!is.na(trim_start) ~ "trimmed (curated)",
                                   !is.na(saved_start) & !is.na(saved_end) ~ "saved manual window",
                                   !is.na(script_start) ~ "scripted (field log + offset)",
                                   TRUE ~ "none"),
         start = case_when(window_source == "trimmed (curated)" ~ trim_start,
                           window_source == "saved manual window" ~ saved_start,
                           TRUE ~ script_start),
         end = case_when(window_source == "trimmed (curated)" ~ trim_end,
                         window_source == "saved manual window" ~ saved_end,
                         TRUE ~ script_end),
         # agreement where both a saved and a scripted window exist
         overlap_s = pmax(0, as.numeric(difftime(pmin(saved_end, script_end), pmax(saved_start, script_start), units = "secs"))),
         overlap_frac_of_saved = overlap_s / as.numeric(difftime(saved_end, saved_start, units = "secs")),
         d_start_s = as.numeric(difftime(script_start, saved_start, units = "secs")),
         d_end_s = as.numeric(difftime(script_end, saved_end, units = "secs"))) %>%
  left_join(cl %>% select(UniqueID, rise_start, closure_flag), by = "UniqueID") %>%
  mutate(rise_minus_window_start_s = as.numeric(difftime(utc(format(rise_start)), start, units = "secs")))

# ---- clips at a chamber lift / placement step (window_clips.csv) ---------------------------
#   end_before_lift : end 10 s before the steepest CO2 drop in the window
#   start_after_lift: start 10 s after the CO2 minimum within 180 s of the steepest drop
#   start_after_step: start 10 s after the steepest CO2 rise in the window
clip_one <- function(an, s, e, rule) {
  r <- read_raw(an, s, e); stopifnot(nrow(r) > 10)
  d <- c(NA, diff(r$CO2dry_ppm)); t <- r$POSIX.time
  switch(rule,
    end_before_lift  = c(s, t[which.min(d)] - 10),
    start_after_lift = { L <- t[which.min(d)]; k <- which(t > L & t <= L + 180); c(t[k][which.min(r$CO2dry_ppm[k])] + 10, e) },
    start_after_step = c(t[which.max(d)] + 10, e),
    stop("unknown clip rule ", rule))
}
w$clip_rule <- clips$rule[match(w$UniqueID, clips$flux_id)]
for (i in which(!is.na(w$clip_rule))) {
  se <- clip_one(w$analyzer[i], w$start[i], w$end[i], w$clip_rule[i])
  w$start[i] <- se[1]; w$end[i] <- se[2]
  w$window_source[i] <- paste0(w$window_source[i], ", clipped (", w$clip_rule[i], ")")
}

out <- w %>% transmute(UniqueID, analyzer, date, campaign, measurement_type, component, window_source,
                       start = format(start, "%Y-%m-%d %H:%M:%S"), end = format(end, "%Y-%m-%d %H:%M:%S"),
                       length_s = as.numeric(difftime(utc(end), utc(start), units = "secs")),
                       offset_s, offset_source, deadband_s = if_else(grepl("scripted", window_source), deadband_s, NA_real_),
                       field_start = format(field_start, "%Y-%m-%d %H:%M:%S"), field_end = format(field_end, "%Y-%m-%d %H:%M:%S"),
                       saved_window_file, overlap_frac_of_saved = round(overlap_frac_of_saved, 3),
                       d_start_s, d_end_s, rise_minus_window_start_s, rise_check = closure_flag) %>%
  arrange(date, analyzer, start)
# ---- no shared record ------------------------------------------------------------------------
ov <- out %>% filter(!is.na(start)) %>% mutate(s = utc(start), e = utc(end)) %>% select(UniqueID, analyzer, s, e)
ov <- ov %>% inner_join(ov, by = "analyzer", suffix = c("_a", "_b"), relationship = "many-to-many") %>%
  filter(UniqueID_a < UniqueID_b) %>%
  mutate(shared_s = as.numeric(pmin(e_a, e_b)) - as.numeric(pmax(s_a, s_b))) %>% filter(shared_s > OVERLAP_S)
if (nrow(ov)) { print(as.data.frame(ov)); stop(nrow(ov), " pairs of closures share more than ", OVERLAP_S,
  " s of record on one analyzer; resolve them in data/flux_metadata (code/qa/window_overlaps.R shows the traces)") }
write_csv(out, "output/flux/02_windows/windows.csv")

dis <- out %>% filter(!is.na(overlap_frac_of_saved),
                      overlap_frac_of_saved < 0.5 | abs(d_start_s) > 90 | abs(d_end_s) > 120)
write_csv(dis, "output/flux/02_windows/windows_disagreements.csv")

cat("Windows:", nrow(out), "\n"); print(table(out$window_source))
cat("\nOffset source:\n"); print(table(out$offset_source))
cat("\nScripted vs saved (", sum(!is.na(out$overlap_frac_of_saved)), "closures with both):\n")
q <- function(x) round(quantile(x, c(.1, .5, .9), na.rm = TRUE), 2)
cat("  overlap fraction of saved [10/50/90]:", q(out$overlap_frac_of_saved), "\n")
cat("  start diff s [10/50/90]:", q(out$d_start_s), " end diff s:", q(out$d_end_s), "\n")
cat("  disagreements listed for review:", nrow(dis), "\n")
cat("\nDead band and typical length by analyzer-campaign:\n"); print(as.data.frame(db))
cat("\nWindow length (s) by source:\n")
print(out %>% group_by(window_source) %>% summarise(n = n(), min = min(length_s, na.rm = TRUE),
      med = median(length_s, na.rm = TRUE), max = max(length_s, na.rm = TRUE), n_na = sum(is.na(length_s))) %>% as.data.frame())
