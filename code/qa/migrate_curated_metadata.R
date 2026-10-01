# =============================================================================
# One-time migration of hand-curated flux metadata into tracked CSVs under
# data/flux_metadata/ (handoff work plan, step 2).
#
# Every value here was previously either typed into a gitignored intermediate,
# hard-coded in a legacy script, or written by an interactive tool. This script
# copies them out of those sources verbatim, adds provenance, and never edits
# a value. After migration the rebuild reads only data/flux_metadata/; the
# legacy sources are kept until the replacement is verified.
#
# Sources (legacy):
#   intermediate/mar2022_soil_correction_log.csv  Mar 2022 6-inch soil chambers
#   code/05_integration/assemble_clean_dataset.R  BL60 water date fix (step 7b)
#   output/ebullition/corrected_time_windows.csv  14 trimmed windows (interactive picker)
#   code/06_ebullition/apply_negative_flux_corrections.R  7 artifact IDs
#   code/06_ebullition/detect_ebullition.R        confirmed bubble traces, exclusions
#   output/ebullition/placements_summary.csv      times of the listed placements
#
# Refuses to overwrite data/flux_metadata/ unless MIGRATE_OVERWRITE=1.
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(lubridate); library(purrr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

out <- "data/flux_metadata"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
# Tables are written only if absent, or if named in MIGRATE_ONLY (comma-
# separated, e.g. MIGRATE_ONLY=trimmed_windows): several tables have since
# been edited by hand with reasons (e.g. excluded_measurements.csv), and a
# blanket regeneration would drop those edits.
only <- strsplit(Sys.getenv("MIGRATE_ONLY"), ",")[[1]]
write_tbl <- function(df, name) {
  f <- file.path(out, paste0(name, ".csv"))
  if (file.exists(f) && !name %in% only) { message("kept existing ", f); return(invisible()) }
  write_csv(df, f); message("wrote ", f)
}
utc <- function(x) suppressWarnings(parse_date_time(x, c("Ymd HMS", "Ymd HM"), tz = "UTC"))
fmt <- function(x) format(x, "%Y-%m-%d %H:%M:%S")

# ---- 1. Air temperatures ------------------------------------------------------
# Not migrated: the 20 worldmet weather-station values (and 2 unparseable rows)
# in intermediate/main_soilwater_complete.csv are replaced by the US-Skr tower
# air temperature in 04_build_auxfile.R.

# ---- 2. Chamber assignment overrides -------------------------------------------
# Mar 2022 soil at BL60 / FLM30 / MI: recorded as "Soil 8 in" but measured
# with the 6-inch dome on a collar 2 cm above the soil (correct_mar2022_soil_chambers.R).
mar <- read_csv("intermediate/mar2022_soil_correction_log.csv", show_col_types = FALSE)
cham <- mar %>%
  transmute(flux_id, recorded_chamber_id = "Soil 8 in", chamber_id = "Soil 6 in",
            collar_offset_cm = 2,
            reason = "Mar 2022 soil at BL60/FLM30/MI used the 6-inch dome on a 2 cm collar (correct_mar2022_soil_chambers.R)")
write_tbl(cham, "chamber_overrides")

# ---- 3. Date corrections --------------------------------------------------------
dates <- tibble(
  flux_id = c("45007_BL60_Water_168", "45007_BL60_Water_169",
              "45007_BL60_Water_170", "45007_BL60_Water_171"),
  recorded_date = as.Date("2023-03-22"), date = as.Date("2023-03-16"),
  reason = "Field notes give 2023-03-22; LGR3 continuation file micro_2023-03-16_f0001.txt holds the traces (assemble_clean_dataset.R step 7b)")
write_tbl(dates, "date_corrections")

# ---- 4. Excluded measurements ---------------------------------------------------
excl <- tibble(
  flux_id = c("Oct_22_199_CP40_stem", "Oct_22_56_SRS6_stem", "Mar_23_119_BL60_stem",
              "Oct_22_33_SRS6_stem", "Oct_22_54_SRS6_stem", "Mar_23_76_FLM30_stem",
              "Oct_22_51_SRS6_stem"),
  reason = "analyzer artifact; removed in apply_negative_flux_corrections.R (ARTIFACT_IDS)")
write_tbl(excl, "excluded_measurements")

# ---- 5. Trimmed fit windows -----------------------------------------------------
# corrected_time_windows.csv gives Etime bounds. apply_negative_flux_corrections.R
# takes Etime from the first saved manual-ID file holding the trace (Etime is
# relative to that file's start.time_corr), else relative to the auxfile
# start.time. Convert to absolute analyzer-clock times with the same rule.
tw <- read_csv("output/ebullition/corrected_time_windows.csv", show_col_types = FALSE)
manual_files <- c(   # order as in apply_negative_flux_corrections.R
  "intermediate/results_trees/lgr1_manual_identification_results.csv",
  "intermediate/results_trees/lgr3_manual_identification_results.csv",
  "intermediate/results_trees/lgr3_manual_identification_results_additional.csv",
  "intermediate/results_surface/lgr3_manual_identification_results_soil.csv",
  "intermediate/results_surface/lgr2_manual_identification_results_soil.csv",
  "intermediate/rescue/lgr2_manual_identification_results.csv")
anchor_saved <- map_dfr(manual_files, function(f) {
  read_csv(f, show_col_types = FALSE, col_types = cols(.default = col_character())) %>%
    filter(UniqueID %in% tw$flux_id) %>%
    group_by(flux_id = UniqueID) %>% summarise(anchor = first(start.time_corr), .groups = "drop") %>%
    mutate(anchor_source = sub("^intermediate/", "", f))
}) %>% distinct(flux_id, .keep_all = TRUE)
# The legacy refit read start.time from the *_goflux auxfiles, which hold the
# local field-log clock time ("2022-10-16 13:24:53"); the *_all_instruments
# files hold the same instant converted to UTC ("...T17:24:53Z") and must not
# be used here.
aux_all <- bind_rows(lapply(c("tree_auxfile_lgr1_goflux", "tree_auxfile_lgr2_goflux", "tree_auxfile_lgr3_goflux",
                              "soilwater_auxfile_lgr2_goflux", "soilwater_auxfile_lgr3_goflux"), function(f)
  read_csv(file.path("intermediate/auxfiles", paste0(f, ".csv")), show_col_types = FALSE,
           col_types = cols(.default = col_character())) %>% mutate(src = f))) %>%
  transmute(flux_id = UniqueID, anchor = start.time,
            anchor_source = paste0("auxfiles/", src, ".csv start.time (field log, analyzer clock = field clock)")) %>%
  distinct(flux_id, .keep_all = TRUE)
anchors <- bind_rows(anchor_saved, anti_join(aux_all, anchor_saved, by = "flux_id"))
trim <- tw %>% left_join(anchors, by = "flux_id") %>%
  mutate(a = utc(anchor),
         window_start = fmt(a + new_start_etime), window_end = fmt(a + new_end_etime)) %>%
  transmute(flux_id, window_start, window_end, etime_start = new_start_etime,
            etime_end = new_end_etime, etime_anchor = fmt(a), anchor_source, action,
            source = "output/ebullition/corrected_time_windows.csv (interactive_time_picker.R)")
stopifnot(!anyNA(trim$window_start))
write_tbl(trim, "trimmed_windows")

# ---- 6. Ebullition: confirmed traces and exclusions -----------------------------
pl <- read_csv("output/ebullition/placements_summary.csv", show_col_types = FALSE)
confirmed_ids <- c("LGR2_2022-10-23_CP40_P02", "LGR2_2022-10-23_CP40_P06",
                   "LGR2_2022-10-23_CP40_P10", "Picarro_2022-10-18_FLM30_P08",
                   "Picarro_2022-10-25_BL60_P02", "Picarro_2022-10-25_BL60_P03")
excluded_ids  <- c("LGR3_2023-03-15_CP40_P07", "Picarro_2022-10-18_FLM30_P09",
                   "Picarro_2022-10-25_BL60_P07")
pick <- function(ids) pl %>% filter(placement_id %in% ids) %>%
  transmute(placement_id, analyzer, site, date = as.Date(date),
            start = fmt(utc(start_time)), end = fmt(utc(end_time)), n_jumps,
            matched_flux_id, trace_type)
conf <- pick(confirmed_ids) %>%
  mutate(note = "manually verified ebullition (detect_ebullition.R CONFIRMED_EBULLITION)")
stopifnot(nrow(conf) == length(confirmed_ids))
exc <- bind_rows(
  pick(excluded_ids) %>% mutate(type = "trace",
         note = "Picarro noise / analyzer artifact, not a placement (detect_ebullition.R EXCLUDE_TRACE_IDS)"),
  tibble(placement_id = NA_character_, analyzer = NA_character_, site = "CP40",
         date = as.Date("2023-03-15"), start = "2023-03-15 12:19:00", end = "2023-03-15 12:27:00",
         type = "window", note = "1.16 ppm analyzer artifact (detect_ebullition.R EXCLUDE_WINDOWS)"))
write_tbl(conf, "ebullition_confirmed_traces")
write_csv(exc,  file.path(out, "ebullition_exclusions.csv"))


# ---- 7. Saved manual windows (legacy click.peak2 / rescue) ----------------------
# The saved manual-ID files live only in the gitignored intermediate/ folder;
# the window each closure was fitted on is copied here (analyzer clock), with
# the file it came from (primary copy as chosen in qa/measurement_inventory.R:
# the rescue copy for rescued closures, else the original).
inv <- read_csv("output/qa/measurement_inventory.csv", show_col_types = FALSE)
saved <- inv %>% filter(has_saved_window) %>%
  transmute(flux_id, analyzer, start = fmt(with_tz(saved_start, "UTC")), end = fmt(with_tz(saved_end, "UTC")),
            n_obs = saved_n_obs, offset_from_fieldlog_s = saved_minus_fieldlog_s,
            source_file = paste0("intermediate/", saved_window_file))
write_tbl(saved, "saved_manual_windows")

cat("Wrote to", out, ":\n")
for (f in list.files(out, "\\.csv$")) cat(sprintf("  %-34s %3d rows\n", f, nrow(read_csv(file.path(out, f), show_col_types = FALSE))))
