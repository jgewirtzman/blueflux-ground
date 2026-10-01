# =============================================================================
# Measurement inventory for the scripted flux rebuild (handoff step 1).
#
# Writes
#   output/rebuild/raw_file_index.csv        one row per raw analyzer file
#   output/rebuild/measurement_inventory.csv one row per measurement: the 834
#       goFlux-fitted measurements (incl. the 7 later removed as artifacts) plus
#       the 40 rows added by ebullition reprocessing
#   output/rebuild/inventory_summary.txt     cross-tabulations
#
# Raw data are read only. Zipped LGR files are extracted to a temporary
# directory, never next to the archive. Times are compared as logged (UTC
# clock, no offset applied): clock offsets are estimated in a later step, and
# saved_minus_fieldlog_s shows the offset implied by the saved manual windows.
#
# Needs data/analyzer/ (gitignored) and intermediate/ (gitignored) from the
# legacy pipeline.
# =============================================================================
suppressMessages({
  library(dplyr); library(readr); library(tidyr); library(stringr); library(purrr); library(lubridate)
})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

raw_root <- "data/analyzer"
out_dir  <- "output/rebuild"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(dir.exists(raw_root), dir.exists("intermediate"))

# Parses "YYYY-mm-dd HH:MM[:SS]" and ISO "YYYY-mm-ddTHH:MM:SS.ffffffZ" alike
to_utc <- function(x) {
  if (inherits(x, "POSIXct")) return(with_tz(x, "UTC"))
  if (is.numeric(x)) return(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
  suppressWarnings(parse_date_time(x, c("Ymd HMS", "Ymd HM"), tz = "UTC"))
}

# ---- 1. Raw file index --------------------------------------------------------

# Interval summary for a vector of timestamps (seconds, in write order)
interval_summary <- function(t) {
  dt <- diff(sort(t))
  dt <- dt[dt > 0 & dt < 600]                      # ignore gaps between sessions
  if (!length(dt)) return(list(dt_median = NA_real_, dt_modes = NA_character_))
  r <- round(dt, 1)
  tab <- sort(table(r), decreasing = TRUE)
  share <- tab / sum(tab)
  list(dt_median = median(dt),
       dt_modes = paste(sprintf("%s(%.0f%%)", names(tab)[share >= 0.05], 100 * share[share >= 0.05]),
                        collapse = ";"))
}

read_lgr_times <- function(path) {
  lines <- readLines(path, warn = FALSE)
  sn <- str_extract(lines[1], "SN:[^ ]+")
  dat <- lines[grepl("^\\s*\\d{1,2}/\\d{1,2}/\\d{4}\\s+\\d{1,2}:\\d{2}:\\d{2}", lines)]
  if (!length(dat)) return(NULL)
  f <- str_split_fixed(dat, ",", 3)
  sys <- as.numeric(as.POSIXct(trimws(f[, 1]), format = "%m/%d/%Y %H:%M:%OS", tz = "UTC"))
  tim <- as.numeric(as.POSIXct(trimws(f[, 2]), format = "%m/%d/%Y %H:%M:%OS", tz = "UTC"))
  ok <- !is.na(sys)
  list(serial = sn, t = sys[ok], sys_minus_time = median(sys[ok] - tim[ok], na.rm = TRUE),
       n_behind = sum(sys[ok] < cummax(sys[ok]) - 30))   # clock-reset rows, as in recover_failed_measurements.R
}

read_picarro_times <- function(path) {
  hdr <- scan(path, what = "", nlines = 1, quiet = TRUE)
  d <- read.table(path, header = TRUE, colClasses = "character")[, c("DATE", "TIME")]
  t <- as.numeric(as.POSIXct(paste(d$DATE, d$TIME), format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"))
  t <- t[!is.na(t)]
  list(serial = NA_character_, t = t, sys_minus_time = NA_real_,
       n_behind = sum(t < cummax(t) - 30))
}

tmp_root <- tempfile("raw_unzip_"); dir.create(tmp_root)   # removed after the index is built

lgr_txt <- list.files(file.path(raw_root, "LGR_GLA131"), pattern = "_f\\d+\\.txt$",
                      recursive = TRUE, full.names = TRUE)
lgr_txt <- lgr_txt[!dir.exists(lgr_txt) & file.size(lgr_txt) > 0 & !grepl("/\\._", lgr_txt)]
lgr_zip <- list.files(file.path(raw_root, "LGR_GLA131"), pattern = "_f\\d+\\.txt\\.zip$",
                      recursive = TRUE, full.names = TRUE)
pic_dat <- list.files(file.path(raw_root, "Picarro_G4301"), pattern = "\\.dat$",
                      recursive = TRUE, full.names = TRUE)

raw_entries <- bind_rows(
  tibble(unit = str_match(lgr_txt, "LGR_GLA131/(LGR\\d)/")[, 2], path = lgr_txt, container = NA_character_),
  map_dfr(seq_along(lgr_zip), function(k) {
    z <- lgr_zip[k]
    ex <- file.path(tmp_root, sprintf("zip%03d", k)); dir.create(ex, recursive = TRUE)   # one folder per archive
    inner <- unzip(z, exdir = ex)
    inner <- inner[grepl("_f\\d+\\.txt$", inner) & !grepl("/\\._", inner)]
    tibble(unit = str_match(z, "LGR_GLA131/(LGR\\d)/")[, 2], path = inner, container = z)
  }),
  tibble(unit = "Picarro", path = pic_dat, container = NA_character_)
)

cat("Reading", nrow(raw_entries), "raw files...\n")
raw_times <- vector("list", nrow(raw_entries))
raw_index <- map_dfr(seq_len(nrow(raw_entries)), function(i) {
  e <- raw_entries[i, ]
  r <- tryCatch(if (e$unit == "Picarro") read_picarro_times(e$path) else read_lgr_times(e$path),
                error = function(err) NULL)
  if (is.null(r) || !length(r$t)) {
    return(tibble(i = i, unit = e$unit, n_rows = 0L))
  }
  raw_times[[i]] <<- r$t
  iv <- interval_summary(r$t)
  tibble(i = i, unit = e$unit, serial = r$serial, n_rows = length(r$t),
         t_first = to_utc(min(r$t)), t_last = to_utc(max(r$t)),
         dt_median_s = iv$dt_median, dt_modes = iv$dt_modes,
         n_clock_reset_rows = r$n_behind, sys_minus_time_s = r$sys_minus_time)
})

rel <- function(p) sub(paste0("^.*?(", raw_root, "/)"), "\\1", p)
raw_index <- raw_index %>%
  mutate(file = ifelse(is.na(raw_entries$container[i]),
                       rel(raw_entries$path[i]),
                       paste0(rel(raw_entries$container[i]), "!", basename(raw_entries$path[i]))),
         from_zip = !is.na(raw_entries$container[i]),
         basename = basename(raw_entries$path[i])) %>%
  # The same logger file can exist both unzipped and zipped: keep the first copy
  group_by(unit, basename, n_rows, t_first) %>%
  mutate(duplicate_of = if (n() > 1) first(file[!from_zip | all(from_zip)]) else NA_character_,
         is_duplicate = !is.na(duplicate_of) & file != duplicate_of) %>%
  ungroup()

write_csv(raw_index %>% select(unit, file, from_zip, is_duplicate, serial, n_rows, t_first, t_last,
                               dt_median_s, dt_modes, n_clock_reset_rows, sys_minus_time_s),
          file.path(out_dir, "raw_file_index.csv"))
cat("  raw files:", nrow(raw_index), "| duplicates:", sum(raw_index$is_duplicate),
    "| unreadable/empty:", sum(raw_index$n_rows == 0), "\n")

unlink(tmp_root, recursive = TRUE)

# Pooled, de-duplicated timestamps per analyzer folder x logger serial. Folders
# are named by unit, but some hold files written by another unit's logger, so
# streams are kept apart by serial (Picarro files carry no serial).
streams <- raw_index %>% filter(!is_duplicate, n_rows > 0) %>%
  mutate(serial = coalesce(serial, "none")) %>%
  group_by(unit, serial) %>% summarise(idx = list(i), .groups = "drop")
stream_times <- lapply(streams$idx, function(ix) sort(unlist(raw_times[ix])))

# ---- 2. Measurement universe --------------------------------------------------

final <- read_csv("output/data_products/combined_gas_flux_dataset.csv", show_col_types = FALSE,
                  col_types = cols(start_time = col_character(), end_time = col_character(),
                                   .default = col_guess()))

artifact_ids <- c("Oct_22_199_CP40_stem", "Oct_22_56_SRS6_stem", "Mar_23_119_BL60_stem",
                  "Oct_22_33_SRS6_stem", "Oct_22_54_SRS6_stem", "Mar_23_76_FLM30_stem",
                  "Oct_22_51_SRS6_stem")   # as in apply_negative_flux_corrections.R
# Field metadata for the artifacts (absent from the final dataset)
tree_meta <- bind_rows(
  read_csv("intermediate/main_trees_complete.csv", show_col_types = FALSE,
           col_types = cols(.default = col_character())),
  read_csv("intermediate/main_trees_complete_additional.csv", show_col_types = FALSE,
           col_types = cols(.default = col_character()))
)
artifact_rows <- tree_meta %>% filter(flux_id %in% artifact_ids) %>% distinct(flux_id, .keep_all = TRUE)
# analyzer for the artifacts comes from the goFlux result files they were fitted in
artifact_analyzer <- map_dfr(list.files("intermediate/results_trees", "_final_complete_dataset.*\\.csv$",
                                        full.names = TRUE), function(f) {
  d <- read_csv(f, show_col_types = FALSE, col_types = cols(.default = col_character()))
  tibble(flux_id = intersect(d$flux_id, artifact_ids),
         analyzer_source = str_extract(basename(f), "LGR\\d|Picarro"))
})

meas <- bind_rows(
  final %>% transmute(flux_id, measurement_type, component, plot, date = as.Date(date),
                      month_year,
                      # tree rows carry "LGR"; the unit is in the goFlux result file name
                      analyzer_source = if_else(analyzer_source == "LGR" & !is.na(source_file),
                                                str_extract(source_file, "LGR\\d"), analyzer_source),
                      data_source, start_time, end_time,
                      chamber = coalesce(as.character(chamber_class), as.character(chamber_id)),
                      CH4_best.flux, CO2_best.flux, CH4_model, CO2_model,
                      CH4_below_MDF, CH4_flagged, CO2_below_MDF, CO2_flagged,
                      in_final_dataset = TRUE),
  artifact_rows %>%
    left_join(artifact_analyzer, by = "flux_id") %>%
    transmute(flux_id, measurement_type = "tree", component = "stem",
              plot = str_extract(flux_id, "(SRS[56]|BL60|CP40|FLM30|RB10|SE1|MI)"),
              date = as.Date(str_sub(coalesce(datetime, date), 1, 10)),
              month_year = format(date, "%Y-%m"), analyzer_source, data_source = NA_character_,
              start_time, end_time, chamber = chamber_class, in_final_dataset = FALSE)
)
stopifnot(!anyDuplicated(meas$flux_id), sum(!meas$in_final_dataset) == length(artifact_ids))

# ---- 3. Current flux source and applied patches -------------------------------

ids_from <- function(path, col = "UniqueID") if (file.exists(path)) unique(read_csv(path, show_col_types = FALSE)[[col]]) else character(0)
rescued   <- union(ids_from("intermediate/rescue/ALL_RESCUED_CH4_BEST_FLUX.csv"),
                   ids_from("intermediate/rescue/ALL_RESCUED_CO2_BEST_FLUX.csv"))
recovered <- unique(c(ids_from("intermediate/rescue/recovered_all_CH4_fluxes.csv"),
                      ids_from("intermediate/rescue/recovered_lgr3_tree_fluxes.csv"),
                      ids_from("intermediate/rescue/recovered_lgr3_water_fluxes.csv")))
trimmed   <- read_csv("output/ebullition/corrected_time_windows.csv", show_col_types = FALSE)
partitioned <- read_csv("output/ebullition/partitioned_fluxes.csv", show_col_types = FALSE)
ebull_processed <- unique(na.omit(partitioned$matched_flux_id[partitioned$trace_type == "processed"]))
ha_hb   <- ids_from("intermediate/ha_hb_flux_corrections.csv", "flux_id")
mar2022 <- ids_from("intermediate/mar2022_soil_correction_log.csv", "flux_id")
date_fix <- c("45007_BL60_Water_168", "45007_BL60_Water_169",
              "45007_BL60_Water_170", "45007_BL60_Water_171")   # assemble_clean_dataset.R step 7b
temp_src <- bind_rows(
  read_csv("intermediate/main_soilwater_complete.csv", show_col_types = FALSE,
           col_types = cols(.default = col_character())) %>% select(flux_id, temp_source),
  tree_meta %>% select(any_of(c("flux_id", "temp_source")))
) %>% filter(!is.na(flux_id)) %>% distinct(flux_id, .keep_all = TRUE)

meas <- meas %>%
  left_join(temp_src, by = "flux_id") %>%
  mutate(
    flux_source = case_when(
      !in_final_dataset                         ~ "artifact_removed",
      data_source == "ebullition_reprocessing"  ~ "ebullition_added_placement",
      flux_id %in% trimmed$flux_id              ~ "trimmed_window_refit",
      flux_id %in% ebull_processed              ~ "ebullition_partitioned",
      flux_id %in% recovered                    ~ "recovered",
      flux_id %in% rescued                      ~ "rescued",
      TRUE                                      ~ "original"
    ),
    also_rescued        = flux_id %in% rescued & flux_source != "rescued",
    patch_ha_hb_geometry = flux_id %in% ha_hb,
    patch_mar2022_soil   = flux_id %in% mar2022,
    patch_date_fix       = flux_id %in% date_fix,
    air_temp_source      = temp_source
  ) %>% select(-temp_source)

# ---- 4. Field-log window and raw coverage -------------------------------------

hms_ok <- function(x) !is.na(x) & grepl("^\\d{1,2}:\\d{2}(:\\d{2})?$", x)
meas <- meas %>%
  mutate(
    has_fieldlog_start = hms_ok(start_time),
    has_fieldlog_end   = hms_ok(end_time),
    fieldlog_start = if_else(has_fieldlog_start, to_utc(paste(date, start_time)), to_utc(NA)),
    fieldlog_end   = if_else(has_fieldlog_end,   to_utc(paste(date, end_time)),   to_utc(NA)),
    fieldlog_end   = if_else(!is.na(fieldlog_end) & fieldlog_end <= fieldlog_start,
                             to_utc(NA), fieldlog_end),
    unit = analyzer_source
  )

coverage <- function(unit, t0, t1, day) {
  k <- which(streams$unit == unit)
  none <- list(raw_on_date = FALSE, n_obs = NA_integer_, dt = NA_real_, serial = NA_character_,
               serials = NA_character_)
  if (!length(k) || is.na(day)) return(none)
  d0 <- as.numeric(to_utc(paste(day, "00:00:00")))
  on_day <- any(vapply(k, function(j) any(stream_times[[j]] >= d0 & stream_times[[j]] < d0 + 86400), TRUE))
  if (is.na(t0)) return(modifyList(none, list(raw_on_date = on_day)))
  t0 <- as.numeric(t0)
  t1 <- if (is.na(t1)) t0 + 600 else as.numeric(t1)   # no end time: assume 10 min
  w <- lapply(k, function(j) { tt <- stream_times[[j]]; tt[tt >= t0 & tt <= t1] })
  n <- lengths(w)
  if (!any(n > 0)) return(modifyList(none, list(raw_on_date = on_day, n_obs = 0L)))
  best <- which.max(n)
  list(raw_on_date = on_day, n_obs = n[best],
       dt = if (n[best] > 2) median(diff(w[[best]])) else NA_real_,
       serial = streams$serial[k[best]],
       serials = paste(streams$serial[k[n > 2]], collapse = ";"))
}
cov <- pmap(list(meas$unit, meas$fieldlog_start, meas$fieldlog_end, meas$date), coverage)
meas <- meas %>%
  mutate(raw_on_date = map_lgl(cov, "raw_on_date"),
         raw_obs_in_fieldlog_window = map_int(cov, ~ as.integer(.x$n_obs)),
         logging_interval_s = map_dbl(cov, "dt"),
         raw_serial = map_chr(cov, "serial"),
         raw_serials_in_window = map_chr(cov, "serials"))

# Day-level interval for rows without points in the field-log window
day_dt <- raw_index %>% filter(!is_duplicate, n_rows > 0) %>%
  mutate(date = as.Date(t_first)) %>%
  group_by(unit, date) %>% summarise(day_dt_s = median(dt_median_s, na.rm = TRUE), .groups = "drop")
meas <- meas %>% left_join(day_dt, by = c("unit", "date")) %>%
  mutate(logging_interval_s = coalesce(logging_interval_s, day_dt_s),
         logging_interval_basis = case_when(!is.na(raw_obs_in_fieldlog_window) &
                                              raw_obs_in_fieldlog_window > 2 ~ "fieldlog_window",
                                            !is.na(day_dt_s) ~ "analyzer_day",
                                            TRUE ~ NA_character_)) %>%
  select(-day_dt_s)

# ---- 5. Saved manual-ID windows -----------------------------------------------

manid_files <- list.files("intermediate", pattern = "manual_identification", recursive = TRUE,
                          full.names = TRUE)
manid_files <- manid_files[grepl("\\.csv$", manid_files)]
saved <- map_dfr(manid_files, function(f) {
  d <- read_csv(f, show_col_types = FALSE, col_types = cols(.default = col_character()))
  if (!"UniqueID" %in% names(d)) return(NULL)
  st <- if ("start.time_corr" %in% names(d)) d$start.time_corr else d$start.time
  en <- if ("end.time_corr" %in% names(d)) d$end.time_corr else NA_character_
  tibble(flux_id = d$UniqueID, s = to_utc(st), e = to_utc(en), p = to_utc(d$POSIX.time)) %>%
    group_by(flux_id) %>%
    summarise(saved_start = min(s, na.rm = TRUE), saved_end = max(e, na.rm = TRUE),
              saved_n_obs = n(), .groups = "drop") %>%
    mutate(saved_file = sub("^intermediate/", "", f))
})
saved <- saved %>% mutate(across(c(saved_start, saved_end), ~ if_else(is.finite(.x), .x, to_utc(NA))))
saved_any <- saved %>% group_by(flux_id) %>%
  summarise(saved_window_files = paste(sort(unique(saved_file)), collapse = ";"),
            n_saved_window_files = n_distinct(saved_file), .groups = "drop")
# Primary saved window: the rescue copy for rescued rows, otherwise a non-rescue copy
saved_primary <- saved %>%
  left_join(meas %>% select(flux_id, flux_source, also_rescued), by = "flux_id") %>%
  mutate(pref = (flux_source == "rescued" | also_rescued) == grepl("^rescue/", saved_file)) %>%
  arrange(flux_id, desc(pref), saved_file) %>%
  distinct(flux_id, .keep_all = TRUE) %>%
  select(flux_id, saved_window_file = saved_file, saved_start, saved_end, saved_n_obs)

meas <- meas %>%
  left_join(saved_any, by = "flux_id") %>%
  left_join(saved_primary, by = "flux_id") %>%
  mutate(has_saved_window = !is.na(saved_window_file),
         saved_minus_fieldlog_s = as.numeric(difftime(saved_start, fieldlog_start, units = "secs")))

# Windows held elsewhere (ebullition placements and trimmed refits)
pl <- read_csv("output/ebullition/placements_summary.csv", show_col_types = FALSE)
meas <- meas %>%
  mutate(other_window_source = case_when(
    flux_id %in% trimmed$flux_id ~ "ebullition/corrected_time_windows.csv",
    flux_source %in% c("ebullition_partitioned", "ebullition_added_placement") ~ "ebullition/placements_summary.csv",
    TRUE ~ NA_character_))

# ---- 6. Write ------------------------------------------------------------------

inv <- meas %>%
  mutate(campaign = recode(month_year, "2022-03" = "Mar2022", "2022-10" = "Oct2022",
                           "2023-03" = "Mar2023", "2023-12" = "Dec2023", .default = month_year),
         has_raw_in_fieldlog_window = !is.na(raw_obs_in_fieldlog_window) & raw_obs_in_fieldlog_window > 2) %>%
  select(flux_id, in_final_dataset, measurement_type, component, plot, date, campaign,
         analyzer = analyzer_source, chamber,
         flux_source, also_rescued, data_source_legacy = data_source,
         patch_ha_hb_geometry, patch_mar2022_soil, patch_date_fix, air_temp_source,
         fieldlog_start_time = start_time, fieldlog_end_time = end_time,
         has_fieldlog_start, has_fieldlog_end,
         raw_on_date, has_raw_in_fieldlog_window, raw_obs_in_fieldlog_window, raw_serial, raw_serials_in_window,
         logging_interval_s, logging_interval_basis,
         has_saved_window, saved_window_file, saved_window_files, n_saved_window_files,
         saved_start, saved_end, saved_n_obs, saved_minus_fieldlog_s, other_window_source,
         CH4_best.flux, CH4_model, CH4_below_MDF, CH4_flagged,
         CO2_best.flux, CO2_model, CO2_below_MDF, CO2_flagged) %>%
  arrange(campaign, analyzer, date, fieldlog_start_time, flux_id)
write_csv(inv, file.path(out_dir, "measurement_inventory.csv"))

sink(file.path(out_dir, "inventory_summary.txt"))
cat("Measurement inventory —", nrow(inv), "rows (", sum(inv$in_final_dataset),
    "in combined_gas_flux_dataset.csv +", sum(!inv$in_final_dataset), "artifacts)\n\n")
show <- function(title, x) { cat("##", title, "\n"); print(as.data.frame(x), row.names = FALSE); cat("\n") }
show("Flux source", count(inv, flux_source))
show("Flux source x analyzer", inv %>% count(analyzer, flux_source) %>% pivot_wider(names_from = flux_source, values_from = n, values_fill = 0))
show("Measurements by campaign x analyzer x type", inv %>% count(campaign, analyzer, measurement_type) %>% pivot_wider(names_from = measurement_type, values_from = n, values_fill = 0))
show("Field-log times", count(inv, has_fieldlog_start, has_fieldlog_end))
show("Raw coverage (as logged, no clock offset)", count(inv, raw_on_date, has_raw_in_fieldlog_window))
show("Saved manual window by analyzer x type", inv %>% count(analyzer, measurement_type, has_saved_window) %>% pivot_wider(names_from = has_saved_window, names_prefix = "saved_", values_from = n, values_fill = 0))
show("Saved-window minus field-log start (s), by analyzer x campaign",
     inv %>% filter(!is.na(saved_minus_fieldlog_s)) %>% group_by(analyzer, campaign) %>%
       summarise(n = n(), median = median(saved_minus_fieldlog_s), q10 = quantile(saved_minus_fieldlog_s, .1),
                 q90 = quantile(saved_minus_fieldlog_s, .9), .groups = "drop"))
show("Logging interval (s), by analyzer x campaign",
     inv %>% group_by(analyzer, campaign) %>%
       summarise(n = n(), median_dt = median(logging_interval_s, na.rm = TRUE),
                 min_dt = suppressWarnings(min(logging_interval_s, na.rm = TRUE)),
                 max_dt = suppressWarnings(max(logging_interval_s, na.rm = TRUE)),
                 n_na = sum(is.na(logging_interval_s)), .groups = "drop"))
show("Patches applied", inv %>% summarise(ha_hb = sum(patch_ha_hb_geometry), mar2022_soil = sum(patch_mar2022_soil),
                                          date_fix = sum(patch_date_fix), trimmed = sum(flux_source == "trimmed_window_refit"),
                                          artifacts = sum(flux_source == "artifact_removed")))
show("Air-temperature source", count(inv, air_temp_source))
show("Logger serials found in each analyzer folder (files, first-last date)",
     raw_index %>% filter(!is_duplicate) %>% group_by(unit, serial) %>%
       summarise(files = n(), first = min(as.Date(t_first)), last = max(as.Date(t_last)), .groups = "drop"))
show("Measurements whose field-log window holds data from >1 logger", inv %>%
       filter(grepl(";", raw_serials_in_window)) %>% count(analyzer, campaign, raw_serials_in_window))
show("Raw files by unit", raw_index %>% group_by(unit) %>%
       summarise(files = n(), duplicates = sum(is_duplicate), empty = sum(n_rows == 0),
                 with_clock_reset = sum(n_clock_reset_rows > 0, na.rm = TRUE),
                 serials = paste(unique(na.omit(serial)), collapse = " "), .groups = "drop"))
sink()
cat("Wrote", file.path(out_dir, c("raw_file_index.csv", "measurement_inventory.csv", "inventory_summary.txt")), sep = "\n  ")
