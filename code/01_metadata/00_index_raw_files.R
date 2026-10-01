# =============================================================================
# Index the raw analyzer files (pipeline stage 01, step 0).
#
# One row per raw logger file: analyzer folder, logger serial, rows, first/last
# timestamp, logging interval (median and modes), clock-reset rows, SysTime -
# Time offset. Zipped LGR files are extracted to a temporary directory only;
# nothing is written next to the raw data. The same logger file can exist
# unzipped and zipped: the copy kept is the first, the other is flagged
# is_duplicate. code/00_lib/lib_raw.R reads this index.
#
# Needs data/analyzer/ (gitignored raw data).
# Writes output/flux/00_raw/raw_file_index.csv.
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(stringr); library(purrr); library(lubridate)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

raw_root <- "data/analyzer"
out_dir  <- "output/flux/00_raw"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(dir.exists(raw_root))
to_utc <- function(x) as.POSIXct(x, origin = "1970-01-01", tz = "UTC")

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

