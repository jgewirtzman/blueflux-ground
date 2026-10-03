# =============================================================================
# Raw analyzer reader for the rebuild (sourced by 06_* and later steps).
#
# read_raw(unit, from, to): concentration records of one analyzer between two
# times (analyzer clock, compared as UTC-labelled), in goFlux column names:
#   POSIX.time, CO2dry_ppm, CH4dry_ppb, H2O_ppm, source_file
# Files come from output/flux/00_raw/raw_file_index.csv (02_measurement_inventory.R):
# only the unit's own logger serial, duplicates skipped, zipped files read
# from a temporary directory. Nothing is written next to the raw data.
#
# fresh_ch4(tr, unit) / fresh_co2(tr, unit): the Picarro G4301 measures one
# gas per logged row, alternating: CH4 is fresh on every other row and carried
# forward (|dCH4| < PICARRO_HELD_PPB) on the rows in between, which are the
# rows where CO2 is fresh (CO2 changes 13-16x more on them;
# code/qa/picarro_update_cadence.R). Fits, noise and bubble detection use each
# gas's fresh rows only. TRUE for every LGR row.
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(stringr)})

RAW_ROOT <- "data/analyzer"
UNIT_SERIAL <- c(LGR1 = "SN:3K60180500001585", LGR2 = "SN:3K60180500001583",
                 LGR3 = "SN:3K60180500001584")
.raw_index <- NULL
.raw_cache <- new.env(parent = emptyenv())

raw_index <- function() {
  if (is.null(.raw_index)) {
    ix <- read_csv("output/flux/00_raw/raw_file_index.csv", show_col_types = FALSE)
    .raw_index <<- ix %>% filter(!is_duplicate, n_rows > 0,
                                 unit == "Picarro" | serial == UNIT_SERIAL[unit])
  }
  .raw_index
}

.read_lgr <- function(path) {
  lines <- readLines(path, warn = FALSE)
  hdr <- grep("\\[CH4\\]", lines)[1]
  cols <- trimws(strsplit(lines[hdr], ",")[[1]])
  dat <- lines[grepl("^\\s*\\d{1,2}/\\d{1,2}/\\d{4}\\s+\\d", lines)]
  m <- read.csv(text = paste(dat, collapse = "\n"), header = FALSE, stringsAsFactors = FALSE,
                strip.white = TRUE)[, seq_along(cols)]
  names(m) <- cols
  tibble(POSIX.time = as.POSIXct(m$SysTime, format = "%m/%d/%Y %H:%M:%OS", tz = "UTC"),
         CO2dry_ppm = as.numeric(m[["[CO2]d_ppm"]]),
         CH4dry_ppb = as.numeric(m[["[CH4]d_ppm"]]) * 1000,
         H2O_ppm    = as.numeric(m[["[H2O]_ppm"]]))
}

.read_picarro <- function(path) {
  m <- read.table(path, header = TRUE, stringsAsFactors = FALSE)
  tibble(POSIX.time = as.POSIXct(paste(m$DATE, m$TIME), format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
         CO2dry_ppm = as.numeric(m$CO2_dry),
         CH4dry_ppb = as.numeric(m$CH4_dry) * 1000,
         H2O_ppm    = as.numeric(m$H2O) * 1e4)          # Picarro H2O is in %
}

.read_file <- function(file, unit) {
  if (exists(file, envir = .raw_cache)) return(get(file, envir = .raw_cache))
  if (grepl("!", file, fixed = TRUE)) {                 # "<archive>.zip!<member>"
    parts <- strsplit(file, "!", fixed = TRUE)[[1]]
    ex <- tempfile("rawzip_"); dir.create(ex)
    on.exit(unlink(ex, recursive = TRUE))
    path <- unzip(parts[1], files = parts[2], exdir = ex)
    if (!length(path)) path <- file.path(ex, parts[2])
  } else path <- file
  d <- tryCatch(if (unit == "Picarro") .read_picarro(path) else .read_lgr(path),
                error = function(e) NULL)
  if (!is.null(d)) d <- d %>% filter(!is.na(POSIX.time)) %>% mutate(source_file = file)
  assign(file, d, envir = .raw_cache)
  d
}

PICARRO_HELD_PPB <- 0.3
fresh_ch4 <- function(tr, unit) {
  if (unit != "Picarro" || nrow(tr) < 2) return(rep(TRUE, nrow(tr)))
  c(TRUE, abs(diff(tr$CH4dry_ppb)) >= PICARRO_HELD_PPB)
}

fresh_co2 <- function(tr, unit) {
  if (unit != "Picarro" || nrow(tr) < 2) return(rep(TRUE, nrow(tr)))
  f <- !fresh_ch4(tr, unit); f[1] <- TRUE; f
}

read_raw <- function(unit, from, to) {
  ix <- raw_index() %>% filter(unit == !!unit, t_last >= from, t_first <= to)
  if (!nrow(ix)) return(NULL)
  bind_rows(lapply(ix$file, .read_file, unit = unit)) %>%
    filter(POSIX.time >= from, POSIX.time <= to) %>%
    distinct(POSIX.time, .keep_all = TRUE) %>%
    arrange(POSIX.time)
}
