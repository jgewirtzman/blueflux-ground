# =============================================================================
# Export the raw analyzer records as clean daily CSVs (deposit granule and the
# pipeline's raw input).
#
# From the vendor files indexed by 00_index_raw_files.R (LGR GLA131 x3 text logs,
# zipped or not; Picarro G4301 .dat), keeping only each unit's own logger serial and
# skipping duplicate files, exactly as code/00_lib/lib_raw.R read_raw() does, one file
# per analyzer and day (analyzer clock date):
#   data/deposit/chamber_fluxes/analyzer_records/<unit>_<YYYY-MM-DD>.csv
# Columns: time_analyzer (analyzer clock, ISO 8601; not corrected - clock offsets are in
# data/flux_metadata and the flux table), co2_dry_ppm, ch4_dry_ppb, h2o_ppm,
# source_file (original vendor file name). Duplicate time stamps keep the first record in
# index order (as read_raw()). Rows sorted by time.
# Once written, lib_raw.R reads these instead of the vendor files.
# =============================================================================
suppressMessages({library(dplyr); library(readr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
Sys.setenv(BLUEFLUX_RAW_VENDOR = "1")                      # force lib_raw to read the vendor files
source("code/00_lib/lib_raw.R")
out <- "data/deposit/chamber_fluxes/analyzer_records"; dir.create(out, recursive = TRUE, showWarnings = FALSE)
# ISO 8601 to the millisecond, rounded (format's %OS3 truncates: x.928999... -> x.928)
iso_ms <- function(t) {
  ms <- round(as.numeric(t) * 1000)
  paste0(format(as.POSIXct(ms %/% 1000, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%dT%H:%M:%S", tz = "UTC"), sprintf(".%03d", ms %% 1000))
}
ix <- raw_index()
n <- 0
for (u in unique(ix$unit)) {
  iu <- ix %>% filter(unit == u)
  d <- bind_rows(lapply(iu$file, .read_file, unit = u))
  if (!nrow(d)) next
  d <- d %>% distinct(POSIX.time, .keep_all = TRUE) %>% arrange(POSIX.time) %>%
    mutate(day = format(POSIX.time, "%Y-%m-%d", tz = "UTC"))
  for (dd in split(d, d$day)) {
    write_csv(dd %>% transmute(time_analyzer = iso_ms(POSIX.time),
                               co2_dry_ppm = CO2dry_ppm, ch4_dry_ppb = CH4dry_ppb, h2o_ppm = H2O_ppm,
                               source_file = sub("^data/analyzer/", "", source_file)),
              file.path(out, sprintf("%s_%s.csv", u, dd$day[1])), na = "-9999")
    n <- n + 1
  }
  rm(list = ls(envir = .raw_cache), envir = .raw_cache)
}
cat("wrote", n, "daily files to", out, "\n")
