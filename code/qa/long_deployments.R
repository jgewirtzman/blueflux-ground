# =============================================================================
# QA / decision support (stage 04): long floating-chamber deployments.
# Runs of >= 15 min without a chamber lift (consecutive legacy placement chunks,
# output/ebullition/placements_summary.csv; legacy offsets inverted to the
# analyzer clock) are cut into 10-min slices; per slice the CH4 and CO2 linear
# flux (floating-chamber flux term). Compared with: the first slice of the same
# run, and the separate (short) water placements at the same site and day.
# Writes output/qa/long_deployments_slices.csv and long_deployments_summary.csv.
# =============================================================================
suppressMessages({library(dplyr); library(readr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/00_lib/lib_raw.R")
utc <- function(x) as.POSIXct(sub("Z$", "", sub("T", " ", x)), tz = "UTC")
SLICE_S <- 600

legacy_offsets <- tibble(analyzer = c("LGR2", "LGR2", "LGR3", "LGR3", "LGR3", "LGR2", "LGR1", "LGR3"),
                         date = as.Date(c("2022-10-23", "2023-03-11", "2023-03-12", "2023-03-15", "2023-03-16",
                                          "2023-03-17", "2023-03-18", "2023-03-22")),
                         lo = c(-1091, -28, -24, -13, -24, -28, -14, -24))
p <- read_csv("output/ebullition/placements_summary.csv", show_col_types = FALSE,
              col_types = cols(start_time = col_character(), end_time = col_character(), .default = col_guess())) %>%
  mutate(date = as.Date(date), s = utc(start_time), e = utc(end_time)) %>%
  left_join(legacy_offsets, by = c("analyzer", "date")) %>%
  mutate(off = case_when(analyzer == "Picarro" ~ 25220, !is.na(lo) ~ -lo, TRUE ~ 0), s = s + off, e = e + off) %>%
  arrange(analyzer, date, s) %>% group_by(analyzer, date) %>%
  mutate(run = cumsum(is.na(lag(e)) | as.numeric(s) - as.numeric(lag(e)) > 5)) %>% ungroup()
runs <- p %>% group_by(analyzer, date, site, run) %>%
  summarise(s = min(s), e = max(e), .groups = "drop") %>%
  filter(as.numeric(e - s, units = "secs") >= 900) %>% mutate(run_id = paste(analyzer, date, site, format(s, "%H%M"), sep = "_"))

aux <- read_csv("output/flux/01_metadata/auxfile.csv", show_col_types = FALSE)
term_of <- function(an, dt, site) {
  g <- aux %>% filter(component == "water", analyzer == an, date == dt, plot == site)
  if (!nrow(g)) g <- aux %>% filter(component == "water", grepl(substr(an, 1, 3), analyzer))
  g$Vtot[1] * mean(g$Pcham) / (8.314 * (mean(g$Tcham) + 273.15) * g$Area[1] / 1e4)
}
slices <- bind_rows(lapply(seq_len(nrow(runs)), function(k) {
  R <- runs[k, ]; r <- read_raw(R$analyzer, R$s, R$e); term <- term_of(R$analyzer, R$date, R$site)
  br <- seq(R$s, R$e, by = SLICE_S); if (as.numeric(R$e - tail(br, 1), units = "secs") < 300) br[length(br)] <- R$e else br <- c(br, R$e)
  bind_rows(lapply(seq_len(length(br) - 1), function(i) {
    sg <- r %>% filter(POSIX.time >= br[i], POSIX.time < br[i + 1]); t <- as.numeric(sg$POSIX.time) - as.numeric(br[i])
    if (nrow(sg) < 10) return(NULL)
    tibble(run_id = R$run_id, analyzer = R$analyzer, date = R$date, site = R$site, slice = i,
           start = format(br[i], "%H:%M"), n = nrow(sg),
           CH4_flux = unname(coef(lm(sg$CH4dry_ppb ~ t))[2]) * term,
           CO2_flux = unname(coef(lm(sg$CO2dry_ppm ~ t))[2]) * term)
  }))
}))
fit <- read_csv("output/flux/03_fit/CH4/fluxes.csv", show_col_types = FALSE) %>% select(UniqueID, best.flux)
win <- read_csv("output/flux/02_windows/windows.csv", show_col_types = FALSE) %>%
  mutate(ws = as.POSIXct(start, tz = "UTC"), we = as.POSIXct(end, tz = "UTC")) %>% filter(component == "water")
sep <- win %>% inner_join(aux %>% select(UniqueID, plot), by = "UniqueID") %>% inner_join(fit, by = "UniqueID")
summ <- runs %>% rowwise() %>% mutate(
  n_slices = sum(slices$run_id == run_id),
  first_slice = slices$CH4_flux[slices$run_id == run_id][1],
  later_slices_mean = mean(slices$CH4_flux[slices$run_id == run_id][-1]),
  run_mean = mean(slices$CH4_flux[slices$run_id == run_id]),
  co2_first = slices$CO2_flux[slices$run_id == run_id][1],
  co2_last = tail(slices$CO2_flux[slices$run_id == run_id], 1),
  sep_same_day_n = sum(sep$plot == site & sep$date == date & (sep$we < s | sep$ws > e)),
  sep_same_day_mean = mean(sep$best.flux[sep$plot == site & sep$date == date & (sep$we < s | sep$ws > e)])) %>%
  ungroup() %>% mutate(start = format(s, "%H:%M"), end = format(e, "%H:%M")) %>% select(-s, -e, -run)
write_csv(slices, "output/qa/long_deployments_slices.csv")
write_csv(summ, "output/qa/long_deployments_summary.csv")
print(as.data.frame(summ %>% mutate(across(where(is.double), ~ round(.x, 2)))), row.names = FALSE)
cat("\nSlices (CH4):\n")
print(as.data.frame(slices %>% group_by(run_id) %>% summarise(slices = paste(round(CH4_flux, 2), collapse = ", "))), row.names = FALSE)
