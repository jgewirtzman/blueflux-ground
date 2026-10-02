# =============================================================================
# Floating-chamber placements (pipeline stage 04, step 1).
#
# One flux per placement (Jon, 2026-10-01): a placement runs from the moment the
# chamber goes on the water to the moment it is lifted, however many closures
# the field sheet logged inside it. For every water closure kept by stage 02,
# the placement is found in the raw CH4 record of its analyzer:
#   - a lift is a fall of CH4 by more than max(LIFT_MIN_PPB, LIFT_FRAC x the
#     excess over background) within LIFT_LAG_S (bubbles only ever raise CH4);
#   - a gap in the record longer than GAP_S (analyzer stopped) also ends a
#     placement, and starts the search for the next one;
#   - end   = the first lift or gap after the closure's fit window starts (or
#             the end of the record / the next closure on that analyzer);
#   - start = the last point at background (CH4 minimum + max(START_TOL_PPB,
#             START_TOL_FRAC x the rise to the fit window)) between the previous
#             lift (or record gap) and the fit window start; with no lift before
#             it (the chamber went on from ambient), the fit window start.
# Start-up readings (CH4 < MIN_CH4_PPB or CO2 < MIN_CO2_PPM, e.g. at power-on
# or after a restart) are dropped first.
# Unlogged placements marked `add` in data/flux_metadata/unlogged_placements.csv
# are placements too, with their checked start and end.
#
# Diffusive window: placements longer than LONG_S use the first DIFF_S after
# the placement starts (+ DEADBAND_S), as the long runs decline with time in
# place (code/qa/long_deployments.R); shorter placements keep their stage-02
# window. The ebullition trace is the whole placement.
#
# Writes output/flux/04_ebullition/placements.csv.
# =============================================================================
suppressMessages({library(dplyr); library(readr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/00_lib/lib_raw.R")

LIFT_LAG_S <- 30; LIFT_FRAC <- 0.5; LIFT_MIN_PPB <- 20; GAP_S <- 60
MIN_CH4_PPB <- 1500; MIN_CO2_PPM <- 300
START_TOL_PPB <- 10; START_TOL_FRAC <- 0.05
SEARCH_S <- 3 * 3600           # how far a placement may extend beyond its fit window
LONG_S <- 12 * 60; DIFF_S <- 600; DEADBAND_S <- 30
utc <- function(x) as.POSIXct(x, tz = "UTC")

aux <- read_csv("output/flux/01_metadata/auxfile.csv", show_col_types = FALSE)
win <- read_csv("output/flux/02_windows/windows.csv", show_col_types = FALSE) %>%
  mutate(ws = utc(start), we = utc(end)) %>% filter(!is.na(ws))
unl <- read_csv("data/flux_metadata/unlogged_placements.csv", show_col_types = FALSE) %>% filter(decision == "add") %>%
  transmute(UniqueID = placement_id, analyzer, plot = site, date = as.Date(date), component = "water",
            ws = utc(start_analyzer_clock), we = utc(end_analyzer_clock), geometry_from, logged = FALSE)

water <- win %>% filter(component == "water") %>%
  left_join(aux %>% select(UniqueID, plot), by = "UniqueID") %>%
  transmute(UniqueID, analyzer, plot, date = as.Date(date), component, ws, we, geometry_from = UniqueID, logged = TRUE)
# every closure on the analyzers (any component) bounds the search
others <- bind_rows(win %>% select(UniqueID, analyzer, ws, we), unl %>% select(UniqueID, analyzer, ws, we))

lifts_in <- function(r) {
  if (nrow(r) < 5) return(as.POSIXct(character(0), tz = "UTC"))
  t <- as.numeric(r$POSIX.time); y <- stats::runmed(r$CH4dry_ppb, if (nrow(r) >= 5) 5 else 1)
  base <- stats::quantile(y, 0.05, names = FALSE)
  ahead <- approx(t, y, xout = t + LIFT_LAG_S, rule = 2)$y
  drop <- y - ahead
  is_lift <- drop > pmax(LIFT_MIN_PPB, LIFT_FRAC * (y - base)) & (y - base) > LIFT_MIN_PPB
  first <- which(is_lift & !c(FALSE, head(is_lift, -1)))
  r$POSIX.time[first]
}

find_placement <- function(p) {
  near <- others %>% filter(analyzer == p$analyzer, UniqueID != p$UniqueID)
  prev_end <- suppressWarnings(max(near$we[near$we <= p$ws], na.rm = TRUE))
  next_start <- suppressWarnings(min(near$ws[near$ws >= p$we - 1], na.rm = TRUE))
  lo <- if (is.finite(prev_end)) max(prev_end, p$ws - SEARCH_S) else p$ws - SEARCH_S
  hi <- if (is.finite(next_start)) min(next_start, p$we + SEARCH_S) else p$we + SEARCH_S
  r <- read_raw(p$analyzer, lo, hi) %>% filter(CH4dry_ppb >= MIN_CH4_PPB, CO2dry_ppm >= MIN_CO2_PPM)
  if (nrow(r) < 5) return(tibble(placement_start = p$ws, placement_end = p$we, end_by = "no record"))
  gi <- which(diff(as.numeric(r$POSIX.time)) > GAP_S)
  gap_start <- r$POSIX.time[gi]; gap_end <- r$POSIX.time[gi + 1]
  L <- lifts_in(r)
  after <- sort(c(L[L > p$ws + 30], gap_start[gap_start > p$ws + 30]))
  end <- if (length(after)) min(after) else max(r$POSIX.time)
  end_by <- if (!length(after)) { if (is.finite(next_start) && hi == next_start) "next closure" else "end of record" } else
    if (end %in% gap_start) "record gap" else "lift"
  before <- c(L[L < p$ws], gap_end[gap_end < p$ws] - LIFT_LAG_S)
  if (length(before)) {
    seg <- r %>% filter(POSIX.time >= max(before) + LIFT_LAG_S, POSIX.time <= p$ws)
    # the chamber goes on where CH4 last sits at background (minimum + tolerance)
    # before it rises into the fit window; a flat stretch of ambient air after
    # the previous lift is not part of the placement
    start <- if (nrow(seg)) {
      y <- stats::runmed(seg$CH4dry_ppb, if (nrow(seg) >= 5) 5 else 1)
      tol <- max(START_TOL_PPB, START_TOL_FRAC * (tail(y, 1) - min(y)))
      at_bg <- which(y <= min(y) + tol)
      seg$POSIX.time[max(at_bg[at_bg <= max(which.min(y), length(y))])]
    } else p$ws
  } else start <- max(p$ws, min(r$POSIX.time))
  tibble(placement_start = min(start, p$ws), placement_end = max(end, min(p$we, end)), end_by)
}

logged <- bind_rows(lapply(seq_len(nrow(water)), function(i) bind_cols(water[i, ], find_placement(water[i, ]))))
pl <- bind_rows(logged, unl %>% mutate(placement_start = ws, placement_end = we, end_by = "curated (unlogged)")) %>%
  mutate(duration_s = as.numeric(placement_end - placement_start, units = "secs"),
         long = duration_s > LONG_S,
         diff_start = if_else(long, placement_start + DEADBAND_S, ws),
         diff_end = if_else(long, pmin(placement_start + DEADBAND_S + DIFF_S, placement_end), pmin(we, placement_end)),
         diffusive_rule = if_else(long, sprintf("first %d s of a %.0f-min placement", DIFF_S, duration_s / 60),
                                  "stage-02 window"))

# two logged closures in one placement would be two fluxes from one placement
dup <- pl %>% arrange(analyzer, placement_start) %>% group_by(analyzer) %>%
  filter(placement_start < lag(placement_end) - 30 | lead(placement_start) < placement_end - 30) %>% ungroup()
if (nrow(dup)) { print(as.data.frame(dup %>% select(UniqueID, analyzer, placement_start, placement_end)))
  stop("closures share a placement; exclude all but one (data/flux_metadata/excluded_measurements.csv)") }

dir.create("output/flux/04_ebullition", recursive = TRUE, showWarnings = FALSE)
f <- function(x) format(x, "%Y-%m-%d %H:%M:%S")
out <- pl %>% transmute(placement_id = UniqueID, logged, analyzer, plot, date, geometry_from,
                        placement_start = f(placement_start), placement_end = f(placement_end), end_by,
                        duration_s = round(duration_s), long, diffusive_start = f(diff_start), diffusive_end = f(diff_end),
                        diffusive_rule, fit_window_start = f(ws), fit_window_end = f(we)) %>%
  arrange(date, analyzer, placement_start)
write_csv(out, "output/flux/04_ebullition/placements.csv")
cat("Placements:", nrow(out), "(", sum(out$logged), "logged,", sum(!out$logged), "unlogged ) | long:", sum(out$long), "\n")
print(table(out$end_by))
cat("\nLong placements:\n")
print(as.data.frame(out %>% filter(long) %>% transmute(placement_id, start = substr(placement_start, 12, 19),
                                                        end = substr(placement_end, 12, 19), min = round(duration_s / 60, 1), end_by)), row.names = FALSE)
