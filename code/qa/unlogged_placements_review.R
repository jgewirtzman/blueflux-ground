# =============================================================================
# QA / decision support (stage 04): chamber placements found by the legacy
# ebullition detector that overlap no logged closure.
#
# For every legacy "additional" placement (detect_ebullition.R, not excluded),
# on the analyzer clock (legacy offsets inverted):
#   - which logged closures (any analyzer, same site and day) are nearest,
#     and how far apart in time;
#   - CH4 and CO2 linear slopes over the placement, and the CH4 flux it would
#     give with floating-chamber geometry, beside that day's logged water and
#     stem fluxes at the site;
#   - a trace plot (CH4, CO2) with +-10 min of context and every logged
#     closure window on that analyzer shaded by component.
# Writes output/qa/unlogged_placements_review.csv and
# output/qa/unlogged_placements_review.pdf (one page per placement).
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(ggplot2); library(patchwork); library(tidyr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/00_lib/lib_raw.R")
utc <- function(x) as.POSIXct(sub("Z$", "", sub("T", " ", x)), tz = "UTC")

pl  <- read_csv("output/ebullition/placements_summary.csv", show_col_types = FALSE,
                col_types = cols(start_time = col_character(), end_time = col_character(), .default = col_guess())) %>%
  filter(!excluded, trace_type == "additional")
win <- read_csv("output/flux/02_windows/windows.csv", show_col_types = FALSE) %>%
  mutate(ws = as.POSIXct(start, tz = "UTC"), we = as.POSIXct(end, tz = "UTC"))
aux <- read_csv("output/flux/01_metadata/auxfile.csv", show_col_types = FALSE)
fit <- read_csv("output/flux/03_fit/CH4/fluxes.csv", show_col_types = FALSE) %>% select(UniqueID, CH4 = best.flux)
fit2 <- read_csv("output/flux/03_fit/CO2/fluxes.csv", show_col_types = FALSE) %>% select(UniqueID, CO2 = best.flux)
win <- win %>% left_join(fit, by = "UniqueID") %>% left_join(fit2, by = "UniqueID") %>%
  left_join(aux %>% select(UniqueID, plot), by = "UniqueID")

legacy_offsets <- tibble(analyzer = c("LGR2", "LGR2", "LGR3", "LGR3", "LGR3", "LGR2", "LGR1", "LGR3"),
                         date = as.Date(c("2022-10-23", "2023-03-11", "2023-03-12", "2023-03-15", "2023-03-16",
                                          "2023-03-17", "2023-03-18", "2023-03-22")),
                         lo = c(-1091, -28, -24, -13, -24, -28, -14, -24))
pl <- pl %>% mutate(date = as.Date(date)) %>% left_join(legacy_offsets, by = c("analyzer", "date")) %>%
  mutate(off = case_when(analyzer == "Picarro" ~ 25220, !is.na(lo) ~ -lo, TRUE ~ 0),
         s = utc(start_time) + off, e = utc(end_time) + off)

# floating-chamber flux term per analyzer type (geometry from the auxfile)
ft <- aux %>% filter(component == "water", !excluded) %>% group_by(inst = ifelse(grepl("^LGR", analyzer), "LGR", "Picarro")) %>%
  summarise(Area = first(Area), Vtot = first(Vtot), .groups = "drop") %>%
  mutate(flux_term = Vtot * 101.3 / (8.314 * (273.15 + 28) * Area / 1e4))   # mol m-2 per mol mol-1 (approx., 28 C)

rows <- list(); plots <- list()
for (i in seq_len(nrow(pl))) {
  p <- pl[i, ]
  r <- read_raw(p$analyzer, p$s - 600, p$e + 600)
  same_an <- win %>% filter(analyzer == p$analyzer, we >= p$s - 600, ws <= p$e + 600)
  site_day <- win %>% filter(date == p$date, plot == p$site)
  ov <- pmax(0, pmin(as.numeric(same_an$we), as.numeric(p$e)) - pmax(as.numeric(same_an$ws), as.numeric(p$s)))
  near <- site_day %>% mutate(gap_s = pmin(abs(as.numeric(ws - p$s)), abs(as.numeric(we - p$e)),
                                           ifelse(ws <= p$e & we >= p$s, 0, Inf))) %>% arrange(gap_s) %>% slice(1:3)
  seg <- r %>% filter(POSIX.time >= p$s, POSIX.time <= p$e)
  tt <- as.numeric(seg$POSIX.time) - as.numeric(p$s)   # seconds from placement start (raw epoch seconds are too large for lm)
  sl <- function(y) if (nrow(seg) > 3) unname(coef(lm(y ~ tt))[2]) else NA_real_
  ch4_slope <- sl(seg$CH4dry_ppb); co2_slope <- sl(seg$CO2dry_ppm)
  term <- ft$flux_term[ft$inst == ifelse(grepl("^LGR", p$analyzer), "LGR", "Picarro")]
  rows[[i]] <- tibble(placement_id = p$placement_id, analyzer = p$analyzer, site = p$site, date = p$date,
                      start = format(p$s, "%H:%M:%S"), end = format(p$e, "%H:%M:%S"), dur_s = as.numeric(p$e - p$s, units = "secs"),
                      overlaps_logged = paste(unique(same_an$component[ov > 30]), collapse = "+"),
                      nearest = paste(sprintf("%s %s (%s, %s-%s, gap %.0f s)", near$UniqueID, near$component, near$analyzer,
                                              format(near$ws, "%H:%M"), format(near$we, "%H:%M"), near$gap_s), collapse = "; "),
                      CH4_slope_ppb_s = ch4_slope, CO2_slope_ppm_s = co2_slope,
                      CH4_flux_if_water = ch4_slope * term,   # nmol m-2 s-1 (ppb s-1 x mol m-2)
                      site_day_water_CH4 = paste(round(site_day$CH4[site_day$component == "water"], 1), collapse = ", "),
                      site_day_stem_CH4_median = round(median(site_day$CH4[site_day$component == "stem"], na.rm = TRUE), 2),
                      legacy_n_jumps = p$n_jumps)
  shade <- same_an %>% transmute(xmin = ws, xmax = we, component)
  long <- r %>% select(POSIX.time, CH4dry_ppb, CO2dry_ppm) %>% pivot_longer(-POSIX.time)
  g <- ggplot(long, aes(POSIX.time, value)) +
    geom_rect(data = shade, aes(xmin = xmin, xmax = xmax, fill = component), ymin = -Inf, ymax = Inf, alpha = 0.25,
              inherit.aes = FALSE) +
    annotate("rect", xmin = p$s, xmax = p$e, ymin = -Inf, ymax = Inf, fill = NA, colour = "black", linetype = 2) +
    geom_line(linewidth = 0.4) + facet_wrap(~ name, ncol = 1, scales = "free_y") +
    scale_fill_manual(values = c(water = "#2a6f97", stem = "#c98b2a", root = "#8c6d31", soil = "#7a7a7a",
                                 cwd = "#5b8c5a", CWD = "#5b8c5a", leaves = "#a3c99a"), drop = FALSE) +
    labs(title = sprintf("%s   %s %s, analyzer clock %s-%s", p$placement_id, p$site, p$date,
                         format(p$s, "%H:%M:%S"), format(p$e, "%H:%M:%S")),
         subtitle = sprintf("dashed box = legacy placement; shaded = logged closure windows on %s. CH4 slope %.2f ppb/s, CO2 slope %.3f ppm/s",
                            p$analyzer, ch4_slope, co2_slope),
         x = NULL, y = NULL, fill = "logged closure") +
    theme_minimal(base_size = 9) + theme(legend.position = "bottom")
  plots[[i]] <- g
}
out <- bind_rows(rows)
write_csv(out, "output/qa/unlogged_placements_review.csv")
# whole-day overview per analyzer-day with placements that overlap no logged closure
days <- out %>% filter(overlaps_logged == "") %>% distinct(analyzer, date, site)
overview <- lapply(seq_len(nrow(days)), function(k) {
  dd <- days[k, ]; lw <- win %>% filter(analyzer == dd$analyzer, date == dd$date)
  r <- read_raw(dd$analyzer, min(lw$ws) - 1800, max(lw$we) + 1800)
  pp <- pl %>% filter(analyzer == dd$analyzer, date == dd$date) %>%
    left_join(out %>% select(placement_id, overlaps_logged), by = "placement_id") %>% mutate(unlogged = overlaps_logged == "")
  long <- r %>% select(POSIX.time, CH4dry_ppb, CO2dry_ppm) %>% pivot_longer(-POSIX.time)
  ggplot(long, aes(POSIX.time, value)) +
    geom_rect(data = lw, aes(xmin = ws, xmax = we, fill = component), ymin = -Inf, ymax = Inf, alpha = 0.3, inherit.aes = FALSE) +
    geom_rect(data = pp, aes(xmin = s, xmax = e, colour = unlogged), ymin = -Inf, ymax = Inf, fill = NA, linetype = 2,
              inherit.aes = FALSE) +
    geom_line(linewidth = 0.3) + facet_wrap(~ name, ncol = 1, scales = "free_y") +
    scale_colour_manual(values = c(`TRUE` = "red", `FALSE` = "grey40"), name = NULL,
                        labels = c(`FALSE` = "legacy placement overlapping a logged closure", `TRUE` = "legacy placement, no logged closure")) +
    labs(title = sprintf("%s, %s, %s: whole-day record; shaded = logged closures on this analyzer; dashed = legacy placements",
                         dd$analyzer, dd$site, dd$date), x = NULL, y = NULL, fill = "logged closure") +
    theme_minimal(base_size = 9) + theme(legend.position = "bottom")
})
pdf("output/qa/unlogged_placements_review.pdf", width = 10, height = 6)
for (g in overview) print(g)
for (g in plots) print(g)
invisible(dev.off())
print(as.data.frame(out %>% select(placement_id, start, end, dur_s, overlaps_logged, CH4_slope_ppb_s, CO2_slope_ppm_s,
                                   CH4_flux_if_water) %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)
