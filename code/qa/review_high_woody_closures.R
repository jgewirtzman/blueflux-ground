# =============================================================================
# QA: high woody CH4 closures under review (SRS6 prop root Oct_22_48; BL60
# Oct 2022 stem bases). Raw CH4 traces in context on the same analyzer, and raw
# CH4 rise rates (ppb s-1, linear over the fit window; independent of chamber
# geometry) beside the geometry factor V/A (system volume / enclosed area),
# to test whether a mis-recorded chamber could explain the magnitudes.
# Needs data/analyzer. Writes output/qa/review_high_woody_{SRS6,BL60}.png and
# output/qa/review_high_woody_rates.csv.
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(ggplot2)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/00_lib/lib_raw.R")
T <- function(x) as.POSIXct(x, tz = "UTC")
win <- read_csv("output/flux/02_windows/windows.csv", show_col_types = FALSE) %>% mutate(ws = T(start), we = T(end))
d <- read.csv("output/data_products/combined_gas_flux_dataset.csv")
rate <- function(an, s, e) { r <- read_raw(an, s, e); t <- as.numeric(r$POSIX.time) - as.numeric(s); coef(lm(r$CH4dry_ppb ~ t))[[2]] }
sets <- list(SRS6 = list(an = "LGR2", from = "2022-10-17 12:50", to = "2022-10-17 14:45"),
             BL60a = list(an = "LGR1", from = "2022-10-25 10:45", to = "2022-10-25 12:00"),
             BL60b = list(an = "LGR3", from = "2022-10-25 14:25", to = "2022-10-25 15:40"))
rates <- list()
for (nm in names(sets)) {
  s <- sets[[nm]]; w <- win %>% filter(analyzer == s$an, we > T(s$from), ws < T(s$to))
  rates[[nm]] <- w %>% rowwise() %>% mutate(raw_CH4_ppb_s = rate(analyzer, ws, we)) %>% ungroup() %>%
    left_join(d %>% transmute(UniqueID = flux_id, chamber_class, height, area_cm2 = surface_area_cm2,
                              vol_cm3 = total_system_volume_cm3, CH4_flux = CH4_best.flux), by = "UniqueID") %>%
    mutate(V_over_A_cm = vol_cm3 / area_cm2) %>% select(UniqueID, component, chamber_class, height, start, raw_CH4_ppb_s, V_over_A_cm, CH4_flux)
}
rt <- bind_rows(rates); write_csv(rt, "output/qa/review_high_woody_rates.csv")
print(as.data.frame(rt %>% mutate(start = format(start, "%H:%M"), across(where(is.double), ~ signif(.x, 3)))), row.names = FALSE)
plot_set <- function(s, file, title) {
  r <- read_raw(s$an, T(s$from), T(s$to)); w <- win %>% filter(analyzer == s$an, we > T(s$from), ws < T(s$to)) %>%
    mutate(lab = sub("^Oct_22_", "", sub("_(SRS6|BL60)_", " ", UniqueID)))
  p <- ggplot(r, aes(POSIX.time, CH4dry_ppb / 1000)) +
    geom_rect(data = w, aes(xmin = ws, xmax = we, ymin = -Inf, ymax = Inf, fill = component), alpha = 0.25, inherit.aes = FALSE) +
    geom_text(data = w, aes(x = ws, y = Inf, label = lab), angle = 90, hjust = 1.1, vjust = 1, size = 2.4, inherit.aes = FALSE) +
    geom_line(linewidth = 0.3) + scale_y_log10() + labs(title = title, x = "analyzer clock", y = "CH4 (ppm, log scale)") +
    theme_bw(base_size = 9) + theme(legend.position = "bottom")
  ggsave(file, p, width = 12, height = 4.5, dpi = 130)
}
plot_set(sets$SRS6, "output/qa/review_high_woody_SRS6.png", "SRS6 2022-10-17, LGR2: tree 44-46 (stems), roots 48 and 47, tree 31-32")
plot_set(sets$BL60b, "output/qa/review_high_woody_BL60.png", "BL60 2022-10-25, LGR3: Laguncularia 254-255, 228-233")
