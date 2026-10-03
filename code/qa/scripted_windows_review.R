# =============================================================================
# QA: tree closures (stem, root) fitted on scripted windows (field log +
# offset; no saved manual window) whose CH4 or CO2 flux differs from the legacy
# value by more than DIFF_FRAC and MIN_ABS. Each is drawn on its raw record with
# 5 min of context, its window and its field-log interval, and every other
# logged window on that analyzer. Writes output/qa/scripted_windows_review.csv
# and output/qa/scripted_windows_review.pdf.
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(ggplot2); library(tidyr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/00_lib/lib_raw.R")
DIFF_FRAC <- 0.2; MIN_ABS <- c(CH4 = 0.1, CO2 = 0.1)
T <- function(x) as.POSIXct(x, tz = "UTC")

d <- read_csv("output/data_products/flux_measurements_all.csv", show_col_types = FALSE, guess_max = 5000)
w <- read_csv("output/flux/02_windows/windows.csv", show_col_types = FALSE) %>%
  mutate(ws = T(start), we = T(end), fs = T(field_start) + offset_s, fe = T(field_end) + offset_s)
x <- d %>% filter(component %in% c("stem", "root"), use_in_analysis, grepl("^scripted", window_source)) %>%
  mutate(dCH4 = CH4_best.flux - legacy_CH4_best.flux, dCO2 = CO2_best.flux - legacy_CO2_best.flux,
         big_CH4 = abs(dCH4) > pmax(DIFF_FRAC * abs(legacy_CH4_best.flux), MIN_ABS[["CH4"]]),
         big_CO2 = abs(dCO2) > pmax(DIFF_FRAC * abs(legacy_CO2_best.flux), MIN_ABS[["CO2"]])) %>%
  filter(big_CH4 %in% TRUE | big_CO2 %in% TRUE) %>%
  transmute(flux_id, plot, date, analyzer = analyzer_source, component, window_source,
            CH4 = CH4_best.flux, CH4_legacy = legacy_CH4_best.flux, CO2 = CO2_best.flux, CO2_legacy = legacy_CO2_best.flux,
            CH4_model, CO2_model, CH4_flagged, CO2_flagged) %>% arrange(date, analyzer, flux_id)
write_csv(x, "output/qa/scripted_windows_review.csv")

pdf("output/qa/scripted_windows_review.pdf", width = 10, height = 6)
for (i in seq_len(nrow(x))) {
  wi <- w %>% filter(UniqueID == x$flux_id[i])
  r <- read_raw(wi$analyzer, wi$ws - 300, wi$we + 300)
  if (is.null(r) || !nrow(r)) next
  ctx <- w %>% filter(analyzer == wi$analyzer, we > wi$ws - 300, ws < wi$we + 300, UniqueID != wi$UniqueID)
  long <- r %>% select(POSIX.time, `CH4 (ppb)` = CH4dry_ppb, `CO2 (ppm)` = CO2dry_ppm) %>% pivot_longer(-POSIX.time)
  g <- ggplot(long, aes(POSIX.time, value)) +
    geom_rect(data = ctx, aes(xmin = ws, xmax = we), ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.6, inherit.aes = FALSE) +
    annotate("rect", xmin = wi$ws, xmax = wi$we, ymin = -Inf, ymax = Inf, fill = "#2a6f97", alpha = 0.2) +
    annotate("segment", x = wi$fs, xend = wi$fe, y = -Inf, yend = -Inf, colour = "#c98b2a", linewidth = 3) +
    geom_line(linewidth = 0.35) + facet_wrap(~ name, ncol = 1, scales = "free_y") +
    labs(title = sprintf("%s (%s, %s)", x$flux_id[i], x$analyzer[i], x$date[i]),
         subtitle = sprintf("CH4 %.2f (legacy %.2f) | CO2 %.2f (legacy %.2f); blue = fit window, orange = field log, grey = other closures",
                            x$CH4[i], x$CH4_legacy[i], x$CO2[i], x$CO2_legacy[i]), x = NULL, y = NULL) +
    theme_minimal(base_size = 9)
  print(g)
}
invisible(dev.off())
cat("Scripted-window tree closures differing from legacy:", nrow(x), "\n")
print(as.data.frame(x %>% count(analyzer, format(date, "%Y-%m"))), row.names = FALSE)
