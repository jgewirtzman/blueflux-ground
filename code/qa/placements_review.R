# =============================================================================
# QA: every floating-chamber placement found by stage 04 (01_placements.R),
# drawn on its raw CH4 and CO2 record with 5 min of context: placement bounds
# (dashed), diffusive window (blue) and the stage-02 fit window (orange bar).
# Writes output/qa/placements_review.pdf (one page per placement).
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(ggplot2); library(tidyr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/00_lib/lib_raw.R")
utc <- function(x) as.POSIXct(x, tz = "UTC")

pl <- read_csv("output/flux/04_ebullition/placements.csv", show_col_types = FALSE,
               col_types = cols(.default = col_character())) %>%
  mutate(across(c(placement_start, placement_end, diffusive_start, diffusive_end, fit_window_start, fit_window_end), utc))
pdf("output/qa/placements_review.pdf", width = 10, height = 6)
for (i in seq_len(nrow(pl))) {
  p <- pl[i, ]
  r <- read_raw(p$analyzer, p$placement_start - 300, p$placement_end + 300)
  long <- r %>% select(POSIX.time, CH4dry_ppb, CO2dry_ppm) %>% pivot_longer(-POSIX.time)
  g <- ggplot(long, aes(POSIX.time, value)) +
    annotate("rect", xmin = p$diffusive_start, xmax = p$diffusive_end, ymin = -Inf, ymax = Inf, fill = "#2a6f97", alpha = 0.2) +
    geom_vline(xintercept = c(p$placement_start, p$placement_end), linetype = 2) +
    annotate("segment", x = p$fit_window_start, xend = p$fit_window_end, y = -Inf, yend = -Inf, colour = "#c98b2a", linewidth = 3) +
    geom_line(linewidth = 0.35) + facet_wrap(~ name, ncol = 1, scales = "free_y") +
    labs(title = sprintf("%s  (%s, %s)  %s-%s, %s min, ends by %s", p$placement_id, p$analyzer, p$date,
                         format(p$placement_start, "%H:%M:%S"), format(p$placement_end, "%H:%M:%S"),
                         round(as.numeric(p$duration_s) / 60, 1), p$end_by),
         subtitle = sprintf("dashed = placement; blue = diffusive window (%s); orange = stage-02 fit window", p$diffusive_rule),
         x = NULL, y = NULL) +
    theme_minimal(base_size = 9)
  print(g)
}
invisible(dev.off())
cat("Wrote output/qa/placements_review.pdf:", nrow(pl), "pages\n")
