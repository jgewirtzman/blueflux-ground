# =============================================================================
# QA: closures on the same analyzer whose fit windows share samples.
# One analyzer has one inlet, so two closures cannot be measured at once: an
# overlap of more than OVERLAP_S means at least one window holds another
# closure's data (or the flush between them). For every such pair (excluded
# closures left out), lists both field-log times and windows, and plots the
# trace (CH4, CO2) with both windows and both field-log intervals.
# Writes output/qa/window_overlaps.csv and output/qa/window_overlaps.pdf.
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(ggplot2); library(tidyr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/00_lib/lib_raw.R")
OVERLAP_S <- 30

w   <- read_csv("output/flux/02_windows/windows.csv", show_col_types = FALSE) %>%
  mutate(ws = as.POSIXct(start, tz = "UTC"), we = as.POSIXct(end, tz = "UTC"),
         fs = as.POSIXct(field_start, tz = "UTC") + offset_s, fe = as.POSIXct(field_end, tz = "UTC") + offset_s)
aux <- read_csv("output/flux/01_metadata/auxfile.csv", show_col_types = FALSE) %>% select(UniqueID, excluded)
fit <- read_csv("output/flux/03_fit/CH4/fluxes.csv", show_col_types = FALSE) %>% select(UniqueID, CH4 = best.flux)
w <- w %>% inner_join(aux, by = "UniqueID") %>% filter(!excluded) %>% left_join(fit, by = "UniqueID")

pairs <- w %>% inner_join(w, by = "analyzer", suffix = c("_a", "_b"), relationship = "many-to-many") %>%
  filter(ws_a < ws_b | (ws_a == ws_b & UniqueID_a < UniqueID_b)) %>%
  mutate(overlap_s = as.numeric(pmin(we_a, we_b)) - as.numeric(pmax(ws_a, ws_b)),
         field_overlap_s = as.numeric(pmin(fe_a, fe_b)) - as.numeric(pmax(fs_a, fs_b))) %>%
  filter(overlap_s > OVERLAP_S) %>% arrange(ws_a)
hm <- function(x) format(x, "%H:%M:%S")
out <- pairs %>% transmute(analyzer, date = as.Date(ws_a), a = UniqueID_a, b = UniqueID_b, overlap_s,
                           field_overlap_s, src_a = window_source_a, src_b = window_source_b,
                           win_a = paste(hm(ws_a), hm(we_a)), field_a = paste(hm(fs_a), hm(fe_a)),
                           win_b = paste(hm(ws_b), hm(we_b)), field_b = paste(hm(fs_b), hm(fe_b)),
                           CH4_a = round(CH4_a, 2), CH4_b = round(CH4_b, 2))
write_csv(out, "output/qa/window_overlaps.csv")
print(as.data.frame(out), row.names = FALSE)

pdf("output/qa/window_overlaps.pdf", width = 10, height = 6)
for (i in seq_len(nrow(pairs))) {
  p <- pairs[i, ]
  lo <- min(p$ws_a, p$ws_b, p$fs_a, p$fs_b, na.rm = TRUE) - 300; hi <- max(p$we_a, p$we_b, p$fe_a, p$fe_b, na.rm = TRUE) + 300
  r <- read_raw(p$analyzer, lo, hi)
  ctx <- w %>% filter(analyzer == p$analyzer, we >= lo, ws <= hi)
  rect <- bind_rows(tibble(id = p$UniqueID_a, kind = "window", s = p$ws_a, e = p$we_a, lane = "a"),
                    tibble(id = p$UniqueID_b, kind = "window", s = p$ws_b, e = p$we_b, lane = "b"))
  fl <- bind_rows(tibble(id = p$UniqueID_a, s = p$fs_a, e = p$fe_a), tibble(id = p$UniqueID_b, s = p$fs_b, e = p$fe_b))
  long <- r %>% select(POSIX.time, CH4dry_ppb, CO2dry_ppm) %>% pivot_longer(-POSIX.time)
  g <- ggplot(long, aes(POSIX.time, value)) +
    geom_rect(data = ctx, aes(xmin = ws, xmax = we), ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.5, inherit.aes = FALSE) +
    geom_rect(data = rect, aes(xmin = s, xmax = e, fill = lane), ymin = -Inf, ymax = Inf, alpha = 0.25, inherit.aes = FALSE) +
    geom_segment(data = fl, aes(x = s, xend = e, colour = id), y = -Inf, yend = -Inf, linewidth = 3, inherit.aes = FALSE) +
    geom_line(linewidth = 0.35) + facet_wrap(~ name, ncol = 1, scales = "free_y") +
    scale_fill_manual(values = c(a = "#2a6f97", b = "#c98b2a"), labels = c(a = p$UniqueID_a, b = p$UniqueID_b), name = "fit window") +
    scale_colour_manual(values = setNames(c("#2a6f97", "#c98b2a"), c(p$UniqueID_a, p$UniqueID_b)), name = "field log (+offset)") +
    labs(title = sprintf("%s %s: %s / %s, overlap %.0f s", p$analyzer, as.Date(p$ws_a), p$UniqueID_a, p$UniqueID_b, p$overlap_s),
         subtitle = sprintf("a: %s (%s), CH4 %.2f    b: %s (%s), CH4 %.2f    grey = other logged windows",
                            p$window_source_a, hm(p$ws_a), p$CH4_a, p$window_source_b, hm(p$ws_b), p$CH4_b), x = NULL, y = NULL) +
    theme_minimal(base_size = 9) + theme(legend.position = "bottom", legend.box = "vertical")
  print(g)
}
invisible(dev.off())
