# =============================================================================
# QA: raw traces with context for closures under review (CH4, CO2, H2O on one
# analyzer), every logged closure window shaded and labelled, the reviewed
# interval outlined. Writes output/qa/review_<name>.png.
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(ggplot2); library(tidyr); library(patchwork)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/00_lib/lib_raw.R")
T <- function(x) as.POSIXct(x, tz = "UTC")

win <- read_csv("output/flux/02_windows/windows.csv", show_col_types = FALSE) %>% mutate(ws = T(start), we = T(end))
pl  <- read_csv("output/flux/04_ebullition/placements.csv", show_col_types = FALSE,
                col_types = cols(.default = col_character())) %>% mutate(ws = T(placement_start), we = T(placement_end))
lab <- function(id) sub("^(Oct_22_|Mar_23_|4[0-9]{4}_)", "", sub("_(CP40|FLM30|BL60|SRS5|SRS6|SE1)", "", id))

panel <- function(an, s, e, focus, title) {
  r <- read_raw(an, T(s), T(e))
  w <- win %>% filter(analyzer == an, we > T(s), ws < T(e)) %>% mutate(kind = component, lab = lab(UniqueID))
  p <- pl %>% filter(analyzer == an, we > T(s), ws < T(e), logged == "FALSE") %>% mutate(kind = "unlogged placement", lab = lab(placement_id))
  sh <- bind_rows(w %>% select(ws, we, kind, lab), p %>% select(ws, we, kind, lab))
  long <- r %>% select(POSIX.time, `CH4 (ppb)` = CH4dry_ppb, `CO2 (ppm)` = CO2dry_ppm, `H2O (ppm)` = H2O_ppm) %>% pivot_longer(-POSIX.time)
  ggplot(long, aes(POSIX.time, value)) +
    geom_rect(data = sh, aes(xmin = ws, xmax = we, fill = kind), ymin = -Inf, ymax = Inf, alpha = 0.25, inherit.aes = FALSE) +
    geom_text(data = sh, aes(x = ws, y = Inf, label = lab), vjust = 1.3, hjust = 0, size = 2.6, inherit.aes = FALSE) +
    annotate("rect", xmin = T(focus[1]), xmax = T(focus[2]), ymin = -Inf, ymax = Inf, fill = NA, colour = "red", linetype = 2) +
    geom_line(linewidth = 0.3) + facet_wrap(~ name, ncol = 1, scales = "free_y") +
    scale_fill_manual(values = c(water = "#2a6f97", stem = "#c98b2a", root = "#8c6d31", soil = "#7a7a7a", CWD = "#5b8c5a",
                                 leaves = "#a3c99a", `unlogged placement` = "#b5446e"), name = NULL) +
    labs(title = title, x = NULL, y = NULL) + theme_minimal(base_size = 9) + theme(legend.position = "bottom")
}

g122 <- panel("LGR3", "2023-03-15 10:25", "2023-03-15 11:50", c("2023-03-15 11:29:23", "2023-03-15 11:37:51"),
              "CP40 2023-03-15, LGR3: water 120-124; red = water 122's placement")
g122z <- panel("LGR3", "2023-03-15 11:26", "2023-03-15 11:41", c("2023-03-15 11:29:23", "2023-03-15 11:37:51"),
               "zoom: water 122 (11:29-11:38)")
ggsave("output/qa/review_water122.png", g122 / g122z, width = 12, height = 11, dpi = 110, bg = "white")

gP08 <- panel("Picarro", "2022-10-18 18:15", "2022-10-18 19:20", c("2022-10-18 18:57:40", "2022-10-18 18:59:42"),
              "FLM30 2022-10-18, Picarro (analyzer clock; local = -7 h): water 52, 53; red = unlogged P08")
gP08z <- panel("Picarro", "2022-10-18 18:54", "2022-10-18 19:08", c("2022-10-18 18:57:40", "2022-10-18 18:59:42"), "zoom: P08")
ggsave("output/qa/review_FLM30_P08.png", gP08 / gP08z, width = 12, height = 11, dpi = 110, bg = "white")

gP10 <- panel("LGR2", "2022-10-23 12:25", "2022-10-23 13:40", c("2022-10-23 13:16:22", "2022-10-23 13:23:53"),
              "CP40 2022-10-23, LGR2: stems 175-178 around unlogged P10 (red)")
gW <- panel("LGR2", "2022-10-23 11:20", "2022-10-23 12:16", c("2022-10-23 11:23:24", "2022-10-23 11:26:07"),
            "same analyzer, same morning: CP40 water 76-80 and unlogged P01 (red), for comparison")
ggsave("output/qa/review_CP40_P10.png", gP10 / gW, width = 12, height = 11, dpi = 110, bg = "white")

# slopes for the P10 comparison (CO2 ppm s-1, CH4 ppb s-1, H2O ppm s-1)
sl <- function(an, s, e, id) { r <- read_raw(an, T(s), T(e)); t <- as.numeric(r$POSIX.time) - as.numeric(T(s))
  tibble(id, CO2 = coef(lm(r$CO2dry_ppm ~ t))[2], CH4 = coef(lm(r$CH4dry_ppb ~ t))[2], H2O = coef(lm(r$H2O_ppm ~ t))[2]) }
cmp <- bind_rows(sl("LGR2", "2022-10-23 13:17:00", "2022-10-23 13:23:30", "P10 (unlogged)"),
                 win %>% filter(analyzer == "LGR2", as.Date(ws) == as.Date("2022-10-23")) %>% rowwise() %>%
                   do(sl("LGR2", .$ws, .$we, paste(.$component, lab(.$UniqueID)))))
write_csv(cmp, "output/qa/review_CP40_P10_slopes.csv")
print(as.data.frame(cmp %>% mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)
