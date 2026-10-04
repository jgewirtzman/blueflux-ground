# =============================================================================
# Fig. S17 | Campaign context: how the two flux campaigns sit in the seasonal cycle.
#   (a) Monthly z-scores (each variable standardised over all its year-months) of
#       SRS6 water level (FCE LTER, 2001-2024), air temperature, tower CH4 flux and
#       midday net CO2 uptake (US-Skr, 2004-2023 where available). Lines: median z
#       for each calendar month (band: interquartile range across years); points: the
#       campaign months (Oct 2022, Mar 2023), bars: +/- 1 SD of daily means within the month.
#       Tower CH4: FCH4 + storage, SSITC flag <= 1, u* > 0.2 (as 01_tower_gpp.R);
#       the record shows a negative offset in the dry season, which z-scores remove,
#       so only its seasonal pattern is shown. Net CO2 uptake: -FC, 10:00-14:00,
#       u* > 0.2.
# Writes output/figures/other/si_campaign_context.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(patchwork); library(data.table)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))

# daily means first; monthly mean and within-month SD of daily means from those
wl_d <- read.csv("data/environmental/water_level/FCE_LTER_1168_water_levels.csv") %>%
  filter(SITENAME == "SRS6", WaterLevel > -9000) %>%
  transmute(d = Date, v = WaterLevel) %>% group_by(d) %>% summarise(v = mean(v), n = n(), .groups = "drop") %>% filter(n >= 12) %>%
  mutate(var = "water level (SRS6)")
tw <- fread("data/tower/AMF_US-Skr_BASE_HH_2-5.csv", skip = 2, na.strings = "-9999",
            select = c("TIMESTAMP_START", "TA_1_1_1", "FCH4", "SCH4", "FCH4_SSITC_TEST", "USTAR", "FC")) %>% as.data.frame() %>%
  mutate(d = paste(substr(TIMESTAMP_START, 1, 4), substr(TIMESTAMP_START, 5, 6), substr(TIMESTAMP_START, 7, 8), sep = "-"),
         hr = as.integer(substr(TIMESTAMP_START, 9, 10)))
day <- function(x, var, minn) x %>% group_by(d) %>% summarise(v = mean(v), n = n(), .groups = "drop") %>% filter(n >= minn) %>% mutate(var = var)
ta_d  <- day(tw %>% filter(is.finite(TA_1_1_1)) %>% mutate(v = TA_1_1_1), "air temperature", 24)
ch4_d <- day(tw %>% filter(is.finite(FCH4), FCH4_SSITC_TEST <= 1, USTAR > 0.2) %>% mutate(v = FCH4 + ifelse(is.finite(SCH4), SCH4, 0)),
             "tower CH₄ flux", 6)
co2_d <- day(tw %>% filter(is.finite(FC), USTAR > 0.2, hr >= 10, hr < 14) %>% mutate(v = -FC), "midday net CO₂ uptake", 3)
vlev <- c("water level (SRS6)", "air temperature", "midday net CO₂ uptake", "tower CH₄ flux")
vcol <- setNames(c("#2C7BB6", "#D55E00", "#1E6B4E", "#A23B72"), vlev)
dd <- bind_rows(wl_d, ta_d, ch4_d, co2_d) %>% mutate(y = as.integer(substr(d, 1, 4)), m = as.integer(substr(d, 6, 7)))
mo <- dd %>% group_by(var, y, m) %>% summarise(v = mean(v), sdd = sd(v), nd = n(), .groups = "drop") %>% filter(nd >= 10)
# z relative to all year-months of each variable; within-month SD of daily means on the same z scale
z <- mo %>% group_by(var) %>% mutate(mu = mean(v), s = sd(v), z = (v - mu) / s) %>% ungroup() %>% mutate(var = factor(var, vlev))
dsd <- dd %>% group_by(var, y, m) %>% summarise(sdd = sd(v), .groups = "drop")
clim <- z %>% group_by(var, m) %>% summarise(med = median(z), q1 = quantile(z, 0.25), q3 = quantile(z, 0.75), nyr = n(), .groups = "drop")
camp <- z %>% filter((y == 2022 & m == 10) | (y == 2023 & m == 3)) %>%
  left_join(dsd %>% mutate(var = factor(var, vlev)), by = c("var", "y", "m"), suffix = c("", ".d")) %>%
  mutate(zsd = sdd.d / s)
base <- function() list(
  annotate("rect", xmin = c(2.6, 9.6), xmax = c(3.4, 10.4), ymin = -Inf, ymax = Inf, fill = "grey93"),
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3),
  scale_x_continuous(breaks = 1:12, labels = substr(month.abb, 1, 1), expand = c(0.01, 0)),
  scale_colour_manual(values = vcol, guide = "none"), scale_fill_manual(values = vcol, guide = "none"),
  labs(x = NULL, y = "z-score (monthly mean)"), theme_fig(), theme(panel.grid.minor = element_blank()))
# (1) single panel with interquartile bands
p1 <- ggplot() + base() +
  geom_ribbon(data = clim, aes(m, ymin = q1, ymax = q3, fill = var), alpha = 0.15) +
  geom_line(data = clim, aes(m, med, colour = var), linewidth = 0.7) +
  geom_errorbar(data = camp, aes(m, ymin = z - zsd, ymax = z + zsd, colour = var), width = 0.15, linewidth = 0.45,
                position = position_dodge(0.5)) +
  geom_point(data = camp, aes(m, z, fill = var, group = var), shape = 21, colour = "white", size = 2.4, stroke = 0.4, position = position_dodge(0.5)) +
  scale_x_continuous(breaks = 1:12, labels = month.abb, expand = c(0.01, 0)) +
  scale_colour_manual(values = vcol, name = NULL) + theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 2))
# (2) facets
p2 <- ggplot() + base() +
  geom_ribbon(data = clim, aes(m, ymin = q1, ymax = q3, fill = var), alpha = 0.2) +
  geom_line(data = clim, aes(m, med, colour = var), linewidth = 0.7) +
  geom_errorbar(data = camp, aes(m, ymin = z - zsd, ymax = z + zsd, colour = var), width = 0.25, linewidth = 0.45) +
  geom_point(data = camp, aes(m, z, fill = var), shape = 21, colour = "white", size = 2.4, stroke = 0.4) +
  facet_wrap(~ var, nrow = 1) + theme(strip.text = element_text(hjust = 0.5))
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/si_campaign_context_single.png", p1, width = 5.2, height = 3.8, dpi = 300, bg = "white")
ggsave("output/figures/other/si_campaign_context.png", p2, width = 7.2, height = 2.6, dpi = 300, bg = "white")
ggsave("output/figures/other/si_campaign_context.pdf", p2, width = 7.2, height = 2.6, device = cairo_pdf)
print(clim %>% group_by(var) %>% summarise(years_per_month = paste(range(nyr), collapse = "-")))
print(camp %>% select(var, y, m, z, zsd) %>% mutate(across(c(z, zsd), ~ round(.x, 2))))
