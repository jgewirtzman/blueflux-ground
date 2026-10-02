# =============================================================================
# Sampling design: when each chamber measurement was made, relative to the tide
# and water level, per site and campaign (analysed campaigns Oct 2022, Mar 2023).
#   Tidal intact sites (SRS5, SRS6): FCE LTER hourly water level above the soil
#   surface (knb-lter-fce.1168) on the measurement days +/- 12 h, with each
#   measurement placed at the logger level at its start time.
#   Non-tidal sites (BL60, CP40, FLM30): no logger; measurements at the water
#   depth recorded at the chamber.
# Also writes output/qa/sampling_tidal_phase.csv (logger level, rate and
# rising / falling / slack class for every intact-site measurement).
# Output: output/figures/other/sampling_design.{png,pdf}
# =============================================================================
suppressMessages({library(dplyr); library(ggplot2); library(readxl)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
dir.create("output/figures/other", recursive = TRUE, showWarnings = FALSE)
CAMPS <- c("2022-10" = "Oct 2022", "2023-03" = "Mar 2023")
SITES <- c("SRS6", "SRS5", "BL60", "CP40", "FLM30")
# paper palette (plot_closure.R / Fig 4)
comp_cols <- c(Water = "#4682B4", Soil = "#8B4513", Root = "#D2691E", Stem = "#228B22", CWD = "#808080", Leaf = "#E6AB02")
site_lab <- c(SRS6 = "SRS6 (intact)", SRS5 = "SRS5 (intact)", BL60 = "BL60 (regenerating)", CP40 = "CP40 (ghost)", FLM30 = "FLM30 (ghost)")
comp_lab <- c(water = "Water", soil = "Soil", root = "Root", stem = "Stem", cwd = "CWD", leaves = "Leaf")

d <- read.csv("output/data_products/combined_gas_flux_dataset.csv") %>%
  filter(plot %in% SITES, month_year %in% names(CAMPS)) %>%
  mutate(t = as.POSIXct(paste(date, start_time), tz = "America/New_York"), campaign = CAMPS[month_year],
         site = factor(plot, levels = SITES))
wl <- read.csv("data/environmental/water_level/FCE_LTER_1168_water_levels.csv") %>%
  filter(SITENAME %in% c("SRS5", "SRS6"), WaterLevel > -9000) %>%
  mutate(t = as.POSIXct(paste(Date, Time), tz = "Etc/GMT+5"), plot = SITENAME)

# logger level and rate at each intact-site measurement
tidal <- d %>% filter(plot %in% c("SRS5", "SRS6"))
tidal$logger <- NA_real_; tidal$rate_cm_h <- NA_real_
for (p in c("SRS5", "SRS6")) {
  w <- wl[wl$plot == p, ]; i <- tidal$plot == p
  tidal$logger[i] <- approx(as.numeric(w$t), w$WaterLevel, as.numeric(tidal$t[i]))$y
  tidal$rate_cm_h[i] <- approx(as.numeric(w$t[-1]) - 1800, diff(w$WaterLevel), as.numeric(tidal$t[i]))$y
}
tidal <- tidal %>% mutate(phase = case_when(rate_cm_h > 1 ~ "rising", rate_cm_h < -1 ~ "falling", TRUE ~ "slack"))
write.csv(tidal %>% select(flux_id, plot, campaign, component, date, start_time, water_depth, logger, rate_cm_h, phase),
          "output/qa/sampling_tidal_phase.csv", row.names = FALSE)

# logger traces on measurement days +/- 12 h
days <- tidal %>% group_by(plot, campaign) %>% summarise(t0 = min(t) - 12 * 3600, t1 = max(t) + 12 * 3600, .groups = "drop")
trace <- wl %>% inner_join(days, by = "plot", relationship = "many-to-many") %>% filter(t >= t0, t <= t1) %>%
  mutate(site = factor(plot, levels = SITES))
pts <- bind_rows(tidal %>% transmute(site, campaign, t, y = logger, component, depth_recorded = TRUE),
                 d %>% filter(!plot %in% c("SRS5", "SRS6")) %>%
                   transmute(site, campaign, t, y = coalesce(water_depth, -2), component, depth_recorded = !is.na(water_depth)))
# dissolved-gas samples at the intact sites (source of the Oct 2022 intact water flux)
tx <- read_excel("data/environmental/aquatic/Everglades_Lateral_C_GHG_Dataset_export_2026-10-01.xlsx", sheet = "BlueFlux Transect Data")
dg <- tx %>% filter(Site %in% c("SRS 5", "SRS 6", "SRS 6 Tidal Creek"), !is.na(Time), !is.na(`Date Sampled`)) %>%
  transmute(plot = ifelse(grepl("SRS 5", Site), "SRS5", "SRS6"),
            t = as.POSIXct(format(as.Date(`Date Sampled`)), tz = "America/New_York") + round(as.numeric(Time) * 86400),
            campaign = CAMPS[format(t, "%Y-%m")]) %>% filter(!is.na(campaign))
for (p in c("SRS5", "SRS6")) { w <- wl[wl$plot == p, ]; i <- dg$plot == p; dg$y[i] <- approx(as.numeric(w$t), w$WaterLevel, as.numeric(dg$t[i]))$y }
dg <- dg %>% mutate(site = factor(plot, levels = SITES))
pts <- pts %>% mutate(campaign = factor(campaign, levels = CAMPS))
trace <- trace %>% mutate(campaign = factor(campaign, levels = CAMPS)); dg <- dg %>% mutate(campaign = factor(campaign, levels = CAMPS))

pts <- pts %>% mutate(component = factor(comp_lab[component], levels = names(comp_cols)))
lab_fun <- function(x) paste0(site_lab[as.character(x$site)], ", ", x$campaign)
facet_lab <- function(labels) list(paste0(site_lab[as.character(labels$site)], ", ", labels$campaign))
g <- ggplot() +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.25) +
  geom_line(data = trace, aes(t, WaterLevel, linetype = "Water level above soil (FCE LTER logger)"), colour = "grey25", linewidth = 0.4) +
  geom_point(data = pts %>% filter(depth_recorded), aes(t, y, colour = component), size = 1.3, alpha = 0.9,
             position = position_jitter(width = 0, height = 0.3, seed = 1)) +
  geom_point(data = pts %>% filter(!depth_recorded), aes(t, y, colour = component, shape = "Water depth not recorded"),
             size = 1.3, stroke = 0.5, position = position_jitter(width = 0, height = 0.3, seed = 1)) +
  geom_point(data = dg, aes(t, y, shape = "Dissolved-gas sample"), size = 2.2, fill = "#4682B4", colour = "black", stroke = 0.3) +
  scale_colour_manual(values = comp_cols, name = "Chamber", drop = FALSE) +
  scale_shape_manual(values = c("Dissolved-gas sample" = 24, "Water depth not recorded" = 1), name = NULL) +
  scale_linetype_manual(values = c("Water level above soil (FCE LTER logger)" = 1), name = NULL) +
  facet_wrap(~ site + campaign, ncol = 2, scales = "free", labeller = facet_lab) +
  scale_x_datetime(date_labels = "%d %b\n%H:%M", breaks = scales::breaks_pretty(n = 4), expand = expansion(mult = 0.03)) +
  labs(x = NULL, y = "Water level / depth above soil (cm)") +
  guides(colour = guide_legend(order = 1, nrow = 1, override.aes = list(size = 2.2)), linetype = guide_legend(order = 2), shape = guide_legend(order = 3)) +
  theme_classic(base_size = 8.5) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.spacing.y = unit(0, "pt"), legend.margin = margin(0, 0, 0, 0),
        strip.background = element_blank(), strip.text = element_text(face = "bold", hjust = 0),
        panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.25), axis.line = element_line(linewidth = 0.3))
ggsave("output/figures/other/sampling_design.png", g, width = 7.2, height = 8.6, dpi = 300)
ggsave("output/figures/other/sampling_design.pdf", g, width = 7.2, height = 8.6)
cat("Intact-site measurements by tidal phase:\n")
print(as.data.frame(tidal %>% count(plot, campaign, component, phase) %>% tidyr::pivot_wider(names_from = phase, values_from = n, values_fill = 0)))
