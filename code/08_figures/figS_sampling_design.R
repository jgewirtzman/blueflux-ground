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
comp_cols <- c(stem = "#2E7D32", root = "#A1887F", soil = "#6D4C41", water = "#1E88E5", cwd = "#757575", leaves = "#9CCC65")

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

g <- ggplot() +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_line(data = trace, aes(t, WaterLevel), colour = "#1565C0", linewidth = 0.5) +
  geom_point(data = pts, aes(t, y, colour = component, shape = depth_recorded), size = 1.6, alpha = 0.85,
             position = position_jitter(width = 0, height = 0.4)) +
  geom_point(data = dg, aes(t, y), shape = 17, size = 3, colour = "#0D47A1") +
  scale_colour_manual(values = comp_cols, name = NULL) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), labels = c(`TRUE` = "depth recorded", `FALSE` = "depth not recorded (plotted at -2)"), name = NULL) +
  facet_wrap(~ site + campaign, ncol = 2, scales = "free", labeller = label_wrap_gen(multi_line = FALSE)) +
  scale_x_datetime(date_labels = "%d %b\n%H:%M") +
  labs(x = NULL, y = "Water level above soil (cm)",
       title = "When each measurement was made, relative to tide and standing water",
       subtitle = "SRS5/SRS6: blue line = FCE LTER water level above the soil surface; points at the logger level at each measurement;\ntriangles = dissolved-gas samples (source of the Oct 2022 intact water flux). BL60, CP40, FLM30 (no logger): points at the water depth recorded at the chamber.") +
  theme_bw(base_size = 10) + theme(legend.position = "bottom", panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"))
ggsave("output/figures/other/sampling_design.png", g, width = 11, height = 10, dpi = 200)
ggsave("output/figures/other/sampling_design.pdf", g, width = 11, height = 10)
cat("Intact-site measurements by tidal phase:\n")
print(as.data.frame(tidal %>% count(plot, campaign, component, phase) %>% tidyr::pivot_wider(names_from = phase, values_from = n, values_fill = 0)))
