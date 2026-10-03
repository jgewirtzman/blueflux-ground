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
source("code/08_figures/palette.R")   # house palette + theme_fig()
comp_cols <- pal_comp
site_cls <- c(SRS6 = "intact", SRS5 = "intact", BL60 = "regenerating", CP40 = "ghost", FLM30 = "ghost")
site_lab <- paste0(names(site_cls), "\n", site_cls); names(site_lab) <- names(site_cls)
comp_lab <- c(water = "water", soil = "soil", root = "prop root", stem = "stem", cwd = "downed wood", leaves = "leaf")

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
lev_logger <- "Water level above soil (FCE LTER logger)"
lev_nr <- "Water depth not recorded (plotted at \u22122 cm)"
g <- ggplot() +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_line(data = trace, aes(t, WaterLevel, linetype = lev_logger), colour = "grey30", linewidth = 0.4) +
  geom_point(data = pts %>% filter(depth_recorded), aes(t, y, fill = component), shape = 21, colour = "grey30",
             size = 1.3, stroke = 0.2, alpha = 0.9, position = position_jitter(width = 0, height = 0.3, seed = 1)) +
  geom_point(data = pts %>% filter(!depth_recorded), aes(t, y, fill = component, shape = lev_nr),
             size = 1.3, stroke = 0.3, colour = "grey30", alpha = 0.9, position = position_jitter(width = 0, height = 0.3, seed = 1)) +
  geom_point(data = dg, aes(t, y, shape = "Dissolved-gas sample"), size = 2, fill = pal_comp[["water"]], colour = "black", stroke = 0.3) +
  scale_fill_manual(values = comp_cols, name = "chamber", drop = FALSE) +
  scale_shape_manual(values = setNames(c(24, 23), c("Dissolved-gas sample", lev_nr)), name = NULL) +
  scale_linetype_manual(values = setNames(1, lev_logger), name = NULL) +
  ggh4x::facet_grid2(site ~ campaign, scales = "free", independent = "x",
                     labeller = labeller(site = site_lab),
                     strip = ggh4x::strip_themed(text_y = ggh4x::elem_list_text(colour = unname(pal_class[site_cls[SITES]]),
                                                                                 face = "bold", angle = 0, hjust = 0))) +
  scale_x_datetime(date_labels = "%d %b\n%H:%M", breaks = scales::breaks_pretty(n = 4), expand = expansion(mult = 0.03)) +
  labs(x = NULL, y = "Water level / depth above soil (cm)", tag = "a") +
  guides(fill = guide_legend(order = 1, nrow = 1, override.aes = list(shape = 21, size = 2.2)),
         linetype = guide_legend(order = 2), shape = guide_legend(order = 3, override.aes = list(fill = c(pal_comp[["water"]], "white"), size = 1.8))) +
  theme_fig(base_size = 8) +
  theme(legend.box = "vertical", legend.spacing.y = unit(0, "pt"), legend.margin = margin(0, 0, 0, 0),
        legend.title = element_text(face = "bold", size = 7), legend.text = element_text(size = 7),
        strip.text.x = element_text(hjust = 0.5, size = 8), strip.text.y = element_text(angle = 0, hjust = 0, size = 7.5),
        panel.grid.major.x = element_blank(), panel.spacing.x = unit(4, "mm"), panel.spacing.y = unit(2.5, "mm"),
        axis.text.x = element_text(size = 6.5, lineheight = 0.9))
ggsave("output/figures/other/sampling_design.png", g, width = 7.2, height = 8, dpi = 300, bg = "white")
ggsave("output/figures/other/sampling_design.pdf", g, width = 7.2, height = 8, device = cairo_pdf)
cat("Intact-site measurements by tidal phase:\n")
print(as.data.frame(tidal %>% count(plot, campaign, component, phase) %>% tidyr::pivot_wider(names_from = phase, values_from = n, values_fill = 0)))
