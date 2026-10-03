# =============================================================================
# SI figure | Flooded share of the intact forest floor (SRS5, SRS6) per campaign
# month (Methods M11; table S13). Visualises code/07_upscaling/01b_flood_fraction.R:
#   (a) FCE LTER hourly water level (knb-lter-fce.1168.15, logger datum) in each
#       campaign month, with the logger level at our chamber water-depth readings
#       (filled: standing water; open: none) and the fitted floor height (mean,
#       band +/- 1 SD).
#   (b) Floor height relative to the logger datum: wet readings give it exactly
#       (logger - depth; histogram, as a share of all readings), dry readings only
#       bound it from below (floor above the logger level then; triangles). Curve:
#       censored-normal fit (survival::survreg, as 01b). Lines: distribution of
#       hourly water level in each campaign month. Floor below the water is flooded.
#   (c) Campaign-month flooded share: area-weighted (central; bar = floor mean
#       +/- 1.96 SE), all-or-nothing switch and long-term (2010 onward) value.
# Fit and shares are read from output/upscaling/flood_fraction.csv; only the
# chamber-time logger interpolation is repeated here (same code as 01b) to plot
# the readings.
# Writes output/figures/other/si_flood_fraction.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(patchwork)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))

TIDAL <- c("SRS5", "SRS6")
CAMPS <- c("2022-03" = "Mar 2022", "2022-10" = "Oct 2022", "2023-03" = "Mar 2023")
pal_month <- c("Mar 2022" = "#D55E00", "Oct 2022" = "#0072B2", "Mar 2023" = "#CC79A7")   # as plot_us_skr_gpp.R
col_floor <- pal_class[["intact"]]; col_wl <- pal_comp[["water"]]

ff <- read.csv("output/upscaling/flood_fraction.csv") %>%
  mutate(campaign = factor(campaign, unname(CAMPS)), floor_el = -floor_mu_cm)   # floor elevation above logger zero

# --- inputs exactly as 01b_flood_fraction.R ----------------------------------
wl <- read.csv("data/environmental/water_level/FCE_LTER_1168_water_levels.csv") %>% filter(SITENAME %in% TIDAL) %>%
  mutate(WaterLevel = ifelse(WaterLevel < -9000, NA, WaterLevel),
         t = as.numeric(as.POSIXct(paste(Date, Time), tz = "Etc/GMT+5")),
         ym = substr(Date, 1, 7)) %>%
  filter(!is.na(WaterLevel))
fx <- read.csv("output/data_products/combined_gas_flux_dataset.csv") %>%
  filter(plot %in% TIDAL, !is.na(water_depth), !is.na(start_time)) %>%
  mutate(t = as.numeric(as.POSIXct(paste(date, start_time), tz = "America/New_York")))
fx$logger <- NA_real_
for (p in TIDAL) {
  i <- fx$plot == p; w <- wl[wl$SITENAME == p, ]
  fx$logger[i] <- approx(w$t, w$WaterLevel, fx$t[i])$y
}
fx <- fx %>% filter(!is.na(logger)) %>%
  mutate(wet = water_depth > 0, floor_el = ifelse(wet, logger - water_depth, logger),   # exact if wet; lower bound if dry
         ym = substr(date, 1, 7), campaign = factor(unname(CAMPS[ym]), unname(CAMPS)), site = plot)
stopifnot(all(table(fx$site) == ff$n_floor[match(names(table(fx$site)), ff$site)]))   # same readings as 01b

wlc <- wl %>% filter(ym %in% names(CAMPS)) %>% rename(site = SITENAME) %>%
  mutate(campaign = factor(unname(CAMPS[ym]), unname(CAMPS)),
         dt = as.POSIXct(t, origin = "1970-01-01", tz = "Etc/GMT+5"),
         day = as.numeric(difftime(dt, as.POSIXct(paste0(ym, "-01"), tz = "Etc/GMT+5"), units = "days")) + 1)
fxc <- fx %>% filter(!is.na(campaign)) %>%
  mutate(dt = as.POSIXct(t, origin = "1970-01-01", tz = "Etc/GMT+5"),
         day = as.numeric(difftime(dt, as.POSIXct(paste0(ym, "-01"), tz = "Etc/GMT+5"), units = "days")) + 1)
fl1 <- ff %>% distinct(site, floor_el, floor_sd_cm)
ylim <- range(c(wlc$WaterLevel, fx$floor_el)) + c(-2, 2)

# --- (a) hourly water level per campaign month --------------------------------
pa <- ggplot() +
  geom_rect(data = fl1, aes(xmin = -Inf, xmax = Inf, ymin = floor_el - floor_sd_cm, ymax = floor_el + floor_sd_cm),
            fill = col_floor, alpha = 0.12) +
  geom_hline(data = fl1, aes(yintercept = floor_el), colour = col_floor, linewidth = 0.4, linetype = "22") +
  geom_line(data = wlc, aes(day, WaterLevel), colour = col_wl, linewidth = 0.25) +
  geom_point(data = arrange(fxc, desc(wet)), aes(day, logger, shape = wet, fill = wet), colour = col_ink, size = 0.9, stroke = 0.3,
             position = position_jitter(width = 0.35, height = 0, seed = 2)) +
  scale_fill_manual(values = c(`TRUE` = col_ink, `FALSE` = "white"), guide = "none") +
  scale_shape_manual(values = c(`TRUE` = 21, `FALSE` = 21), labels = c(`TRUE` = "standing water", `FALSE` = "no standing water"),
                     breaks = c("TRUE", "FALSE"), name = "chamber reading") +
  guides(shape = guide_legend(override.aes = list(fill = c(col_ink, "white")))) +
  facet_grid(site ~ campaign) +
  scale_x_continuous(breaks = c(1, 8, 15, 22, 29), expand = expansion(mult = 0.01)) +
  coord_cartesian(ylim = ylim) +
  labs(x = "Day of campaign month", y = "Water level (cm, logger datum)") +
  theme_fig() + theme(legend.position = "bottom", legend.margin = margin(0, 0, 0, 0), legend.title = element_text(size = 7))

# --- (b) floor-height distribution vs water level ----------------------------
bw <- 3
yy <- seq(ylim[1], ylim[2], length.out = 300)
dens_floor <- fl1 %>% crossing(y = yy) %>% mutate(d = dnorm(y, floor_el, floor_sd_cm))
dens_wl <- wlc %>% group_by(site, campaign) %>%
  reframe(y = density(WaterLevel, bw = 2, from = ylim[1], to = ylim[2], n = 300)$x,
          d = density(WaterLevel, bw = 2, from = ylim[1], to = ylim[2], n = 300)$y)
ntot <- fx %>% count(site, name = "ntot")
hist_wet <- fx %>% filter(wet) %>% mutate(bin = floor(floor_el / bw) * bw + bw / 2) %>% count(site, bin) %>%
  left_join(ntot, by = "site") %>% mutate(d = n / ntot / bw)
dmax <- max(c(dens_floor$d, dens_wl$d, hist_wet$d))
dry <- fx %>% filter(!wet)

pb <- ggplot() +
  geom_ribbon(data = dens_floor, aes(y = y, xmin = 0, xmax = d), fill = col_floor, alpha = 0.15, orientation = "y") +
  geom_tile(data = hist_wet, aes(x = d / 2, y = bin, width = d, height = bw * 0.92), fill = col_floor, alpha = 0.55) +
  geom_path(data = dens_floor, aes(d, y), colour = col_floor, linewidth = 0.5) +
  geom_path(data = dens_wl, aes(d, y, colour = campaign), linewidth = 0.45) +
  geom_point(data = dry, aes(x = -0.006, y = floor_el), shape = 2, size = 0.8, stroke = 0.3, colour = col_floor,
             position = position_jitter(width = 0.004, height = 0, seed = 1)) +
  facet_grid(site ~ .) +
  scale_colour_manual(values = pal_month, name = "water level") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05)), breaks = seq(0, 0.1, 0.04)) +
  coord_cartesian(ylim = ylim, xlim = c(-0.01, dmax)) +
  labs(x = "Density (per cm)", y = "Floor height or water level (cm)") +
  theme_fig() + theme(legend.position = "bottom", legend.margin = margin(0, 0, 0, 0),
                       legend.background = element_rect(fill = "white", colour = NA), legend.title = element_text(size = 7)) +
  guides(colour = guide_legend(ncol = 1, override.aes = list(linewidth = 0.8)))

# --- (c) flooded share per site x campaign ----------------------------------
cc <- ff %>% select(site, campaign, frac_flooded, frac_flooded_lo, frac_flooded_hi, frac_flooded_switch, frac_flooded_longterm) %>%
  pivot_longer(c(frac_flooded, frac_flooded_switch), names_to = "variant", values_to = "v") %>%
  mutate(variant = factor(recode(variant, frac_flooded = "area-weighted (central)", frac_flooded_switch = "all-or-nothing switch"),
                          c("area-weighted (central)", "all-or-nothing switch")),
         x = as.numeric(campaign) + ifelse(variant == "all-or-nothing switch", 0.16, -0.08))
lt <- ff %>% distinct(site, frac_flooded_longterm)
pc <- ggplot(cc) +
  geom_hline(data = lt, aes(yintercept = frac_flooded_longterm, linetype = "long-term, 2010 onward (area-weighted)"),
             colour = "grey45", linewidth = 0.4) +
  geom_errorbar(data = filter(cc, variant == "area-weighted (central)"),
                aes(x = x, ymin = frac_flooded_lo, ymax = frac_flooded_hi), width = 0.08, colour = col_floor, linewidth = 0.4) +
  geom_point(aes(x, v, shape = variant), colour = col_floor, fill = col_floor, size = 1.8, stroke = 0.5) +
  geom_label(data = filter(cc, variant == "area-weighted (central)"), aes(x - 0.09, v, label = sprintf("%.2f", v)),
             hjust = 1, size = 6.5 / .pt, colour = col_ink, fill = "white", linewidth = 0, label.padding = unit(0.6, "pt")) +
  facet_grid(. ~ site) +
  scale_shape_manual(values = c("area-weighted (central)" = 16, "all-or-nothing switch" = 4), name = NULL) +
  scale_linetype_manual(values = c("long-term, 2010 onward (area-weighted)" = "22"), name = NULL) +
  scale_x_continuous(breaks = 1:3, labels = levels(ff$campaign), expand = expansion(add = 0.45)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(x = NULL, y = "Flooded share of floor") +
  theme_fig() + theme(legend.position = "bottom", legend.margin = margin(0, 0, 0, 0), panel.grid.major.x = element_blank())

top <- (pa | pb) + plot_layout(widths = c(3, 1.15))
fig <- (top / pc) + plot_layout(heights = c(2.3, 1)) + plot_annotation(tag_levels = "a")
ggsave("output/figures/other/si_flood_fraction.png", fig, width = 7.2, height = 5.4, dpi = 300, bg = "white")
ggsave("output/figures/other/si_flood_fraction.pdf", fig, width = 7.2, height = 5.4, device = cairo_pdf)
