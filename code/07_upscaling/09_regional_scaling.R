# =============================================================================
# Regional scaling of the sink-to-source switch to hurricane-killed mangrove
# that did not recover after the 2017 hurricane season (Caribbean / Gulf).
# Extent: data/gis/ghost_extent/ (CIFOR short-term loss 2017 on the Global
# Mangrove Watch v1 baseline, from D. Lagomasino; see README there). The
# supplied `area` attribute is planar World Mercator (inflated ~18 % at these
# latitudes); areas here are recomputed in an Albers equal-area projection.
# Per-area switch = ghost - intact net forcing (04_net_forcing.R; GWP20
# headline, GWP100, GWP* for the change within 20 years of conversion), the
# intact NECB alternative (06_carbon_budget.R, alkalinity retained) and a
# conservative Monte Carlo range (ghost 2.5 % - intact 97.5 % to ghost 97.5 % -
# intact 2.5 %). Assumes the Everglades per-area switch applies across the
# region: a first-order estimate.
# Writes output/upscaling/regional_ghost_forcing.csv and
# output/figures/other/regional_ghost_forcing.{png,pdf} (Fig 5d draft) and
# regional_ghost_forcing_grid.csv (loss area and added forcing per 0.5 degree cell).
# =============================================================================
suppressMessages({library(dplyr); library(sf); library(ggplot2); library(patchwork)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
sf_use_s2(FALSE)
x <- st_read("data/gis/ghost_extent/CIFOR_shortTermLoss_2017_GMW_V1_wCountry_Area.shp", quiet = TRUE) %>% st_make_valid()
aea <- "+proj=aea +lat_1=10 +lat_2=30 +lat_0=20 +lon_0=-75 +datum=WGS84 +units=m"
# split into individual patches; patches with no country in the source (small
# island territories: Turks and Caicos, British and US Virgin Islands, Saint
# Martin, Sint Maarten, Anguilla) take the nearest Natural Earth territory
land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
pieces <- st_transform(x, aea) %>% st_collection_extract("POLYGON") %>% st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON", warn = FALSE) %>% mutate(area_m2 = as.numeric(st_area(geometry))) %>% filter(area_m2 > 0)
cent <- suppressWarnings(st_centroid(st_transform(pieces, 4326)))
na_c <- is.na(pieces$COUNTRY)
pieces$COUNTRY[na_c] <- land$admin[st_nearest_feature(cent[na_c, ], land)]
x <- pieces

nf <- read.csv("output/upscaling/net_forcing_by_class.csv")
g <- nf[nf$disturbance_level == "ghost", ]; h <- nf[nf$disturbance_level == "healthy", ]
mc <- read.csv("output/upscaling/mc_net_forcing_by_class.csv")
mg <- mc[mc$class == "ghost", ]; mh <- mc[mc$class == "healthy", ]
fr <- read.csv("output/upscaling/forcing_framings.csv")
necb_int20 <- fr$net20[fr$class == "Healthy" & fr$framing == "necb_alk_retained"]
necb_int100 <- fr$net100[fr$class == "Healthy" & fr$framing == "necb_alk_retained"]
sw <- c(gwp20 = g$net20 - h$net20, gwp100 = g$net100 - h$net100, gwpstar = g$net_gwpstar - (h$co2_g_yr + h$ch4_co2we_gwpstar),
        gwp20_necb = g$net20 - necb_int20, gwp100_necb = g$net100 - necb_int100,
        gwp20_lo = mg$net20_lo - mh$net20_hi, gwp20_hi = mg$net20_hi - mh$net20_lo)
# induced CH4 (ghost - intact), g CH4 m-2 yr-1, with a conservative Monte Carlo
# range (ghost 2.5% - intact 97.5% to ghost 97.5% - intact 2.5%; class means of
# the site x campaign MC intervals, tide-weighted)
mcc <- read.csv("output/upscaling/mc_component_uncertainty.csv") %>% filter(component == "total") %>%
  group_by(site, campaign, disturbance_level) %>%
  summarise(lo = weighted.mean(mc_ci_lo, tide_weight), hi = weighted.mean(mc_ci_hi, tide_weight), .groups = "drop") %>%
  group_by(disturbance_level) %>% summarise(lo = mean(lo) * 365 / 1000, hi = mean(hi) * 365 / 1000, .groups = "drop")
GWP20 <- 81.2; GWP100 <- 27.9
dch4 <- c(mid = g$ch4_g_yr - h$ch4_g_yr,
          lo = mcc$lo[mcc$disturbance_level == "ghost"] - mcc$hi[mcc$disturbance_level == "healthy"],
          hi = mcc$hi[mcc$disturbance_level == "ghost"] - mcc$lo[mcc$disturbance_level == "healthy"])
dch4_star <- g$ch4_co2we_gwpstar - h$ch4_co2we_gwpstar          # g CO2-we m-2 yr-1
dco2 <- g$co2_g_yr - h$co2_g_yr
cat(sprintf("Induced CH4 %.2f (%.2f-%.2f) g CH4 m-2 yr-1; CO2 change %.0f g CO2 m-2 yr-1\n", dch4[["mid"]], dch4[["lo"]], dch4[["hi"]], dco2))

by_cty <- st_drop_geometry(x) %>% group_by(country = COUNTRY) %>% summarise(area_km2 = sum(area_m2) / 1e6, .groups = "drop") %>%
  arrange(desc(area_km2))
tg <- function(a_km2, s) a_km2 * 1e6 * s / 1e12            # Tg CO2-eq yr-1
out <- bind_rows(by_cty, tibble(country = "Total", area_km2 = sum(by_cty$area_km2))) %>%
  mutate(switch_gwp20_Tg = tg(area_km2, sw[["gwp20"]]), switch_gwp20_lo_Tg = tg(area_km2, sw[["gwp20_lo"]]),
         switch_gwp20_hi_Tg = tg(area_km2, sw[["gwp20_hi"]]), switch_gwp100_Tg = tg(area_km2, sw[["gwp100"]]),
         switch_gwpstar_Tg = tg(area_km2, sw[["gwpstar"]]), switch_gwp20_necb_Tg = tg(area_km2, sw[["gwp20_necb"]]),
         switch_gwp100_necb_Tg = tg(area_km2, sw[["gwp100_necb"]]),
         ch4_induced_Gg = area_km2 * 1e6 * dch4[["mid"]] / 1e9, ch4_induced_lo_Gg = area_km2 * 1e6 * dch4[["lo"]] / 1e9,
         ch4_induced_hi_Gg = area_km2 * 1e6 * dch4[["hi"]] / 1e9,
         ch4_forcing_gwp20_Tg = tg(area_km2, dch4[["mid"]] * GWP20), ch4_forcing_gwp100_Tg = tg(area_km2, dch4[["mid"]] * GWP100),
         ch4_forcing_gwpstar_Tg = tg(area_km2, dch4_star), co2_change_Tg = tg(area_km2, dco2))
write.csv(out, "output/upscaling/regional_ghost_forcing.csv", row.names = FALSE)
cat(sprintf("Per-area switch (g CO2-eq m-2 yr-1): GWP20 %.0f [%.0f, %.0f]; GWP100 %.0f; GWP* %.0f; GWP20 NECB %.0f\n",
            sw[["gwp20"]], sw[["gwp20_lo"]], sw[["gwp20_hi"]], sw[["gwp100"]], sw[["gwpstar"]], sw[["gwp20_necb"]]))
print(as.data.frame(out %>% mutate(across(where(is.numeric), ~ signif(.x, 3)))), row.names = FALSE)

# --- Fig 5d,e draft: induced CH4 on a 0.5 degree grid; regional added forcing by gas and metric
source("code/08_figures/palette.R")
xy <- st_coordinates(cent)
grid <- data.frame(lon = floor(xy[, 1] / 0.5) * 0.5 + 0.25, lat = floor(xy[, 2] / 0.5) * 0.5 + 0.25, a = pieces$area_m2 / 1e6) %>%
  group_by(lon, lat) %>% summarise(area_km2 = sum(a), .groups = "drop") %>%
  mutate(ch4_Mg = area_km2 * 1e6 * dch4[["mid"]] / 1e6, forcing_gwp20_Gg = area_km2 * 1e6 * sw[["gwp20"]] / 1e9)
write.csv(grid, "output/upscaling/regional_ghost_forcing_grid.csv", row.names = FALSE)
seq_pal <- c("#E4E2EC", "#B9B5CC", "#8E8AA8", "#6E6A86", "#3F3B52")
pe <- ggplot() +
  geom_sf(data = land, fill = "grey93", colour = "grey75", linewidth = 0.12) +
  geom_tile(data = grid, aes(lon, lat, fill = ch4_Mg), width = 0.5, height = 0.5, colour = "white", linewidth = 0.1) +
  scale_fill_gradientn(colours = seq_pal, trans = "log10", breaks = c(0.1, 1, 10, 100), labels = c("0.1", "1", "10", "100"),
                       name = expression("Induced CH"[4]*" emission (Mg CH"[4]*" yr"^-1*" per 0.5"*degree*" cell)")) +
  coord_sf(xlim = c(-98, -59), ylim = c(8, 31), expand = FALSE) +
  labs(x = NULL, y = NULL) + theme_fig() +
  theme(legend.position = "bottom", legend.key.width = unit(28, "pt"), legend.key.height = unit(6, "pt"),
        panel.grid.major = element_line(colour = "grey88", linewidth = 0.2), axis.line = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top"))
tot <- out %>% filter(country == "Total")
dec <- data.frame(metric = factor(rep(c("GWP20", "GWP100", "GWP*"), each = 2), c("GWP20", "GWP100", "GWP*")),
                  gas = factor(rep(c("CO2 (lost uptake + respiration)", "CH4 (induced emission)"), 3),
                               c("CO2 (lost uptake + respiration)", "CH4 (induced emission)")),
                  Tg = c(tot$co2_change_Tg, tot$ch4_forcing_gwp20_Tg, tot$co2_change_Tg, tot$ch4_forcing_gwp100_Tg,
                         tot$co2_change_Tg, tot$ch4_forcing_gwpstar_Tg)) %>%
  group_by(metric) %>% mutate(pct = 100 * Tg / sum(Tg)) %>% ungroup()
lab_ch4 <- dec %>% filter(gas == "CH4 (induced emission)")
pf <- ggplot(dec, aes(Tg, metric, fill = gas)) +
  geom_col(width = 0.6, colour = "white", linewidth = 0.2, position = position_stack(reverse = TRUE)) +
  geom_text(data = dec %>% group_by(metric) %>% summarise(Tg = sum(Tg)), aes(Tg, metric, label = sprintf("%.2f", Tg)),
            inherit.aes = FALSE, hjust = -0.15, size = 2.4) +
  geom_text(data = lab_ch4 %>% filter(pct >= 8), aes(x = tot$co2_change_Tg + Tg / 2, y = metric, label = sprintf("%.0f%%", pct)),
            inherit.aes = FALSE, size = 2.2, colour = "white") +
  scale_fill_manual(values = c(`CO2 (lost uptake + respiration)` = "#A7A9AC", `CH4 (induced emission)` = pal_class[["ghost"]]), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) + scale_y_discrete(limits = rev) +
  labs(x = expression("Added forcing (Tg CO"[2]*"-eq yr"^-1*")"), y = NULL,
       subtitle = sprintf("%.0f km\u00b2 of mangrove lost after 2017\ninduced CH4: %.1f Gg CH4 yr\u207b\u00b9 (%.1f\u2013%.1f)",
                          tot$area_km2, tot$ch4_induced_Gg, tot$ch4_induced_lo_Gg, tot$ch4_induced_hi_Gg)) +
  theme_fig() + theme(legend.position = "bottom", legend.direction = "vertical", plot.subtitle = element_text(size = 6.5, colour = "grey30"),
                      panel.grid.major.y = element_blank())
fig <- (pe + labs(tag = "d")) + (pf + labs(tag = "e")) + plot_layout(widths = c(1.9, 1))
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/regional_ghost_forcing.png", fig, width = 7.2, height = 3.4, dpi = 300, bg = "white")
ggsave("output/figures/other/regional_ghost_forcing.pdf", fig, width = 7.2, height = 3.4, device = cairo_pdf)
