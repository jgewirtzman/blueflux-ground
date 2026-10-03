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

by_cty <- st_drop_geometry(x) %>% group_by(country = COUNTRY) %>% summarise(area_km2 = sum(area_m2) / 1e6, .groups = "drop") %>%
  arrange(desc(area_km2))
tg <- function(a_km2, s) a_km2 * 1e6 * s / 1e12            # Tg CO2-eq yr-1
out <- bind_rows(by_cty, tibble(country = "Total", area_km2 = sum(by_cty$area_km2))) %>%
  mutate(switch_gwp20_Tg = tg(area_km2, sw[["gwp20"]]), switch_gwp20_lo_Tg = tg(area_km2, sw[["gwp20_lo"]]),
         switch_gwp20_hi_Tg = tg(area_km2, sw[["gwp20_hi"]]), switch_gwp100_Tg = tg(area_km2, sw[["gwp100"]]),
         switch_gwpstar_Tg = tg(area_km2, sw[["gwpstar"]]), switch_gwp20_necb_Tg = tg(area_km2, sw[["gwp20_necb"]]),
         switch_gwp100_necb_Tg = tg(area_km2, sw[["gwp100_necb"]]))
write.csv(out, "output/upscaling/regional_ghost_forcing.csv", row.names = FALSE)
cat(sprintf("Per-area switch (g CO2-eq m-2 yr-1): GWP20 %.0f [%.0f, %.0f]; GWP100 %.0f; GWP* %.0f; GWP20 NECB %.0f\n",
            sw[["gwp20"]], sw[["gwp20_lo"]], sw[["gwp20_hi"]], sw[["gwp100"]], sw[["gwpstar"]], sw[["gwp20_necb"]]))
print(as.data.frame(out %>% mutate(across(where(is.numeric), ~ signif(.x, 3)))), row.names = FALSE)

# --- Fig 5d draft: added forcing on a 0.5 degree grid + country summary -------
source("code/08_figures/palette.R")
xy <- st_coordinates(cent)
grid <- data.frame(lon = floor(xy[, 1] / 0.5) * 0.5 + 0.25, lat = floor(xy[, 2] / 0.5) * 0.5 + 0.25, a = pieces$area_m2 / 1e6) %>%
  group_by(lon, lat) %>% summarise(area_km2 = sum(a), .groups = "drop") %>%
  mutate(Gg = area_km2 * 1e6 * sw[["gwp20"]] / 1e9)
write.csv(grid, "output/upscaling/regional_ghost_forcing_grid.csv", row.names = FALSE)
seq_pal <- c("#E4E2EC", "#B9B5CC", "#8E8AA8", "#6E6A86", "#3F3B52")
pe <- ggplot() +
  geom_sf(data = land, fill = "grey93", colour = "grey75", linewidth = 0.12) +
  geom_tile(data = grid, aes(lon, lat, fill = Gg), width = 0.5, height = 0.5, colour = "white", linewidth = 0.1) +
  scale_fill_gradientn(colours = seq_pal, trans = "log10", breaks = c(0.1, 1, 10, 100),
                       labels = c("0.1", "1", "10", "100"),
                       name = expression("Added forcing, Gg CO"[2]*"-eq yr"^-1*" per 0.5"*degree*" cell")) +
  coord_sf(xlim = c(-98, -59), ylim = c(8, 31), expand = FALSE) +
  labs(x = NULL, y = NULL) + theme_fig() +
  theme(legend.position = "bottom", legend.key.width = unit(28, "pt"), legend.key.height = unit(6, "pt"),
        panel.grid.major = element_line(colour = "grey88", linewidth = 0.2), axis.line = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top"))
small <- out %>% filter(country != "Total", area_km2 < 0.5)
cty <- bind_rows(out %>% filter(country != "Total", area_km2 >= 0.5),
                 small %>% summarise(country = sprintf("%d other islands", n()), across(where(is.numeric), sum))) %>%
  mutate(country = reorder(country, switch_gwp20_Tg))
tot <- out %>% filter(country == "Total")
pf <- ggplot(cty, aes(y = country)) +
  geom_segment(aes(x = switch_gwp20_lo_Tg, xend = switch_gwp20_hi_Tg, yend = country), colour = pal_class[["ghost"]], linewidth = 0.6) +
  geom_point(aes(x = switch_gwp20_Tg), shape = 21, fill = pal_class[["ghost"]], colour = "white", size = 2.6) +
  geom_point(aes(x = switch_gwp100_Tg), shape = 23, fill = "white", colour = pal_class[["ghost"]], size = 1.8) +
  geom_text(aes(x = switch_gwp20_hi_Tg, label = sprintf("%.0f km\u00b2", area_km2)), hjust = -0.2, size = 2.2, colour = "grey40") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.25))) +
  labs(x = expression("Added forcing (Tg CO"[2]*"-eq yr"^-1*")"), y = NULL,
       subtitle = sprintf("Total %.0f km\u00b2: %.2f Tg yr\u207b\u00b9 (GWP20; %.2f\u2013%.2f)\n%.2f Tg yr\u207b\u00b9 (GWP100, open diamonds)",
                          tot$area_km2, tot$switch_gwp20_Tg, tot$switch_gwp20_lo_Tg, tot$switch_gwp20_hi_Tg, tot$switch_gwp100_Tg)) +
  theme_fig() + theme(plot.subtitle = element_text(size = 6.5, colour = "grey30"))
fig <- (pe + labs(tag = "d")) + pf + plot_layout(widths = c(1.9, 1))
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/regional_ghost_forcing.png", fig, width = 7.2, height = 3.4, dpi = 300, bg = "white")
ggsave("output/figures/other/regional_ghost_forcing.pdf", fig, width = 7.2, height = 3.4, device = cairo_pdf)
