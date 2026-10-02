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
# output/figures/other/regional_ghost_forcing.{png,pdf} (Fig 6e,f draft).
# =============================================================================
suppressMessages({library(dplyr); library(sf); library(ggplot2); library(patchwork)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
sf_use_s2(FALSE)
x <- st_read("data/gis/ghost_extent/CIFOR_shortTermLoss_2017_GMW_V1_wCountry_Area.shp", quiet = TRUE) %>% st_make_valid()
aea <- "+proj=aea +lat_1=10 +lat_2=30 +lat_0=20 +lon_0=-75 +datum=WGS84 +units=m"
x$area_m2 <- as.numeric(st_area(st_transform(x, aea)))
x$COUNTRY[is.na(x$COUNTRY)] <- "Unassigned"

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

# --- Fig 6e,f draft ---------------------------------------------------------
land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
cent <- st_centroid(st_transform(x, 4326)) %>% mutate(area_km2 = area_m2 / 1e6)
pe <- ggplot() +
  geom_sf(data = land, fill = "grey92", colour = "grey70", linewidth = 0.15) +
  geom_sf(data = cent, aes(size = area_km2), shape = 21, fill = "#b2182b", colour = "black", alpha = 0.75, stroke = 0.3) +
  scale_size_area(max_size = 12, breaks = c(1, 10, 50, 100), name = expression("Mangrove loss with little recovery (km"^2*")")) +
  coord_sf(xlim = c(-98, -59), ylim = c(8, 31), expand = FALSE) +
  labs(x = NULL, y = NULL, tag = "e") + theme_bw(base_size = 9) +
  theme(legend.position = "bottom", plot.tag = element_text(face = "bold"))
bars <- out %>% filter(country != "Total", area_km2 >= 0.5) %>% mutate(country = reorder(country, switch_gwp20_Tg))
pf <- ggplot(bars, aes(country, switch_gwp20_Tg)) +
  geom_col(fill = "#b2182b", width = 0.6) +
  geom_errorbar(aes(ymin = switch_gwp20_lo_Tg, ymax = switch_gwp20_hi_Tg), width = 0.2, linewidth = 0.3) +
  geom_point(aes(y = switch_gwp100_Tg), shape = 23, fill = "white", size = 2) +
  coord_flip() + labs(x = NULL, y = expression("Added forcing (Tg CO"[2]*"-eq yr"^-1*")"), tag = "f",
                      caption = sprintf("Bars: GWP20 (MC range); diamonds: GWP100.\nTotal %.0f km2: %.2f Tg (GWP20), %.2f Tg (GWP100).",
                                        out$area_km2[out$country == "Total"], out$switch_gwp20_Tg[out$country == "Total"], out$switch_gwp100_Tg[out$country == "Total"])) +
  theme_bw(base_size = 9) + theme(plot.tag = element_text(face = "bold"), plot.caption = element_text(size = 7, hjust = 0))
fig <- pe + pf + plot_layout(widths = c(1.6, 1))
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/regional_ghost_forcing.png", fig, width = 7.2, height = 3.6, dpi = 300)
ggsave("output/figures/other/regional_ghost_forcing.pdf", fig, width = 7.2, height = 3.6)
