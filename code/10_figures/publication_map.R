# =============================================================================
# Publication Map: Study Sites in the Florida Coastal Everglades
# =============================================================================
# Uses FCE LTER grayscale base map shapefiles:
#   - ENP boundary, Shark River Slough, Taylor Slough
#   - Florida state boundary (main map + inset)
# Overlays Blueflux study sites with disturbance classification
# =============================================================================

library(sf)
library(ggplot2)
library(dplyr)
library(cowplot)
library(ggspatial)
library(ggrepel)

# --- Paths -------------------------------------------------------------------
shp_dir <- "data/gis/fce_shapefiles/"

# --- Load shapefiles ---------------------------------------------------------
cat("--- Loading shapefiles ---\n")

# All projected to UTM 17N (EPSG:32617) for consistent mapping
target_crs <- 32617

ENP        <- st_read(paste0(shp_dir, "enp_boundary_line.shp"), quiet = TRUE)
FL_state   <- st_read(paste0(shp_dir, "Florida_State_Boundary.shp"), quiet = TRUE) %>%
  st_transform(target_crs)
FL_inset   <- st_read(paste0(shp_dir, "statebnd_poly.shp"), quiet = TRUE) %>%
  st_transform(target_crs)
SRS        <- st_read(paste0(shp_dir, "srs_utm_clipped.shp"), quiet = TRUE) %>%
  st_transform(target_crs)
TS         <- st_read(paste0(shp_dir, "taylor_slough_utm_clipped.shp"), quiet = TRUE) %>%
  st_transform(target_crs)

cat("  All shapefiles loaded successfully\n")

# --- Study site locations ----------------------------------------------------
sites <- data.frame(
  site = c("MI", "RB10", "SRS5", "SRS6", "FLM30", "CP40", "BL60", "SE1"),
  latitude  = c(25.929833, 25.862889, 25.377028, 25.364639,
                25.159889, 25.149278, 25.157833, 25.353889),
  longitude = c(-81.672167, -81.561194, -81.032361, -81.077944,
                -80.912000, -80.911028, -80.922694, -80.381000),
  disturbance = c("ghost", "healthy", "healthy", "healthy",
                   "ghost", "ghost", "regenerating", "scrub"),
  stringsAsFactors = FALSE
)

sites$disturbance <- factor(sites$disturbance,
                            levels = c("healthy", "regenerating", "ghost", "scrub"))

# Convert to sf in WGS84 then project to UTM 17N
sites_sf <- st_as_sf(sites, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(target_crs)

# Extract projected coordinates for label positioning
sites_coords <- st_coordinates(sites_sf)
sites_sf$X <- sites_coords[, 1]
sites_sf$Y <- sites_coords[, 2]

# --- Color scheme (consistent with other publication figures) ----------------
disturbance_colors <- c(
  "healthy"      = "#228B22",
  "regenerating" = "#808080",
  "ghost"        = "#8B4513",
  "scrub"        = "#DAA520"
)

# --- Map extent (UTM 17N coordinates) ----------------------------------------
# Shifted north to capture MI, trimmed south
easting_min  <- 415000
easting_max  <- 600000
northing_min <- 2743000
northing_max <- 2905000

# --- Main map ----------------------------------------------------------------
cat("--- Building main map ---\n")

main_map <- ggplot() +
  # Florida land mass (background)
  geom_sf(data = FL_state, fill = "#f0f0f0", color = "#525252",
          linewidth = 0.3) +
  # Shark River Slough
  geom_sf(data = SRS, fill = "#c0c0c0", color = "#c0c0c0", linewidth = 0.2) +
  # Taylor Slough
  geom_sf(data = TS, fill = "#d9d9d9", color = "#d9d9d9", linewidth = 0.2) +
  # ENP boundary (dashed)
  geom_sf(data = ENP, color = "#525252", linewidth = 0.6, linetype = "dashed") +
  # --- Nudged points for overlapping clusters ---------------------------------
  # Define display offsets for crowded sites (dx, dy in UTM meters)
  # Clustered sites get nudged apart; isolated sites stay put (dx=dy=0)
  {
    # Build nudge table
    nudge_tbl <- data.frame(
      site  = c("MI",   "RB10",  "SRS5", "SRS6",  "FLM30", "CP40",  "BL60",  "SE1"),
      dx    = c(  0,       0,       0,      0,       2000,    1000,   -2000,     0),
      dy    = c(  0,       0,       0,      0,       1500,   -1500,    0,        0),
      stringsAsFactors = FALSE
    )
    sites_sf <- sites_sf %>% left_join(nudge_tbl, by = "site") %>%
      mutate(X_disp = X + dx, Y_disp = Y + dy)
  } +
  # True-location markers (small open circles)
  geom_point(data = sites_sf %>% filter(dx != 0 | dy != 0),
             aes(x = X, y = Y), shape = 1, size = 2,
             color = "grey40", stroke = 0.5) +
  # Tie-back lines from nudged to true position
  geom_segment(data = sites_sf %>% filter(dx != 0 | dy != 0),
               aes(x = X, y = Y, xend = X_disp, yend = Y_disp),
               color = "grey50", linewidth = 0.3, linetype = "solid") +
  # Nudged study-site points
  geom_point(data = sites_sf,
             aes(x = X_disp, y = Y_disp, fill = disturbance),
             shape = 21, size = 5.25, color = "black", stroke = 0.9) +
  # Site labels — MI & RB10 nudged top-right
  geom_label_repel(data = sites_sf %>% filter(site %in% c("MI", "RB10")),
                   aes(x = X_disp, y = Y_disp, label = site),
                   size = 4.5, fontface = "bold",
                   fill = alpha("white", 0.8), label.size = 0.15,
                   label.padding = unit(0.15, "lines"),
                   nudge_x = 12000, nudge_y = 8000,
                   segment.color = "grey40", segment.size = 0.3,
                   min.segment.length = 0, max.overlaps = Inf) +
  # Site labels — SE1 nudged top-left
  geom_label_repel(data = sites_sf %>% filter(site == "SE1"),
                   aes(x = X_disp, y = Y_disp, label = site),
                   size = 4.5, fontface = "bold",
                   fill = alpha("white", 0.8), label.size = 0.15,
                   label.padding = unit(0.15, "lines"),
                   nudge_x = -9000, nudge_y = 6000,
                   segment.color = "grey40", segment.size = 0.3,
                   min.segment.length = 0, max.overlaps = Inf) +
  # Site labels — all others
  geom_label_repel(data = sites_sf %>% filter(!site %in% c("MI", "RB10", "SE1")),
                   aes(x = X_disp, y = Y_disp, label = site),
                   size = 4.5, fontface = "bold",
                   fill = alpha("white", 0.8), label.size = 0.15,
                   label.padding = unit(0.15, "lines"),
                   box.padding = 0.5, point.padding = 0.4,
                   segment.color = "grey40", segment.size = 0.3,
                   min.segment.length = 0, max.overlaps = Inf,
                   force = 2, seed = 42) +
  # Scales
  scale_fill_manual(values = disturbance_colors, name = "Mangrove Type") +
  # Map extent
  coord_sf(xlim = c(easting_min, easting_max),
           ylim = c(northing_min, northing_max),
           expand = FALSE, crs = target_crs) +
  # Scale bar and north arrow
  annotation_scale(location = "br", width_hint = 0.2,
                   text_cex = 1.05, line_width = 0.75,
                   pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         height = unit(1.2, "cm"), width = unit(1.2, "cm"),
                         pad_x = unit(0.75, "cm"), pad_y = unit(6.0, "cm"),
                         style = north_arrow_fancy_orienteering(
                           line_width = 0.8, text_size = 12)) +
  # Annotations for sloughs
  annotate("text", x = 505000, y = 2826000, label = "Shark River\nSlough",
           size = 4.2, color = "grey40", fontface = "italic") +
  annotate("text", x = 529900, y = 2806000, label = "Taylor\nSlough",
           size = 4.2, color = "grey40", fontface = "italic") +
  # Manual legend: ENP boundary (positioned above the Mangrove Type legend)
  annotate("segment", x = easting_min + 8000, xend = easting_min + 22000,
           y = northing_min + 48000, yend = northing_min + 48000,
           color = "#525252", linewidth = 0.6, linetype = "dashed") +
  annotate("text", x = easting_min + 24000, y = northing_min + 48000,
           label = "ENP boundary", size = 4.5, hjust = 0, color = "black") +
  # Theme
  theme_bw(base_size = 15) +
  theme(
    axis.title       = element_blank(),
    axis.text        = element_text(size = 10.5),
    panel.grid       = element_line(color = "grey92", linewidth = 0.2),
    legend.position  = c(0.02, 0.02),
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = alpha("white", 0.85), color = NA),
    legend.key.size  = unit(0.6, "cm"),
    legend.title     = element_text(size = 13.5, face = "bold"),
    legend.text      = element_text(size = 12),
    plot.margin      = margin(2, 2, 2, 2, "mm")
  )

# --- Florida inset map -------------------------------------------------------
cat("--- Building inset map ---\n")

# Bounding box of main map extent as a polygon for the inset
bbox_coords <- matrix(
  c(easting_min, northing_min,
    easting_min, northing_max,
    easting_max, northing_max,
    easting_max, northing_min,
    easting_min, northing_min),
  ncol = 2, byrow = TRUE
)
bbox_poly <- st_polygon(list(bbox_coords)) %>%
  st_sfc(crs = target_crs)

inset_map <- ggplot() +
  geom_sf(data = FL_inset, fill = "#f0f0f0", color = "#525252",
          linewidth = 0.3) +
  geom_sf(data = bbox_poly, fill = NA, color = "black",
          linewidth = 1.0) +
  coord_sf(crs = target_crs) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    plot.margin = margin(0, 0, 0, 0)
  )

# --- Combine with inset in top-right corner ----------------------------------
cat("--- Compositing final map ---\n")

final_map <- ggdraw() +
  draw_plot(main_map) +
  draw_plot(inset_map, x = 0.74, y = 0.68, width = 0.24, height = 0.24)

final_map

# --- Save --------------------------------------------------------------------
ggsave("output/figures/other/pub_study_sites_map.pdf", final_map,
       width = 190, height = 170, units = "mm")
ggsave("output/figures/other/pub_study_sites_map.png", final_map,
       width = 190, height = 170, units = "mm", dpi = 300)

cat("Saved: pub_study_sites_map.pdf/.png\n")
cat("===== Publication map complete =====\n")
