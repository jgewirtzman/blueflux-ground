# =============================================================================
# Publication Composite: Map + Representative Site Photos
# =============================================================================
# Left panel:  Study sites map (from publication_map.R)
# Right panel: 2×2 photo grid — one representative photo per mangrove type
# =============================================================================

library(sf)
library(ggplot2)
library(dplyr)
library(cowplot)
library(ggspatial)
library(ggrepel)
library(jpeg)
library(grid)

# ---- Color scheme (consistent with map) -------------------------------------
disturbance_colors <- c(
  "healthy"      = "#228B22",
  "regenerating" = "#808080",
  "ghost"        = "#8B4513",
  "scrub"        = "#DAA520"
)

# ---- Photo info -------------------------------------------------------------
photo_dir <- "output/figures/other/photos"

photo_info <- data.frame(
  file        = c("SRS5.jpg", "BL60-2.jpg", "CP40.jpg", "SE1.jpg"),
  label       = c("Healthy",  "Regenerating", "Ghost", "Scrub"),
  site        = c("SRS5",     "BL60",         "CP40",  "SE1"),
  disturbance = c("healthy",  "regenerating",  "ghost", "scrub"),
  stringsAsFactors = FALSE
)

# ---- Layout dimensions -------------------------------------------------------
fig_w <- 380   # mm
fig_h <- 180   # mm — trimmed to match map's natural height
photo_frac <- 0.8 / (1 + 0.8)   # fraction of width for photo grid

# Target cell aspect ratio (w/h) so photos crop to fill without distortion
target_asp <- (fig_w * photo_frac / 2) / (fig_h / 2)

# ---- Build photo panels -----------------------------------------------------
make_photo_panel <- function(file, label, tag, border_color) {
  img <- readJPEG(file.path(photo_dir, file))
  img_h <- dim(img)[1]   # rows = height
  img_w <- dim(img)[2]   # cols = width
  img_asp <- img_w / img_h

  # Center-crop to match target cell aspect ratio
  if (img_asp > target_asp) {
    # Image is wider than cell → crop sides
    new_w <- round(img_h * target_asp)
    margin <- round((img_w - new_w) / 2)
    img <- img[, (margin + 1):(margin + new_w), , drop = FALSE]
  } else {
    # Image is taller than cell → crop top/bottom
    new_h <- round(img_w / target_asp)
    margin <- round((img_h - new_h) / 2)
    img <- img[(margin + 1):(margin + new_h), , , drop = FALSE]
  }

  g <- rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"),
                   interpolate = TRUE)

  # Use a ggplot canvas; label border colored by disturbance type
  ggplot() +
    annotation_custom(g, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
    annotate("label", x = 0.04, y = 0.96,
             label = label,
             hjust = 0, vjust = 1, size = 5, fontface = "bold",
             fill = alpha("white", 0.85),
             label.padding = unit(0.3, "lines")) +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    theme_void() +
    theme(plot.margin = margin(1, 1, 1, 1, "mm"))
}

# Create the 4 photo panels with disturbance-colored borders
photo_panels <- mapply(
  make_photo_panel,
  file         = photo_info$file,
  label        = photo_info$label,
  tag          = c("b", "c", "d", "e"),
  border_color = disturbance_colors[photo_info$disturbance],
  SIMPLIFY = FALSE
)

# Arrange as 2×2 grid
photo_grid <- plot_grid(
  photo_panels[[1]], photo_panels[[2]],
  photo_panels[[3]], photo_panels[[4]],
  ncol = 2, nrow = 2
)

# ---- Build the map (same code as publication_map.R) -------------------------
cat("--- Building map panel ---\n")

shp_dir    <- "data/gis/fce_shapefiles/"
target_crs <- 32617

ENP      <- st_read(paste0(shp_dir, "enp_boundary_line.shp"), quiet = TRUE)
FL_state <- st_read(paste0(shp_dir, "Florida_State_Boundary.shp"), quiet = TRUE) %>%
  st_transform(target_crs)
FL_inset <- st_read(paste0(shp_dir, "statebnd_poly.shp"), quiet = TRUE) %>%
  st_transform(target_crs)
SRS_shp  <- st_read(paste0(shp_dir, "srs_utm_clipped.shp"), quiet = TRUE) %>%
  st_transform(target_crs)
TS       <- st_read(paste0(shp_dir, "taylor_slough_utm_clipped.shp"), quiet = TRUE) %>%
  st_transform(target_crs)

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

sites_sf <- st_as_sf(sites, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(target_crs)
sites_coords <- st_coordinates(sites_sf)
sites_sf$X <- sites_coords[, 1]
sites_sf$Y <- sites_coords[, 2]

easting_min  <- 415000;  easting_max  <- 600000
northing_min <- 2743000; northing_max <- 2905000

main_map <- ggplot() +
  geom_sf(data = FL_state, fill = "#f0f0f0", color = "#525252", linewidth = 0.3) +
  geom_sf(data = SRS_shp, fill = "#c0c0c0", color = "#c0c0c0", linewidth = 0.2) +
  geom_sf(data = TS, fill = "#d9d9d9", color = "#d9d9d9", linewidth = 0.2) +
  geom_sf(data = ENP, color = "#525252", linewidth = 0.6, linetype = "dashed") +
  {
    nudge_tbl <- data.frame(
      site = c("MI", "RB10", "SRS5", "SRS6", "FLM30", "CP40", "BL60", "SE1"),
      dx   = c(  0,    0,      0,      0,      2000,    1000,  -2000,    0),
      dy   = c(  0,    0,      0,      0,      1500,   -1500,   0,       0),
      stringsAsFactors = FALSE
    )
    sites_sf <<- sites_sf %>% left_join(nudge_tbl, by = "site") %>%
      mutate(X_disp = X + dx, Y_disp = Y + dy)
  } +
  geom_point(data = sites_sf %>% filter(dx != 0 | dy != 0),
             aes(x = X, y = Y), shape = 1, size = 2,
             color = "grey40", stroke = 0.5) +
  geom_segment(data = sites_sf %>% filter(dx != 0 | dy != 0),
               aes(x = X, y = Y, xend = X_disp, yend = Y_disp),
               color = "grey50", linewidth = 0.3, linetype = "solid") +
  geom_point(data = sites_sf,
             aes(x = X_disp, y = Y_disp, fill = disturbance),
             shape = 21, size = 5.25, color = "black", stroke = 0.9) +
  geom_label_repel(data = sites_sf %>% filter(site %in% c("MI", "RB10")),
                   aes(x = X_disp, y = Y_disp, label = site),
                   size = 4.5, fontface = "bold",
                   fill = alpha("white", 0.8), label.size = 0.15,
                   label.padding = unit(0.15, "lines"),
                   nudge_x = 12000, nudge_y = 8000,
                   segment.color = "grey40", segment.size = 0.3,
                   min.segment.length = 0, max.overlaps = Inf) +
  geom_label_repel(data = sites_sf %>% filter(site == "SE1"),
                   aes(x = X_disp, y = Y_disp, label = site),
                   size = 4.5, fontface = "bold",
                   fill = alpha("white", 0.8), label.size = 0.15,
                   label.padding = unit(0.15, "lines"),
                   nudge_x = -9000, nudge_y = 6000,
                   segment.color = "grey40", segment.size = 0.3,
                   min.segment.length = 0, max.overlaps = Inf) +
  geom_label_repel(data = sites_sf %>% filter(!site %in% c("MI", "RB10", "SE1")),
                   aes(x = X_disp, y = Y_disp, label = site),
                   size = 4.5, fontface = "bold",
                   fill = alpha("white", 0.8), label.size = 0.15,
                   label.padding = unit(0.15, "lines"),
                   box.padding = 0.5, point.padding = 0.4,
                   segment.color = "grey40", segment.size = 0.3,
                   min.segment.length = 0, max.overlaps = Inf,
                   force = 2, seed = 42) +
  scale_fill_manual(values = disturbance_colors, name = "Mangrove Type") +
  coord_sf(xlim = c(easting_min, easting_max),
           ylim = c(northing_min, northing_max),
           expand = FALSE, crs = target_crs) +
  annotation_scale(location = "br", width_hint = 0.2,
                   text_cex = 1.05, line_width = 0.75,
                   pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         height = unit(1.2, "cm"), width = unit(1.2, "cm"),
                         pad_x = unit(0.75, "cm"), pad_y = unit(8.0, "cm"),
                         style = north_arrow_fancy_orienteering(
                           line_width = 0.8, text_size = 12)) +
  annotate("text", x = 505000, y = 2826000, label = "Shark River\nSlough",
           size = 4.2, color = "grey40", fontface = "italic") +
  annotate("text", x = 529900, y = 2806000, label = "Taylor\nSlough",
           size = 4.2, color = "grey40", fontface = "italic") +
  annotate("segment", x = easting_min + 8000, xend = easting_min + 22000,
           y = northing_min + 48000, yend = northing_min + 48000,
           color = "#525252", linewidth = 0.6, linetype = "dashed") +
  annotate("text", x = easting_min + 24000, y = northing_min + 48000,
           label = "ENP boundary", size = 4.5, hjust = 0, color = "black") +
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

# --- Florida inset -----------------------------------------------------------
bbox_coords <- matrix(
  c(easting_min, northing_min,
    easting_min, northing_max,
    easting_max, northing_max,
    easting_max, northing_min,
    easting_min, northing_min),
  ncol = 2, byrow = TRUE
)
bbox_poly <- st_polygon(list(bbox_coords)) %>% st_sfc(crs = target_crs)

inset_map <- ggplot() +
  geom_sf(data = FL_inset, fill = "#f0f0f0", color = "#525252", linewidth = 0.3) +
  geom_sf(data = bbox_poly, fill = NA, color = "black", linewidth = 1.0) +
  coord_sf(crs = target_crs) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    plot.margin = margin(0, 0, 0, 0)
  )

# Map panel with inset
map_with_inset <- ggdraw() +
  draw_plot(main_map) +
  draw_plot(inset_map, x = 0.74, y = 0.7, width = 0.24, height = 0.24)

# ---- Composite: map (left) + photo grid (right) ----------------------------
cat("--- Building composite figure ---\n")

composite <- plot_grid(
  map_with_inset, photo_grid,
  ncol = 2, rel_widths = c(1, 0.8)
)
composite
# ---- Save -------------------------------------------------------------------
ggsave("output/figures/other/pub_map_photo_composite.pdf", composite,
       width = fig_w, height = fig_h, units = "mm")
ggsave("output/figures/other/pub_map_photo_composite.png", composite,
       width = fig_w, height = fig_h, units = "mm", dpi = 300)

cat("Saved: pub_map_photo_composite.pdf/.png\n")
cat("===== Composite figure complete =====\n")
