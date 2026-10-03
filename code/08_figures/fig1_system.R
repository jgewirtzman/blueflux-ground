# =============================================================================
# Fig. 1 | System and design.
#   (a) South Florida: sites by forest class (core sites large and bold, supporting
#       sites small), mangrove with little recovery after the 2017 hurricanes (CIFOR
#       short-term loss on GMW v1), the US-Skr tower, Shark River and Taylor
#       Sloughs and the Everglades National Park boundary; Florida inset.
#   (b) Intact (SRS5), regenerating (BL60) and ghost (CP40) forest.
#   (c) PLACEHOLDER for the measurement-scales schematic (artist).
# Site coordinates from data/sites/site_metadata.csv; photos in data/photos/sites.
# Writes output/figures/other/fig1_system.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(sf); library(ggplot2); library(patchwork); library(ggspatial); library(jpeg); library(grid)})
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
sf_use_s2(FALSE)
utm <- 32617
shp <- "data/gis/fce_shapefiles/"
fl   <- st_read(paste0(shp, "Florida_State_Boundary.shp"), quiet = TRUE) %>% st_transform(utm)
fl_in <- st_read(paste0(shp, "statebnd_poly.shp"), quiet = TRUE) %>% st_transform(utm)
srs  <- st_read(paste0(shp, "srs_utm_clipped.shp"), quiet = TRUE) %>% st_transform(utm)
ts   <- st_read(paste0(shp, "taylor_slough_utm_clipped.shp"), quiet = TRUE) %>% st_transform(utm)
enp  <- st_read(paste0(shp, "enp_boundary_line.shp"), quiet = TRUE) %>% st_transform(utm)
xl <- c(415000, 600000); yl <- c(2743000, 2905000)
box <- st_as_sfc(st_bbox(c(xmin = xl[1], xmax = xl[2], ymin = yl[1], ymax = yl[2]), crs = st_crs(utm)))
loss <- st_read("data/gis/ghost_extent/CIFOR_shortTermLoss_2017_GMW_V1_wCountry_Area.shp", quiet = TRUE) %>%
  st_make_valid() %>% st_transform(utm) %>% st_crop(box) %>% suppressWarnings()

core <- c(SRS5 = "intact", SRS6 = "intact", BL60 = "regenerating", CP40 = "ghost", FLM30 = "ghost")
support <- c(MI = "ghost", RB10 = "intact", SE1 = "intact")   # SE1: living scrub mangrove
sites <- read.csv("data/sites/site_metadata.csv") %>%
  mutate(class = c(core, support)[site_id], core = site_id %in% names(core)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(utm)
sxy <- cbind(st_drop_geometry(sites), st_coordinates(sites))
tower <- st_as_sf(data.frame(lon = -81.0776, lat = 25.3629), coords = c("lon", "lat"), crs = 4326) %>% st_transform(utm)
txy <- data.frame(st_coordinates(tower), lab = "US-Skr tower")
# clustered sites: symbols displaced (m) with leader lines to the true position
# (small dot); labels sit beside each symbol
off <- data.frame(site_id = c("SRS5", "SRS6", "BL60", "CP40", "FLM30", "MI", "RB10", "SE1"),
                  sx = c(7000, -8000, -8000, 2500, 8000, 0, 0, 0),
                  sy = c(5500, -6000, 4500, -8000, 5500, 0, 0, 0),
                  side = c(1, -1, -1, 1, 1, 1, 1, -1))
lab <- sxy %>% left_join(off, by = "site_id") %>%
  mutate(px = X + sx, py = Y + sy, moved = sx != 0 | sy != 0,
         lx = px + side * 3000, ly = py, hj = ifelse(side > 0, 0, 1))
pal_map <- pal_class

pa <- ggplot() +
  geom_sf(data = fl, fill = "grey95", colour = "grey60", linewidth = 0.2) +
  geom_sf(data = srs, fill = "grey85", colour = NA) +
  geom_sf(data = ts, fill = "grey88", colour = NA) +
  geom_sf(data = enp, colour = "grey45", linewidth = 0.35, linetype = "22") +
  geom_sf(data = loss, aes(fill = "2017 hurricane dieback"), colour = pal_class[["ghost"]], linewidth = 0.25) +
  geom_segment(data = lab %>% filter(moved), aes(X, Y, xend = px, yend = py), colour = "grey35", linewidth = 0.3) +
  geom_point(data = lab %>% filter(moved), aes(X, Y), size = 0.7, colour = col_ink) +
  geom_point(data = txy, aes(X, Y), shape = 24, size = 2.4, fill = "white", colour = col_ink, stroke = 0.5) +
  geom_text(data = txy, aes(X - 3500, Y + 4500, label = lab), size = 2.7, hjust = 1, colour = "grey25") +
  geom_point(data = lab %>% filter(!core), aes(px, py, fill = class), shape = 21, size = 2.2, colour = "white", stroke = 0.3) +
  geom_point(data = lab %>% filter(core), aes(px, py, fill = class), shape = 21, size = 3.1, colour = "white", stroke = 0.4) +
  geom_label(data = lab, aes(lx, ly, label = site_id, hjust = hj, colour = class, fontface = ifelse(core, "bold", "plain")),
             size = 2.9, label.size = 0, label.padding = unit(0.08, "lines"), fill = alpha("white", 0.8)) +
  annotate("text", x = 505000, y = 2826000, label = "Shark River\nSlough", size = 2.8, colour = "grey45", fontface = "italic", lineheight = 0.9) +
  annotate("text", x = 531000, y = 2806000, label = "Taylor\nSlough", size = 2.8, colour = "grey45", fontface = "italic", lineheight = 0.9) +
  annotate("text", x = 438000, y = 2815000, label = "Gulf of\nMexico", lineheight = 0.9, size = 2.9, colour = "grey55", fontface = "italic") +
  annotate("text", x = 560000, y = 2752000, label = "Florida Bay", size = 2.9, colour = "grey55", fontface = "italic") +
  scale_fill_manual(values = c(pal_class, `2017 hurricane dieback` = pal_class[["ghost"]]),
                    breaks = c("intact", "regenerating", "ghost", "2017 hurricane dieback"),
                    labels = c("intact", "regenerating", "ghost", "2017 hurricane dieback"), name = NULL,
                    guide = guide_legend(override.aes = list(shape = c(21, 21, 21, NA), colour = c("white", "white", "white", pal_class[["ghost"]])))) +
  scale_colour_manual(values = pal_map, guide = "none") +
  annotation_scale(location = "br", width_hint = 0.22, text_cex = 0.7, height = unit(0.12, "cm"), line_width = 0.4,
                   bar_cols = c("grey30", "white")) +
  annotation_north_arrow(location = "tl", height = unit(0.55, "cm"), width = unit(0.4, "cm"),
                         pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm"), style = north_arrow_orienteering(text_size = 6, line_width = 0.5)) +
  coord_sf(xlim = xl, ylim = yl, expand = FALSE, crs = utm, datum = 4326) +
  labs(x = NULL, y = NULL) + theme_fig() +
  theme(legend.position = "inside", legend.position.inside = c(0.015, 0.02), legend.justification = c(0, 0),
        legend.background = element_rect(fill = alpha("white", 0.85), colour = NA), legend.text = element_text(size = 8),
        legend.key.size = unit(10, "pt"), axis.line = element_blank(), panel.border = element_rect(fill = NA, colour = "grey40", linewidth = 0.3),
        axis.text = element_text(size = 7))
inset <- ggplot() + geom_sf(data = fl_in, fill = "grey90", colour = "grey55", linewidth = 0.15) +
  geom_sf(data = box, fill = NA, colour = col_ink, linewidth = 0.4) + theme_void() +
  theme(panel.background = element_rect(fill = "white", colour = "grey40", linewidth = 0.3))
pa <- (pa + labs(tag = "a")) + inset_element(inset, left = 0.73, bottom = 0.68, right = 0.995, top = 0.995, align_to = "panel")

# ---- (b) photos ----
photo <- function(file, class, site, asp) {
  img <- readJPEG(file.path("data/photos/sites", file)); h <- dim(img)[1]; w <- dim(img)[2]
  if (w / h > asp) { nw <- round(h * asp); m <- (w - nw) %/% 2; img <- img[, (m + 1):(m + nw), ] }
  else { nh <- round(w / asp); m <- (h - nh) %/% 2; img <- img[(m + 1):(m + nh), , ] }
  ggplot() + annotation_custom(rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)) +
    annotate("label", x = 0.03, y = 0.95, label = class, hjust = 0, vjust = 1, size = 3.2, fontface = "bold",
             colour = "white", fill = pal_class[[class]], label.size = 0, label.padding = unit(0.18, "lines")) +
    annotate("text", x = 0.97, y = 0.05, label = site, hjust = 1, vjust = 0, size = 2.7, colour = "white", fontface = "bold") +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) + scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    theme_void() + theme(plot.margin = margin(1, 0, 1, 0))
}
asp <- 1.55
pb <- photo("SRS5.jpg", "intact", "SRS5", asp) / photo("BL60-2.jpg", "regenerating", "BL60", asp) / photo("CP40.jpg", "ghost", "CP40", asp)

# ---- (c) schematic placeholder ----
pc <- ggplot() + annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "grey96", colour = "grey70", linetype = 2) +
  annotate("text", x = 0.5, y = 0.5, size = 3.2, colour = "firebrick", lineheight = 1,
           label = "PLACEHOLDER: measurement-scales schematic (artist)\ncomponent chambers -> laser-scanned surfaces -> stand budget -> eddy-covariance tower -> aircraft -> region") +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + theme_void()

top <- pa + (wrap_elements(full = pb) + labs(tag = "b") + theme(plot.tag.position = c(0, 1.02), plot.margin = margin(14, 0, 0, 4), plot.tag = element_text(face = "bold", size = 11))) +
  plot_layout(widths = c(1.75, 1))
fig <- top / (pc + labs(tag = "c") + theme(plot.tag = element_text(face = "bold", size = 11))) + plot_layout(heights = c(1, 0.42)) 
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/fig1_system.png", fig, width = 7.2, height = 5.9, dpi = 300, bg = "white")
ggsave("output/figures/other/fig1_system.pdf", fig, width = 7.2, height = 5.9, device = cairo_pdf)
