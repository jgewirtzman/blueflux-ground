# =============================================================================
# Fig. 1 | System and design.
# Layout: (a) map; (b) photos; (c) trajectories; (d) measurement schematic.
#   (a) South Florida: sites by forest class (core sites large and bold, supporting
#       sites small), mangrove extent (GMW v3 2016), mangrove with little recovery after the 2017 hurricanes (CIFOR
#       short-term loss on GMW v1), the US-Skr tower, Shark River and Taylor
#       Sloughs and the Everglades National Park boundary; Florida inset.
#   (b) Intact (SRS5), regenerating (BL60) and ghost (CP40) forest.
#   (c) Site trajectories: Landsat dry-season (Jan-Apr) NDVI at the five core plots,
#       1995-2025 (median of clear scenes, 90 m window; 06_analysis/09_site_ndvi_history.R
#       cache). SRS5 and SRS6 sit on the Shark River bank, so their window is taken
#       100 m inland into continuous forest (06_analysis/10_ndvi_grain_srs.R). Dashed:
#       hurricanes Wilma (Oct 2005) and Irma (Sep 2017). Bars: BlueFlux ground campaigns
#       (dark; Mar 2022, Oct 2022, Mar 2023) and airborne deployments (light),
#       one month wide.
#   (d) Measurement schematic (draft illustration, data/figures/fig1c_schematic.jpg).
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
xl <- c(420000, 580000); yl <- c(2741000, 2880000)   # study area incl. full ENP boundary
box <- st_as_sfc(st_bbox(c(xmin = xl[1], xmax = xl[2], ymin = yl[1], ymax = yl[2]), crs = st_crs(utm)))
mang <- st_read("data/gis/mangrove_extent/gmw_v3_2016_sfl.gpkg", quiet = TRUE) %>% st_transform(utm)   # GMW v3 2016 (01d_mangrove_extent.R)
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
                  side = c(1, -1, -1, 1, 1, 1, 1, 0),                    # 0 = label centred above the symbol
                  ldy = c(0, 0, 0, 0, 0, 3500, -3500, 6500))              # label vertical offset (m)
nm <- c(SRS5 = "Gunboat Island", SRS6 = "Lower Shark River", BL60 = "Bear Lake", CP40 = "Christian Point",
        FLM30 = "Flamingo", MI = "Marco Island", RB10 = "Rookery Bay", SE1 = "Intrusion marsh")
lab <- sxy %>% left_join(off, by = "site_id") %>% mutate(txt = ifelse(nm[site_id] == "", site_id, paste0(site_id, "\n", nm[site_id]))) %>%
  mutate(px = X + sx, py = Y + sy, moved = sx != 0 | sy != 0,
         lx = px + side * 3000, ly = py + ldy, hj = ifelse(side > 0, 0, ifelse(side < 0, 1, 0.5)))
pal_map <- pal_class

pa <- ggplot() +
  geom_sf(data = fl, fill = "grey95", colour = "grey60", linewidth = 0.2) +
  geom_sf(data = enp, colour = "grey45", linewidth = 0.35, linetype = "22") +
  geom_sf(data = mang, aes(fill = "mangrove (2016)"), colour = NA) +
  geom_sf(data = loss, aes(fill = "2017 hurricane dieback"), colour = "#A9A5BD", linewidth = 0.2) +
  geom_sf(data = srs, fill = alpha("grey45", 0.18), colour = "grey55", linewidth = 0.2) +
  geom_sf(data = ts, fill = alpha("grey45", 0.18), colour = "grey55", linewidth = 0.2) +
  geom_point(data = txy, aes(X, Y), shape = 24, size = 2.4, fill = "white", colour = col_ink, stroke = 0.5) +
  geom_segment(data = lab %>% filter(moved), aes(X, Y, xend = px, yend = py), colour = "grey35", linewidth = 0.3) +
  geom_point(data = lab %>% filter(moved), aes(X, Y), size = 0.7, colour = col_ink) +
  geom_text(data = txy, aes(X - 3500, Y + 4500, label = lab), size = 2.7, hjust = 1, colour = "grey25") +
  geom_point(data = lab %>% filter(!core), aes(px, py, fill = class), shape = 21, size = 2.2, colour = "white", stroke = 0.3) +
  geom_point(data = lab %>% filter(core), aes(px, py, fill = class), shape = 21, size = 3.1, colour = "white", stroke = 0.4) +
  geom_label(data = lab, aes(lx, ly, label = txt, hjust = hj, colour = class, fontface = ifelse(core, "bold", "plain")),
             size = 2.5, lineheight = 0.85, label.size = 0, label.padding = unit(0.08, "lines"), fill = alpha("white", 0.8)) +
  annotate("text", x = 505000, y = 2826000, label = "Shark River\nSlough", size = 2.8, colour = "grey45", fontface = "italic", lineheight = 0.9) +
  annotate("text", x = 540000, y = 2797500, label = "Taylor\nSlough", size = 2.8, colour = "grey45", fontface = "italic", lineheight = 0.9) +
  annotate("text", x = 436000, y = 2788000, label = "Gulf of\nMexico", lineheight = 0.9, size = 2.9, colour = "grey55", fontface = "italic") +
  annotate("text", x = 519000, y = 2762500, label = "Florida Bay", size = 2.9, colour = "grey55", fontface = "italic") +
  scale_fill_manual(values = c(pal_class, `mangrove (2016)` = "#A8CDB9", `2017 hurricane dieback` = "#B9B5CB"),
                    breaks = c("intact", "regenerating", "ghost", "mangrove (2016)", "2017 hurricane dieback"),
                    labels = c("intact", "regenerating", "ghost", "mangrove (2016)", "2017 hurricane dieback"), name = NULL,
                    guide = guide_legend(override.aes = list(shape = c(21, 21, 21, NA, NA), colour = c("white", "white", "white", NA, "#A9A5BD")))) +
  scale_colour_manual(values = pal_map, guide = "none") +
  annotation_scale(location = "br", width_hint = 0.22, text_cex = 0.7, height = unit(0.12, "cm"), line_width = 0.4,
                   bar_cols = c("grey30", "white")) +
  annotation_north_arrow(location = "tl", height = unit(0.55, "cm"), width = unit(0.4, "cm"),
                         pad_x = unit(0.45, "cm"), pad_y = unit(2.5, "cm"), style = north_arrow_orienteering(text_size = 6, line_width = 0.5)) +
  coord_sf(xlim = xl, ylim = yl, expand = FALSE, crs = utm, datum = 4326) +
  labs(x = NULL, y = NULL) + theme_fig() +
  theme(legend.position = "inside", legend.position.inside = c(0.015, 0.02), legend.justification = c(0, 0),
        legend.background = element_rect(fill = alpha("white", 0.85), colour = NA), legend.text = element_text(size = 8),
        legend.key.size = unit(10, "pt"), axis.line = element_blank(), panel.border = element_rect(fill = NA, colour = "grey40", linewidth = 0.3),
        axis.text = element_text(size = 7))
inset <- ggplot() + geom_sf(data = fl_in, fill = "grey90", colour = "grey55", linewidth = 0.15) +
  geom_sf(data = box, fill = NA, colour = col_ink, linewidth = 0.4) + theme_void() +
  theme(panel.background = element_rect(fill = "white", colour = "grey40", linewidth = 0.3))
pa_map <- (pa + labs(tag = "a") + theme(plot.tag = element_text(face = "bold", size = 11))) + inset_element(inset, left = 0.80, bottom = 0.75, right = 1.0, top = 1.04, align_to = "panel", clip = FALSE)

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

# ---- (c) measurement schematic (draft illustration; data/figures/fig1c_schematic.jpg) ----
sch <- readJPEG("data/figures/fig1c_schematic.jpg")
pc <- ggplot() + annotation_custom(rasterGrob(sch, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) + scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  theme_void() + theme(aspect.ratio = dim(sch)[1] / dim(sch)[2])

# ---- (c) trajectories ----
tcls <- core
lsf <- file.path("data/environmental/satellite/landsat_ndvi", paste0(names(tcls), ".csv"))
ls_raw <- bind_rows(lapply(lsf[file.exists(lsf)], read.csv)) %>% filter(!is.na(ndvi))
gr <- "output/analysis/si/site_ndvi_grain.csv"            # river-edge sites: inland window
if (file.exists(gr)) {
  inl <- read.csv(gr) %>% filter(variant == "inland") %>% transmute(site, year, ndvi)
  ls_ann <- ls_raw %>% filter(!site %in% unique(inl$site)) %>% mutate(year = as.integer(substr(date, 1, 4))) %>%
    group_by(site, year) %>% summarise(ndvi = median(ndvi), .groups = "drop") %>% bind_rows(inl)
} else ls_ann <- ls_raw %>% mutate(year = as.integer(substr(date, 1, 4))) %>% group_by(site, year) %>% summarise(ndvi = median(ndvi), .groups = "drop")
nd <- ls_ann %>% mutate(cls = factor(tcls[site], c("intact", "regenerating", "ghost")))
storms <- data.frame(name = c("Wilma", "Irma"), x = c(2005.8, 2017.7))
mo <- function(y, m) y + (m - 1) / 12
camps <- rbind(data.frame(type = "ground", y = c(2022, 2022, 2023), m = c(3, 10, 3)),
               data.frame(type = "airborne", y = c(2022, 2022, 2023, 2023, 2024), m = c(4, 10, 2, 4, 7))) %>%
  mutate(xmin = mo(y, m), xmax = xmin + 1 / 12)
traj_panel <- function(v, ylab, ylim, ybr, camp_key = TRUE) {
  endlab <- nd %>% mutate(val = .data[[v]]) %>% group_by(site) %>% filter(year == max(year)) %>% ungroup() %>% arrange(desc(val)) %>% mutate(ylab = val)
  gap <- diff(ylim) * 0.065
  for (i in seq_len(nrow(endlab))[-1]) endlab$ylab[i] <- min(endlab$ylab[i], endlab$ylab[i - 1] - gap)
  ggplot(nd, aes(year, .data[[v]], colour = cls, group = site)) +
    geom_rect(data = camps %>% filter(type == "airborne"), aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = "#B9D3E8") +
    geom_rect(data = camps %>% filter(type == "ground"), aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = "grey55", alpha = 0.6) +
    { if (camp_key) list(
        annotate("text", x = 2025.4, y = ylim[1] + diff(ylim) * 0.56, label = "ground\ncampaign", size = 2.1, colour = "grey40", hjust = 0, lineheight = 0.85),
        annotate("text", x = 2025.4, y = ylim[1] + diff(ylim) * 0.42, label = "airborne\ncampaign", size = 2.1, colour = "#4A7DB0", hjust = 0, lineheight = 0.85)) } +
    geom_vline(data = storms, aes(xintercept = x), colour = "grey55", linetype = "22", linewidth = 0.3) +
    geom_text(data = storms, aes(x = x, y = ylim[1] + diff(ylim) * 0.03, label = name), inherit.aes = FALSE, angle = 90, hjust = 0, vjust = -0.4, size = 2.1, colour = "grey40") +
    geom_line(linewidth = 0.45) + geom_point(size = 0.7) +
    geom_text(data = endlab, aes(x = year + 0.4, y = ylab, label = site), hjust = 0, size = 2.1, show.legend = FALSE) +
    scale_colour_manual(values = pal_class[c("intact", "regenerating", "ghost")], guide = "none") +
    scale_x_continuous(limits = c(1995, 2027.5), breaks = seq(1995, 2025, 5), expand = c(0.01, 0)) +
    scale_y_continuous(limits = ylim, breaks = ybr) + labs(x = NULL, y = ylab) + theme_fig()
}
p_traj <- traj_panel("ndvi", "NDVI (Landsat, Jan-Apr)", c(0, 1), seq(0, 1, 0.25))
fig_with <- function(ptraj) {
  T11 <- theme(plot.tag = element_text(face = "bold", size = 11))
  top2 <- pa_map + (wrap_elements(full = pb) + labs(tag = "b", title = " ") + theme(plot.title = element_text(size = 9, margin = margin(0, 0, 2, 0)), plot.tag.position = c(0.02, 0.995), plot.margin = margin(0, 0, 0, 6)) + T11) +
    plot_layout(widths = c(2.15, 1))
  top2 / (ptraj + labs(tag = "c") + T11) / (pc + labs(tag = "d") + T11) + plot_layout(heights = c(1, 0.42, 0.62))
}
fig <- fig_with(p_traj)
ggsave("output/figures/other/fig1_system.png", fig, width = 7.2, height = 8.4, dpi = 300, bg = "white")
ggsave("output/figures/other/fig1_system.pdf", fig, width = 7.2, height = 8.4, device = cairo_pdf)
ggsave("output/figures/other/fig1_system_nokey.png",
       fig_with(traj_panel("ndvi", "NDVI (Landsat, Jan-Apr)", c(0, 1), seq(0, 1, 0.25), camp_key = FALSE)), width = 7.2, height = 8.4, dpi = 300, bg = "white")
