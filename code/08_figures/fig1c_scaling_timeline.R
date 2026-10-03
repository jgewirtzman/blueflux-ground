# =============================================================================
# Fig. 1c drafts: (i) the cross-scale measurement and scaling chain, and
# (ii) the timeline of the 2017 hurricanes, chamber campaigns, laser scanning,
# porewater rounds, airborne deployments and the tower record used.
# Dates: chamber campaigns from flux_measurements_all.csv; TLS scan dates from
# data/tls/tls_scan_files_ornl.csv (file-name dates); airborne deployments from
# data/carafe_topdown (plus July 2024, pending); porewater rounds as sampled.
# Writes output/figures/other/fig1c_{scaling,timeline}_draft.png.
# =============================================================================
suppressMessages({library(dplyr); library(ggplot2); library(patchwork)})
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
pal_gas <- c(CO2 = "#9A9DA1", CH4 = "#A23B72")

# ---- (i) scaling chain ----
nodes <- data.frame(
  x = 1:6,
  title = c("Microbes and\nporewater", "Component\nchambers", "Laser-scanned\nsurfaces", "Stand\nbudget", "Tower and\naircraft", "Region"),
  body = c("sediment\nmetagenomes;\nporewater chemistry,\n0\u201390 cm",
           "CH₄ and CO₂ per m²\nof surface: soil, water,\nroots, stems by height,\ndowned wood, leaves",
           "surface area per\nm\u00b2 of ground, by\ncomponent and\n0.5 m height bin",
           "Σ flux × area,\ntide-weighted;\nMonte Carlo\nuncertainty",
           "eddy covariance:\ntower (CO₂),\naircraft (CH₄, CO₂)",
           "× 2017 hurricane\ndieback area\n→ added forcing"),
  scale = c("µm–cm", "cm", "cm–m", "30–100 m", "0.1–10 km", "1,000 km"),
  role = c("why", "measure", "scale", "integrate", "check", "extrapolate"),
  accent = c(pal_comp[["soil"]], pal_comp[["prop root"]], pal_class[["intact"]], col_ink, "grey45", pal_gas[["CH4"]]))
w <- 0.40; ht <- 0.55
ops <- data.frame(x = c(1.5, 2.5, 3.5, 4.5, 5.5), y = c(0.36, 0.55, 0.55, 0.55, 0.55), lab = c("explains", "×", "=", "vs", "×"),
                  size = c(1.9, 5, 5, 3, 5))
chips <- data.frame(x = 2 + seq(-0.25, 0.25, length.out = 6), y = 0.22, comp = names(pal_comp))
bars <- data.frame(y = 0.15 + (0:4) * 0.04, len = c(0.32, 0.26, 0.17, 0.1, 0.06))   # area by height (schematic)
ps <- ggplot() +
  geom_rect(data = nodes, aes(xmin = x - w, xmax = x + w, ymin = 0, ymax = 1), fill = "grey97", colour = "grey80", linewidth = 0.3) +
  geom_rect(data = nodes, aes(xmin = x - w, xmax = x + w, ymin = 0.97, ymax = 1, fill = accent), colour = NA) +
  geom_text(data = nodes, aes(x, 0.86, label = title), size = 2.7, fontface = "bold", lineheight = 0.9, colour = col_ink) +
  geom_text(data = nodes, aes(x, 0.55, label = body), size = 2.1, lineheight = 0.95, colour = "grey25") +
  geom_point(data = chips, aes(x, y, fill = comp), shape = 21, size = 2.6, colour = "white", stroke = 0.3) +
  geom_rect(data = bars, aes(xmin = 3 - 0.16, xmax = 3 - 0.16 + len, ymin = y, ymax = y + 0.035), fill = pal_class[["intact"]], alpha = 0.8) +
  annotate("text", x = 3 - 0.2, y = 0.24, label = "height", angle = 90, size = 1.8, colour = "grey40") +
  geom_text(data = ops, aes(x, y, label = lab, size = I(size)), colour = "grey30", fontface = "bold") +
  annotate("segment", x = 1.43, xend = 1.57, y = 0.31, yend = 0.31, arrow = arrow(length = unit(3, "pt")), colour = "grey40", linewidth = 0.3) +
  geom_text(data = nodes, aes(x, 0.08, label = role), size = 2.2, fontface = "italic", colour = "grey45") +
  # scale axis
  annotate("segment", x = 0.6, xend = 6.4, y = -0.12, yend = -0.12, colour = "grey40", linewidth = 0.4,
           arrow = arrow(length = unit(4, "pt"), ends = "last")) +
  geom_point(data = nodes, aes(x, -0.12), size = 1.2, colour = "grey40") +
  geom_text(data = nodes, aes(x, -0.2, label = scale), size = 2.3, colour = "grey25") +
  annotate("text", x = 0.6, y = -0.28, label = "spatial scale", hjust = 0, size = 2.1, fontface = "italic", colour = "grey45") +
  scale_fill_manual(values = c(pal_comp, setNames(unique(nodes$accent), unique(nodes$accent))), guide = "none") +
  coord_cartesian(xlim = c(0.55, 6.45), ylim = c(-0.3, 1.02), expand = FALSE) + theme_void()

# ---- (ii) timeline ----
D <- as.Date
ev <- bind_rows(
  data.frame(row = "Hurricanes", start = D(c("2017-09-10", "2017-09-20")), end = NA, lab = c("Irma and Maria, Sep 2017", NA)),
  data.frame(row = "Chambers", start = D(c("2022-03-18", "2022-10-15", "2023-03-10")), end = D(c("2022-03-24", "2022-10-26", "2023-03-22")),
             lab = c("pilot", "Oct 2022", "Mar 2023")),
  data.frame(row = "Laser scanning", start = D(c("2022-03-20", "2022-10-15", "2023-03-10")), end = NA, lab = NA),
  data.frame(row = "Porewater", start = D(c("2022-10-20", "2023-03-15", "2025-10-15")), end = NA, lab = c(NA, NA, "profiles")),
  data.frame(row = "Aircraft", start = D(c("2022-04-15", "2022-10-15", "2023-02-15", "2023-04-15", "2024-07-15")), end = NA,
             lab = c(NA, NA, NA, NA, "Jul 2024")),
  data.frame(row = "Tower (US-Skr)", start = D("2022-01-01"), end = D("2023-12-31"), lab = "record used"))
rows <- c("Hurricanes", "Chambers", "Laser scanning", "Porewater", "Aircraft", "Tower (US-Skr)")
ev <- ev %>% mutate(row = factor(row, rev(rows)))
row_col <- c(Hurricanes = pal_gas[["CH4"]], Chambers = pal_comp[["prop root"]], `Laser scanning` = pal_class[["intact"]],
             Porewater = pal_comp[["soil"]], Aircraft = "grey35", `Tower (US-Skr)` = "grey55")
pt <- ggplot(ev) + geom_blank(aes(start, row)) + scale_y_discrete(limits = rev(rows)) +
  annotate("rect", xmin = D("2017-10-01"), xmax = D("2022-02-28"), ymin = -Inf, ymax = Inf, fill = "grey96") +
  annotate("text", x = D("2019-12-15"), y = 3.5, label = "ghost forests persist, no canopy recovery", size = 2.3, colour = "grey50", fontface = "italic") +
  geom_segment(data = ev %>% filter(!is.na(end)), aes(x = start, xend = end, y = row, yend = row, colour = row), linewidth = 3.2, lineend = "butt") +
  geom_point(data = ev %>% filter(is.na(end)), aes(start, row, colour = row), size = 2.2) +
  geom_text(data = ev %>% filter(!is.na(lab)) %>% mutate(vj = ifelse(lab == "Mar 2023", 2.1, -1.1), hj = ifelse(lab %in% c("Oct 2022", "pilot"), 0.9, 0.1)),
            aes(start, row, label = lab, vjust = vj, hjust = hj), size = 2.1, colour = "grey30") +
  scale_colour_manual(values = row_col, guide = "none") +
  scale_x_date(limits = D(c("2017-06-01", "2026-09-01")), date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0)) +
  labs(x = NULL, y = NULL) + theme_fig() +
  theme(panel.grid.major.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 7, colour = "grey20"), axis.text.x = element_text(size = 7))

dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/fig1c_scaling_draft.png", ps, width = 7.2, height = 2.1, dpi = 300, bg = "white")
ggsave("output/figures/other/fig1c_timeline_draft.png", pt, width = 7.2, height = 1.7, dpi = 300, bg = "white")
ggsave("output/figures/other/fig1c_combined_draft.png", ps / pt + plot_layout(heights = c(1.25, 1)), width = 7.2, height = 3.8, dpi = 300, bg = "white")
