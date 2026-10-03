# =============================================================================
# PROTOTYPE tree panels for the new Fig. 2 (where the methane comes from).
#   (b) CH4 flux per m2 of woody surface by height above the water, stems and
#       prop roots, one panel per forest class; log axis for positive fluxes,
#       values <= 0.01 (including zero and negative) in a gutter at the left;
#       line = fitted profile; band = 95% CI where its lower bound > 0.01 (06_analysis/03_woody_height_model.R;
#       live stem, flooded position, wet season)
#   (c) CH4 per m2 of ground from each 0.5 m height stratum (flux x laser-scanned
#       surface area; 07_upscaling/02_upscale_methane.R, woody_ch4_by_height.csv),
#       stems (trunk + branch) and prop roots, mean of sites and campaigns
#   (d) share of stand CH4 by component (summary_CH4_by_component.csv)
# Intact = SRS5, SRS6; ghost = CP40, FLM30; regenerating (BL60) has no TLS scan
# so appears in (b) only.
# Writes output/figures/other/fig2_trees_prototype.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(ggplot2); library(patchwork)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
set.seed(3)
comp_cols <- c(stem = "#31a354", `prop root` = "#e6550d", soil = "#8c6d31", water = "#3182bd", `downed wood` = "#969696")
theme_f <- theme_bw(base_size = 8.5) + theme(panel.spacing.x = unit(10, "pt")) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        strip.background = element_blank(), strip.text = element_text(face = "bold", size = 9),
        legend.position = "bottom", plot.tag = element_text(face = "bold", size = 11))
classes <- c("intact", "regenerating", "ghost")
floor_v <- 0.01; gut <- 0.0035                                   # gutter position for values <= 0.01

d <- read.csv("output/data_products/combined_gas_flux_dataset.csv") %>%
  filter(component %in% c("stem", "root"), plot %in% c("SRS5", "SRS6", "BL60", "CP40", "FLM30"),
         month_year %in% c("2022-10", "2023-03"), !is.na(height_corrected), !is.na(CH4_best.flux)) %>%
  mutate(class = factor(case_when(plot %in% c("SRS5", "SRS6") ~ "intact", plot == "BL60" ~ "regenerating", TRUE ~ "ghost"), classes),
         surface = factor(ifelse(component == "root", "prop root", "stem"), c("stem", "prop root")),
         x = ifelse(CH4_best.flux > floor_v, CH4_best.flux, gut * exp(runif(n(), -0.25, 0.25))),
         h = pmax(height_corrected, 0) / 100)
cur <- read.csv("output/analysis/woody_height_model_curves.csv") %>%
  mutate(class = factor(class, classes), h = h / 100,
         x = ifelse(flux > floor_v, flux, NA), xl = pmax(lo, floor_v), xh = pmax(hi, floor_v)) %>%
  filter(hi > floor_v)
br <- c(0.01, 0.1, 1, 10, 100, 1000)
pb <- ggplot() +
  annotate("rect", xmin = 0.0022, xmax = 0.0058, ymin = -Inf, ymax = Inf, fill = "grey92") +
  geom_point(data = d, aes(x, h, colour = surface, shape = surface), size = 1.3, alpha = 0.6) +
  geom_ribbon(data = cur %>% filter(lo > floor_v), aes(y = h, xmin = xl, xmax = xh), fill = "black", alpha = 0.12, orientation = "y") +
  geom_path(data = cur, aes(x, h), colour = "black", linewidth = 0.8, na.rm = TRUE) +
  facet_wrap(~ class, nrow = 1) +
  scale_x_log10(breaks = c(gut, 0.1, 1, 10, 100), labels = expression("" <= 0.01, 0.1, 1, 10, 100),
                limits = c(0.0022, 1200), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1.9), expand = c(0.01, 0)) +
  scale_colour_manual(values = comp_cols[c("stem", "prop root")], name = NULL) +
  scale_shape_manual(values = c(stem = 16, `prop root` = 17), name = NULL) +
  labs(x = expression("CH"[4]*" flux (nmol m"^-2*" of woody surface s"^-1*")"), y = "Height above water (m)") +
  theme_f + theme(panel.grid.major.x = element_line(colour = "grey92"))

w <- read.csv("output/upscaling/woody_ch4_by_height.csv") %>%
  mutate(class = factor(ifelse(site %in% c("SRS5", "SRS6"), "intact", "ghost"), c("intact", "ghost"))) %>%
  group_by(class, surface, height_bin_m, campaign) %>% summarise(v = mean(ch4_nmol_m2_s), .groups = "drop") %>%
  group_by(class, surface, z = height_bin_m + 0.25) %>% summarise(v = mean(v), .groups = "drop") %>%
  filter(v > 0) %>% mutate(surface = factor(surface, c("stem", "prop root")),
                           z = z + ifelse(surface == "stem", 0.09, -0.09))
wt <- w %>% group_by(class, surface) %>% summarise(v = sum(v), .groups = "drop") %>%
  summarise(lab = paste0("total: stems ", formatC(v[surface == "stem"], format = "fg", digits = 2),
                         ", roots ", formatC(v[surface == "prop root"], format = "fg", digits = 2)), .by = class)
pc <- ggplot(w, aes(v, z, colour = surface)) +
  annotate("rect", xmin = 1e-5, xmax = 2, ymin = 0, ymax = 0.5, fill = "#deebf7", alpha = 0.7) +
  geom_segment(aes(x = 1e-5, xend = v, yend = z), linewidth = 0.4, alpha = 0.6) +
  geom_point(size = 1.2) +
  facet_wrap(~ class, nrow = 1) +
  geom_text(data = wt, aes(x = 2, y = Inf, label = lab), inherit.aes = FALSE, hjust = 1, vjust = 1.5, size = 2.3) +
  scale_x_log10(breaks = 10^(-4:0), labels = c("", "0.001", "", "0.1", "1"), limits = c(1e-5, 2)) +
  scale_colour_manual(values = comp_cols[c("stem", "prop root")], guide = "none") +
  labs(x = expression("CH"[4]*" per stratum (nmol m"^-2*" ground s"^-1*")"), y = "Height (m)") +
  theme_f + theme(panel.grid.major.x = element_line(colour = "grey92"))

sh <- read.csv("output/upscaling/summary_CH4_by_component.csv") %>% filter(scenario == "exponential") %>%
  mutate(class = recode(disturbance_level, healthy = "intact")) %>% filter(class %in% c("intact", "ghost")) %>%
  group_by(class) %>% summarise(across(c(stem, root, soil, water, cwd), mean), total = mean(total), .groups = "drop") %>%
  tidyr::pivot_longer(c(stem, root, soil, water, cwd), names_to = "component", values_to = "mg") %>%
  mutate(component = factor(recode(component, root = "prop root", cwd = "downed wood"), names(comp_cols)),
         class = factor(class, c("ghost", "intact")), pct = 100 * mg / total,
         lab = ifelse(pct >= 5, sprintf("%.0f%%", pct), ""))
tl <- sh %>% distinct(class, total) %>% mutate(lab = sprintf("%.1f~mg~m^-2~d^-1", total))
pd <- ggplot(sh, aes(pct, class, fill = component)) +
  geom_col(width = 0.6, colour = "white", linewidth = 0.2, position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = lab), position = position_stack(vjust = 0.5, reverse = TRUE), size = 2.5, colour = "white") +
  geom_text(data = tl, aes(x = 101, y = class, label = lab), inherit.aes = FALSE, hjust = 0, size = 2.5, parse = TRUE) +
  scale_x_continuous(limits = c(0, 135), breaks = c(0, 50, 100), expand = c(0, 0)) +
  scale_fill_manual(values = comp_cols, name = NULL) +
  labs(x = expression("Share of stand CH"[4]*" (%)"), y = NULL) + theme_f

fig <- (pb + labs(tag = "b")) / ((pc + labs(tag = "c")) | (pd + labs(tag = "d"))) +
  plot_layout(heights = c(1, 0.9))
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/fig2_trees_prototype.png", fig, width = 7.2, height = 6, dpi = 300)
ggsave("output/figures/other/fig2_trees_prototype.pdf", fig, width = 7.2, height = 6, device = cairo_pdf)
