# =============================================================================
# PROTOTYPE of the tree panels for the new Fig. 2 (where the methane comes from):
#   (b) woody-surface CH4 by height above water: stems and prop roots, all
#       closures, colour = forest class, shape = surface, open = dead; fitted
#       profiles from 06_analysis/03_woody_height_model.R (live stem, flooded,
#       wet season; 95% CI)
#   (c) where the woody surface is: TLS trunk + branch and prop-root area per m2
#       of ground in 0.5 m bins (intact = mean of SRS5, SRS6; ghost = mean of
#       CP40, FLM30; no TLS at the regenerating site), with the share of each
#       class's stand CH4 carried by stems and by prop roots.
# Writes output/figures/other/fig2_trees_prototype.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(ggplot2); library(patchwork)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
cols <- c(intact = "#228B22", regenerating = "#808080", ghost = "#8B4513")
theme_f <- theme_bw(base_size = 9) +
  theme(panel.grid.minor = element_blank(), strip.background = element_rect(fill = "grey95", colour = "grey70"),
        strip.text = element_text(face = "bold"), legend.position = "bottom", plot.tag = element_text(face = "bold"))

d <- read.csv("output/data_products/combined_gas_flux_dataset.csv") %>%
  filter(component %in% c("stem", "root"), plot %in% c("SRS5", "SRS6", "BL60", "CP40", "FLM30"),
         month_year %in% c("2022-10", "2023-03"), !is.na(height_corrected), !is.na(CH4_best.flux)) %>%
  mutate(class = factor(case_when(plot %in% c("SRS5", "SRS6") ~ "intact", plot == "BL60" ~ "regenerating", TRUE ~ "ghost"), names(cols)),
         surface = factor(ifelse(component == "root", "prop root", "stem"), c("stem", "prop root")),
         dead = status %in% "dead", key = interaction(surface, ifelse(dead, "dead", "alive"), sep = ", "),
         h = pmax(height_corrected, 0))
cur <- read.csv("output/analysis/woody_height_model_curves.csv") %>% mutate(class = factor(class, names(cols)))
br <- c(0, 1, 10, 100, 1000)
pb <- ggplot() +
  geom_point(data = d, aes(asinh(CH4_best.flux), h / 100, colour = class, shape = key), size = 1.5, alpha = 0.55, stroke = 0.5) +
  geom_ribbon(data = cur, aes(y = h / 100, xmin = fit - 1.96 * se, xmax = fit + 1.96 * se, fill = class), alpha = 0.18, orientation = "y") +
  geom_path(data = cur, aes(fit, h / 100, colour = class), linewidth = 1) +
  scale_x_continuous(breaks = asinh(br), labels = br) +
  scale_colour_manual(values = cols, name = NULL) + scale_fill_manual(values = cols, guide = "none") +
  scale_shape_manual(values = c(`stem, alive` = 16, `prop root, alive` = 17, `stem, dead` = 1, `prop root, dead` = 2), name = NULL) +
  coord_cartesian(ylim = c(0, 1.9)) +
  labs(x = expression("CH"[4]*" flux (nmol m"^-2*" s"^-1*")"), y = "Height above water (m)") +
  theme_f + guides(colour = guide_legend(nrow = 1, override.aes = list(alpha = 1)), shape = guide_legend(nrow = 2))

# (c) TLS area by height
ts <- read.csv("data/tls/tree_stats_per_site.csv")
a <- read.csv("data/tls/all_sites_summary.csv") %>%
  left_join(ts %>% select(site, area_m2), by = "site") %>%
  mutate(class = ifelse(site %in% c("SRS5", "SRS6"), "intact", "ghost"),
         surface = ifelse(segment_class == "root", "prop root", "stem (trunk + branch)"),
         sa = Total_surface_area_m2 / area_m2) %>%
  group_by(class, site, surface, z = height_bin_num) %>% summarise(sa = sum(sa), .groups = "drop") %>%
  group_by(class, surface, z) %>% summarise(sa = sum(sa) / 2, .groups = "drop") %>%
  mutate(class = factor(class, c("intact", "ghost")), sa = ifelse(surface == "prop root", -sa, sa))
pct <- function(x) ifelse(x < 0.1, "<0.1%", ifelse(x < 10, sprintf("%.1f%%", x), sprintf("%.0f%%", x)))
ch <- read.csv("output/upscaling/summary_CH4_by_component.csv") %>% filter(scenario == "exponential") %>%
  mutate(class = recode(disturbance_level, healthy = "intact")) %>% filter(class %in% c("intact", "ghost")) %>%
  group_by(class) %>% summarise(stem = 100 * mean(stem) / mean(total), root = 100 * mean(root) / mean(total), .groups = "drop") %>%
  mutate(class = factor(class, c("intact", "ghost")),
         lab = paste0("share of stand CH4\nstems ", pct(stem), "\nprop roots ", pct(root)))
pc <- ggplot(a %>% filter(z < 10), aes(sa, z + 0.25, fill = surface)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.5, fill = "#9ecae1", alpha = 0.35) +
  geom_col(orientation = "y", width = 0.45) + geom_vline(xintercept = 0, linewidth = 0.3) +
  geom_text(data = ch, aes(x = -Inf, y = Inf, label = lab), inherit.aes = FALSE, hjust = -0.05, vjust = 1.15, size = 2.5, lineheight = 0.9) +
  facet_wrap(~ class) +
  scale_x_continuous(labels = function(x) abs(x)) +
  scale_fill_manual(values = c(`prop root` = "#D2691E", `stem (trunk + branch)` = "#6b8e23"), name = NULL) +
  labs(x = expression("Surface per ground area (m"^2*" m"^-2*" per 0.5 m)"), y = "Height above TLS ground (m)") +
  theme_f
fig <- (pb + labs(tag = "b")) + (pc + labs(tag = "c")) + plot_layout(widths = c(1.15, 1))
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/fig2_trees_prototype.png", fig, width = 7.2, height = 3.9, dpi = 300)
ggsave("output/figures/other/fig2_trees_prototype.pdf", fig, width = 7.2, height = 3.9)
