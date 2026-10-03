# =============================================================================
# House-style SI upscaling figures, rebuilt from CSVs written by
# code/07_upscaling/02_upscale_methane.R (no values recomputed):
#   Fig. S7b  height_extrap_sensitivity.csv  -> si_S7b_extrap_sensitivity.png
#             total plot CH4 under six stem height-extrapolation rules
#             (non-stem held constant; high tide at tidal sites), as p13b.
#   Fig. S9   plot_level_CH4_totals.csv      -> si_S9_tide_scenarios.png
#             total CH4 by site x campaign x tide state x stem rule, as p3.
#   Fig. S10  mc_component_uncertainty.csv   -> si_S10_mc_uncertainty.png
#             Monte Carlo SE per component (fixed / high-tide rows, SE > 0), as pub4.
# =============================================================================
suppressMessages({library(dplyr); library(tidyr); library(ggplot2)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
out <- "output/figures/other"
CAMP <- c("Oct 2022", "Mar 2023")
site_class <- c(SRS5 = "intact", SRS6 = "intact", CP40 = "ghost", FLM30 = "ghost")
site_lab <- setNames(paste0(names(site_class), " (", site_class, ")"), names(site_class))
strip_cls <- function(sites, size = 8) ggh4x::strip_themed(text_x = lapply(pal_class[site_class[sites]],
  function(cc) element_text(colour = cc, face = "bold", size = size, hjust = 0.5)))
camp_shape <- c(`Oct 2022` = 16, `Mar 2023` = 2)
save_si <- function(p, name, h) {
  ggsave(file.path(out, paste0(name, ".png")), p, width = 7.2, height = h, dpi = 300, bg = "white", device = ragg::agg_png)
  ggsave(file.path(out, paste0(name, ".pdf")), p, width = 7.2, height = h, device = cairo_pdf)
  cat("  written", name, "\n")
}

# ---- S7b: total CH4 under stem height-extrapolation rules --------------------
rule_lab <- c(zero_above = "zero above 1.5 m", exp_zero_asym = "exponential, asymptote 0 (default)",
              linear_clamp = "linear, clamped \u2265 0", linear_obs_range = "linear, capped at observed range",
              exp_free_asym = "exponential, free asymptote", constant_at_max = "exponential, constant above 1.5 m")
ex <- read.csv("output/upscaling/height_extrap_sensitivity.csv")
stopifnot(all(ex$scenario %in% names(rule_lab)))
ex <- ex %>% mutate(rule = factor(rule_lab[scenario], rev(rule_lab)),
                    site = factor(site, names(site_class)), campaign = factor(campaign, CAMP))
p7b <- ggplot(ex, aes(total_mg_m2_d, rule, shape = campaign, colour = site)) +
  geom_point(size = 1.8, stroke = 0.6) +
  ggh4x::facet_wrap2(~ site, nrow = 1, scales = "free_x", labeller = as_labeller(site_lab),
                     strip = strip_cls(levels(ex$site))) +
  scale_colour_manual(values = setNames(pal_class[site_class], names(site_class)), guide = "none") +
  scale_shape_manual(values = camp_shape, name = NULL) +
  scale_x_continuous(expand = expansion(mult = 0.12)) +
  labs(x = expression("Total plot CH"[4]*" (mg m"^-2*" d"^-1*")"), y = NULL, tag = "b") +
  theme_fig() + theme(panel.spacing.x = unit(8, "pt"), legend.margin = margin(0, 0, 0, 0),
                      legend.box.spacing = unit(2, "pt"), legend.text = element_text(size = 7),
                      plot.tag.position = c(0, 1))
save_si(p7b, "si_S7b_extrap_sensitivity", 2.6)

# ---- S9: tide state x stem rule ---------------------------------------------
tide_lab <- c(fixed = "non-tidal", high_tide = "high tide", low_tide = "low tide")
stem_lab <- c(exponential = "exponential decay above 1.5 m", zero_above_max = "zero above 1.5 m")
sc <- read.csv("output/upscaling/plot_level_CH4_totals.csv") %>%
  filter(campaign %in% CAMP) %>%
  mutate(site = factor(site, rev(names(site_class))), campaign = factor(campaign, CAMP),
         tide = factor(tide_lab[tide_state], tide_lab), stem = factor(stem_lab[scenario], stem_lab),
         y = as.numeric(site) + ifelse(scenario == "exponential", 0.13, -0.13))
rng <- sc %>% group_by(site, campaign, stem, y) %>% summarise(lo = min(total_mg), hi = max(total_mg), .groups = "drop")
p9 <- ggplot(sc) +
  geom_segment(data = rng, aes(x = lo, xend = hi, y = y, yend = y, colour = stem), linewidth = 0.4) +
  geom_point(aes(total_mg, y, shape = tide, colour = stem, fill = stem), size = 1.9, stroke = 0.6) +
  facet_wrap(~ campaign, nrow = 1) +
  scale_y_continuous(breaks = seq_along(levels(sc$site)), labels = levels(sc$site), expand = expansion(add = 0.4)) +
  scale_x_log10(breaks = c(0.5, 1, 2, 5, 10, 20, 50, 100), labels = c(0.5, 1, 2, 5, 10, 20, 50, 100)) +
  scale_shape_manual(values = c(`non-tidal` = 23, `high tide` = 21, `low tide` = 1), name = NULL) +
  scale_colour_manual(values = c(col_ink, "#E07B39"), name = "stem rule", drop = FALSE) +
  scale_fill_manual(values = c(col_ink, "#E07B39"), guide = "none") +
  guides(shape = guide_legend(order = 1, override.aes = list(colour = col_ink, fill = col_ink)),
         colour = guide_legend(order = 2, override.aes = list(shape = 16))) +
  labs(x = expression("Total plot CH"[4]*" (mg m"^-2*" d"^-1*", log scale)"), y = NULL) +
  theme_fig() + theme(panel.grid.major.y = element_blank(), panel.spacing.x = unit(12, "pt"),
                      strip.text = element_text(hjust = 0.5, size = 8),
                      axis.text.y = element_text(colour = pal_class[site_class[levels(sc$site)]], face = "bold", size = 7.5),
                      legend.box = "vertical", legend.spacing.y = unit(0, "pt"), legend.margin = margin(0, 0, 0, 0),
                      legend.box.spacing = unit(2, "pt"), legend.text = element_text(size = 7),
                      legend.title = element_text(size = 7, face = "bold"))
save_si(p9, "si_S9_tide_scenarios", 3.0)

# ---- S10: Monte Carlo SE by component ---------------------------------------
comp_lab <- c(soil = "soil", water = "water", root = "prop root", cwd = "downed wood",
              stem_measured = "stem \u2264 1.5 m", stem_extrapolated = "stem > 1.5 m")
comp_col <- c(pal_comp[c("soil", "water", "prop root", "downed wood", "stem")], "#F6EEDA")
names(comp_col) <- comp_lab
mc <- read.csv("output/upscaling/mc_component_uncertainty.csv") %>%
  filter(component != "total", tide_state %in% c("fixed", "high_tide"), mc_se > 0, campaign %in% CAMP) %>%
  mutate(comp = factor(comp_lab[component], rev(comp_lab)), site = factor(site, names(site_class)),
         campaign = factor(campaign, CAMP))
p10 <- ggplot(mc, aes(mc_se, comp)) +
  geom_segment(aes(x = 1e-4, xend = mc_se, yend = comp), colour = "grey75", linewidth = 0.3) +
  geom_point(aes(fill = comp), shape = 21, size = 2, colour = "grey25", stroke = 0.3) +
  ggh4x::facet_grid2(campaign ~ site, labeller = labeller(site = site_lab),
                     strip = ggh4x::strip_themed(text_x = lapply(pal_class[site_class], function(cc)
                       element_text(colour = cc, face = "bold", size = 8, hjust = 0.5)),
                       text_y = list(element_text(face = "bold", size = 8, angle = -90)))) +
  scale_fill_manual(values = comp_col, guide = "none") +
  scale_x_log10(breaks = 10^(-3:1), labels = c("0.001", "0.01", "0.1", "1", "10"),
                expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(xlim = c(1e-4, 40)) +
  scale_y_discrete(drop = FALSE) +
  labs(x = expression("Monte Carlo SE of component CH"[4]*" (mg m"^-2*" d"^-1*", log scale)"), y = NULL) +
  theme_fig() + theme(panel.grid.major.y = element_blank(), panel.spacing = unit(8, "pt"),
                      axis.text.y = element_text(size = 7))
save_si(p10, "si_S10_mc_uncertainty", 3.4)
