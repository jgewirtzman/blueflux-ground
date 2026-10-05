# =============================================================================
# House-style SI upscaling figures, rebuilt from CSVs written by
# code/07_upscaling/02_upscale_methane.R (no values recomputed):
#   (legacy) si_S7b_extrap_sensitivity.png, si_S9_tide_scenarios.png (superseded; not collected)
#   Fig. S9   plot_level_CH4_totals.csv + mc_component_uncertainty.csv -> si_tide_states.png
#   Fig. S13  qa/sensitivity_summary.csv     -> si_sensitivity_switch.png
#   Fig. S14  mc_component_uncertainty.csv   -> si_S10_mc_uncertainty.png
#             Monte Carlo SE per component (fixed / high-tide rows, SE > 0), as pub4.
# =============================================================================
suppressMessages({library(dplyr); library(tidyr); library(ggplot2)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))
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

# ---- S11 (file si_S10_mc_uncertainty): component CH4 with Monte Carlo 95% intervals ----
# Tide states weighted by the flooded share of the floor (as in the budgets); right-hand labels give
# each component's share of the stand variance (SE^2 / sum SE^2). Components that are always zero omitted.
comp_lab <- c(water = "water", soil = "soil", root = "prop root", cwd = "downed wood",
              stem_measured = "stem ≤ 1.5 m", stem_extrapolated = "stem > 1.5 m")
comp_col <- c(pal_comp[c("water", "soil", "prop root", "downed wood", "stem")], "#F6EEDA"); names(comp_col) <- comp_lab
mc <- read.csv("output/upscaling/mc_component_uncertainty.csv") %>% filter(component != "total", campaign %in% CAMP) %>%
  group_by(site, campaign, component) %>%
  summarise(mean = weighted.mean(mc_mean, tide_weight), lo = weighted.mean(mc_ci_lo, tide_weight),
            hi = weighted.mean(mc_ci_hi, tide_weight), se = weighted.mean(mc_se, tide_weight), .groups = "drop") %>%
  group_by(site, campaign) %>% mutate(vshare = 100 * se^2 / sum(se^2)) %>% ungroup() %>%
  filter(!(mean == 0 & hi == 0)) %>%
  mutate(comp = factor(comp_lab[component], rev(comp_lab)), site = factor(site, names(site_class)), campaign = factor(campaign, CAMP))
p10 <- ggplot(mc, aes(y = comp)) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = 0.5, colour = "grey35", orientation = "y") +
  geom_point(aes(x = mean, fill = comp), shape = 21, size = 2, colour = col_ink, stroke = 0.3) +
  geom_text(aes(x = Inf, label = ifelse(vshare < 0.5, "<1%", sprintf("%.0f%%", vshare))), hjust = 1.05, size = 2.1, colour = "grey35") +
  ggh4x::facet_grid2(campaign ~ site, strip = ggh4x::strip_themed(text_x = lapply(pal_class[site_class], function(cc)
    element_text(colour = cc, face = "bold", size = 8)))) +
  scale_fill_manual(values = comp_col, guide = "none") +
  scale_x_continuous(trans = "asinh", breaks = c(0, 1, 10, 100), expand = expansion(mult = c(0.05, 0.28))) +
  labs(x = expression("Component CH"[4]*" (mg m"^-2*" ground d"^-1*"; Monte Carlo mean and 95% interval)"), y = NULL) + theme_fig()
save_si(p10, "si_S10_mc_uncertainty", 3.6)

# ---- Tide: intact stand CH4 at high vs low tide, by component -----------------
# SRS5/SRS6 (the tidal sites), exponential stem rule; bars = high- and low-tide
# states, diamond = the tide-weighted value used in the budgets (flooded share of
# the floor in the campaign month; M11).
td <- read.csv("output/upscaling/plot_level_CH4_totals.csv") %>%
  filter(scenario == "exponential", tide_state %in% c("high_tide", "low_tide")) %>%
  mutate(campaign = factor(campaign, CAMP), site = factor(site, c("SRS5", "SRS6")),
         tide = factor(ifelse(tide_state == "high_tide", "high tide", "low tide"), c("high tide", "low tide")))
tw <- td %>% group_by(site, campaign) %>% summarise(w = sum(total_mg * tide_weight) / sum(tide_weight), .groups = "drop")
tl <- td %>% select(site, campaign, tide, water = water_mg, soil = soil_mg, `prop root` = root_mg, stem = stem_mg,
                    `downed wood` = cwd_mg) %>%
  pivot_longer(c(water, soil, `prop root`, stem, `downed wood`), names_to = "comp", values_to = "mg") %>%
  mutate(comp = factor(comp, c("water", "soil", "prop root", "stem", "downed wood")))
# Monte Carlo 95% intervals of the stand total for each tide state (mc_component_uncertainty.csv)
mci <- read.csv("output/upscaling/mc_component_uncertainty.csv") %>%
  filter(site %in% c("SRS5", "SRS6"), component == "total", tide_state %in% c("high_tide", "low_tide")) %>%
  mutate(campaign = factor(campaign, CAMP), site = factor(site, c("SRS5", "SRS6")),
         tide = factor(ifelse(tide_state == "high_tide", "high tide", "low tide"), c("high tide", "low tide"))) %>%
  left_join(td %>% select(site, campaign, tide, total_mg), by = c("site", "campaign", "tide"))
# stacked segments built on the raw scale so the asinh axis does not distort the stacking
seg_t <- tl %>% group_by(site, campaign, tide) %>% arrange(comp, .by_group = TRUE) %>%
  mutate(hi = cumsum(mg), lo = hi - mg) %>% ungroup() %>% filter(mg > 0)
xi <- function(f) as.numeric(f)
p_tide <- ggplot(seg_t) +
  geom_rect(aes(xmin = xi(tide) - 0.3, xmax = xi(tide) + 0.3, ymin = lo, ymax = hi, fill = comp), colour = "white", linewidth = 0.25) +
  geom_errorbar(data = mci, aes(x = xi(tide), ymin = mc_ci_lo, ymax = mc_ci_hi), width = 0.1, linewidth = 0.4, colour = col_ink) +
  geom_point(data = mci, aes(x = xi(tide), y = total_mg), shape = 23, size = 1.6, fill = "white", colour = col_ink, stroke = 0.4) +
  geom_hline(data = tw, aes(yintercept = w), linetype = "22", colour = col_ink, linewidth = 0.35) +
  geom_label(data = tw, aes(x = 1.5, y = w, label = "tide-weighted"), inherit.aes = FALSE, size = 2.1,
             colour = "grey25", fill = "white", label.size = 0, label.padding = unit(1.2, "pt")) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
  facet_grid(campaign ~ site) +
  scale_fill_manual(values = pal_comp, name = NULL) +
  scale_x_continuous(breaks = 1:2, labels = c("high tide", "low tide"), expand = expansion(add = 0.45)) +
  scale_y_continuous(trans = "asinh", breaks = c(-1, 0, 1, 2, 5, 10, 20, 50)) +
  labs(x = NULL, y = expression("Stand CH"[4]*" (mg m"^-2*" ground d"^-1*")")) +
  theme_fig() + theme(legend.position = "right", panel.grid.major.x = element_blank(),
                      strip.text.x = element_text(colour = pal_class[["intact"]], face = "bold", size = 8, hjust = 0.5))
save_si(p_tide + labs(tag = "d"), "si_tide_states", 4.0)   # panel d below the flooded-share panels

# ---- Sensitivity of the intact-to-ghost switch to analytical choices (table S3) ----
sens_lab <- c(`none (1.00)` = "none (1.00)", `literature (2.00)` = "literature (2.00)", `stem_chambers (4.33)` = "stem chambers (4.33)",
  `negligible (0.01 m3/ha)` = "negligible (0.01 m³ ha⁻¹)", `krauss_lo (13 m3/ha)` = "Krauss low (13 m³ ha⁻¹)",
  `krauss_eyewall (132 m3/ha)` = "Krauss eyewall (132 m³ ha⁻¹)", `krauss_hi (181 m3/ha)` = "Krauss high (181 m³ ha⁻¹)",
  equal_split = "high and low tide weighted equally", switch_campaign = "all-or-nothing, campaign months",
  switch_longterm = "all-or-nothing, long-term record", area_campaign_lo = "area-weighted, campaign months (low)",
  area_campaign_hi = "area-weighted, campaign months (high)", area_longterm = "area-weighted, long-term record",
  necb_alk_retained = "lateral export, alkalinity retained", necb_all_export = "lateral export, all returned to air",
  storage = "storage only (burial + wood)")
choice_lab <- c(`Q10 (day -> 24 h, chamber CO2)` = "Q10, day to 24 h (chamber CO₂)", `Downed CWD volume` = "Downed wood volume",
  `Flooding representation (intact)` = "Flooding of the intact floor", `Tidal phase, intact water CH4` = "Tidal phase, intact water CH₄",
  `CH4 day -> 24 h` = "CH₄, day to 24 h", `Ghost floor without standing water (Mar 2023)` = "Ghost floor without standing water (Mar 2023)",
  `Leaf respiration` = "Leaf respiration", `Carbon-balance framing` = "Carbon-balance framing")
ss <- read.csv("output/qa/sensitivity_summary.csv")
cen <- ss$switch_net20[ss$choice == "central"]
sd <- ss %>% filter(!choice %in% c("central", "Monte Carlo 95 % interval")) %>%
  mutate(setting = gsub("\\s+", " ", setting), key = sub(" \\(SRS5.*$", "", setting),
         lab = ifelse(key %in% names(sens_lab), sens_lab[key], gsub("<= ", "≤", setting)),
         choice = choice_lab[choice])
ord <- sd %>% group_by(choice) %>% summarise(r = max(switch_net20) - min(switch_net20), .groups = "drop") %>% arrange(desc(r)) %>% pull(choice)
ord <- c(setdiff(ord, choice_lab[["Carbon-balance framing"]]), choice_lab[["Carbon-balance framing"]])
sd <- sd %>% mutate(choice = factor(choice, ord)) %>% arrange(choice, switch_net20) %>%
  mutate(row = factor(paste(choice, lab, sep = "||"), unique(paste(choice, lab, sep = "||"))))
p_sens <- ggplot(sd, aes(y = row)) +
  geom_vline(xintercept = cen, colour = col_ink, linewidth = 0.4) +
  geom_segment(aes(x = cen, xend = switch_net20, yend = row), colour = "grey70", linewidth = 0.6) +
  geom_point(aes(x = switch_net20, fill = switch_net20 > cen), shape = 21, size = 2, colour = "white", stroke = 0.3) +
  facet_grid(choice ~ ., scales = "free_y", space = "free_y", switch = "y", labeller = label_wrap_gen(24)) +
  scale_y_discrete(labels = function(x) sub("^.*\\|\\|", "", x)) +
  scale_fill_manual(values = c(`TRUE` = "#A23B72", `FALSE` = "#2C7BB6"), guide = "none") +
  labs(x = expression("Intact-to-ghost switch (g CO"[2]*"-eq m"^-2*" yr"^-1*", GWP20)"), y = NULL) +
  theme_fig() + theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold", size = 7),
                      axis.text.y = element_text(size = 6.5), panel.grid.major.y = element_blank(), panel.spacing.y = unit(3, "pt"))
save_si(p_sens, "si_sensitivity_switch", 6.2)
