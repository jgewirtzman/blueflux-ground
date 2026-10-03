# Fig. S7 | Stem CH4 height extrapolation and its consequences.
#   (a) Measured stem CH4 by height (y) for each TLS site x campaign, with the stem
#       profile under each of the six extrapolation rules used in the sensitivity
#       analysis (default: exponential decay to zero). Shaded: above the 1.5 m
#       chamber limit. x axes are inverse-hyperbolic-sine and differ by site.
#   (b) Stem CH4 per unit ground area (absolute tree flux) under each rule.
#   (c) Total plot CH4 under each rule (non-stem components held constant).
# Inputs: output/upscaling/{height_extrap_profiles.csv, height_extrap_sensitivity.csv}
#   (02_upscale_methane.R) and the measured stem fluxes.
suppressMessages({library(dplyr); library(ggplot2); library(patchwork)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))
dir.create("output/figures/other", recursive = TRUE, showWarnings = FALSE)

site_lv <- c("SRS5", "SRS6", "CP40", "FLM30")
site_cls <- c(SRS5 = "intact", SRS6 = "intact", CP40 = "ghost", FLM30 = "ghost")
camp_lv <- c("Oct 2022", "Mar 2023")
rule_lv <- c(exp_zero_asym = "exponential to zero (default)", zero_above = "zero above 1.5 m",
             constant_at_max = "constant above 1.5 m", exp_free_asym = "exponential, free asymptote",
             linear_clamp = "linear, clamped at zero", linear_obs_range = "linear, capped at observed range")
rule_col <- setNames(c("#222222", "#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00"), rule_lv)
rule_lty <- setNames(c("solid", "22", "solid", "42", "solid", "13"), rule_lv)
fac <- function(x) x %>% mutate(site = factor(site, site_lv), campaign = factor(campaign, camp_lv))
strip_site <- ggh4x::strip_themed(text_x = lapply(pal_class[site_cls[site_lv]], function(cc) element_text(colour = cc, face = "bold")))

# ---- (a) profiles under each rule, with the measurements
prof <- read.csv("output/upscaling/height_extrap_profiles.csv") %>% fac() %>%
  mutate(rule = factor(rule_lv[scenario], rule_lv), h = height_m)
meas <- read.csv("output/data_products/combined_gas_flux_dataset.csv") %>%
  filter(component == "stem", plot %in% site_lv, !is.na(CH4_best.flux), !is.na(height_corrected), height_corrected >= 0) %>%
  mutate(campaign = ifelse(year == 2022 & month == 10, "Oct 2022", ifelse(year == 2023 & month == 3, "Mar 2023", NA)),
         site = plot, h = height_corrected / 100) %>% filter(!is.na(campaign)) %>% fac()
HTOP <- 12
pa <- ggplot() +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1.5, ymax = HTOP, fill = "grey95") +
  geom_vline(xintercept = 0, colour = "grey35", linewidth = 0.35) +
  geom_point(data = meas, aes(asinh(CH4_best.flux), h), colour = "grey45", alpha = 0.5, size = 0.6, stroke = 0) +
  geom_path(data = prof %>% filter(h <= HTOP) %>% arrange(h), aes(asinh(pred_flux), h, colour = rule, linetype = rule),
            linewidth = 0.55) +
  ggh4x::facet_grid2(campaign ~ site, scales = "free_x", independent = "x", strip = strip_site) +
  ggh4x::facetted_pos_scales(x = list(
    site %in% c("SRS5", "SRS6") ~ scale_x_continuous(breaks = asinh(c(-1, -0.5, 0, 0.5, 1, 2)), labels = c(-1, -0.5, 0, 0.5, 1, 2)),
    site %in% c("CP40", "FLM30") ~ scale_x_continuous(breaks = asinh(c(-1, 0, 1, 3, 10, 30, 100)), labels = c(-1, 0, 1, 3, 10, 30, 100)))) +
  # square-root height axis: the measured zone (0-1.5 m) gets room while the canopy stays visible
  scale_y_sqrt(breaks = c(0, 0.5, 1, 1.5, 3, 6, 12), limits = c(0, HTOP), expand = c(0, 0)) +
  scale_colour_manual(values = rule_col, name = "extrapolation rule") +
  scale_linetype_manual(values = rule_lty, name = "extrapolation rule") +
  labs(x = expression("Stem CH"[4]*" flux (nmol m"^-2*" stem s"^-1*"; x axes differ by site)"),
       y = "Height (m; square-root axis)", tag = "a") + theme_fig() +
  theme(legend.position = "bottom", legend.key.width = unit(16, "pt")) +
  guides(colour = guide_legend(ncol = 3), linetype = guide_legend(ncol = 3))

# ---- (b) absolute stem flux and (c) total plot CH4 under each rule
ex <- read.csv("output/upscaling/height_extrap_sensitivity.csv") %>% fac() %>%
  mutate(rule = factor(rule_lv[scenario], rev(rule_lv)))
dot <- function(v, xlab, tag) {
  ggplot(ex, aes(.data[[v]], rule, shape = campaign)) +
    geom_point(aes(colour = rule), size = 1.7, stroke = 0.5) +
    ggh4x::facet_wrap2(~ site, nrow = 1, scales = "free_x", strip = strip_site) +
    scale_colour_manual(values = rule_col, guide = "none") +
    scale_shape_manual(values = c(`Oct 2022` = 16, `Mar 2023` = 1), name = NULL) +
    scale_x_continuous(expand = expansion(mult = 0.15)) +
    labs(x = xlab, y = NULL, tag = tag) + theme_fig() + theme(panel.grid.major.y = element_blank())
}
pb <- dot("stem_mg_m2_d", expression("Stem CH"[4]*" (mg m"^-2*" ground d"^-1*")"), "b")
pc <- dot("total_mg_m2_d", expression("Total plot CH"[4]*" (mg m"^-2*" ground d"^-1*")"), "c") + theme(legend.position = "bottom")

p <- pa / pb / pc + plot_layout(heights = c(2.2, 1, 1))
ggsave("output/figures/other/stem_extrap_clean.pdf", p, width = 7.2, height = 8.6, device = cairo_pdf)
ggsave("output/figures/other/stem_extrap_clean.png", p, width = 7.2, height = 8.6, dpi = 300, bg = "white")
