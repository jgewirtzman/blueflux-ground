# =============================================================================
# Fig. 2 | Flux rates: what each surface emits.
#   (a) CH4 and (b) CO2 per m2 of surface by component (rows) and forest class
#       (side by side, class colours, one shared axis): individual measurements
#       and bootstrapped means with 95% CIs (5,000 resamples). Soil collars
#       include pneumatophores in the footprint. Core sites (intact SRS5, SRS6;
#       regenerating BL60; ghost CP40, FLM30), all campaigns, as in the stand
#       budgets; context sites (RB10, MI, SE1) are in Extended Data 3.
#   (c) CH4 and (d) CO2 per m2 of woody surface (stems + prop roots) by height
#       above the water, all classes on one axis, with fitted profiles
#       (06_analysis/03_woody_height_model.R; live stem, flooded position, wet
#       season; typical rather than mean flux).
# Writes output/figures/other/fig2_rates.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(ggplot2); library(patchwork)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
set.seed(42)

comp_rows <- c(water = "water", soil = "soil", pneumatophore = "soil", root = "prop root", stem = "stem",
               cwd = "downed wood", leaves = "leaf")
d <- read.csv("output/data_products/combined_gas_flux_dataset.csv") %>%
  filter(disturbance_level %in% c("healthy", "regenerating", "ghost"),
         is.na(CO2_best.flux) | CO2_best.flux >= -10) %>%
  mutate(class = factor(class_labels[disturbance_level], names(pal_class)),
         comp = factor(comp_rows[component], rev(unique(comp_rows))),
         site_type = factor(ifelse(plot %in% c("SRS5", "SRS6", "BL60", "CP40", "FLM30"), "core site", "context site"),
                            c("core site", "context site")))
boot <- function(x, R = 5000) { x <- x[is.finite(x)]; if (length(x) < 3) return(c(mean(x), NA, NA))
  b <- replicate(R, mean(sample(x, replace = TRUE))); c(mean(x), quantile(b, c(0.025, 0.975))) }
dodge <- c(intact = 0.25, regenerating = 0, ghost = -0.25)

rate_panel <- function(dd, gas, status, breaks, xlab, show_y = TRUE) {
  x <- dd %>% filter(.data[[status]] == "valid", !is.na(comp)) %>% mutate(v = .data[[paste0(gas, "_best.flux")]])
  s <- x %>% group_by(comp, class) %>% summarise(n = n(), m = boot(v)[1], lo = boot(v)[2], hi = boot(v)[3], .groups = "drop") %>%
    mutate(y = as.numeric(comp) + dodge[as.character(class)])
  x <- x %>% mutate(y = as.numeric(comp) + dodge[as.character(class)] + runif(n(), -0.06, 0.06))
  bands <- data.frame(y = seq_along(levels(dd$comp))) %>% filter(y %% 2 == 1)
  ggplot() +
    geom_rect(data = bands, aes(ymin = y - 0.5, ymax = y + 0.5), xmin = -Inf, xmax = Inf, fill = "grey95") +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
    geom_point(data = x, aes(asinh(v), y, colour = class, shape = site_type), size = 0.75, alpha = 0.35, stroke = 0.35) +
    geom_errorbar(data = s, aes(xmin = asinh(lo), xmax = asinh(hi), y = y, colour = class), width = 0, linewidth = 0.6, orientation = "y") +
    geom_point(data = s, aes(asinh(m), y, fill = class), shape = 21, colour = "white", size = 2.1, stroke = 0.5) +
    scale_y_continuous(breaks = seq_along(levels(dd$comp)), labels = if (show_y) levels(dd$comp) else NULL,
                       expand = c(0, 0), limits = c(0.5, length(levels(dd$comp)) + 0.5)) +
    scale_x_continuous(breaks = asinh(breaks), labels = breaks) +
    scale_colour_manual(values = pal_class, name = NULL, guide = "none") + scale_fill_manual(values = pal_class, name = NULL) +
    scale_shape_manual(values = c(`core site` = 16, `context site` = 4), guide = "none") +
    labs(x = xlab, y = NULL) + theme_fig() + theme(panel.grid.major.y = element_blank(), axis.ticks.y = element_blank())
}
w <- d %>% filter(site_type == "core site", component %in% c("stem", "root"), month_year %in% c("2022-10", "2023-03"), !is.na(height_corrected)) %>%
  mutate(surface = factor(ifelse(component == "root", "prop root", "stem"), c("stem", "prop root")),
         h = pmax(height_corrected, 0) / 100)
prof_panel <- function(gas, curves, breaks, xlab) {
  cur <- read.csv(curves) %>% mutate(class = factor(class, names(pal_class)))
  ww <- w %>% mutate(v = .data[[paste0(gas, "_best.flux")]]) %>% filter(!is.na(v))
  ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -0.03, ymax = 0.05, fill = col_waterline) +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
    geom_point(data = ww, aes(asinh(v), h, colour = class, shape = surface), size = 0.9, alpha = 0.35, stroke = 0.4) +
    geom_ribbon(data = cur, aes(y = h / 100, xmin = fit - 1.96 * se, xmax = fit + 1.96 * se, fill = class),
                alpha = 0.18, orientation = "y") +
    geom_path(data = cur, aes(fit, h / 100, colour = class), linewidth = 1) +
    scale_x_continuous(breaks = asinh(breaks), labels = breaks) +
    scale_y_continuous(limits = c(-0.03, 1.9), expand = c(0, 0)) +
    scale_colour_manual(values = pal_class, guide = "none") + scale_fill_manual(values = pal_class, guide = "none") +
    scale_shape_manual(values = c(stem = 16, `prop root` = 2), name = NULL) +
    labs(x = xlab, y = "Height above water (m)") + theme_fig() +
    guides(shape = guide_legend(override.aes = list(size = 1.8, alpha = 1)))
}
pc <- prof_panel("CH4", "output/analysis/woody_height_model_curves.csv", c(0, 1, 10, 100, 1000),
                 expression("Woody-surface CH"[4]*" (nmol m"^-2*" s"^-1*")"))
pd <- prof_panel("CO2", "output/analysis/woody_height_model_CO2_curves.csv", c(-10, -1, 0, 1, 10),
                 expression("Woody-surface CO"[2]*" ("*mu*"mol m"^-2*" s"^-1*")")) + labs(y = NULL)

build <- function(dd, file, note) {
  pa <- rate_panel(dd, "CH4", "CH4_flux_status", c(0, 1, 10, 100, 1000), expression("CH"[4]*" (nmol m"^-2*" s"^-1*")")) +
    theme(legend.position = c(0.99, 0.02), legend.justification = c(1, 0), legend.background = element_rect(fill = "white", colour = NA)) +
    guides(fill = guide_legend(override.aes = list(size = 2.5)))
  pb <- rate_panel(dd, "CO2", "CO2_flux_status", c(-10, -1, 0, 1, 10), expression("CO"[2]*" ("*mu*"mol m"^-2*" s"^-1*")"), show_y = FALSE) +
    theme(legend.position = "none")
  pc2 <- pc + theme(legend.position = c(0.99, 0.98), legend.justification = c(1, 1), legend.background = element_rect(fill = "white", colour = NA))
  pd2 <- pd + theme(legend.position = "none")
  fig <- (pa + labs(tag = "a")) + (pb + labs(tag = "b")) + (pc2 + labs(tag = "c")) + (pd2 + labs(tag = "d")) +
    plot_layout(ncol = 2, widths = c(1, 1), heights = c(1, 0.9)) +
    plot_annotation(caption = note, theme = theme(plot.caption = element_text(size = 6.5, colour = "grey40", hjust = 0)))
  ggsave(paste0(file, ".png"), fig, width = 7.2, height = 5.4, dpi = 300, bg = "white")
  ggsave(paste0(file, ".pdf"), fig, width = 7.2, height = 5.4, device = cairo_pdf)
}
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
build(d %>% filter(site_type == "core site"), "output/figures/other/fig2_rates", "")
build(d, "output/figures/other/fig2_rates_with_context",
      "a, b: includes context sites (Rookery Bay with intact, Marco Island with ghost; crosses); SE-1 (scrub) not shown.")
