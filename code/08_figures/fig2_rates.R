# =============================================================================
# Fig. 2 | Flux rates: what each surface emits.
#   (a) CH4 and (b) CO2 per m2 of surface by component (rows) and forest class
#       (side by side, class colours, one shared axis): individual measurements
#       and bootstrapped means with 95% CIs (5,000 resamples). Soil collars
#       include pneumatophores in the footprint. All campaigns; sites by class
#       (intact SRS5, SRS6, RB10; regenerating BL60; ghost CP40, FLM30, MI).
#   (c) Woody-surface (stem + prop-root) CH4 by height above the water, all
#       classes on one axis, with fitted profiles (06_analysis/
#       03_woody_height_model.R; live stem, flooded position, wet season;
#       typical rather than mean flux).
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
         comp = factor(comp_rows[component], rev(unique(comp_rows))))
boot <- function(x, R = 5000) { x <- x[is.finite(x)]; if (length(x) < 3) return(c(mean(x), NA, NA))
  b <- replicate(R, mean(sample(x, replace = TRUE))); c(mean(x), quantile(b, c(0.025, 0.975))) }
dodge <- c(intact = 0.25, regenerating = 0, ghost = -0.25)

rate_panel <- function(gas, status, breaks, xlab, show_y = TRUE) {
  x <- d %>% filter(.data[[status]] == "valid", !is.na(comp)) %>% mutate(v = .data[[paste0(gas, "_best.flux")]])
  s <- x %>% group_by(comp, class) %>% summarise(n = n(), m = boot(v)[1], lo = boot(v)[2], hi = boot(v)[3], .groups = "drop") %>%
    mutate(y = as.numeric(comp) + dodge[as.character(class)])
  x <- x %>% mutate(y = as.numeric(comp) + dodge[as.character(class)] + runif(n(), -0.07, 0.07))
  ggplot() +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
    geom_point(data = x, aes(asinh(v), y, colour = class), size = 0.7, alpha = 0.28, stroke = 0) +
    geom_errorbar(data = s, aes(xmin = asinh(lo), xmax = asinh(hi), y = y, colour = class), width = 0, linewidth = 0.6, orientation = "y") +
    geom_point(data = s, aes(asinh(m), y, fill = class), shape = 21, colour = "white", size = 2.2, stroke = 0.5) +
    scale_y_continuous(breaks = seq_along(levels(d$comp)), labels = if (show_y) levels(d$comp) else NULL,
                       expand = expansion(add = 0.45)) +
    scale_x_continuous(breaks = asinh(breaks), labels = breaks) +
    scale_colour_manual(values = pal_class, name = NULL) + scale_fill_manual(values = pal_class, name = NULL) +
    labs(x = xlab, y = NULL) + theme_fig() + theme(panel.grid.major.y = element_blank())
}
pa <- rate_panel("CH4", "CH4_flux_status", c(0, 1, 10, 100, 1000),
                 expression("CH"[4]*" (nmol m"^-2*" s"^-1*")"))
pb <- rate_panel("CO2", "CO2_flux_status", c(-10, -1, 0, 1, 10),
                 expression("CO"[2]*" ("*mu*"mol m"^-2*" s"^-1*")"), show_y = FALSE)

w <- d %>% filter(component %in% c("stem", "root"), plot %in% c("SRS5", "SRS6", "BL60", "CP40", "FLM30"),
                  month_year %in% c("2022-10", "2023-03"), !is.na(height_corrected), !is.na(CH4_best.flux)) %>%
  mutate(surface = factor(ifelse(component == "root", "prop root", "stem"), c("stem", "prop root")),
         h = pmax(height_corrected, 0) / 100)
cur <- read.csv("output/analysis/woody_height_model_curves.csv") %>% mutate(class = factor(class, names(pal_class)))
pc <- ggplot() +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -0.03, ymax = 0.05, fill = col_waterline) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_point(data = w, aes(asinh(CH4_best.flux), h, colour = class, shape = surface), size = 0.9, alpha = 0.35, stroke = 0.4) +
  geom_ribbon(data = cur, aes(y = h / 100, xmin = fit - 1.96 * se, xmax = fit + 1.96 * se, fill = class),
              alpha = 0.18, orientation = "y") +
  geom_path(data = cur, aes(fit, h / 100, colour = class), linewidth = 1) +
  scale_x_continuous(breaks = asinh(c(0, 1, 10, 100, 1000)), labels = c(0, 1, 10, 100, 1000)) +
  scale_y_continuous(limits = c(-0.03, 1.9), expand = c(0, 0)) +
  scale_colour_manual(values = pal_class, guide = "none") + scale_fill_manual(values = pal_class, guide = "none") +
  scale_shape_manual(values = c(stem = 16, `prop root` = 2), name = NULL) +
  labs(x = expression("Woody-surface CH"[4]*" (nmol m"^-2*" s"^-1*")"), y = "Height above water (m)") +
  theme_fig()

fig <- (pa + labs(tag = "a")) + (pb + labs(tag = "b")) + (pc + labs(tag = "c")) +
  plot_layout(widths = c(1, 0.8, 1), guides = "collect") & theme(legend.position = "bottom")
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/fig2_rates.png", fig, width = 7.2, height = 3.6, dpi = 300, bg = "white")
ggsave("output/figures/other/fig2_rates.pdf", fig, width = 7.2, height = 3.6, device = cairo_pdf)
