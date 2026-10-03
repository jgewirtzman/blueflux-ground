# =============================================================================
# Figure 2: Component-specific flux rates (CH4 + CO2) — bootstrapped means
# Output: pub_component_by_class_boot (main-text Fig 2: component x class)
#         pub_component_by_plot_campaign_combined_condensed_boot (Fig S4)
# =============================================================================
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())   # run from the project root
source("code/08_figures/publication_figures_common.R")

cat("\n--- Figure 2c-boot: Condensed with Bootstrapped Mean + CI ---\n")

boot_mean_ci <- function(x, R = 5000, conf = 0.95) {
  x <- x[!is.na(x) & is.finite(x)]
  n <- length(x)
  if (n < 3) return(data.frame(y = mean(x), ymin = NA_real_, ymax = NA_real_))
  set.seed(42)
  boot_means <- replicate(R, mean(sample(x, n, replace = TRUE)))
  alpha <- (1 - conf) / 2
  data.frame(
    y = mean(boot_means),
    ymin = unname(quantile(boot_means, alpha)),
    ymax = unname(quantile(boot_means, 1 - alpha))
  )
}

# Supplement figure (S3), house style (palette.R): component colours, class-coloured
# site/class strips, individual measurements as points, mean as a filled dot, and the
# bootstrap 95% CI drawn only where n > 3. Means and CIs are arithmetic, computed on the
# raw flux scale before the asinh axis transform (as in fig2_rates.R).
source("code/08_figures/palette.R")
comp_disp <- c(water = "water", soil = "soil", pneumatophore = "soil", root = "prop root", stem = "stem",
               cwd = "downed wood", leaves = "leaf")
class_disp <- c(class_labels, scrub = "scrub")
pal_class_s <- c(pal_class, scrub = "grey45")            # SE1 (scrub) has no class colour in palette.R
ci_if_n_gt3 <- function(x) {
  r <- boot_mean_ci(x)
  if (sum(is.finite(x)) <= 3) r$ymin <- r$ymax <- NA_real_
  r
}

make_campaign_grid_condensed_boot <- function(data, gas = "CH4", tag_label = "a") {
  if (gas == "CH4") {
    flux_var <- "CH4_best.flux"
    status_var <- "CH4_flux_status"
    brk <- asinh_brk_pos
    x_lab <- expression(CH[4]~flux~(nmol~m^{-2}~s^{-1}))
  } else {
    flux_var <- "CO2_best.flux"
    status_var <- "CO2_flux_status"
    brk <- asinh_brk
    x_lab <- expression(CO[2]~flux~(mu*mol~m^{-2}~s^{-1}))
  }

  d <- data %>%
    filter(.data[[status_var]] == "valid", !is.na(component), !is.na(campaign)) %>%
    mutate(comp = factor(comp_disp[as.character(component)], rev(unique(comp_disp))),
           class = factor(class_disp[as.character(disturbance_level)], unname(class_disp)))
  # arithmetic bootstrap mean and 95% CI on the raw flux scale (computed before the asinh axis transform)
  sm <- d %>% group_by(class, plot, campaign, comp) %>%
    summarise(n = sum(is.finite(.data[[flux_var]])), r = list(boot_mean_ci(.data[[flux_var]])), .groups = "drop") %>%
    tidyr::unnest(r)
  # strip colours: class layer first, then site layer (sites are ordered by class)
  site_cls <- d %>% distinct(plot, class) %>% arrange(plot)
  cls_present <- levels(droplevels(d$class))
  strip_cols <- c(pal_class_s[cls_present], pal_class_s[as.character(site_cls$class)])

  d %>%
    ggplot(aes(x = .data[[flux_var]], y = comp)) +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
    geom_point(aes(fill = comp), shape = 21, colour = alpha("grey30", 0.5), alpha = 0.55, size = 1, stroke = 0.12,
               position = position_jitter(width = 0, height = 0.15, seed = 1)) +
    geom_linerange(data = sm %>% filter(n > 3), aes(xmin = ymin, xmax = ymax, y = comp), inherit.aes = FALSE,
                   colour = col_ink, linewidth = 0.45) +
    geom_point(data = sm, aes(x = y, y = comp, fill = comp), inherit.aes = FALSE,
               shape = 21, colour = col_ink, size = 1.5, stroke = 0.35) +
    facet_nested(class + plot ~ campaign,
                 nest_line = element_line(linewidth = 0.3, colour = "grey60"),
                 scales = "free_y", space = "free_y", switch = NULL,
                 strip = strip_nested(size = "variable",
                                      text_y = elem_list_text(colour = unname(strip_cols), face = "bold",
                                                              angle = 0, hjust = 0, size = 7),
                                      by_layer_y = FALSE)) +
    scale_x_continuous(trans = "asinh", breaks = brk, labels = asinh_labels) +
    scale_colour_manual(values = pal_comp, guide = "none") +
    scale_fill_manual(values = pal_comp, guide = "none") +
    labs(x = x_lab, y = NULL, tag = tag_label) +
    theme_fig(base_size = 8) +
    theme(
      axis.text.y        = element_text(size = 6.5, margin = margin(0, 1, 0, 0)),
      axis.ticks.y       = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.border       = element_rect(fill = NA, colour = "grey75", linewidth = 0.3),
      axis.line          = element_blank(),
      strip.text.x       = element_text(size = 7.5, face = "bold", hjust = 0.5,
                                        margin = margin(1, 0.5, 1, 0.5)),
      strip.text.y       = element_text(angle = 0, hjust = 0),
      strip.text.y.right = element_text(angle = 0, hjust = 0),
      strip.background   = element_blank(),
      panel.spacing.y    = unit(0.6, "mm"),
      panel.spacing.x    = unit(2, "mm"),
      plot.margin        = margin(2, 2, 2, 2)
    )
}

fig2c_ch4_boot <- make_campaign_grid_condensed_boot(df, "CH4", tag_label = "a")
save_pub(fig2c_ch4_boot, "component_by_plot_campaign_ch4_condensed_boot", width = 183, height = 120)

fig2c_co2_boot <- make_campaign_grid_condensed_boot(df, "CO2", tag_label = "b")
save_pub(fig2c_co2_boot, "component_by_plot_campaign_co2_condensed_boot", width = 183, height = 120)

# Combined CH4 + CO2 condensed boot (supplement Fig S3), 7.2 in wide
fig2c_combined_boot <- fig2c_ch4_boot / fig2c_co2_boot
ggsave("output/figures/other/pub_component_by_plot_campaign_combined_condensed_boot.png", fig2c_combined_boot,
       width = 7.2, height = 9.2, dpi = 300, bg = "white")
ggsave("output/figures/other/pub_component_by_plot_campaign_combined_condensed_boot.pdf", fig2c_combined_boot,
       width = 7.2, height = 9.2, device = cairo_pdf)

# --- Main-text Fig 2: component x disturbance class ---------------------------
cat("\n--- Figure 2: Component x class with Bootstrapped Mean + CI ---\n")

class_comp_levels <- c("soil", "water", "root", "stem", "cwd", "leaves")
class_comp_labels <- c("Soil", "Water", "Root", "Stem", "CWD", "Leaves")
class_comp_colors <- c("Soil" = "#8B4513", "Water" = "#4682B4", "Root" = "#D2691E",
                       "Stem" = "#228B22", "CWD" = "#808080", "Leaves" = "#90EE90")

make_class_panel <- function(data, gas = "CH4", tag_label = "a") {
  if (gas == "CH4") {
    flux_var <- "CH4_best.flux"
    status_var <- "CH4_flux_status"
    brk <- asinh_brk_pos
    y_lab <- expression(CH[4]~(nmol~m^{-2}~s^{-1}))
  } else {
    flux_var <- "CO2_best.flux"
    status_var <- "CO2_flux_status"
    brk <- asinh_brk
    y_lab <- expression(CO[2]~(mu*mol~m^{-2}~s^{-1}))
  }

  data %>%
    filter(.data[[status_var]] == "valid",
           disturbance_level %in% c("healthy", "regenerating", "ghost")) %>%
    mutate(
      # Soil collars include pneumatophores within the footprint
      comp = factor(recode(as.character(component), pneumatophore = "soil"),
                    levels = class_comp_levels, labels = class_comp_labels),
      class = factor(tools::toTitleCase(as.character(disturbance_level)),
                     levels = c("Healthy", "Regenerating", "Ghost"))
    ) %>%
    filter(!is.na(comp)) %>%
    ggplot(aes(x = comp, y = .data[[flux_var]], fill = comp, color = comp)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.3) +
    geom_jitter(alpha = 0.35, size = 1.2, width = 0.12, stroke = 0) +
    geom_boxplot(alpha = 0.4, outlier.shape = NA, color = "black",
                 width = 0.55, linewidth = 0.3) +
    stat_summary(
      fun.data = function(x) boot_mean_ci(x),
      geom = "pointrange", shape = 23,
      size = 0.5, linewidth = 0.5,
      fill = "white", color = "black", stroke = 0.8,
      fatten = 4
    ) +
    facet_wrap(~ class, nrow = 1) +
    scale_x_discrete(drop = FALSE) +
    scale_y_continuous(trans = "asinh", breaks = brk, labels = asinh_labels) +
    scale_fill_manual(values = class_comp_colors, guide = "none") +
    scale_color_manual(values = class_comp_colors, guide = "none") +
    labs(x = NULL, y = y_lab, tag = tag_label) +
    theme_pub(base_size = 11) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
}

fig2_class <- make_class_panel(df, "CH4", "a") / make_class_panel(df, "CO2", "b")
save_pub(fig2_class, "component_by_class_boot", width = 230, height = 200)
source("code/08_figures/figure_cleanup.R")
