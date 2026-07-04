# =============================================================================
# Figure 2: Component-specific flux rates (CH4 + CO2) — bootstrapped means
# Output: pub_component_by_plot_campaign_combined_condensed_boot
# =============================================================================
source("code/10_figures/publication_figures_common.R")

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

make_campaign_grid_condensed_boot <- function(data, gas = "CH4", tag_label = "(a)") {
  if (gas == "CH4") {
    flux_var <- "CH4_best.flux"
    status_var <- "CH4_flux_status"
    brk <- asinh_brk_pos
    x_lab <- expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1}))
  } else {
    flux_var <- "CO2_best.flux"
    status_var <- "CO2_flux_status"
    brk <- asinh_brk
    x_lab <- expression(CO[2]~Flux~(mu*mol~m^{-2}~s^{-1}))
  }

  d <- data %>%
    filter(.data[[status_var]] == "valid", !is.na(component), !is.na(campaign))

  d %>%
    ggplot(aes(x = .data[[flux_var]], y = component,
               fill = component, color = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.3) +
    geom_jitter(alpha = 0.45, size = 2, height = 0.12, stroke = 0) +
    geom_boxplot(alpha = 0.4, outlier.shape = NA, color = "black",
                 width = 0.5, linewidth = 0.3) +
    # Bootstrapped mean + 95% CI (horizontal error bar + diamond)
    stat_summary(
      fun.data = function(x) boot_mean_ci(x),
      geom = "pointrange", shape = 23,
      size = 0.4, linewidth = 0.5,
      fill = alpha("white", 0.6), color = alpha("black", 0.6), stroke = 0.5,
      fatten = 4
    ) +
    facet_nested(disturbance_level + plot ~ campaign,
                 nest_line = element_line(linewidth = 0.4),
                 scales = "free_y",
                 space = "free_y",
                 strip = strip_nested(size = "variable")) +
    scale_x_continuous(trans = "asinh", breaks = brk, labels = asinh_labels) +
    scale_fill_manual(values = component_colors_lc, name = "Component") +
    scale_color_manual(values = component_colors_lc, name = "Component") +
    labs(x = x_lab, y = NULL, tag = tag_label) +
    theme_pub(base_size = 8) +
    theme(
      legend.position    = "bottom",
      legend.key.size    = unit(3, "mm"),
      legend.text        = element_text(size = 7),
      legend.title       = element_text(size = 7, face = "bold"),
      legend.margin      = margin(0, 0, 0, 0),
      legend.box.margin  = margin(0, 0, 0, 0),
      axis.text.y        = element_text(size = 6, margin = margin(0, 1, 0, 0)),
      axis.text.x        = element_text(size = 7),
      axis.title.x       = element_text(size = 8, face = "bold"),
      strip.text         = element_text(size = 8, face = "bold",
                                        margin = margin(0.3, 0.5, 0.3, 0.5)),
      strip.text.y       = element_text(size = 8, face = "bold", angle = 0,
                                        margin = margin(0.3, 0.3, 0.3, 0.3)),
      strip.background   = element_blank(),
      panel.spacing.y    = unit(0.3, "mm"),
      panel.spacing.x    = unit(1.5, "mm"),
      plot.margin        = margin(2, 14, 2, 3)
    )
}

fig2c_ch4_boot <- make_campaign_grid_condensed_boot(df, "CH4", tag_label = "(a)")
save_pub(fig2c_ch4_boot, "component_by_plot_campaign_ch4_condensed_boot", width = 150, height = 120)

fig2c_co2_boot <- make_campaign_grid_condensed_boot(df, "CO2", tag_label = "(b)")
save_pub(fig2c_co2_boot, "component_by_plot_campaign_co2_condensed_boot", width = 150, height = 120)

# Combined CH4 + CO2 condensed boot
fig2c_combined_boot <- fig2c_ch4_boot / fig2c_co2_boot +
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(legend.position = "bottom"))
save_pub(fig2c_combined_boot, "component_by_plot_campaign_combined_condensed_boot",
         width = 170, height = 230)
source("code/10_figures/figure_cleanup.R")
