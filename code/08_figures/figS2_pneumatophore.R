# =============================================================================
# Figure S2 (SI Fig. S5): Soil CH4/CO2 flux vs pneumatophore density, by site
# (dry-season collars). OLS fit + 95% band per site; compact Pearson r and
# Spearman rho (with p) per panel.
# Output: pub_SI_pneumatophore_density.{png,pdf}
# =============================================================================
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())   # run from the project root
source("code/08_figures/publication_figures_common.R")
source("code/08_figures/palette.R")
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))

cat("\n--- Figure S2: Pneumatophore Density ---\n")

pn <- df %>%
  filter(!is.na(pneumatophore_density), component == "soil",
         CH4_flux_status == "valid") %>%
  mutate(disturbance = factor(class_labels[as.character(disturbance_level)],
                              levels = names(pal_class)),
         plot = droplevels(plot))

# Per-site correlation labels (same tests as ggpubr::stat_cor: Pearson and Spearman)
fmt_p <- function(p) sprintf("p = %.2g", p)
# Pearson r is computed on the plotted (asinh) scale for CH4, as stat_cor did in the original figure
cor_lab <- function(d, yvar, f = identity) {
  d %>% filter(!is.na(.data[[yvar]])) %>% mutate(.y = f(.data[[yvar]])) %>% group_by(plot) %>%
    group_modify(function(x, ...) {
      if (n_distinct(x$pneumatophore_density) < 3) return(tibble(label = "density 0 at all collars"))
      pe <- suppressWarnings(cor.test(x$pneumatophore_density, x$.y, method = "pearson"))
      sp <- suppressWarnings(cor.test(x$pneumatophore_density, x$.y, method = "spearman"))
      tibble(label = sprintf("r = %.2f, %s\nρ = %.2f, %s", pe$estimate, fmt_p(pe$p.value),
                             sp$estimate, fmt_p(sp$p.value)))
    }) %>% ungroup()
}

panel <- function(yvar, ylab, ytrans = NULL) {
  d <- pn %>% filter(!is.na(.data[[yvar]]))
  p <- ggplot(d, aes(pneumatophore_density, .data[[yvar]])) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60", linewidth = 0.3) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = col_ink,
                linewidth = 0.5, fill = "grey70", alpha = 0.3) +
    geom_point(aes(colour = disturbance), alpha = 0.8, size = 1.3, stroke = 0) +
    geom_text(data = cor_lab(d, yvar, if (is.null(ytrans)) identity else asinh), aes(x = -Inf, y = Inf, label = label),
              hjust = -0.1, vjust = 1.15, size = 2.1, lineheight = 0.95, colour = "grey20") +
    facet_wrap(~ plot, nrow = 1, scales = "free_y") +
    scale_x_continuous(breaks = c(0, 200, 400)) +
    (if (is.null(ytrans)) scale_y_continuous(expand = expansion(mult = c(0.05, 0.32))) else
       scale_y_continuous(trans = "asinh", breaks = c(0, 1, 10, 100, 1000), labels = c("0", "1", "10", "100", "1000"),
                          expand = expansion(mult = c(0.05, 0.32)))) +
    scale_colour_manual(values = pal_class, limits = names(pal_class), name = "forest class", drop = FALSE) +
    labs(x = expression(Pneumatophore~density~(m^{-2})), y = ylab) +
    theme_fig() + theme(panel.spacing.x = unit(6, "pt"))
  p
}

p_ch4 <- panel("CH4_best.flux", expression(Soil~CH[4]~(nmol~m^{-2}~s^{-1})), ytrans = "asinh") +
  theme(axis.title.x = element_blank())
p_co2 <- panel("CO2_best.flux", expression(Soil~CO[2]~(mu*mol~m^{-2}~s^{-1})))

combined <- (p_ch4 / p_co2) + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") & theme(legend.position = "bottom")

ggsave("output/figures/other/pub_SI_pneumatophore_density.png", combined,
       width = 7.2, height = 4.6, dpi = 300, bg = "white")
ggsave("output/figures/other/pub_SI_pneumatophore_density.pdf", combined,
       width = 7.2, height = 4.6, device = cairo_pdf)
cat("Saved: pub_SI_pneumatophore_density.pdf/.png\n")
source("code/08_figures/figure_cleanup.R")
