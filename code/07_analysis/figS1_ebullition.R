# =============================================================================
# Figure S1: Ebullition partitioning method and results
#   (a) 4 example traces showing partitioning process
#       - 2 with ebullition (CP40 P10, BL60 P03)
#       - 2 diffusive-only for contrast
#       Each trace: row 1 = raw + jumps; row 2 = de-ebulliated + goFlux fit
#   (b) Stacked bars of diffusive vs ebullitive per site x season
#       Uses full combined water flux dataset (not just partitioned traces)
# Output: pub_SI_ebullition_partition
# =============================================================================
source("code/07_analysis/publication_figures_common.R")
library(patchwork)

cat("\n--- Figure S1: Ebullition Partitioning ---\n")

# ---- Load data ----
pf <- read.csv("output/ebullition/partitioned_fluxes.csv", stringsAsFactors = FALSE)
all_traces <- readRDS("output/ebullition/all_traces.rds")

# ---- Select 4 traces ----
ebull_ids <- c("LGR2_2022-10-23_CP40_P10", "Picarro_2022-10-25_BL60_P03")

diff_candidates <- pf %>%
  filter(n_jumps == 0, grepl("wet", season),
         site %in% c("CP40", "BL60", "FLM30"),
         !is.na(diffusive_flux_nmol), duration_sec > 120) %>%
  arrange(desc(abs(diffusive_flux_nmol)))

diff_cp40 <- diff_candidates %>% filter(site == "CP40") %>% slice(1)
diff_other <- diff_candidates %>% filter(site != "CP40") %>% slice(1)
diff_ids <- c(diff_cp40$placement_id, diff_other$placement_id)

selected_ids <- c(ebull_ids, diff_ids)
selected_ids <- selected_ids[selected_ids %in% names(all_traces)]
cat("Selected traces:", paste(selected_ids, collapse = ", "), "\n")

EBULL_BUFFER_SEC <- 15

# ---- Build trace panels ----
make_trace_panels <- function(pid) {
  trace_obj <- all_traces[[pid]]
  tr <- trace_obj$trace
  summ_row <- pf %>% filter(placement_id == pid)
  if (is.null(tr) || nrow(tr) == 0) return(NULL)

  n_j <- summ_row$n_jumps
  has_ebull <- n_j > 0
  jumps <- tr %>% filter(is_jump)

  site <- summ_row$site
  diff_rate <- round(summ_row$diffusive_flux_nmol, 1)
  total_rate <- round(summ_row$total_flux_nmol, 1)
  model_used <- summ_row$diffusive_model

  # Ensure de-ebulliation columns exist
  if (!"cum_jump" %in% names(tr)) {
    tr$cum_jump <- cumsum(ifelse(tr$is_jump, tr$dCH4, 0))
    tr$CH4_deebull <- tr$CH4_ppm - tr$cum_jump
  }

  # Buffer exclusion around jumps
  jump_times <- tr$elapsed_sec[tr$is_jump]
  if (!"near_jump" %in% names(tr)) {
    tr$near_jump <- if (length(jump_times) > 0) {
      sapply(tr$elapsed_sec, function(t) any(abs(t - jump_times) <= EBULL_BUFFER_SEC))
    } else {
      rep(FALSE, nrow(tr))
    }
  }

  if (has_ebull) {
    ebull_pct <- round(summ_row$ebullitive_fraction * 100, 0)
    title1 <- paste0(site, ": raw trace, ", n_j, " ebullition events detected")
    title2 <- paste0("De-ebulliated + ", model_used, " fit: diffusive = ", diff_rate,
                     " | total = ", total_rate, " nmol/m\u00b2/s (",
                     ebull_pct, "% ebullitive)")
  } else {
    title1 <- paste0(site, ": raw trace, no ebullition")
    title2 <- paste0(model_used, " fit: ", diff_rate, " nmol/m\u00b2/s (100% diffusive)")
  }

  # ---- Panel 1: Raw trace with jumps and arrows ----
  p1 <- ggplot(tr, aes(x = elapsed_sec, y = CH4_ppm)) +
    geom_line(color = "grey30", linewidth = 0.5)

  if (has_ebull && nrow(jumps) > 0) {
    p1 <- p1 +
      geom_point(data = jumps, aes(x = elapsed_sec, y = CH4_ppm),
                 color = "#D2691E", size = 2, shape = 17) +
      geom_segment(data = jumps,
                   aes(x = elapsed_sec, xend = elapsed_sec,
                       y = CH4_ppm - dCH4, yend = CH4_ppm),
                   color = "#D2691E", linewidth = 0.5,
                   arrow = arrow(length = unit(0.06, "inches")))
  }

  p1 <- p1 +
    labs(title = title1, x = NULL, y = expression(CH[4]~(ppm))) +
    theme_pub(base_size = 8) +
    theme(plot.title = element_text(size = 7, face = "bold"),
          axis.text.x = element_blank(), axis.ticks.x = element_blank())

  # ---- Panel 2: De-ebulliated trace with goFlux fit ----
  # Points used for fitting (not near jumps)
  tr_fit <- tr %>% filter(!near_jump)
  # Points excluded (near jumps but not jumps themselves)
  tr_excluded <- tr %>% filter(near_jump & !is_jump)

  # Fit LM on de-ebulliated data
  lm_pred <- NULL
  hm_pred <- NULL
  tryCatch({
    lm_fit <- lm(CH4_deebull ~ elapsed_sec, data = tr_fit)
    lm_pred <- predict(lm_fit, newdata = data.frame(elapsed_sec = tr_fit$elapsed_sec))

    # HM fit (nonlinear saturation curve)
    C0 <- tr_fit$CH4_deebull[1]
    t_vals <- tr_fit$elapsed_sec - min(tr_fit$elapsed_sec)
    C_vals <- tr_fit$CH4_deebull

    hm_nls <- tryCatch(
      nls(C_vals ~ C0_est + a * (1 - exp(-k * t_vals)),
          start = list(C0_est = C0, a = max(C_vals) - C0 + 0.01, k = 0.01),
          control = nls.control(maxiter = 200, warnOnly = TRUE)),
      error = function(e) NULL
    )
    if (!is.null(hm_nls)) hm_pred <- predict(hm_nls)
  }, error = function(e) {})

  p2 <- ggplot(tr_fit, aes(x = elapsed_sec, y = CH4_deebull)) +
    geom_point(color = "grey50", size = 0.8, alpha = 0.6)

  # Show excluded points as open circles
  if (nrow(tr_excluded) > 0) {
    p2 <- p2 +
      geom_point(data = tr_excluded, aes(x = elapsed_sec, y = CH4_deebull),
                 color = "#D2691E", size = 0.8, alpha = 0.4, shape = 1)
  }

  if (!is.null(lm_pred)) {
    p2 <- p2 + geom_line(aes(y = lm_pred), color = "#4682B4",
                          linewidth = 0.7, linetype = "dashed")
  }
  if (!is.null(hm_pred)) {
    p2 <- p2 + geom_line(aes(y = hm_pred), color = "#1B4F72",
                          linewidth = 0.7)
  }

  fit_label <- if (!is.null(hm_pred) && !is.null(lm_pred)) {
    paste0("Light blue dashed: LM | Dark blue: HM | Selected: ", model_used)
  } else {
    paste0("Blue: LM fit | Selected: ", model_used)
  }

  p2 <- p2 +
    labs(title = title2, subtitle = fit_label,
         x = "Elapsed time (s)", y = expression(CH[4]~de-ebulliated~(ppm))) +
    theme_pub(base_size = 8) +
    theme(plot.title = element_text(size = 7, face = "bold"),
          plot.subtitle = element_text(size = 6, color = "grey50"))

  p1 / p2 + plot_layout(heights = c(1, 1))
}

all_panels <- lapply(selected_ids, make_trace_panels)
all_panels <- all_panels[!sapply(all_panels, is.null)]

fig_traces <- wrap_plots(all_panels, ncol = length(all_panels))

# ---- Panel (b): Stacked bar chart from water_fluxes_with_ebullition ----
# This file is the single source of truth: every water flux with its
# diffusive and ebullitive components already computed by the integration script.
df_water <- read.csv("output/ebullition/water_fluxes_with_ebullition.csv",
                     stringsAsFactors = FALSE) %>%
  filter(!is.na(CH4_best.flux)) %>%
  mutate(
    season_display = factor(ifelse(season == "wet", "Wet", "Dry"), levels = c("Wet", "Dry")),
    plot = factor(plot, levels = c("BL60", "CP40", "FLM30", "SE1", "SRS5", "SRS6"))
  ) %>%
  filter(!is.na(plot))

si_long <- df_water %>%
  tidyr::pivot_longer(cols = c(CH4_diffusive_flux, CH4_ebull_flux),
                      names_to = "flux_component", values_to = "flux_nmol") %>%
  mutate(flux_component = factor(
    ifelse(flux_component == "CH4_diffusive_flux", "Diffusive", "Ebullitive"),
    levels = c("Diffusive", "Ebullitive")))

flux_comp_colors <- c("Diffusive" = "#4682B4", "Ebullitive" = "#D2691E")

si_means <- si_long %>%
  group_by(plot, season_display, flux_component) %>%
  summarise(mean_flux = mean(flux_nmol, na.rm = TRUE), .groups = "drop")

si_totals <- df_water %>%
  group_by(plot, season_display) %>%
  summarise(mean_total = mean(CH4_best.flux, na.rm = TRUE),
            se_total = sd(CH4_best.flux, na.rm = TRUE) / sqrt(n()),
            n = n(), .groups = "drop")

fig_bars <- ggplot(si_means,
                   aes(x = season_display, y = mean_flux, fill = flux_component)) +
  geom_col(position = "stack", width = 0.6, color = "black", linewidth = 0.3) +
  geom_errorbar(data = si_totals,
                aes(x = season_display, y = mean_total,
                    ymin = mean_total - se_total, ymax = mean_total + se_total,
                    fill = NULL),
                width = 0.15, linewidth = 0.5, inherit.aes = FALSE) +
  geom_text(data = si_totals,
            aes(x = season_display, y = -Inf, label = paste0("n=", n), fill = NULL),
            vjust = 1.5, size = 2.8, inherit.aes = FALSE) +
  facet_wrap(~ plot, nrow = 1, scales = "free_y") +
  scale_fill_manual(values = flux_comp_colors, name = "Flux component") +
  scale_y_continuous(expand = expansion(mult = c(0.15, 0.1))) +
  labs(x = NULL, y = expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1})),
       tag = "(b)") +
  theme_pub(base_size = 10) +
  theme(legend.position = "bottom",
        panel.spacing = unit(0.5, "lines"))

# ---- Combine ----
all_panels[[1]] <- all_panels[[1]] + plot_annotation(tag_levels = list("(a)"))

fig_s1 <- fig_traces / fig_bars +
  plot_layout(heights = c(1.5, 0.8))

save_pub(fig_s1, "SI_ebullition_partition", width = 340, height = 280)
