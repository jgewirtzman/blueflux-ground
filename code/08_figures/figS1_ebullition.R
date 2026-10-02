# =============================================================================
# Figure S1: Ebullition partitioning method and results (stage 04 outputs)
#   (a) 4 example floating-chamber placements: the two LGR placements with the
#       largest ebullitive flux and the two largest bubble-free LGR placements
#       at different sites. Top: CH4 over the placement with the detected
#       bubbles (goAquaFlux, goFlux fork); bottom: de-ebulliated CH4 with the
#       diffusive window and its linear fit.
#   (b) Mean diffusive + ebullitive water CH4 per site x season (analysis set).
# Inputs: output/flux/04_ebullition/{partition.csv, bubbles.csv, traces.csv.gz},
#         output/data_products/combined_gas_flux_dataset.csv
# Output: pub_SI_ebullition_partition
# =============================================================================
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())   # run from the project root
source("code/08_figures/publication_figures_common.R")
library(patchwork)

cat("\n--- Figure S1: Ebullition Partitioning ---\n")

part <- read.csv("output/flux/04_ebullition/partition.csv", stringsAsFactors = FALSE)
bub  <- read.csv("output/flux/04_ebullition/bubbles.csv", stringsAsFactors = FALSE)
tr   <- read.csv("output/flux/04_ebullition/traces.csv.gz", stringsAsFactors = FALSE) %>%
  mutate(CH4_deebulliated = coalesce(CH4_deebulliated, CH4_ppb))
ds   <- read.csv("output/data_products/combined_gas_flux_dataset.csv", stringsAsFactors = FALSE)
used <- ds$flux_id[ds$component == "water"]

# ---- (a) example placements ----
lgr <- part %>% filter(placement_id %in% used, grepl("^LGR", analyzer), !is.na(CH4_total))
ebull_ids <- lgr %>% filter(n_bubbles > 0) %>% arrange(desc(CH4_ebullitive)) %>% slice(1:2) %>% pull(placement_id)
diff_ids <- lgr %>% filter(n_bubbles == 0, duration_s >= 180) %>% arrange(desc(CH4_total)) %>%
  distinct(plot, .keep_all = TRUE) %>% slice(1:2) %>% pull(placement_id)
selected_ids <- c(ebull_ids, diff_ids)
cat("Selected placements:", paste(selected_ids, collapse = ", "), "\n")

make_trace_panels <- function(pid) {
  p <- part %>% filter(placement_id == pid)
  t <- tr %>% filter(placement_id == pid) %>% mutate(CH4_ppm = CH4_ppb / 1000, deeb_ppm = CH4_deebulliated / 1000)
  b <- bub %>% filter(placement_id == pid)
  title1 <- if (nrow(b)) sprintf("%s %s: %d bubble%s, %.0f min placement", p$plot, p$date, nrow(b), ifelse(nrow(b) > 1, "s", ""), p$duration_s / 60)
            else sprintf("%s %s: no bubbles, %.0f min placement", p$plot, p$date, p$duration_s / 60)
  title2 <- sprintf("diffusive %.1f + ebullitive %.1f = %.1f nmol m-2 s-1", p$CH4_diffusive, p$CH4_ebullitive, p$CH4_total)
  p1 <- ggplot(t, aes(Etime, CH4_ppm))
  if (nrow(b)) p1 <- p1 + geom_rect(data = b, aes(xmin = start, xmax = end), ymin = -Inf, ymax = Inf,
                                     fill = "#D2691E", alpha = 0.25, inherit.aes = FALSE)
  p1 <- p1 + geom_line(colour = "grey30", linewidth = 0.5) +
    labs(title = title1, x = NULL, y = expression(CH[4]~(ppm))) + theme_pub(base_size = 8) +
    theme(plot.title = element_text(size = 7, face = "bold"), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  # readings inside a detected bubble are not part of the de-ebulliated series
  in_bub <- if (nrow(b)) vapply(t$Etime, function(e) any(e >= b$start & e <= b$end), TRUE) else rep(FALSE, nrow(t))
  t <- t %>% mutate(deeb_ppm = ifelse(in_bub, NA, deeb_ppm))
  w <- t %>% filter(in_diffusive_window, !is.na(deeb_ppm))
  p2 <- ggplot(t, aes(Etime, deeb_ppm)) + geom_line(colour = "grey60", linewidth = 0.4, na.rm = TRUE) +
    geom_point(data = w, colour = "#4682B4", size = 0.5, alpha = 0.6)
  if (nrow(w) > 2) p2 <- p2 + geom_smooth(data = w, method = "lm", formula = y ~ x, se = FALSE, colour = "#1B4F72", linewidth = 0.7)
  p2 <- p2 + labs(title = title2, subtitle = "bubbles removed; blue = diffusive window",
                  x = "Elapsed time (s)", y = expression(CH[4]~de-ebulliated~(ppm))) + theme_pub(base_size = 8) +
    theme(plot.title = element_text(size = 7, face = "bold"), plot.subtitle = element_text(size = 6, colour = "grey50"))
  p1 / p2 + plot_layout(heights = c(1, 1))
}
all_panels <- lapply(selected_ids, make_trace_panels)
fig_traces <- wrap_plots(all_panels, ncol = length(all_panels))

# ---- (b) diffusive vs ebullitive per site x season ----
df_water <- ds %>% filter(component == "water", !is.na(CH4_best.flux)) %>%
  mutate(CH4_diffusive_flux = coalesce(CH4_diffusive_flux, CH4_best.flux), CH4_ebull_flux = coalesce(CH4_ebull_flux, 0),
         season_display = factor(ifelse(season == "wet", "Wet", "Dry"), levels = c("Wet", "Dry")),
         plot = factor(plot, levels = c("BL60", "CP40", "FLM30", "SE1", "SRS5", "SRS6"))) %>%
  filter(!is.na(plot))
si_means <- df_water %>%
  tidyr::pivot_longer(cols = c(CH4_diffusive_flux, CH4_ebull_flux), names_to = "flux_component", values_to = "flux_nmol") %>%
  mutate(flux_component = factor(ifelse(flux_component == "CH4_diffusive_flux", "Diffusive", "Ebullitive"),
                                 levels = c("Diffusive", "Ebullitive"))) %>%
  group_by(plot, season_display, flux_component) %>% summarise(mean_flux = mean(flux_nmol, na.rm = TRUE), .groups = "drop")
si_totals <- df_water %>% group_by(plot, season_display) %>%
  summarise(mean_total = mean(CH4_best.flux), se_total = sd(CH4_best.flux) / sqrt(n()), n = n(), .groups = "drop")
fig_bars <- ggplot(si_means, aes(x = season_display, y = mean_flux, fill = flux_component)) +
  geom_col(position = "stack", width = 0.6, color = "black", linewidth = 0.3) +
  geom_errorbar(data = si_totals, aes(x = season_display, ymin = mean_total - se_total, ymax = mean_total + se_total),
                width = 0.15, linewidth = 0.5, inherit.aes = FALSE) +
  geom_text(data = si_totals, aes(x = season_display, y = -Inf, label = paste0("n=", n)), vjust = 1.5, size = 2.8, inherit.aes = FALSE) +
  facet_wrap(~ plot, nrow = 1, scales = "free_y") +
  scale_fill_manual(values = c("Diffusive" = "#4682B4", "Ebullitive" = "#D2691E"), name = "Flux component") +
  scale_y_continuous(expand = expansion(mult = c(0.15, 0.1))) +
  labs(x = NULL, y = expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1})), tag = "(b)") +
  theme_pub(base_size = 10) + theme(legend.position = "bottom", panel.spacing = unit(0.5, "lines"))

all_panels[[1]] <- all_panels[[1]] + plot_annotation(tag_levels = list("(a)"))
fig_s1 <- fig_traces / fig_bars + plot_layout(heights = c(1.5, 0.8))
save_pub(fig_s1, "SI_ebullition_partition", width = 340, height = 280)
source("code/08_figures/figure_cleanup.R")
