# =============================================================================
# Figure S1: Ebullition partitioning method and results (stage 04 outputs)
#   (a) 4 example floating-chamber placements: the largest ebullitive LGR
#       placements in two different site x campaigns and the largest clean
#       (no residual step) bubble-free placements at two other sites. Top: CH4 over the placement with the detected
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
# residual-step screen: largest 5-reading rise relative to the series' typical rise (robust z);
# placements with any step-like rise (z > 8) or a non-linear diffusive window (R2 < 0.98) are not used as bubble-free examples
step_z <- tr %>% filter(placement_id %in% lgr$placement_id) %>% group_by(placement_id) %>% arrange(Etime, .by_group = TRUE) %>%
  summarise(z = { d <- diff(CH4_ppb, lag = 5); max((d - median(d, na.rm = TRUE)) / (mad(d, na.rm = TRUE) + 1e-9), na.rm = TRUE) },
            r2 = { w <- in_diffusive_window & !is.na(CH4_ppb); if (sum(w) > 10) summary(lm(CH4_ppb[w] ~ Etime[w]))$r.squared else NA_real_ },
            .groups = "drop")
lgr <- lgr %>% left_join(step_z, by = "placement_id") %>% mutate(site_camp = paste(plot, substr(date, 1, 7)))
# examples favour long placements (>= 10 min): the largest ebullitive placement overall (Oct 2022 had no
# placements >= 10 min), the largest ebullitive placement >= 10 min from another site x campaign, and the
# largest clean bubble-free placements >= 10 min in dead and in intact forest
LONG <- 600
e1 <- lgr %>% filter(n_bubbles > 0) %>% arrange(desc(CH4_ebullitive)) %>% slice(1)
e2 <- lgr %>% filter(n_bubbles > 0, duration_s >= LONG, site_camp != e1$site_camp) %>% arrange(desc(CH4_ebullitive)) %>% slice(1)
ebull_ids <- c(e1$placement_id, e2$placement_id)
used_plots <- c(e1$plot, e2$plot)
clean <- lgr %>% filter(n_bubbles == 0, duration_s >= LONG, z <= 8, r2 >= 0.98, plot != used_plots[2]) %>% arrange(desc(CH4_total))
diff_ids <- c(clean %>% filter(plot %in% c("CP40", "FLM30", "BL60")) %>% slice(1) %>% pull(placement_id),   # one dead-forest
              clean %>% filter(plot %in% c("SRS5", "SRS6")) %>% slice(1) %>% pull(placement_id))             # one intact
# Examples chosen from the full placement gallery (author choice, 2026-10-03): a clean mid-trace bubble,
# a placement with two bubbles, and long clean bubble-free placements in ghost and intact forest.
selected_ids <- c("44857_CP40_Water_80", "45000_CP40_Water_124", "45003_FLM30_Water_154", "44996_SRS6_Water_86")
stopifnot(all(selected_ids %in% lgr$placement_id))
cat("Selected placements:", paste(selected_ids, collapse = ", "), "\n")

source("code/08_figures/palette.R")   # house palette + theme_fig()
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
col_diff <- pal_comp[["water"]]; col_ebul <- "#C2513A"
make_trace_panels <- function(pid, first = FALSE) {
  p <- part %>% filter(placement_id == pid)
  t <- tr %>% filter(placement_id == pid) %>% mutate(CH4_ppm = CH4_ppb / 1000, deeb_ppm = CH4_deebulliated / 1000)
  b <- bub %>% filter(placement_id == pid)
  hdr <- sprintf("%s, %s\n%s, %.0f min", p$plot, format(as.Date(p$date), "%d %b %Y"),
                 if (nrow(b)) sprintf("%d bubble%s", nrow(b), ifelse(nrow(b) > 1, "s", "")) else "no bubbles", p$duration_s / 60)
  flux_lab <- sprintf("nmol m\u207b\u00b2 s\u207b\u00b9\ndiffusive %.1f\nebullitive %.1f\ntotal %.1f", p$CH4_diffusive, p$CH4_ebullitive, p$CH4_total)
  t$hdr <- hdr
  p1 <- ggplot(t, aes(Etime, CH4_ppm))
  if (nrow(b)) p1 <- p1 + geom_rect(data = b, aes(xmin = start, xmax = end), ymin = -Inf, ymax = Inf,
                                     fill = col_ebul, alpha = 0.25, inherit.aes = FALSE)
  p1 <- p1 + geom_line(colour = "grey25", linewidth = 0.45) + facet_wrap(~ hdr) +
    labs(x = NULL, y = if (first) expression(CH[4]~(ppm)) else NULL) + theme_fig(base_size = 8) +
    theme(axis.text.x = element_blank(), strip.text = element_text(size = 7, hjust = 0, lineheight = 0.95),
          plot.margin = margin(2, 4, 0, 2))
  # readings inside a detected bubble are not part of the de-ebulliated series
  in_bub <- if (nrow(b)) vapply(t$Etime, function(e) any(e >= b$start & e <= b$end), TRUE) else rep(FALSE, nrow(t))
  t <- t %>% mutate(deeb_ppm = ifelse(in_bub, NA, deeb_ppm))
  w <- t %>% filter(in_diffusive_window, !is.na(deeb_ppm))
  p2 <- ggplot(t, aes(Etime, deeb_ppm)) + geom_line(colour = "grey60", linewidth = 0.4, na.rm = TRUE) +
    geom_point(data = w, colour = col_diff, size = 0.4, alpha = 0.6)
  if (nrow(w) > 2) p2 <- p2 + geom_smooth(data = w, method = "lm", formula = y ~ x, se = FALSE, colour = col_ink, linewidth = 0.5)
  p2 <- p2 + annotate("text", x = -Inf, y = Inf, label = flux_lab, hjust = -0.05, vjust = 1.1, size = 2.3,
                      lineheight = 0.9, colour = col_ink) +
    labs(x = "Elapsed time (s)", y = if (first) expression(CH[4]*", bubbles removed (ppm)") else NULL) +
    theme_fig(base_size = 8) + theme(plot.margin = margin(2, 4, 2, 2))
  p1 / p2
}
all_panels <- lapply(seq_along(selected_ids), function(i) make_trace_panels(selected_ids[i], first = i == 1))
all_panels[[1]][[1]] <- all_panels[[1]][[1]] + labs(tag = "b")
fig_traces <- wrap_plots(all_panels, ncol = length(all_panels))

# ---- (b) diffusive vs ebullitive per site x season ----
df_water <- ds %>% filter(component == "water", !is.na(CH4_best.flux)) %>%
  mutate(CH4_diffusive_flux = coalesce(CH4_diffusive_flux, CH4_best.flux), CH4_ebull_flux = coalesce(CH4_ebull_flux, 0),
         season_display = factor(ifelse(season == "wet", "Wet", "Dry"), levels = c("Wet", "Dry")),
         plot = factor(plot, levels = c("BL60", "CP40", "FLM30", "SE1", "SRS5", "SRS6"))) %>%
  filter(!is.na(plot)) %>%
  # upper bound on undetected ebullition (gradual or end-of-placement releases): the excess of the
  # whole-placement fit over diffusive + detected ebullitive flux (LGR placements; Picarro totals are two-point)
  left_join(part %>% transmute(flux_id = placement_id, whole = CH4_total_whole_placement, tot = CH4_total, analyzer), by = "flux_id") %>%
  mutate(CH4_missed = ifelse(grepl("^LGR", analyzer) & !is.na(whole), pmax(whole - tot, 0), 0))
missed <- df_water %>% group_by(plot, season_display) %>% summarise(Missed = mean(CH4_missed), .groups = "drop")
si_means <- df_water %>%
  tidyr::pivot_longer(cols = c(CH4_diffusive_flux, CH4_ebull_flux), names_to = "flux_component", values_to = "flux_nmol") %>%
  mutate(flux_component = factor(ifelse(flux_component == "CH4_diffusive_flux", "Diffusive", "Ebullitive"),
                                 levels = c("Diffusive", "Ebullitive"))) %>%
  group_by(plot, season_display, flux_component) %>% summarise(mean_flux = mean(flux_nmol, na.rm = TRUE), .groups = "drop")
si_totals <- df_water %>% group_by(plot, season_display) %>%
  summarise(mean_total = mean(CH4_best.flux), se_total = sd(CH4_best.flux) / sqrt(n()), n = n(), .groups = "drop")
# display only: sites ordered and labelled by forest class; seasons named with their campaign
site_cls <- c(SRS5 = "intact", SRS6 = "intact", BL60 = "regenerating", CP40 = "ghost", FLM30 = "ghost", SE1 = "scrub")
site_x <- function(x) factor(as.character(x), names(site_cls))
season_lab <- c(Wet = "wet season (Oct 2022)", Dry = "dry season (Mar 2023)")
disp <- function(d) d %>% mutate(site = site_x(plot), season_lab = factor(season_lab[as.character(season_display)], season_lab))
si_means <- disp(si_means); si_totals <- disp(si_totals)
pts_tot <- disp(df_water)
x_labs <- setNames(paste0(names(site_cls), "\n", site_cls), names(site_cls))
wide <- si_means %>% tidyr::pivot_wider(names_from = flux_component, values_from = mean_flux) %>%
  mutate(Ebullitive = coalesce(Ebullitive, 0)) %>% left_join(disp(missed), by = c("plot", "season_display", "site", "season_lab"))
seg <- wide %>%
  { bind_rows(transmute(., site, season_lab, flux_component = "Diffusive", lo = 0, hi = Diffusive),
              transmute(., site, season_lab, flux_component = "Ebullitive", lo = Diffusive, hi = Diffusive + Ebullitive),
              transmute(., site, season_lab, flux_component = "possible undetected", lo = Diffusive + Ebullitive,
                        hi = Diffusive + Ebullitive + Missed)) } %>%
  filter(hi > lo + 1e-6)
# ebullitive share: detected, and with the undetected upper bound
shr <- wide %>% mutate(det = 100 * Ebullitive / (Diffusive + Ebullitive), up = 100 * (Ebullitive + Missed) / (Diffusive + Ebullitive + Missed),
                       lab = ifelse(round(up) > round(det), sprintf("%.0f\u2013%.0f%%", det, up), sprintf("%.0f%%", det)),
                       ytop = Diffusive + Ebullitive + Missed)
# stacked segments built on the raw scale so the asinh axis does not distort them
# one panel per site x season, each with its own linear y axis so the ebullitive share of each bar is legible
xl <- shr %>% left_join(si_totals %>% select(site, season_lab, n), by = c("site", "season_lab")) %>%
  mutate(xlab = sprintf("%s\n%s\nn = %d\nebullitive\n%s", site, site_cls[as.character(site)], n, lab))
xmap <- setNames(xl$xlab, paste(xl$season_lab, xl$site))
key <- function(d) d %>% mutate(k = paste(season_lab, site))
fig_bars <- ggplot(key(seg), aes(x = k)) +
  geom_crossbar(aes(y = lo, ymin = lo, ymax = hi, fill = flux_component), width = 0.6, colour = "white", linewidth = 0.25,
                middle.linewidth = 0) +
  geom_point(data = key(pts_tot), aes(k, CH4_best.flux), inherit.aes = FALSE, shape = 21, fill = "white", colour = "grey35",
             size = 0.9, stroke = 0.3, alpha = 0.8, position = position_jitter(width = 0.12, height = 0, seed = 1)) +
  # SE of the total shown only where n > 3
  geom_errorbar(data = key(si_totals) %>% filter(n > 3), aes(x = k, ymin = mean_total - se_total, ymax = mean_total + se_total),
                width = 0.15, linewidth = 0.4, colour = col_ink, inherit.aes = FALSE) +
  ggh4x::facet_nested(~ season_lab + k, scales = "free", independent = "y",
                      strip = ggh4x::strip_nested(text_x = ggh4x::elem_list_text(size = c(7, 0)), by_layer_x = TRUE)) +
  scale_x_discrete(labels = xmap) +
  scale_fill_manual(values = c("Diffusive" = col_diff, "Ebullitive" = col_ebul, "possible undetected" = "#E9B3A6"),
                    labels = c(Diffusive = "diffusive", Ebullitive = "ebullitive (detected)", `possible undetected` = "possible undetected ebullition (upper bound)"), name = NULL) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
  labs(x = NULL, y = expression(CH[4]~flux~(nmol~m^{-2}~s^{-1})*"; y axes differ by panel"), tag = "a") +
  theme_fig(base_size = 8) +
  theme(legend.position = "bottom", legend.text = element_text(size = 7), legend.key.size = unit(7, "pt"),
        panel.grid.major.x = element_blank(), axis.ticks.x = element_blank(), panel.spacing.x = unit(4, "pt"),
        axis.text.x = element_text(lineheight = 0.9, size = 6.3), axis.text.y = element_text(size = 6.3),
        strip.text = element_text(hjust = 0.5), strip.text.y = element_blank())

fig_s1 <- fig_bars / fig_traces + plot_layout(heights = c(1.15, 1.7))
ggsave("output/figures/other/pub_SI_ebullition_partition.png", fig_s1, width = 7.2, height = 7, dpi = 300, bg = "white")
ggsave("output/figures/other/pub_SI_ebullition_partition.pdf", fig_s1, width = 7.2, height = 7, device = cairo_pdf)
cat("Saved: pub_SI_ebullition_partition.pdf/.png\n")
source("code/08_figures/figure_cleanup.R")
