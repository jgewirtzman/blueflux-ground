# =============================================================================
# Fig. S | Above-water exposure of woody surfaces (Methods M9/M12).
# Reproduces the exposure terms of 07_upscaling/02_upscale_methane.R with the
# shared functions in code/00_lib/exposure.R and code/00_lib/cwd_scaling.R:
#   (a) water depths w used per TLS site x campaign (depth_samples(): hourly
#       FCE LTER level at the plot mean floor height at SRS5/SRS6, thinned to
#       60 quantiles; our recorded stem/root/downed-wood depths at CP40/FLM30),
#       expressed relative to the TLS ground (scan-time water level subtracted).
#   (b) exposed share of surface: lowest stem bin (0-0.5 m), all stem surface
#       (TLS-area weighted), prop roots (TLS-area weighted, root_exposed()),
#       downed wood (cwd_exposed(); tidal sites: 1 at low tide, 0 at high tide,
#       shown as the flood-fraction-weighted mean; ghost sites: arc of a log).
#   (c) downed-wood geometry at the ghost sites: acos((h - r)/r)/pi for a log of
#       the median measured CWD diameter, at the site x campaign mean depth.
# Geometric exposure only (the stem CH4 profile weighting is not applied).
# Writes output/figures/other/si_exposure.{png,pdf} and si_exposure_values.csv.
# =============================================================================
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(patchwork)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
source("code/00_lib/exposure.R"); source("code/00_lib/cwd_scaling.R")
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))

tls_sites <- c("CP40", "FLM30", "SRS5", "SRS6"); campaigns <- c("Oct 2022", "Mar 2023")
site_class <- c(CP40 = "ghost", FLM30 = "ghost", SRS5 = "intact", SRS6 = "intact")
flux_raw <- read.csv("output/data_products/combined_gas_flux_dataset.csv") %>%
  filter(plot %in% tls_sites) %>%
  mutate(campaign = factor(month_year, levels = c("2022-10", "2023-03"), labels = campaigns)) %>%
  filter(!is.na(campaign))
tls_all <- read.csv(file.path(Sys.getenv("BLUEFLUX_TLS_DIR", "data/tls"), "all_sites_summary.csv"))
tls_stem <- tls_all %>% filter(segment_class %in% c("trunk", "branch")) %>%
  group_by(site, height_bin_num) %>% summarise(sa = sum(Total_surface_area_m2, na.rm = TRUE), .groups = "drop")
tls_root <- tls_all %>% filter(segment_class == "root") %>%
  group_by(site, height_bin_num) %>% summarise(sa = sum(Total_surface_area_m2, na.rm = TRUE), .groups = "drop")
ff <- read.csv("output/upscaling/flood_fraction.csv")
datum <- read.csv("output/upscaling/tls_datum_offset.csv")

depth_samples <- depth_samples_setup(flux_raw, ".")
cwd_exposed   <- cwd_exposure_setup(flux_raw)
d_obs <- median(flux_raw$diameter[flux_raw$component == "cwd"], na.rm = TRUE) / 100

grid <- expand.grid(site = tls_sites, campaign = campaigns, stringsAsFactors = FALSE)
w_all <- bind_rows(lapply(seq_len(nrow(grid)), function(i) {
  w <- depth_samples(grid$site[i], grid$campaign[i]); tibble(site = grid$site[i], campaign = grid$campaign[i], w_cm = w * 100)
}))
wexp <- function(sa_df, s, w) { d <- sa_df %>% filter(site == s)
  sum(d$sa * sapply(d$height_bin_num, function(z) exposed_frac(z, z + 0.5, w))) / sum(d$sa) }
vals <- bind_rows(lapply(seq_len(nrow(grid)), function(i) {
  s <- grid$site[i]; cp <- grid$campaign[i]; w <- depth_samples(s, cp)
  tidal <- s %in% c("SRS5", "SRS6")
  f_fl <- if (tidal) ff$frac_flooded[ff$site == s & ff$campaign == cp] else NA
  tibble(site = s, campaign = cp, n_depths = length(w),
         w_median_cm = median(w) * 100, w_min_cm = min(w) * 100, w_max_cm = max(w) * 100,
         w_scan_cm = datum$w_scan_cm[datum$site == s],
         stem_bin0 = exposed_frac(0, 0.5, w), stem_all = wexp(tls_stem, s, w), root = wexp(tls_root, s, w),
         cwd = if (tidal) (1 - f_fl) * cwd_exposed(s, cp, "low_tide") + f_fl * cwd_exposed(s, cp, "high_tide")
               else cwd_exposed(s, cp, "fixed"),
         cwd_rule = if (tidal) sprintf("tidal: 1 at low, 0 at high tide; flooded %.0f%% of hours", 100 * f_fl)
                    else "arc above mean depth",
         cwd_mean_depth_cm = if (tidal) NA else
           mean(flux_raw$water_depth[flux_raw$plot == s & as.character(flux_raw$campaign) == cp], na.rm = TRUE))
}))
write.csv(vals, "output/figures/other/si_exposure_values.csv", row.names = FALSE)
print(as.data.frame(vals %>% mutate(across(where(is.numeric), ~ round(.x, 3)))))
cat(sprintf("Median CWD diameter %.2f cm (n = %d)\n", d_obs * 100, sum(!is.na(flux_raw$diameter[flux_raw$component == "cwd"]))))

lab <- function(d) d %>% mutate(site = factor(site, tls_sites), campaign = factor(campaign, campaigns),
                                cls = factor(site_class[as.character(site)], names(pal_class)))
# (a) depth samples
pa <- ggplot(lab(w_all), aes(x = site, y = w_cm, colour = cls)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3, linetype = "dashed") +
  geom_boxplot(outlier.shape = NA, width = 0.55, linewidth = 0.3, fill = NA) +
  geom_point(position = position_jitter(width = 0.15, height = 0, seed = 1), size = 0.5, alpha = 0.6, stroke = 0) +
  facet_wrap(~ campaign) + scale_colour_manual(values = pal_class, name = NULL, drop = FALSE) +
  labs(x = NULL, y = "water depth above TLS ground (cm)") + theme_fig() + theme(legend.position = "none")
# (b) exposed share
surf_lev <- c(stem_bin0 = "stem, 0–0.5 m", stem_all = "stem, all heights", root = "prop root", cwd = "downed wood")
surf_col <- c("stem, 0–0.5 m" = "#B89B4F", "stem, all heights" = "#EAD7A6", "prop root" = pal_comp[["prop root"]],
              "downed wood" = pal_comp[["downed wood"]])
vb <- lab(vals) %>% pivot_longer(c(stem_bin0, stem_all, root, cwd), names_to = "surf", values_to = "f") %>%
  mutate(surf = factor(surf_lev[surf], surf_lev))
pb <- ggplot(vb, aes(x = site, y = f, fill = surf)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75, colour = "grey30", linewidth = 0.15) +
  facet_wrap(~ campaign) + scale_fill_manual(values = surf_col, name = NULL) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "share of surface above water") + theme_fig() +
  guides(fill = guide_legend(nrow = 1))
# (c) downed-wood arc geometry
r <- d_obs / 2
arc <- tibble(h = seq(0, 2 * r * 100 + 2, length.out = 300)) %>%
  mutate(f = ifelse(h / 100 <= 0, 1, ifelse(h / 100 >= 2 * r, 0, acos(pmin(1, pmax(-1, (h / 100 - r) / r))) / pi)))
pc_pts <- lab(vals %>% filter(site %in% c("CP40", "FLM30")))
pc <- ggplot(arc, aes(h, f)) + geom_line(colour = "grey30", linewidth = 0.4) +
  geom_point(data = pc_pts, aes(x = cwd_mean_depth_cm, y = cwd, shape = campaign), colour = pal_class[["ghost"]], size = 1.8) +
  geom_text(data = pc_pts, aes(x = cwd_mean_depth_cm, y = cwd, label = site), size = 2.2, hjust = 0, vjust = 0, nudge_x = 0.5, nudge_y = 0.02,
            colour = "grey25") +
  scale_shape_manual(values = c("Oct 2022" = 16, "Mar 2023" = 1), name = NULL) +
  scale_x_continuous(limits = c(0, 20.5)) + scale_y_continuous(limits = c(0, 1.05)) +
  labs(x = "mean water depth (cm)", y = "downed-wood surface above water") +
  theme_fig() + theme(legend.position = c(0.75, 0.85), legend.background = element_blank())

p <- (pa + pc + plot_layout(widths = c(2.2, 1))) / pb + plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 11))
ggsave("output/figures/other/si_exposure.png", p, width = 7.2, height = 5.2, dpi = 300, bg = "white")
ggsave("output/figures/other/si_exposure.pdf", p, width = 7.2, height = 5.2, device = cairo_pdf)
