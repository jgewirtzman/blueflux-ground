# =============================================================================
# Fig S12 | Porewater CH4 and salinity by site, sampling round and depth (a, b) and
#   salinity against CH4 per site (c).
#   All three rounds (Oct 2022, Mar 2023, Oct 2025), porewater only (>= 0 cm),
#   five core sites including FLM30. Inputs written by
#   site_characterization_figures.R. The same samples (paired) form Fig. 4b.
# Writes output/figures/other/ed_porewater_rounds.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(ggplot2); library(patchwork)})
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
small <- theme(axis.text = element_text(size = 6.5), axis.title = element_text(size = 7.5),
               legend.text = element_text(size = 7), legend.title = element_text(size = 7.5),
               strip.text = element_text(size = 7))
site_lv <- c("SRS5", "SRS6", "BL60", "CP40", "FLM30")
site_cls <- c(SRS5 = "intact", SRS6 = "intact", BL60 = "regenerating", CP40 = "ghost", FLM30 = "ghost")
strip_cls <- ggh4x::strip_themed(text_x = lapply(pal_class[site_cls[site_lv]], function(cc)
  element_text(colour = cc, face = "bold", size = 7, hjust = 0.5)))
rnd <- c("wet (Oct 2022)" = "Oct 22", "dry (Mar 2023)" = "Mar 23", "Oct 2025" = "Oct 25")
bub <- function(f, val, name, pal) {
  d <- read.csv(f) %>% filter(depth_cm >= 0) %>%
    mutate(round = factor(rnd[season], rnd), site = factor(site, c("SRS5", "SRS6", "BL60", "CP40", "FLM30")),
           v = .data[[val]])
  ggplot(d, aes(round, depth_cm)) +
    geom_point(aes(size = v, fill = v), shape = 21, colour = "grey30", stroke = 0.25) +
    ggh4x::facet_grid2(~ site, strip = strip_cls) + scale_y_reverse(breaks = c(0, 25, 50, 75, 100)) +
    scale_x_discrete(drop = FALSE) +
    scale_size_area(max_size = 5, name = name) +
    scale_fill_distiller(palette = pal, direction = 1, name = name, guide = "legend") +
    labs(x = NULL, y = "Depth (cm)") + theme_fig() + small +
    theme(panel.spacing.x = unit(4, "pt"))
}
pa <- bub("output/data_products/porewater_ch4_by_round.csv", "CH4_mean", expression(CH[4]~(mu*M)), "YlOrBr")
pb <- bub("output/data_products/porewater_salinity_by_round.csv", "PSU_mean", "Salinity (PSU)", "Blues")
# (c) salinity against dissolved CH4 per site (site x round x depth means, porewater and surface water)
sc <- read.csv("output/data_products/porewater_salinity_ch4_merged.csv") %>%
  filter(site %in% site_lv, !is.na(PSU_mean), !is.na(CH4_mean)) %>%
  mutate(site = factor(site, site_lv), cls = factor(site_cls[as.character(site)], names(pal_class)),
         round = factor(rnd[season], rnd))
pc <- ggplot(sc, aes(PSU_mean, CH4_mean)) +
  geom_point(aes(colour = cls, shape = round), size = 1.7, alpha = 0.9) +
  ggh4x::facet_grid2(~ site, strip = strip_cls) +
  scale_colour_manual(values = pal_class, guide = "none") +
  scale_shape_manual(values = c(`Oct 22` = 16, `Mar 23` = 17, `Oct 25` = 15), name = "round") +
  scale_y_continuous(trans = "log1p", breaks = c(0, 1, 5, 10, 25, 50, 100)) +
  labs(x = "Salinity (PSU)", y = expression("Dissolved CH"[4]*" ("*mu*"M)")) + theme_fig() + small +
  theme(panel.spacing.x = unit(4, "pt"))
fig <- (pa + labs(tag = "a")) / (pb + labs(tag = "b")) &   # salinity vs CH4 by site is Fig. 4B
  theme(legend.position = "right", legend.justification = "left", legend.key.size = unit(9, "pt"))
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/ed_porewater_rounds.png", fig, width = 7.2, height = 5.0, dpi = 300, bg = "white")
ggsave("output/figures/other/ed_porewater_rounds.pdf", fig, width = 7.2, height = 5.0, device = cairo_pdf)
