# =============================================================================
# ED8 | Porewater CH4 and salinity by site, sampling round and depth.
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
rnd <- c("wet (Oct 2022)" = "Oct 22", "dry (Mar 2023)" = "Mar 23", "Oct 2025" = "Oct 25")
bub <- function(f, val, name, pal) {
  d <- read.csv(f) %>% filter(depth_cm >= 0) %>%
    mutate(round = factor(rnd[season], rnd), site = factor(site, c("SRS5", "SRS6", "BL60", "CP40", "FLM30")),
           v = .data[[val]])
  ggplot(d, aes(round, depth_cm)) +
    geom_point(aes(size = v, fill = v), shape = 21, colour = "grey30", stroke = 0.25) +
    facet_grid(~ site) + scale_y_reverse(breaks = c(0, 25, 50, 75, 100)) +
    scale_x_discrete(drop = FALSE) +
    scale_size_area(max_size = 5, name = name) +
    scale_fill_distiller(palette = pal, direction = 1, name = name, guide = "legend") +
    labs(x = NULL, y = "Depth (cm)") + theme_fig() + small +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.spacing.x = unit(4, "pt"),
          strip.text = element_text(hjust = 0.5, face = "bold"))
}
pa <- bub("output/data_products/porewater_ch4_by_round.csv", "CH4_mean", "CH4 (µM)", "YlOrBr")
pb <- bub("output/data_products/porewater_salinity_by_round.csv", "PSU_mean", "Salinity (PSU)", "Blues")
fig <- (pa + labs(tag = "a")) / (pb + labs(tag = "b")) &
  theme(legend.position = "right", legend.justification = "left", legend.key.size = unit(9, "pt"))
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/ed_porewater_rounds.png", fig, width = 7.2, height = 5.2, dpi = 300, bg = "white")
ggsave("output/figures/other/ed_porewater_rounds.pdf", fig, width = 7.2, height = 5.2, device = cairo_pdf)
