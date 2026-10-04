# =============================================================================
# Fig. S3 | Site greenness through time: (a) dry-season Landsat NDVI by year, 1995-2025
# (06_analysis/09_site_ndvi_history.R); (b) wet- vs dry-season Sentinel-2 NDVI after Irma,
# 2018-2025 (06_analysis/11_ndvi_seasonal_s2.R). Reads the saved panels (rds).
# Writes output/figures/other/si_ndvi.{png,pdf}.
# =============================================================================
suppressMessages({library(ggplot2); library(patchwork)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
pa <- readRDS("output/figures/other/si_ndvi_history.rds"); pb <- readRDS("output/figures/other/si_ndvi_seasonal.rds")
fig <- (pa + labs(tag = "a")) / (pb + labs(tag = "b")) + plot_layout(heights = c(1.75, 1)) &
  theme(plot.tag = element_text(face = "bold", size = 11))
ggsave("output/figures/other/si_ndvi.png", fig, width = 7.2, height = 6.8, dpi = 300, bg = "white")
ggsave("output/figures/other/si_ndvi.pdf", fig, width = 7.2, height = 6.8, device = cairo_pdf)
