# =============================================================================
# Fig. S13 | Porewater total alkalinity vs salinity (October 2025) with a
#   conservative-mixing reference line (Florida endmembers: S = 0, TA 3000 uM;
#   S = 35, TA 2400 uM; heuristic, not calibrated). FLM30 not sampled.
# Same data, filters and endmembers as the legacy panels pub_SI_ta_vs_dic /
# pub_SI_ta_vs_salinity in publication_figures_soilprofile.R (porewater only,
# surface water excluded). Values shown in mM (uM / 1000).
# Encoding: fill colour = forest class, shape = site (as Fig. 4).
# Writes output/figures/other/si_S12_carbonate.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(ggplot2); library(patchwork)})
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
source("code/00_lib/porewater_dic.R")
site_cls <- c(SRS5 = "intact", SRS6 = "intact", BL60 = "regenerating", CP40 = "ghost", FLM30 = "ghost")
site_shape <- c(SRS5 = 21, SRS6 = 24, BL60 = 22, CP40 = 23, FLM30 = 25)

pw <- read.csv("output/data_products/porewater_all_parameters.csv", check.names = FALSE) %>%
  filter(Depth_cm != "Surface", Site %in% c("SRS5", "SRS6", "BL60", "CP40")) %>%
  add_dic() %>%
  mutate(class = factor(site_cls[Site], names(pal_class)),
         Site = factor(Site, intersect(names(site_shape), unique(Site))),
         TA = Alkalinity_uM / 1000, DIC = DIC_uM / 1000)

# ---- Total alkalinity vs salinity, with the conservative-mixing reference ----
# (A TA vs DIC panel is not shown: DIC is calculated from pH and TA, so it is not independent.)
TA_fw <- 3000; TA_sw <- 2400                       # uM, freshwater and seawater end-members
mix_df <- data.frame(PSU = seq(0, 65, length.out = 100)) %>%
  mutate(TA = (TA_fw + (TA_sw - TA_fw) * PSU / 35) / 1000)
db <- pw %>% filter(!is.na(PSU), !is.na(TA))
fig <- ggplot(db, aes(PSU, TA)) +
  geom_ribbon(data = mix_df, aes(ymin = TA, ymax = Inf), fill = "grey96", inherit.aes = TRUE) +
  geom_line(data = mix_df, linetype = "dashed", colour = "grey45", linewidth = 0.4) +
  annotate("text", x = 64, y = mix_df$TA[100], label = "conservative mixing", hjust = 1, vjust = -0.7,
           size = 2.3, colour = "grey35") +
  annotate("text", x = 64, y = 37, label = "excess alkalinity\n(above mixing)", hjust = 1, vjust = 1,
           size = 2.3, colour = "grey45", lineheight = 0.9) +
  geom_point(aes(shape = Site, fill = class), colour = "white", size = 2.4, stroke = 0.35) +
  scale_fill_manual(values = pal_class, name = "forest class",
                    guide = guide_legend(override.aes = list(shape = 21, size = 2.4, colour = "white"))) +
  scale_shape_manual(values = site_shape, name = "site",
                     guide = guide_legend(override.aes = list(fill = "grey35", colour = "white", size = 2.2))) +
  scale_x_continuous(breaks = seq(0, 60, 20), limits = c(0, 65), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 38), expand = c(0, 0)) +
  labs(x = "Salinity (PSU)", y = "Total alkalinity (mM)") +
  theme_fig() + theme(legend.position = "right", legend.box = "vertical",
                      legend.text = element_text(size = 7), legend.title = element_text(size = 7))
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/si_S12_carbonate.png", fig, width = 4.6, height = 3.0, dpi = 300, bg = "white")
ggsave("output/figures/other/si_S12_carbonate.pdf", fig, width = 4.6, height = 3.0, device = cairo_pdf)
cat("S13 points:", nrow(db), "\n")
