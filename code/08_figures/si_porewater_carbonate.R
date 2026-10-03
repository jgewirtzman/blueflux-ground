# =============================================================================
# Fig. S12 | Porewater carbonate chemistry (October 2025).
#   (a) Total alkalinity vs calculated DIC (seacarb, pH + TA; add_dic()), with
#       the 1:1 line and per-site least-squares fits.
#   (b) Total alkalinity vs salinity, with class regressions (95% CI), Pearson r
#       and a conservative-mixing reference line (Florida endmembers: S = 0,
#       TA 3000 uM; S = 35, TA 2400 uM; heuristic, not calibrated).
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

# ---- (a) TA vs DIC ----
da <- pw %>% filter(!is.na(DIC), !is.na(TA))
lim_a <- range(c(da$DIC, da$TA))
pa <- ggplot(da, aes(DIC, TA)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  annotate("text", x = 4, y = 4, label = "1:1", hjust = -0.2, vjust = 1.4, size = 2.3, colour = "grey40") +
  geom_smooth(aes(group = Site, colour = class), method = "lm", formula = y ~ x, se = FALSE,
              linewidth = 0.5) +
  geom_point(aes(shape = Site, fill = class), colour = "white", size = 2.2, stroke = 0.35) +
  scale_colour_manual(values = pal_class, guide = "none") +
  scale_fill_manual(values = pal_class, guide = "none") +
  scale_shape_manual(values = site_shape, guide = "none") +
  scale_x_continuous(breaks = seq(0, 40, 10)) +
  labs(x = "DIC (mM)", y = "Total alkalinity (mM)") +
  theme_fig()

# ---- (b) TA vs salinity ----
TA_fw <- 3000; TA_sw <- 2400                       # uM, as legacy script
mix_df <- data.frame(PSU = seq(0, 60, length.out = 100)) %>%
  mutate(TA = (TA_fw + (TA_sw - TA_fw) * PSU / 35) / 1000)
db <- pw %>% filter(!is.na(PSU), !is.na(TA))
st <- db %>% group_by(class) %>%
  summarise(n = n(), r = cor(PSU, TA), .groups = "drop") %>%
  mutate(lab = sprintf("%s: r = %.2f, n = %d", class, r, n))
pb <- ggplot(db, aes(PSU, TA)) +
  geom_line(data = mix_df, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  annotate("text", x = 60, y = mix_df$TA[100], label = "conservative mixing", hjust = 1, vjust = -0.6,
           size = 2.3, colour = "grey40") +
  geom_smooth(aes(group = class, colour = class, fill = class), method = "lm", formula = y ~ x,
              se = TRUE, alpha = 0.15, linewidth = 0.6) +
  geom_point(aes(shape = Site, fill = class), colour = "white", size = 2.2, stroke = 0.35) +
  geom_text(data = st, aes(x = 1, y = 40 - 2.6 * (as.integer(class) - 1), label = lab, colour = class),
            inherit.aes = FALSE, hjust = 0, vjust = 1, size = 2.3, show.legend = FALSE) +
  scale_colour_manual(values = pal_class, name = "forest class") +
  scale_fill_manual(values = pal_class, guide = "none") +
  scale_shape_manual(values = site_shape, name = "site",
                     guide = guide_legend(override.aes = list(fill = "grey35", colour = "white", size = 2.2))) +
  guides(colour = guide_legend(override.aes = list(fill = NA, linewidth = 0.9))) +
  scale_x_continuous(breaks = seq(0, 60, 20)) +
  labs(x = "Salinity (PSU)", y = NULL) +
  theme_fig() + theme(axis.text.y = element_blank())

yl <- c(0, 40)
fig <- ((pa + coord_cartesian(xlim = c(0, 40), ylim = yl) + labs(tag = "a")) |
        (pb + coord_cartesian(xlim = c(0, 60), ylim = yl) + labs(tag = "b"))) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.box = "horizontal",
        legend.text = element_text(size = 7), legend.title = element_text(size = 7))
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/si_S12_carbonate.png", fig, width = 7.2, height = 3.4, dpi = 300, bg = "white")
ggsave("output/figures/other/si_S12_carbonate.pdf", fig, width = 7.2, height = 3.4, device = cairo_pdf)
cat("S12 points: TA-DIC", nrow(da), "; TA-salinity", nrow(db), "\n"); print(st)
