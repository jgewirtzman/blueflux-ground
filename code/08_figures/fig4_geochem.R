# =============================================================================
# Fig. 4 | A geochemical regime shift.
#   (a) PCA of porewater chemistry (October 2025 profiles, SRS5, SRS6, BL60,
#       CP40; surface water and 0-90 cm; same variables and treatment as
#       06_analysis/02_manuscript_results.R: DO correction, numeric variables
#       with <= 20% missing, CO2 and SD columns excluded, inorganic N with
#       below-detection = 0; centred and scaled). Points by class, class
#       ellipses (68%), loading arrows for the six strongest PC1/PC2 loadings.
#   (b) Porewater salinity vs dissolved CH4 by class, all three rounds
#       (Oct 2022, Mar 2023, Oct 2025; site x round x depth means;
#       site_characterization_figures.R), with class regressions of
#       log(1 + CH4) on salinity and Pearson r.
#   (c) Depth profiles (October 2025): dissolved CH4, sulfate, delta13C-CH4 and
#       NH4 by site, coloured by class.
#   (d) PLACEHOLDER for sediment metagenomes (Peccia lab).
# Writes output/figures/other/fig4_geochem.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(patchwork)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
cls3 <- c(healthy = "intact", regenerating = "regenerating", ghost = "ghost")

# ---- (a) PCA ----
pw <- read.csv("output/data_products/porewater_all_parameters.csv", check.names = FALSE) %>%
  mutate(ppmDO = ppmDO - 1.67,
         class = factor(cls3[case_when(Site %in% c("SRS5", "SRS6") ~ "healthy", Site == "BL60" ~ "regenerating", Site == "CP40" ~ "ghost")],
                        names(pal_class)),
         depth = case_when(Depth_cm == "Surface" ~ -5, TRUE ~ suppressWarnings(as.numeric(Depth_cm))))
drop <- c("Lat", "Long", "Depth_numeric", "SpCond", "TempC", "n_replicates", "Tds ppt", "%DO", "Br_ppm", "F_ppm", "depth")
num <- pw %>% select(where(is.numeric)) %>% select(-any_of(drop)) %>%
  select(-matches("_sd$|_sd_|CO2|_raw$|_bdl$"))
keep <- names(num)[colMeans(is.na(num)) <= 0.20]
dd <- pw %>% select(Site, Depth_cm, class, all_of(keep)) %>% drop_na()
pr <- prcomp(dd %>% select(all_of(keep)), center = TRUE, scale. = TRUE)
ve <- round(100 * summary(pr)$importance[2, 1:2], 1)
sc <- data.frame(dd %>% select(Site, Depth_cm, class), pr$x[, 1:2])
nice <- c(CH4_mean_uM = "CH4", PSU = "salinity", SO4_ppm = "SO4", NH4_N_mgL = "NH4", ORP = "ORP", ppmDO = "O2",
          Sulfide = "sulfide", pH = "pH", `Total Iron` = "Fe", DOC_mg_L = "DOC", Alkalinity_uM = "alkalinity",
          d13C_CH4_mean = "d13C-CH4", NO3_N_mgL = "NO3", PO4_P_ppm = "PO4", Cl_ppm = "Cl", NO2_N_ppm = "NO2", NO3_N_ppm = "NO3 (IC)")
ld <- data.frame(var = rownames(pr$rotation), pr$rotation[, 1:2]) %>%
  mutate(len = sqrt(PC1^2 + PC2^2), lab = ifelse(var %in% names(nice), nice[var], var)) %>% arrange(desc(len)) %>% head(6)
k <- 0.85 * max(abs(sc$PC1), abs(sc$PC2)) / max(ld$len)
pa <- ggplot(sc, aes(PC1, PC2)) +
  geom_hline(yintercept = 0, colour = "grey85") + geom_vline(xintercept = 0, colour = "grey85") +
  stat_ellipse(aes(colour = class, fill = class), geom = "polygon", alpha = 0.12, level = 0.68, linewidth = 0.4) +
  geom_segment(data = ld, aes(x = 0, y = 0, xend = PC1 * k, yend = PC2 * k), colour = "grey35", linewidth = 0.35,
               arrow = arrow(length = unit(3, "pt"))) +
  geom_text(data = ld, aes(PC1 * k * 1.12, PC2 * k * 1.12, label = lab), size = 2.3, colour = "grey20") +
  geom_point(aes(fill = class), shape = 21, colour = "white", size = 2.3, stroke = 0.3) +
  scale_colour_manual(values = pal_class, guide = "none") + scale_fill_manual(values = pal_class, name = "forest class") +
  labs(x = sprintf("PC1 (%.1f%%)", ve[1]), y = sprintf("PC2 (%.1f%%)", ve[2])) + theme_fig()

# ---- (b) salinity vs CH4 ----
sm <- read.csv("output/data_products/porewater_salinity_ch4_merged.csv") %>%
  mutate(class = factor(cls3[disturbance], names(pal_class)),
         round = factor(season, c("wet (Oct 2022)", "dry (Mar 2023)", "Oct 2025"), c("Oct 2022", "Mar 2023", "Oct 2025")))
st <- read.csv("output/data_products/porewater_salinity_ch4_correlations.csv") %>%
  mutate(class = factor(cls3[disturbance], names(pal_class)),
         lab = sprintf("%s  r = %.2f, p = %.2f", class, r, p_val))
pb <- ggplot(sm, aes(PSU_mean, log1p(CH4_mean), colour = class)) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, aes(fill = class), alpha = 0.12, linewidth = 0.7) +
  geom_point(aes(shape = round), size = 1.8, alpha = 0.85) +
  geom_text(data = st, aes(x = -Inf, y = Inf, label = lab, colour = class), inherit.aes = FALSE,
            hjust = -0.05, vjust = c(1.5, 3.0, 4.5)[as.integer(st$class)], size = 2.2) +
  scale_y_continuous(breaks = log1p(c(0, 1, 10, 100)), labels = c(0, 1, 10, 100)) +
  coord_cartesian(ylim = c(0, log1p(150))) +
  scale_colour_manual(values = pal_class, guide = "none") + scale_fill_manual(values = pal_class, guide = "none") +
  scale_shape_manual(values = c(`Oct 2022` = 16, `Mar 2023` = 17, `Oct 2025` = 15), name = "porewater round") +
  labs(x = "Porewater salinity (PSU)", y = expression("Dissolved CH"[4]*" ("*mu*"M)")) + theme_fig()

# ---- (c) depth profiles ----
vlab <- c(CH4 = "CH[4]~(mu*M)", SO4 = 'SO[4]^{"2-"}~(mM)', d13C = "delta^13*C-CH[4]~(per~mil)", NH4 = 'NH[4]^"+"~(mu*M)')
prof <- pw %>% filter(!is.na(depth)) %>%
  transmute(Site, class, depth, CH4 = CH4_mean_uM, SO4 = SO4_ppm / 96.06, d13C = d13C_CH4_mean, NH4 = NH4_N_mgL * 1000 / 14.007) %>%
  pivot_longer(-c(Site, class, depth), names_to = "var", values_to = "v") %>% filter(!is.na(v)) %>%
  mutate(var = factor(vlab[var], vlab))
pc <- ggplot(prof, aes(v, depth, colour = class, group = Site)) +
  geom_path(linewidth = 0.6) + geom_point(aes(shape = Site), size = 1.5) +
  scale_y_reverse(breaks = c(-5, 0, 15, 45, 90), labels = c("water", "0", "15", "45", "90")) +
  facet_wrap(~ var, nrow = 1, scales = "free_x", labeller = label_parsed) +
  scale_colour_manual(values = pal_class, guide = "none") +
  scale_shape_manual(values = c(SRS5 = 16, SRS6 = 1, BL60 = 17, CP40 = 15), name = "site") +
  labs(x = NULL, y = "Depth (cm)") + theme_fig() + theme(strip.text = element_text(hjust = 0.5), panel.spacing.x = unit(10, "pt"))

# ---- (d) metagenome placeholder ----
pd <- ggplot() + annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "grey96", colour = "grey70", linetype = 2) +
  annotate("text", x = 0.5, y = 0.5, size = 2.5, colour = "firebrick", lineheight = 0.95,
           label = "PLACEHOLDER\nsediment\nmetagenomes\n\nmcrA, mttB,\npmoA / mmoX\nby site x depth") +
  theme_void()

row1 <- ((pa + labs(tag = "a")) | (pb + labs(tag = "b"))) + plot_layout(guides = "collect")
row2 <- ((pc + labs(tag = "c")) | (pd + labs(tag = "d"))) + plot_layout(widths = c(4, 1), guides = "collect")
fig <- (row1 / row2) + plot_layout(heights = c(1, 0.85)) &
  theme(legend.position = "right", legend.justification = "left")
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/fig4_geochem.png", fig, width = 7.2, height = 5.6, dpi = 300, bg = "white")
ggsave("output/figures/other/fig4_geochem.pdf", fig, width = 7.2, height = 5.6, device = cairo_pdf)
cat(sprintf("PCA: %d variables, %d samples; PC1 %.1f%%, PC2 %.1f%%\n", length(keep), nrow(dd), ve[1], ve[2]))
