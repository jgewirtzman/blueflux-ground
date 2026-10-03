# =============================================================================
# Fig. 4 | A geochemical regime shift.
#   (a) PCA of porewater chemistry (October 2025, 0-90 cm at SRS5, SRS6, BL60,
#       CP40; same variables and treatment as 06_analysis/02_manuscript_results.R
#       plus DIC from pH + alkalinity: DO correction, numeric variables with
#       <= 20% missing, CO2 and SD columns excluded, inorganic N with
#       below-detection = 0; centred and scaled). Shape = site, fill = depth,
#       68% class ellipses, all loadings (CH4 highlighted).
#   (b) Porewater salinity vs dissolved CH4 by class, all three rounds
#       (Oct 2022, Mar 2023, Oct 2025; site x round x depth means incl. surface
#       water; site_characterization_figures.R), class regressions of
#       log(1 + CH4) on salinity with Pearson r.
#   (c) October 2025 depth profiles (0-90 cm): CH4, CO2, salinity, SO4, ORP, DO,
#       pH, DOC, TA + DIC, d13C-CH4, NH4, sulfide.
#   (d, e) Dissolved CH4 and salinity by site x round x depth (porewater, all
#       five core sites including FLM30).
#   (f) PLACEHOLDER for sediment metagenomes (Peccia lab).
# Writes output/figures/other/fig4_geochem.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(patchwork)})
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))   # plotmath unicode (per mil, mu)
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
source("code/00_lib/porewater_dic.R")
cls3 <- c(healthy = "intact", regenerating = "regenerating", ghost = "ghost")
site_cls <- c(SRS5 = "intact", SRS6 = "intact", BL60 = "regenerating", CP40 = "ghost", FLM30 = "ghost")
site_shape <- c(SRS5 = 21, SRS6 = 24, BL60 = 22, CP40 = 23)
small <- theme(axis.text = element_text(size = 6.5), axis.title = element_text(size = 7.5),
               legend.text = element_text(size = 7), legend.title = element_text(size = 7.5),
               strip.text = element_text(size = 7))

pw <- read.csv("output/data_products/porewater_all_parameters.csv", check.names = FALSE) %>%
  mutate(ppmDO = ppmDO - 1.67) %>% add_dic() %>%
  mutate(class = factor(site_cls[Site], names(pal_class)), Site = factor(Site, names(site_shape)),
         depth = case_when(Depth_cm == "Surface" ~ -5, TRUE ~ suppressWarnings(as.numeric(Depth_cm))))

# ---- (a) PCA ----
drop <- c("Lat", "Long", "Depth_numeric", "SpCond", "TempC", "n_replicates", "Tds ppt", "%DO", "Br_ppm", "F_ppm", "depth")
num <- pw %>% select(where(is.numeric)) %>% select(-any_of(drop)) %>%
  select(-matches("_sd$|_sd_|CO2|_raw$|_bdl$"))
keep <- names(num)[colMeans(is.na(num)) <= 0.20]
dd <- pw %>% select(Site, depth, class, all_of(keep)) %>% drop_na()
pr <- prcomp(dd %>% select(all_of(keep)), center = TRUE, scale. = TRUE)
ve <- round(100 * summary(pr)$importance[2, 1:2], 1)
sc <- data.frame(dd %>% select(Site, depth, class), pr$x[, 1:2]) %>%
  mutate(depth = factor(depth, c(0, 15, 45, 90)))
nice <- c(CH4_mean_uM = "CH4", PSU = "salinity", SO4_ppm = "SO4", NH4_N_mgL = "NH4", ORP = "ORP", ppmDO = "DO",
          Sulfide = "sulfide", pH = "pH", `Total Iron` = "Fe", DOC_mg_L = "DOC", Alkalinity_uM = "TA", DIC_uM = "DIC",
          d13C_CH4_mean = "d13C-CH4", NO3_N_mgL = "NO3", PO4_P_ppm = "PO4", Cl_ppm = "Cl", NO2_N_ppm = "NO2", NO3_N_ppm = "NO3 (IC)")
ld <- data.frame(var = rownames(pr$rotation), pr$rotation[, 1:2]) %>%
  mutate(lab = ifelse(var %in% names(nice), nice[var], var), ch4 = var == "CH4_mean_uM")
k <- 0.9 * max(abs(sc$PC1), abs(sc$PC2)) / max(sqrt(ld$PC1^2 + ld$PC2^2))
cen <- sc %>% group_by(class) %>%
  summarise(PC1 = mean(PC1), PC2 = mean(PC2) + ifelse(first(class) == "regenerating", -2.1, 1.5))
pal_depth <- c(`0` = "#F3E3C3", `15` = "#C9A46B", `45` = "#8C6235", `90` = "#4A2E16")
pa <- ggplot(sc, aes(PC1, PC2)) +
  geom_hline(yintercept = 0, colour = "grey88") + geom_vline(xintercept = 0, colour = "grey88") +
  stat_ellipse(aes(colour = class, fill = class), geom = "polygon", alpha = 0.10, level = 0.68, linewidth = 0.4) +
  geom_segment(data = ld, aes(x = 0, y = 0, xend = PC1 * k, yend = PC2 * k, colour = ch4), linewidth = 0.3,
               arrow = arrow(length = unit(2.5, "pt"))) +
  ggrepel::geom_text_repel(data = ld, aes(PC1 * k, PC2 * k, label = lab, colour = ch4,
                           fontface = ifelse(ch4, "bold", "plain")), size = 2.1, min.segment.length = Inf,
                           box.padding = 0.15, point.padding = 0, seed = 1) +
  geom_point(aes(shape = Site, fill = depth), colour = "grey15", size = 2.2, stroke = 0.35) +
  geom_text(data = cen, aes(label = class, colour = class), size = 2.6, fontface = "bold") +
  scale_colour_manual(values = c(pal_class, `TRUE` = "firebrick", `FALSE` = "grey40"), guide = "none") +
  scale_fill_manual(values = c(pal_class, pal_depth), breaks = names(pal_depth), name = "depth (cm)",
                    guide = guide_legend(override.aes = list(shape = 21))) +
  scale_shape_manual(values = site_shape, guide = "none") +
  labs(x = sprintf("PC1 (%.1f%%)", ve[1]), y = sprintf("PC2 (%.1f%%)", ve[2])) + theme_fig() + small

# ---- (b) salinity vs CH4 ----
sm <- read.csv("output/data_products/porewater_salinity_ch4_merged.csv") %>%
  mutate(class = factor(cls3[disturbance], names(pal_class)),
         round = factor(season, c("wet (Oct 2022)", "dry (Mar 2023)", "Oct 2025"), c("Oct 2022", "Mar 2023", "Oct 2025")))
st <- read.csv("output/data_products/porewater_salinity_ch4_correlations.csv") %>%
  mutate(class = factor(cls3[disturbance], names(pal_class)),
         lab = sprintf("%s  r = %.2f, p = %.2f", class, r, p_val))
pb <- ggplot(sm, aes(PSU_mean, log1p(CH4_mean), colour = class)) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, aes(fill = class), alpha = 0.12, linewidth = 0.7) +
  geom_point(aes(shape = round), size = 1.6, alpha = 0.85) +
  geom_text(data = st, aes(x = -Inf, y = Inf, label = lab, colour = class), inherit.aes = FALSE,
            hjust = -0.05, vjust = c(1.5, 3.0, 4.5)[as.integer(st$class)], size = 2.1) +
  coord_cartesian(ylim = c(0, log1p(150))) +
  scale_y_continuous(breaks = log1p(c(0, 1, 5, 10, 25, 50, 100)), labels = c(0, 1, 5, 10, 25, 50, 100)) +
  scale_colour_manual(values = pal_class, name = "forest class") + scale_fill_manual(values = pal_class, guide = "none") +
  scale_shape_manual(values = c(`Oct 2022` = 16, `Mar 2023` = 17, `Oct 2025` = 15), name = "porewater round") +
  labs(x = "Salinity (PSU)", y = expression("Dissolved CH"[4]*" ("*mu*"M)")) + theme_fig() + small

# ---- (c) depth profiles, 0-90 cm ----
vlab <- c(CH4 = "CH[4]~(mu*M)", CO2 = "CO[2]~(mM)", PSU = "Salinity~(PSU)", SO4 = 'SO[4]^{"2-"}~(mM)',
          ORP = "ORP~(mV)", DO = "DO~(mg~L^-1)", pH = "pH", DOC = "DOC~(mg~L^-1)", TA = "TA~and~DIC~(mM)",
          d13C = paste0("delta^13*C-CH[4]~('", intToUtf8(0x2030), "')"), NH4 = 'NH[4]^"+"~(mu*M)', Sulfide = "Sulfide~(mg~L^-1)")
prof <- pw %>% filter(depth >= 0) %>%
  transmute(Site, class, depth, CH4 = CH4_mean_uM, CO2 = CO2_mean_uM / 1000, PSU, SO4 = SO4_ppm / 96.06, ORP, DO = ppmDO, pH,
            DOC = DOC_mg_L, TA = Alkalinity_uM / 1000, DIC = DIC_uM / 1000, d13C = d13C_CH4_mean,
            NH4 = NH4_N_mgL * 1000 / 14.007, Sulfide) %>%
  pivot_longer(-c(Site, class, depth), names_to = "var", values_to = "v") %>% filter(!is.na(v)) %>%
  mutate(carb = ifelse(var == "DIC", "DIC", "measured"), var = ifelse(var == "DIC", "TA", var),
         var = factor(vlab[var], vlab), pfill = ifelse(carb == "DIC", "open", as.character(class)))
pc <- ggplot(prof, aes(v, depth, colour = class, group = interaction(Site, carb))) +
  geom_path(aes(linetype = carb), linewidth = 0.5) +
  geom_point(aes(shape = Site, fill = pfill), size = 1.3, stroke = 0.4) +
  scale_y_reverse(breaks = c(0, 15, 45, 90)) +
  facet_wrap(~ var, nrow = 2, scales = "free_x", labeller = label_parsed) +
  scale_colour_manual(values = pal_class, guide = "none") +
  scale_fill_manual(values = c(pal_class, open = "white"), guide = "none") +
  scale_shape_manual(values = site_shape, name = "site",
                     guide = guide_legend(override.aes = list(fill = "grey30", colour = "grey30"))) +
  scale_linetype_manual(values = c(measured = "solid", DIC = "22"), breaks = "DIC", labels = "DIC (open, dashed)",
                        name = NULL) +
  scale_x_continuous(n.breaks = 4) +
  labs(x = NULL, y = "Depth (cm)") + theme_fig() + small +
  theme(strip.text = element_text(hjust = 0.5), panel.spacing.x = unit(11, "pt"))

# ---- (d, e) CH4 and salinity by site x round x depth ----
rnd <- c("wet (Oct 2022)" = "Oct 22", "dry (Mar 2023)" = "Mar 23", "Oct 2025" = "Oct 25")
bub <- function(f, val, name, pal) {
  d <- read.csv(f) %>% filter(depth_cm >= 0) %>%
    mutate(round = factor(rnd[season], rnd), site = factor(site, c("SRS5", "SRS6", "BL60", "CP40", "FLM30")),
           v = .data[[val]])
  ggplot(d, aes(round, depth_cm)) +
    geom_point(aes(size = v, fill = v), shape = 21, colour = "grey30", stroke = 0.25) +
    facet_grid(~ site) + scale_y_reverse(breaks = c(0, 25, 50, 75, 100)) +
    scale_x_discrete(drop = FALSE) +
    scale_size_area(max_size = 4.5, name = name) +
    scale_fill_distiller(palette = pal, direction = 1, name = name, guide = "legend") +
    labs(x = NULL, y = "Depth (cm)") + theme_fig() + small +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.spacing.x = unit(3, "pt"),
          strip.text = element_text(hjust = 0.5, face = "bold"))
}
pd <- bub("output/data_products/porewater_ch4_by_round.csv", "CH4_mean", "CH4 (\u00b5M)", "YlOrBr")
pe <- bub("output/data_products/porewater_salinity_by_round.csv", "PSU_mean", "Salinity (PSU)", "Blues")

# ---- (f) metagenome placeholder ----
pf <- ggplot() + annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "grey96", colour = "grey70", linetype = 2) +
  annotate("text", x = 0.5, y = 0.5, size = 2.0, colour = "firebrick", lineheight = 0.95,
           label = "PLACEHOLDER\nsediment\nmetagenomes\n\nmcrA, mttB,\npmoA / mmoX\nby site x depth") +
  theme_void()

row1 <- ((pa + labs(tag = "a")) | (pb + labs(tag = "b"))) + plot_layout(widths = c(1.15, 1), guides = "collect")
row2 <- (pc + labs(tag = "c")) + plot_layout(guides = "collect")
row3 <- ((pd + labs(tag = "d")) | (pe + labs(tag = "e")) | (pf + labs(tag = "f"))) +
  plot_layout(widths = c(1, 1, 0.42), guides = "collect")
fig <- (row1 / row2 / row3) + plot_layout(heights = c(1, 1.15, 0.6)) &
  theme(legend.position = "right", legend.justification = "left", legend.key.size = unit(9, "pt"))
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/fig4_geochem.png", fig, width = 7.2, height = 8.6, dpi = 300, bg = "white")
ggsave("output/figures/other/fig4_geochem.pdf", fig, width = 7.2, height = 8.6, device = cairo_pdf)
cat(sprintf("PCA: %d variables, %d samples; PC1 %.1f%%, PC2 %.1f%%\n", length(keep), nrow(dd), ve[1], ve[2]))
