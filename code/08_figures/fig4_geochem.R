# =============================================================================
# Fig. 4 | A geochemical regime shift.
#   (a) PCA of porewater chemistry (October 2025, 0-90 cm at SRS5, SRS6, BL60,
#       CP40; same variables and treatment as 06_analysis/02_manuscript_results.R
#       plus DIC from pH + alkalinity: DO correction, numeric variables with
#       <= 20% missing, CO2 and SD columns excluded, inorganic N with
#       below-detection = 0; centred and scaled). 68% class ellipses, all
#       loadings (CH4 highlighted). Descriptive only (one core per site).
#   (b) Porewater salinity vs dissolved CH4 by class, all three rounds
#       (Oct 2022, Mar 2023, Oct 2025; site x round x depth means incl. surface
#       water; site_characterization_figures.R), class regressions of
#       log(1 + CH4) on salinity with Pearson r.
#   (c) October 2025 depth profiles (0-90 cm), 14 analytes in two rows:
#       salinity, electron acceptors and redox; carbon.
#   (d-f) MOCKUP of the sediment metagenome results (hypothetical values,
#       clearly marked; replaced when the Peccia-lab data arrive).
# Encoding throughout: colour = forest class, shape = site.
# Per-round CH4 / salinity bubbles moved to ed_porewater_rounds.R (ED8).
# Writes output/figures/other/fig4_geochem.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(patchwork)})
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))   # plotmath unicode (per mil, mu)
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
source("code/00_lib/porewater_dic.R")
cls3 <- c(healthy = "intact", regenerating = "regenerating", ghost = "ghost")
site_cls <- c(SRS5 = "intact", SRS6 = "intact", BL60 = "regenerating", CP40 = "ghost", FLM30 = "ghost")
site_shape <- c(SRS5 = 21, SRS6 = 24, BL60 = 22, CP40 = 23, FLM30 = 25)
small <- theme(axis.text = element_text(size = 6), axis.title = element_text(size = 7),
               legend.text = element_text(size = 6.5), legend.title = element_text(size = 7),
               strip.text = element_text(size = 6.5), plot.title = element_text(size = 7, face = "bold"))
mock_lab <- function(t = "MOCKUP: hypothetical values") labs(title = t)
mock_theme <- theme(plot.title = element_text(colour = "firebrick", face = "italic", size = 6.5, hjust = 0))

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
sc <- data.frame(dd %>% select(Site, depth, class), pr$x[, 1:2])
nice <- c(CH4_mean_uM = "CH4", PSU = "salinity", SO4_ppm = "SO4", NH4_N_mgL = "NH4", ORP = "ORP", ppmDO = "DO",
          Sulfide = "sulfide", pH = "pH", `Total Iron` = "Fe", DOC_mg_L = "DOC", Alkalinity_uM = "TA", DIC_uM = "DIC",
          d13C_CH4_mean = "d13C-CH4", NO3_N_mgL = "NO3", PO4_P_ppm = "PO4", Cl_ppm = "Cl", NO2_N_ppm = "NO2", NO3_N_ppm = "NO3 (IC)")
ld <- data.frame(var = rownames(pr$rotation), pr$rotation[, 1:2]) %>%
  mutate(lab = ifelse(var %in% names(nice), nice[var], var), ch4 = var == "CH4_mean_uM")
k <- 0.9 * max(abs(sc$PC1), abs(sc$PC2)) / max(sqrt(ld$PC1^2 + ld$PC2^2))
cen <- sc %>% group_by(class) %>%
  summarise(PC1 = mean(PC1), PC2 = mean(PC2) + ifelse(first(class) == "regenerating", -2.1, 1.5))
pa <- ggplot(sc, aes(PC1, PC2)) +
  geom_hline(yintercept = 0, colour = "grey88") + geom_vline(xintercept = 0, colour = "grey88") +
  stat_ellipse(aes(colour = class, fill = class), geom = "polygon", alpha = 0.10, level = 0.68, linewidth = 0.4) +
  geom_segment(data = ld, aes(x = 0, y = 0, xend = PC1 * k, yend = PC2 * k, colour = ch4), linewidth = 0.3,
               arrow = arrow(length = unit(2.5, "pt"))) +
  ggrepel::geom_text_repel(data = ld, aes(PC1 * k, PC2 * k, label = lab, colour = ch4,
                           fontface = ifelse(ch4, "bold", "plain")), size = 2.0, min.segment.length = Inf,
                           box.padding = 0.15, point.padding = 0, seed = 1) +
  geom_point(aes(shape = Site, fill = class), colour = "white", size = 2.1, stroke = 0.35) +
  geom_text(data = cen, aes(label = class, colour = class), size = 2.5, fontface = "bold") +
  scale_colour_manual(values = c(pal_class, `TRUE` = "firebrick", `FALSE` = "grey40"), guide = "none") +
  scale_fill_manual(values = pal_class, guide = "none") +
  scale_shape_manual(values = site_shape, guide = "none") +
  labs(x = sprintf("PC1 (%.1f%%)", ve[1]), y = sprintf("PC2 (%.1f%%)", ve[2])) + theme_fig() + small

# ---- (b) salinity vs CH4 ----
sm <- read.csv("output/data_products/porewater_salinity_ch4_merged.csv") %>%
  mutate(class = factor(cls3[disturbance], names(pal_class)), site = factor(site, names(site_shape)))
st <- read.csv("output/data_products/porewater_salinity_ch4_correlations.csv") %>%
  mutate(class = factor(cls3[disturbance], names(pal_class)),
         lab = sprintf("%s  r = %.2f, p = %.2f", class, r, p_val))
pb <- ggplot(sm, aes(PSU_mean, log1p(CH4_mean), colour = class)) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, aes(fill = class), alpha = 0.12, linewidth = 0.7) +
  geom_point(aes(shape = site, fill = class), colour = "white", size = 1.9, stroke = 0.3) +
  geom_text(data = st, aes(x = -Inf, y = Inf, label = lab, colour = class), inherit.aes = FALSE,
            hjust = -0.05, vjust = c(1.5, 3.0, 4.5)[as.integer(st$class)], size = 2.0) +
  coord_cartesian(ylim = c(0, log1p(150))) +
  scale_y_continuous(breaks = log1p(c(0, 1, 5, 10, 25, 50, 100)), labels = c(0, 1, 5, 10, 25, 50, 100)) +
  scale_colour_manual(values = pal_class, name = "forest class") +
  scale_fill_manual(values = pal_class, guide = "none") +
  scale_shape_manual(values = site_shape, name = "site",
                     guide = guide_legend(override.aes = list(fill = "grey35", colour = "white"))) +
  guides(colour = guide_legend(override.aes = list(fill = NA, linewidth = 0.9, shape = NA))) +
  labs(x = "Salinity (PSU)", y = expression("Dissolved CH"[4]*" ("*mu*"M)")) + theme_fig() + small

# ---- (c) depth profiles, 0-90 cm, 14 analytes ----
vlab <- c(PSU = "Salinity~(PSU)", SO4 = 'SO[4]^{"2-"}~(mM)', Sulfide = "H[2]*S~(mg~L^-1)", Fe = "Fe~(mg~L^-1)",
          DO = "O[2]~(mg~L^-1)", ORP = "ORP~(mV)", NH4 = 'NH[4]^"+"~(mu*M)',
          CH4 = "CH[4]~(mu*M)", d13C = paste0("delta^13*C-CH[4]~('", intToUtf8(0x2030), "')"), CO2 = "CO[2]~(mM)",
          DIC = "DIC~(mM)", TA = "TA~(mM)", DOC = "DOC~(mg~L^-1)", pH = "pH")
prof <- pw %>% filter(depth >= 0) %>%
  transmute(Site, class, depth, PSU, SO4 = SO4_ppm / 96.06, Sulfide, Fe = `Total Iron`, DO = ppmDO, ORP,
            NH4 = NH4_N_mgL * 1000 / 14.007, CH4 = CH4_mean_uM, d13C = d13C_CH4_mean, CO2 = CO2_mean_uM / 1000,
            DIC = DIC_uM / 1000, TA = Alkalinity_uM / 1000, DOC = DOC_mg_L, pH) %>%
  pivot_longer(-c(Site, class, depth), names_to = "var", values_to = "v") %>% filter(!is.na(v)) %>%
  mutate(var = factor(vlab[var], vlab))
prof_row <- function(vars, title, ylab = TRUE) {
  ggplot(prof %>% filter(var %in% vlab[vars]), aes(v, depth, colour = class, group = Site)) +
    geom_path(linewidth = 0.45) +
    geom_point(aes(shape = Site, fill = class), colour = "white", size = 1.25, stroke = 0.25) +
    scale_y_reverse(breaks = c(0, 15, 45, 90)) +
    scale_x_continuous(breaks = scales::breaks_extended(3)) +
    facet_wrap(~ var, nrow = 1, scales = "free_x", labeller = label_parsed) +
    scale_colour_manual(values = pal_class, guide = "none") + scale_fill_manual(values = pal_class, guide = "none") +
    scale_shape_manual(values = site_shape, guide = "none") +
    labs(x = NULL, y = "Depth (cm)", title = title) + theme_fig() + small +
    theme(strip.text = element_text(hjust = 0.5), panel.spacing.x = unit(8, "pt"),
          plot.title = element_text(hjust = 0, margin = margin(0, 0, 1, 0)))
}
pc1 <- prof_row(c("PSU", "SO4", "Sulfide", "Fe", "DO", "ORP", "NH4"), "Salinity, electron acceptors and redox")
pc2 <- prof_row(c("CH4", "d13C", "CO2", "DIC", "TA", "DOC", "pH"), "Carbon")

# ---- (d-f) metagenome MOCKUP (hypothetical values) ----
# Expected pattern under the working hypothesis: sulfate reducers abundant at all
# sites; methanogens (mcrA) highest in ghost forest and dominated by methylotrophic
# lineages (mttB, Methanosarcinaceae / Methanococcoides) that use non-competitive
# substrates (methylamines, from osmolyte breakdown), consistent with high NH4.
set.seed(42)
lvl <- c(intact = 0, regenerating = 0.8, ghost = 1.6)
mg <- expand.grid(Site = c("SRS5", "SRS6", "BL60", "CP40"), depth = c(0, 15, 45, 90), stringsAsFactors = FALSE) %>%
  mutate(class = factor(site_cls[Site], names(pal_class)), Site = factor(Site, names(site_shape)), L = lvl[as.character(class)])
genes <- bind_rows(
  mg %>% mutate(gene = "dsrA", v = 8.2 + 0.1 * L + rnorm(n(), 0, 0.15)),
  mg %>% mutate(gene = "mcrA", v = 5.6 + L + rnorm(n(), 0, 0.25)),
  mg %>% mutate(gene = "mttB", v = 4.2 + 1.5 * L + rnorm(n(), 0, 0.25)),
  mg %>% mutate(gene = "pmoA", v = 6.0 - 0.3 * L - depth / 60 + rnorm(n(), 0, 0.2))) %>%
  mutate(gene = factor(gene, c("dsrA", "mcrA", "mttB", "pmoA")))
gene_role <- c(dsrA = "SO4 red.", mcrA = "CH4 prod.", mttB = "methylotr.", pmoA = "CH4 ox.")
pd <- ggplot(genes, aes(gene, v, colour = class)) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.6, linewidth = 0.25, position = position_dodge(0.75)) +
  geom_point(aes(shape = Site, fill = class), colour = "white", size = 1.3, stroke = 0.25,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75, seed = 1)) +
  scale_x_discrete(labels = paste0(levels(genes$gene), "\n", gene_role)) +
  scale_y_continuous(breaks = 4:9, labels = parse(text = paste0("10^", 4:9))) +
  scale_colour_manual(values = pal_class, guide = "none") + scale_fill_manual(values = pal_class, guide = "none") +
  scale_shape_manual(values = site_shape, guide = "none") +
  labs(x = NULL, y = expression("Gene copies g"^-1*" sediment")) +
  mock_lab("Sediment metagenomes. MOCKUP: hypothetical values") + theme_fig() + small + mock_theme

pal_path <- c(hydrogenotrophic = "#5B8DB8", acetoclastic = "#C9B27C", methylotrophic = "#9C4F86")
comp <- data.frame(Site = rep(c("SRS5", "SRS6", "BL60", "CP40"), each = 3), path = rep(names(pal_path), 4),
                   share = c(62, 28, 10, 58, 30, 12, 50, 27, 23, 22, 13, 65)) %>%
  mutate(Site = factor(Site, names(site_shape)[1:4]), path = factor(path, names(pal_path)))
pe <- ggplot(comp, aes(Site, share, fill = path)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.3) +
  scale_fill_manual(values = pal_path, name = "methanogen\npathway") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 108), breaks = c(0, 50, 100)) +
  labs(x = NULL, y = "Share of mcrA reads (%)") + mock_lab() + theme_fig() + small + mock_theme +
  theme(axis.text.x = element_text(colour = pal_class[site_cls[levels(comp$Site)]], face = "bold"))

pf_dat <- mg %>% left_join(prof %>% filter(var == vlab["CH4"]) %>% select(Site, depth, CH4 = v), by = c("Site", "depth")) %>%
  left_join(genes %>% filter(gene == "mcrA") %>% select(Site, depth, mcrA = v), by = c("Site", "depth"))
pf <- ggplot(pf_dat, aes(mcrA, CH4, colour = class)) +
  geom_smooth(aes(group = 1), method = "lm", formula = y ~ x, colour = "grey40", fill = "grey85", linewidth = 0.5) +
  geom_point(aes(shape = Site, fill = class), colour = "white", size = 1.8, stroke = 0.3) +
  scale_x_continuous(breaks = 5:8, labels = parse(text = paste0("10^", 5:8))) +
  scale_y_log10(breaks = c(1, 3, 10, 30, 100)) +
  scale_colour_manual(values = pal_class, guide = "none") + scale_fill_manual(values = pal_class, guide = "none") +
  scale_shape_manual(values = site_shape, guide = "none") +
  labs(x = expression(italic(mcrA)~"copies g"^-1), y = expression("Porewater CH"[4]*" ("*mu*"M)")) +
  mock_lab() + theme_fig() + small + mock_theme

row1 <- ((pa + labs(tag = "a")) | (pb + labs(tag = "b"))) + plot_layout(widths = c(1.1, 1), guides = "collect")
row2 <- ((pc1 + labs(tag = "c")) / pc2)
row3 <- ((pd + labs(tag = "d")) | (pe + labs(tag = "e")) | (pf + labs(tag = "f"))) +
  plot_layout(widths = c(1.35, 0.8, 0.85), guides = "collect")
fig <- (row1 / row2 / row3) + plot_layout(heights = c(1, 1.15, 0.72)) &
  theme(legend.position = "right", legend.justification = "left", legend.key.size = unit(8, "pt"))
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/fig4_geochem.png", fig, width = 7.2, height = 8.4, dpi = 300, bg = "white")
ggsave("output/figures/other/fig4_geochem.pdf", fig, width = 7.2, height = 8.4, device = cairo_pdf)
cat(sprintf("PCA: %d variables, %d samples; PC1 %.1f%%, PC2 %.1f%%\n", length(keep), nrow(dd), ve[1], ve[2]))
