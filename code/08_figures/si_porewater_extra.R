# =============================================================================
# Fig. S18 | Porewater variables not drawn in Fig. 4, and the salinity-CH4 relationship
# by sample type.
#   (a-c) October 2025 profiles of pH, ammonium and nitrate (0-90 cm), with surface water
#         (where measured) in its own band; inorganic N below detection = 0.
#   (d-e) Salinity vs dissolved CH4 (site x campaign x depth means of the cleaned vials,
#         written by fig4_geochem.R), split into porewater and plot surface water: the same
#         model as Fig. 4B (OLS of ln(1 + CH4) on salinity x forest class + sample type),
#         drawn at each sample type, 95% CI; shape = campaign. River and bay channels
#         (ORNL DAAC 2333) with the surface water.
# Writes output/figures/other/si_porewater_extra.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(patchwork)})
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
site_cls <- c(SRS5 = "intact", SRS6 = "intact", BL60 = "regenerating", CP40 = "ghost", FLM30 = "ghost")
site_shape <- c(SRS5 = 21, SRS6 = 24, BL60 = 22, CP40 = 23, FLM30 = 25)
small <- theme(axis.text = element_text(size = 6), axis.title = element_text(size = 7), strip.text = element_text(size = 6.5, face = "plain"),
               legend.text = element_text(size = 6.5), legend.title = element_text(size = 7))
col_chan <- "#8AA9C8"

# ---- (a-c) profiles ----
pw <- read.csv("output/data_products/porewater_all_parameters.csv", check.names = FALSE) %>%
  mutate(class = factor(site_cls[Site], names(pal_class)), Site = factor(Site, names(site_shape)),
         depth = ifelse(Depth_cm == "Surface", -12, suppressWarnings(as.numeric(Depth_cm))))
lab <- c(pH = '"pH"', NH4 = 'NH[4]^"+"~"("*mu*"M)"', NO3 = 'NO[3]^"-"~"("*mu*"M)"')
pr <- pw %>% transmute(Site, class, depth, pH, NH4 = NH4_N_mgL * 1000 / 14.007, NO3 = NO3_N_mgL * 1000 / 14.007) %>%
  pivot_longer(c(pH, NH4, NO3), names_to = "key", values_to = "v") %>% filter(!is.na(v)) %>% mutate(var = factor(lab[key], lab))
bands <- data.frame(var = factor(lab, lab))
pa <- ggplot(pr, aes(v, depth, colour = class, group = Site)) +
  geom_rect(data = bands, aes(xmin = -Inf, xmax = Inf, ymin = -20, ymax = -4), inherit.aes = FALSE, fill = "#DCEAF5", alpha = 0.8) +
  geom_rect(data = bands, aes(xmin = -Inf, xmax = Inf, ymin = -2, ymax = 17), inherit.aes = FALSE, fill = "#F3E9D2", alpha = 0.6) +
  geom_path(data = pr %>% filter(depth >= 0), linewidth = 0.4) +
  geom_point(aes(shape = Site, fill = class), colour = "white", size = 1.6, stroke = 0.25) +
  scale_y_reverse(breaks = c(-12, 0, 15, 45, 90), labels = c("surface", 0, 15, 45, 90), expand = expansion(add = c(3, 1))) +
  facet_wrap(~ var, nrow = 1, scales = "free_x", labeller = label_parsed) +
  scale_colour_manual(values = pal_class, guide = "none") + scale_fill_manual(values = pal_class, name = NULL) +
  scale_shape_manual(values = site_shape, name = NULL) +
  guides(fill = guide_legend(override.aes = list(shape = 21, colour = "white", size = 2.2)),
         shape = guide_legend(override.aes = list(fill = "grey40", colour = "white", size = 2.2))) +
  labs(x = NULL, y = "Depth (cm)") + theme_fig() + small + theme(legend.position = "right")

# ---- (d-e) salinity vs CH4 by sample type ----
sm <- read.csv("output/analysis/salinity_ch4_means.csv") %>%
  mutate(class = factor(class, names(pal_class)), type = factor(type, c("porewater", "surface")), y = log1p(CH4_uM),
         campaign = factor(c("wet (Oct 2022)" = "Oct 2022", "dry (Mar 2023)" = "Mar 2023", "Oct 2025" = "Oct 2025")[season], c("Oct 2022", "Mar 2023", "Oct 2025")))
fit <- lm(y ~ class * PSU_mean + type, sm); b <- coef(fit); V <- vcov(fit)
K0_ch4 <- function(T_C, S) { T <- T_C + 273.15
  exp(-67.1962 + 99.1624 * (100 / T) + 27.9015 * log(T / 100) + S * (-0.072909 + 0.041674 * (T / 100) - 0.0064603 * (T / 100)^2)) / 22.4136 }
chan <- read.csv("data/environmental/aquatic/ORNL_DAAC_2333_BLUEFLUX_Transect_Shark_Haney_Rivers_TarponBay.csv", fileEncoding = "UTF-8-BOM", na.strings = "-9999") %>%
  filter(!is.na(salinity), !is.na(pCH4), !is.na(temp)) %>% transmute(PSU_mean = salinity, y = log1p(pCH4 * K0_ch4(temp, salinity)), type = factor("surface", levels(sm$type)))
gl <- sm %>% group_by(class, type) %>% summarise(lo = min(PSU_mean), hi = max(PSU_mean), .groups = "drop") %>% rowwise() %>%
  reframe(class, type, PSU_mean = seq(lo, hi, length.out = 60))
X <- model.matrix(~ class * PSU_mean + type, gl)
gl <- gl %>% mutate(fit = drop(X %*% b), se = sqrt(rowSums((X %*% V) * X)), lo = fit - 1.96 * se, hi = fit + 1.96 * se)
tl <- c(porewater = "porewater", surface = "plot surface water (with river and bay channels)")
pb <- ggplot() +
  geom_point(data = chan, aes(PSU_mean, y), colour = col_chan, size = 1.2, alpha = 0.5) +
  geom_smooth(data = chan, aes(PSU_mean, y), method = "lm", formula = y ~ x, colour = col_chan, fill = col_chan, alpha = 0.25, linewidth = 0.7) +
  geom_ribbon(data = gl, aes(PSU_mean, ymin = lo, ymax = hi, fill = class), alpha = 0.14) +
  geom_line(data = gl, aes(PSU_mean, fit, colour = class), linewidth = 0.7) +
  geom_point(data = sm, aes(PSU_mean, y, colour = class, shape = campaign), size = 1.8) +
  facet_wrap(~ type, labeller = as_labeller(tl)) +
  scale_shape_manual(values = c(`Oct 2022` = 15, `Mar 2023` = 17, `Oct 2025` = 16), name = NULL) +
  scale_colour_manual(values = pal_class, name = NULL) + scale_fill_manual(values = pal_class, guide = "none") +
  scale_y_continuous(breaks = log1p(c(0, 1, 5, 10, 25, 50, 100)), labels = c(0, 1, 5, 10, 25, 50, 100)) +
  coord_cartesian(ylim = c(-0.05, log1p(160))) +
  labs(x = "Salinity (PSU)", y = expression("Dissolved CH"[4]*" ("*mu*"M)")) + theme_fig() + small + theme(legend.position = "right")

fig <- (pa + labs(tag = "a")) / (pb + labs(tag = "b")) + plot_layout(heights = c(1, 1.1))
ggsave("output/figures/other/si_porewater_extra.png", fig, width = 7.2, height = 6, dpi = 300, bg = "white")
ggsave("output/figures/other/si_porewater_extra.pdf", fig, width = 7.2, height = 6, device = cairo_pdf)
