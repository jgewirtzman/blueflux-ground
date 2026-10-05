# =============================================================================
# Fig. 4 | A geochemical regime shift.
#   (a) PCA of porewater chemistry (October 2025, 0-90 cm at SRS5, SRS6, BL60,
#       CP40; same 13 variables and treatment as 06_analysis/02_manuscript_results.R
#       (DIC is not included: it is calculated from pH and TA): DO correction, numeric variables with
#       <= 20% missing, CO2 and SD columns excluded, inorganic N with
#       below-detection = 0; centred and scaled). 68% class ellipses, all
#       loadings (CH4 highlighted). Descriptive only (one core per site).
#   (b) Salinity vs dissolved CH4, all three rounds (site x round x depth means of
#       the cleaned vials, porewater and plot surface water): salinity effect by
#       forest class at porewater (OLS of ln(1 + CH4) on salinity x class + sample
#       type, 95% CI, partial r), with the river and bay channels (ORNL DAAC 2333).
#   (d) October 2025 depth profiles (0-90 cm), 12 measured analytes in two rows
#       grouped by the PCA axis each loads on most (CO2, not in the PCA,
#       correlates with PC2/PC3 and sits with the carbon row). (DIC, a
#       derived quantity, and pH, which varies little, are left to the PCA/SI.)
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
cls3 <- c(healthy = "intact", regenerating = "regenerating", ghost = "ghost")
site_cls <- c(SRS5 = "intact", SRS6 = "intact", BL60 = "regenerating", CP40 = "ghost", FLM30 = "ghost")
site_shape <- c(SRS5 = 21, SRS6 = 24, BL60 = 22, CP40 = 23, FLM30 = 25)
small <- theme(axis.text = element_text(size = 6), axis.title = element_text(size = 7),
               legend.text = element_text(size = 6.5), legend.title = element_text(size = 7),
               strip.text = element_text(size = 6.5), plot.title = element_text(size = 7, face = "bold"))
mock_lab <- function(t = "MOCKUP: hypothetical values") labs(title = t)
mock_theme <- theme(plot.title = element_text(colour = "firebrick", face = "italic", size = 6.5, hjust = 0))

pw <- read.csv("output/data_products/porewater_all_parameters.csv", check.names = FALSE) %>%
  mutate(class = factor(site_cls[Site], names(pal_class)), Site = factor(Site, names(site_shape)),
         depth = case_when(Depth_cm == "Surface" ~ -5, TRUE ~ suppressWarnings(as.numeric(Depth_cm))))

# ---- (a) PCA ----
drop <- c("Lat", "Long", "Depth_numeric", "SpCond", "TempC", "n_replicates", "Tds ppt", "%DO", "DO_pct_raw", "ppmDO_raw", "Br_ppm", "F_ppm", "depth")
num <- pw %>% select(where(is.numeric)) %>% select(-any_of(drop)) %>%
  select(-matches("_sd$|_sd_|d13C_CO2|_raw$|_bdl$"))   # dissolved CO2 in; d13C-CO2 out (H2S interference)
keep <- names(num)[colMeans(is.na(num)) <= 0.20]
dd <- pw %>% select(Site, depth, class, all_of(keep)) %>% drop_na()
pr <- prcomp(dd %>% select(all_of(keep)), center = TRUE, scale. = TRUE)
ve <- round(100 * summary(pr)$importance[2, 1:2], 1)
sc <- data.frame(dd %>% select(Site, depth, class), pr$x[, 1:2])
# Loadings: each variable is assigned to the axis it loads on most strongly; the
# profile rows in (c) follow the same split. pH and NO3 (weak loadings; NO3 mostly
# below detection) stay in the PCA but are not drawn.
nice <- c(PSU = "Salinity", SO4_ppm = 'SO[4]^"2-"', CH4_mean_uM = "CH[4]", NH4_N_mgL = 'NH[4]^"+"', ORP = "ORP",
          ppmDO = "O[2]", DOC_mg_L = "DOC", Alkalinity_uM = "TA", d13C_CH4_mean = "delta^13*C[CH4]",
          Sulfide = "sulfide", `Total Iron` = "Fe", CO2_mean_uM = "CO[2]", pH = "pH", NO3_N_mgL = 'NO[3]^"-"')
ld <- data.frame(var = rownames(pr$rotation), pr$rotation[, 1:2]) %>%
  mutate(axis = ifelse(abs(PC1) >= abs(PC2), "PC1", "PC2"), ch4 = var == "CH4_mean_uM")
write.csv(ld %>% mutate(across(c(PC1, PC2), ~ round(.x, 3))), "output/analysis/porewater_pca_loadings.csv", row.names = FALSE)
ldp <- ld %>% filter(var %in% names(nice)) %>%
  mutate(lab = nice[var], grp = ifelse(ch4, "CH4", axis))
k <- 0.9 * max(abs(sc$PC1), abs(sc$PC2)) / max(sqrt(ldp$PC1^2 + ldp$PC2^2))
cen <- sc %>% group_by(class) %>%
  summarise(PC1 = mean(PC1), PC2 = mean(PC2) + ifelse(first(class) == "regenerating", -2.1, 1.6))
col_axis <- c(PC1 = "grey20", PC2 = "grey55", CH4 = "firebrick")
pa <- ggplot(sc, aes(PC1, PC2)) +
  geom_hline(yintercept = 0, colour = "grey90") + geom_vline(xintercept = 0, colour = "grey90") +
  stat_ellipse(aes(colour = class, fill = class), geom = "polygon", alpha = 0.10, level = 0.68, linewidth = 0.4) +
  geom_point(aes(shape = Site, fill = class), colour = "white", size = 2.1, stroke = 0.35) +
  geom_segment(data = ldp, aes(x = 0, y = 0, xend = PC1 * k, yend = PC2 * k, colour = grp), linewidth = 0.35,
               arrow = arrow(length = unit(2.2, "pt"), type = "closed")) +
  ggrepel::geom_text_repel(data = ldp, aes(PC1 * k, PC2 * k, label = lab, colour = grp), parse = TRUE,
                           size = 2.2, min.segment.length = Inf, box.padding = 0.12, point.padding = 0,
                           bg.color = "white", bg.r = 0.12, seed = 3) +
  geom_text(data = cen, aes(label = class, colour = class), size = 2.5, fontface = "bold") +
  scale_colour_manual(values = c(pal_class, col_axis), guide = "none") +
  scale_fill_manual(values = pal_class, guide = "none") +
  scale_shape_manual(values = site_shape, guide = "none") +
  labs(x = sprintf("PC1 (%.1f%%): salinity and redox", ve[1]), y = sprintf("PC2 (%.1f%%): carbon and sulfur", ve[2])) +
  theme_fig() + small + theme(panel.grid = element_blank())

# ---- (b) salinity vs CH4 ----
# Location means (site x campaign x depth) of dissolved CH4 from the cleaned vials: 2022-2023 GC
# vials (calibrated, air headspace subtracted; code/00_lib/gc_dec2023.R) and 2025 Picarro vials,
# failed vials dropped; salinity per location from site_characterization_figures.R. OLS of
# ln(1 + CH4) on salinity x forest class + sample type (surface water / porewater); lines are the
# salinity effect at porewater with 95% CI over each class's salinity range; r is the partial
# correlation. River and bay channels: BlueFlux survey (ORNL DAAC 2333), pCH4 x K0, OLS.
source("code/00_lib/gc_dec2023.R")
K0_ch4 <- function(T_C, S) { T <- T_C + 273.15                       # Yamamoto et al. (1976), mol L-1 atm-1
  exp(-67.1962 + 99.1624 * (100 / T) + 27.9015 * log(T / 100) + S * (-0.072909 + 0.041674 * (T / 100) - 0.0064603 * (T / 100)^2)) / 22.4136 }
vials <- bind_rows(
  gc_dec2023() %>% filter(!grepl("NO RUN", notes, ignore.case = TRUE)) %>%
    transmute(id = sample_id, CH4_uM = headspace_dissolved_uM(CH4_ppm, "CH4"),
              site = case_when(grepl("BL.?60", id, TRUE) ~ "BL60", grepl("^CP|CP.?4", id, TRUE) ~ "CP40", grepl("FLM|FML", id, TRUE) ~ "FLM30",
                               grepl("SRS.?5", id) ~ "SRS5", grepl("SRS.?6", id) ~ "SRS6"),
              type = case_when(grepl("pore|pour", id, TRUE) ~ "porewater", grepl("surface", id, TRUE) ~ "surface", id == "CP 40" ~ "porewater", id == "FML 30" ~ "surface"),
              depth_cm = case_when(grepl("100 cm", id) ~ 100, grepl("40 cm", id) ~ 40, grepl("15 cm", id) ~ 15, type == "surface" ~ -5, TRUE ~ 40),
              season = ifelse(date < as.Date("2023-01-01"), "wet (Oct 2022)", "dry (Mar 2023)")),
  read.csv("data/porewater/porewater_gas_samples_2025.csv") %>%
    transmute(site, season = "Oct 2025", depth_cm = ifelse(depth_cm == "Surface", -5, suppressWarnings(as.numeric(depth_cm))),
              type = ifelse(sample_type == "surface_water", "surface", "porewater"), CH4_uM)) %>%
  filter(site %in% names(site_cls), !is.na(type)) %>% drop_failed_vials(site, season, depth_cm)
sm <- vials %>% group_by(site, season, depth_cm, type) %>% summarise(CH4 = mean(CH4_uM), .groups = "drop") %>%
  inner_join(read.csv("output/data_products/porewater_salinity_ch4_merged.csv") %>% select(site, season, depth_cm, PSU_mean),
             by = c("site", "season", "depth_cm")) %>%
  mutate(class = factor(site_cls[site], names(pal_class)), type = factor(type, c("porewater", "surface")), y = log1p(CH4))
sfit <- lm(y ~ class * PSU_mean + type, sm); sb <- coef(sfit); sV <- vcov(sfit)
s_stat <- sapply(levels(sm$class), function(k) { g <- setNames(numeric(length(sb)), names(sb)); g["PSU_mean"] <- 1
  if (k != "intact") g[paste0("class", k, ":PSU_mean")] <- 1
  tt <- sum(g * sb) / sqrt(drop(t(g) %*% sV %*% g)); c(slope = sum(g * sb), r = tt / sqrt(tt^2 + sfit$df.residual), p = 2 * pt(-abs(tt), sfit$df.residual)) })
chan <- read.csv("data/environmental/aquatic/ORNL_DAAC_2333_BLUEFLUX_Transect_Shark_Haney_Rivers_TarponBay.csv", fileEncoding = "UTF-8-BOM", na.strings = "-9999") %>%
  filter(!is.na(salinity), !is.na(pCH4), !is.na(temp)) %>% transmute(salinity, y = log1p(pCH4 * K0_ch4(temp, salinity)))
cfit <- lm(y ~ salinity, chan); c_ct <- cor.test(chan$salinity, chan$y)
write.csv(data.frame(series = c(levels(sm$class), "channels"), slope_ln1p_per_PSU = c(s_stat["slope", ], coef(cfit)[2]),
                     r = c(s_stat["r", ], c_ct$estimate), p = c(s_stat["p", ], c_ct$p.value), n = c(table(sm$class), nrow(chan))),
          "output/analysis/salinity_ch4_model.csv", row.names = FALSE)
write.csv(sm %>% select(site, season, depth_cm, class, type, PSU_mean, CH4_uM = CH4), "output/analysis/salinity_ch4_means.csv", row.names = FALSE)   # fig. S18
gl <- sm %>% group_by(class) %>% summarise(lo = min(PSU_mean), hi = max(PSU_mean)) %>% rowwise() %>%
  reframe(class, PSU_mean = seq(lo, hi, length.out = 80))
X <- model.matrix(~ class * PSU_mean + type, gl %>% mutate(type = factor("porewater", levels(sm$type))))
gl <- gl %>% mutate(fit = drop(X %*% sb), se = sqrt(rowSums((X %*% sV) * X)), lo = fit - 1.96 * se, hi = fit + 1.96 * se)
cg <- data.frame(salinity = seq(0, 31, length.out = 40)); cp <- predict(cfit, cg, se.fit = TRUE)
cg <- cg %>% mutate(fit = cp$fit, lo = fit - 1.96 * cp$se.fit, hi = fit + 1.96 * cp$se.fit)
col_chan <- "#8AA9C8"; col_chan_txt <- "#5F7F9E"; LAB <- 6 / .pt
fmt_rp <- function(r, p) sprintf("r = %s, %s", sub("-", "\u2212", sprintf("%.2f", r)),
                                 ifelse(p < 0.001, "p < 0.001", ifelse(p < 0.01, sprintf("p = %.3f", p), sprintf("p = %.2f", p))))
blab <- data.frame(txt = c("intact", "regenerating", "ghost", "river and bay channels"), x = c(1.5, 66, 66, 66),
                   y = log1p(c(2.6, 7.5, 19, 0.62)), hj = c(0, 1, 1, 1), col = c(pal_class, col_chan_txt),
                   stat = c(fmt_rp(s_stat["r", ], s_stat["p", ]), fmt_rp(c_ct$estimate, c_ct$p.value)))
pb <- ggplot() +
  geom_point(data = chan, aes(salinity, y), colour = col_chan, size = 1.35, alpha = 0.45) +
  geom_ribbon(data = cg, aes(salinity, ymin = lo, ymax = hi), fill = col_chan, alpha = 0.25) +
  geom_line(data = cg, aes(salinity, fit), colour = col_chan, linewidth = 0.8) +
  geom_ribbon(data = gl, aes(PSU_mean, ymin = lo, ymax = hi, fill = class), alpha = 0.14) +
  geom_line(data = gl, aes(PSU_mean, fit, colour = class), linewidth = 0.8) +
  geom_point(data = sm, aes(PSU_mean, y, colour = class), size = 1.35, alpha = 0.45) +
  geom_text(data = blab, aes(x, y, label = txt, hjust = hj), colour = blab$col, size = LAB, fontface = "bold", vjust = 1) +
  geom_text(data = blab, aes(x, y - 0.2, label = stat, hjust = hj), colour = blab$col, size = LAB, vjust = 1) +
  scale_x_continuous(breaks = seq(0, 60, 20)) +
  scale_y_continuous(breaks = log1p(c(0, 1, 5, 10, 25, 50, 100)), labels = c(0, 1, 5, 10, 25, 50, 100)) +
  coord_cartesian(xlim = c(-1, 66.5), ylim = c(-0.05, log1p(160)), expand = FALSE) +
  scale_colour_manual(values = pal_class, guide = "none") + scale_fill_manual(values = pal_class, guide = "none") +
  labs(x = "Salinity (PSU)", y = expression("Dissolved CH"[4]*" ("*mu*"M)")) + theme_fig() + small

# ---- (c) water level: tidal intact sites vs ponded ghost site, 21-27 Oct 2022 ----
# SRS5/SRS6: FCE LTER hourly loggers (data/environmental/water_level). FLM30: PLACEHOLDER, a
# schematic of the HOBO pressure logger (SN 21285796) as read from its display: ~13 cm rise over
# the week and a residual tide of ~2 cm after barometric correction (US-Skr PA). Replace with the file.
wl <- read.csv("data/environmental/water_level/FCE_LTER_1168_water_levels.csv") %>%
  filter(SITENAME %in% c("SRS5", "SRS6"), Date >= "2022-10-21", Date <= "2022-10-27", WaterLevel > -9000) %>%
  transmute(site = SITENAME, t = as.POSIXct(paste(Date, Time), tz = "EST"), h = WaterLevel) %>%
  group_by(site) %>% mutate(h = h - mean(h)) %>% ungroup()
tt <- seq(as.POSIXct("2022-10-21 12:00", tz = "EST"), as.POSIXct("2022-10-27 00:00", tz = "EST"), by = "1 hour")
hh <- as.numeric(difftime(tt, tt[1], units = "hours"))
flm <- data.frame(site = "FLM30 (placeholder)", t = tt, h = 13 * hh / max(hh) + sin(2 * pi * hh / 12.42)) %>% mutate(h = h - mean(h))
wl <- bind_rows(wl, flm) %>% mutate(site = factor(site, c("SRS6", "SRS5", "FLM30 (placeholder)")))
pwl <- ggplot(wl, aes(t, h, colour = site, linetype = site)) + geom_line(linewidth = 0.45) +
  scale_colour_manual(values = c(SRS6 = pal_class[["intact"]], SRS5 = "#6FAE8C", `FLM30 (placeholder)` = pal_class[["ghost"]]), name = NULL) +
  scale_linetype_manual(values = c(1, 1, 2), name = NULL) +
  scale_x_datetime(date_labels = "%d", date_breaks = "2 days") +
  labs(x = "October 2022", y = "Water level, relative\nto weekly mean (cm)") +
  mock_lab("FLM30: PLACEHOLDER") + theme_fig() + small + mock_theme +
  theme(legend.position = "inside", legend.position.inside = c(0.02, 0.98), legend.justification = c(0, 1),
        legend.background = element_blank(), legend.key.height = unit(6, "pt"), legend.text = element_text(size = 5.5))

# ---- (d) depth profiles, 0-90 cm: all PCA variables, ordered by the axis they load on (PC1 then
# PC2) and by loading strength; strip colour = axis (as the biplot arrows); shaded = 0-15 cm ----
pm <- intToUtf8(0x2030)
col_of <- c(PSU = "PSU", SO4_ppm = "SO4", CH4_mean_uM = "CH4", NH4_N_mgL = "NH4", ORP = "ORP", ppmDO = "DO", pH = "pH",
            NO3_N_mgL = "NO3", Sulfide = "Sulfide", `Total Iron` = "Fe", DOC_mg_L = "DOC", Alkalinity_uM = "TA",
            CO2_mean_uM = "CO2", d13C_CH4_mean = "d13C")
plab <- c(PSU = '"Salinity (PSU)"', SO4 = 'SO[4]^"2-"~"(mM)"', CH4 = 'CH[4]~"("*mu*"M)"', NH4 = 'NH[4]^"+"~"("*mu*"M)"',
          ORP = '"Redox (mV)"', DO = 'O[2]~"(% sat.)"', pH = '"pH"', NO3 = 'NO[3]^"-"~"("*mu*"M)"', Sulfide = '"Sulfide (mM)"',
          Fe = '"Fe ("*mu*"M)"', DOC = '"DOC (mg L"^-1*")"', TA = '"Alkalinity (mM)"', CO2 = 'CO[2]~"(mM)"',
          d13C = paste0('delta^13*C-CH[4]~"(', pm, ')"'))
ord <- ld %>% mutate(key = col_of[var], w = pmax(abs(PC1), abs(PC2))) %>% filter(!is.na(key), key != "NO3") %>%   # NO3 (mostly below detection) in the PCA, not drawn
  arrange(axis, desc(w)) %>% pull(key)
ax_of <- setNames(ld$axis, col_of[ld$var])
LOGV <- c("CH4", "Sulfide")
prof <- pw %>% filter(depth >= 0) %>%
  transmute(Site, class, depth, PSU, SO4 = SO4_ppm / 96.06, CH4 = CH4_mean_uM, NH4 = NH4_N_mgL * 1000 / 14.007, ORP,
            DO = `%DO`, pH, NO3 = NO3_N_mgL * 1000 / 14.007, Sulfide = Sulfide / 34.08, Fe = `Total Iron` / 55.85 * 1000,
            DOC = DOC_mg_L, TA = Alkalinity_uM / 1000, CO2 = CO2_mean_uM / 1000, d13C = d13C_CH4_mean) %>%
  pivot_longer(-c(Site, class, depth), names_to = "key", values_to = "v") %>% filter(!is.na(v), key %in% ord) %>%
  mutate(var = factor(plab[key], plab[ord]))
band <- data.frame(key = ord) %>% mutate(var = factor(plab[key], plab[ord]), xmin = ifelse(key %in% LOGV, 0, -Inf))
brk <- list(PSU = c(20, 35, 50), DO = c(0, 20, 40), SO4 = c(0, 15, 30), CH4 = c(0.3, 3, 30), Sulfide = c(0.1, 0.3, 1), pH = c(6.4, 6.6, 6.8), ORP = c(-350, -200), TA = c(10, 20, 30), CO2 = c(2, 4),
            NH4 = c(0, 150, 300), DOC = c(0, 150, 300), d13C = c(-90, -75, -60))
sc <- lapply(ord, function(v) {
  b <- if (v %in% names(brk)) brk[[v]] else scales::breaks_pretty(n = 2)
  if (v %in% LOGV) scale_x_log10(breaks = b, labels = function(x) format(x, drop0trailing = TRUE, trim = TRUE))
  else scale_x_continuous(breaks = b) })
# Two rows of five, one per PCA axis, read left to right from driver to outcome:
# PC1 (salinity and redox): salinity -> sulfate -> O2 -> redox -> CH4 -> d13C-CH4;
# PC2 (carbon and sulfur): DOC -> CO2 -> alkalinity -> sulfide -> Fe, plus the key.
# pH, NH4 and NO3 stay in the PCA; their profiles (and surface-water values) are in the SI.
rows <- list(PC1 = c("PSU", "SO4", "DO", "ORP", "CH4", "d13C"), PC2 = c("DOC", "CO2", "TA", "Sulfide", "Fe"))
row_title <- c(PC1 = "PC1 \u00b7 salinity and redox", PC2 = "PC2 \u00b7 carbon and sulfur")
prof_row <- function(ax) {
  k <- rows[[ax]]
  d <- prof %>% filter(key %in% k) %>% mutate(var = factor(plab[key], plab[k]))
  b <- data.frame(key = k) %>% mutate(var = factor(plab[key], plab[k]), xmin = ifelse(key %in% LOGV, 0, -Inf))
  ggplot(d, aes(v, depth, colour = class, group = Site)) +
    geom_rect(data = b, aes(xmin = xmin, xmax = Inf, ymin = -3, ymax = 18), inherit.aes = FALSE, fill = "#F3E9D2", alpha = 0.6) +
    geom_path(linewidth = 0.4) +
    geom_point(aes(shape = Site, fill = class), colour = "white", size = 1.35, stroke = 0.25) +
    scale_y_reverse(breaks = c(0, 15, 45, 90), expand = expansion(add = c(4, 3))) +
    ggh4x::facet_wrap2(~ var, nrow = 1, scales = "free_x", axes = "x", labeller = label_parsed) +
    ggh4x::facetted_pos_scales(x = sc[match(k, ord)]) +
    scale_colour_manual(values = pal_class, guide = "none") + scale_fill_manual(values = pal_class, guide = "none") +
    scale_shape_manual(values = site_shape, guide = "none") +
    labs(x = NULL, y = "Depth (cm)", title = row_title[[ax]]) + theme_fig() + small +
    theme(axis.text.x = element_text(size = 5.2), panel.spacing.x = unit(6, "pt"), panel.grid.minor = element_blank(),
          strip.text = element_text(size = 6, hjust = 0.5, face = "plain"),
          plot.title = element_text(size = 6.5, face = "bold", colour = "grey15", hjust = 0, margin = margin(b = 1)))
}

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

pf_dat <- mg %>% left_join(prof %>% filter(key == "CH4") %>% select(Site, depth, CH4 = v), by = c("Site", "depth")) %>%
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

row1 <- ((pa + labs(tag = "a")) | (pb + labs(tag = "b")) | (pwl + labs(tag = "c"))) + plot_layout(widths = c(1, 1.2, 0.85))
key_pts <- data.frame(Site = factor(names(site_shape)[1:4], names(site_shape)), y = 4:1) %>%
  mutate(class = factor(site_cls[as.character(Site)], names(pal_class)), lab = paste0(Site, " (", class, ")"))
pkey <- ggplot(key_pts) +
  geom_point(aes(0, y, shape = Site, fill = class), colour = "white", size = 1.8, stroke = 0.25) +
  geom_text(aes(0.35, y, label = lab), hjust = 0, size = 5.6 / .pt, colour = "grey20") +
  annotate("rect", xmin = -0.15, xmax = 0.15, ymin = -0.35, ymax = 0.25, fill = "#F3E9D2", alpha = 0.9) +
  annotate("text", x = 0.35, y = -0.05, label = "0-15 cm (root zone)", hjust = 0, size = 5.6 / .pt, colour = "grey20") +
  scale_shape_manual(values = site_shape, guide = "none") + scale_fill_manual(values = pal_class, guide = "none") +
  coord_cartesian(xlim = c(-0.3, 3.2), ylim = c(-0.8, 4.6), expand = FALSE) + theme_void()
row2 <- (prof_row("PC1") + labs(tag = "d")) / (prof_row("PC2") + pkey + plot_layout(widths = c(5.25, 1)))
row3 <- ((pd + labs(tag = "e")) | (pe + labs(tag = "f") + theme(legend.position = "right", legend.text = element_text(size = 5.5), legend.title = element_text(size = 6), legend.key.size = unit(6, "pt"), legend.margin = margin(0, 0, 0, -4))) | (pf + labs(tag = "g"))) +
  plot_layout(widths = c(1.3, 1, 0.85))
fig <- (row1 / row2 / row3) + plot_layout(heights = c(1, 1.45, 0.8)) &
  theme(plot.margin = margin(2, 3, 2, 3))
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/fig4_geochem.png", fig, width = 7.2, height = 7.9, dpi = 300, bg = "white")
ggsave("output/figures/other/fig4_geochem.pdf", fig, width = 7.2, height = 7.9, device = cairo_pdf)
cat(sprintf("PCA: %d variables, %d samples; PC1 %.1f%%, PC2 %.1f%%\n", length(keep), nrow(dd), ve[1], ve[2]))
