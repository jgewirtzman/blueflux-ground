# =============================================================================
# Fig. S | Water-surface fluxes from dissolved gases (Methods M8) and an
# empirical check of the gas-transfer velocity.
# Reproduces code/03_fit/02_water_flux_from_dissolved.R (same constants and
# functions, copied verbatim; that script's CSV outputs are read where they exist):
#   F = k600 (Sc/600)^-0.5 (Cw - Ceq); CH4 solubility Yamamoto et al. 1976,
#   CO2 Weiss 1974, Sc Wanninkhof 2014; atmosphere 1.95 ppm CH4, 417 uatm CO2;
#   k600 = median of plot x campaign pairs (mean chamber water flux / mean
#   dissolved gradient), pairs with k600 > 50 cm h-1 dropped.
#   (a) dissolved CH4 per plot x campaign (samples; Ceq dashed).
#   (b) chamber CH4 flux vs air-water gradient Cw - Ceq: plot x campaign means
#       (the calibration) and individual closures; lines = constant k600.
#   (c) implied k600 per pairing: plot x campaign (as in the code), per closure,
#       and same-day only; CH4 and, for comparison, CO2 (chamber CO2 water flux /
#       dissolved CO2 gradient); k600 used (median, range) shaded.
#   (d, e) chamber water fluxes (closures, means) at all plots/campaigns beside
#       the dissolved-gas estimates for SRS5/SRS6 Oct 2022 (k600 used, range),
#       and the estimate with the empirical alternatives.
# Writes output/figures/other/si_dissolved_gas.{png,pdf} and
# output/figures/other/si_dissolved_gas_k_pairs.csv.
# =============================================================================
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(patchwork); library(readr); library(readxl)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))

# ---- constants / functions, verbatim from 03_fit/02_water_flux_from_dissolved.R ----
ATM_CH4_UATM <- 1.95; ATM_CO2_UATM <- 417; K600_MAX <- 50
STATION <- c(SRS5 = "SRS 5", SRS6 = "SRS 6")
bunsen_ch4 <- function(T_C, S) { T <- T_C + 273.15
  exp(-67.1962 + 99.1624 * (100 / T) + 27.9015 * log(T / 100) +
        S * (-0.072909 + 0.041674 * (T / 100) - 0.0064603 * (T / 100)^2)) }
K0  <- function(T_C, S) bunsen_ch4(T_C, S) / 22.4136
Ceq_nM <- function(T_C, S) ATM_CH4_UATM * 1e-6 * K0(T_C, S) * 1e9
sc_ch4 <- function(T_C) 1909.4 - 120.78 * T_C + 4.1555 * T_C^2 - 0.080578 * T_C^3 + 0.00065777 * T_C^4
k_from <- function(F, dC_nM, T_C) F / (dC_nM * 1e3) * 3600 * 100 * (sc_ch4(T_C) / 600)^0.5
F_from <- function(k600, dC_nM, T_C) k600 * (sc_ch4(T_C) / 600)^-0.5 / 3600 / 100 * dC_nM * 1e3
K0_co2 <- function(T_C, S) { T <- T_C + 273.15
  exp(-58.0931 + 90.5069 * (100 / T) + 22.2940 * log(T / 100) + S * (0.027766 - 0.025888 * (T / 100) + 0.0050578 * (T / 100)^2)) }
sc_co2 <- function(T_C) 1923.6 - 125.06 * T_C + 4.3773 * T_C^2 - 0.085681 * T_C^3 + 0.00070284 * T_C^4
F_co2 <- function(k600, dC_uM, T_C) k600 * (sc_co2(T_C) / 600)^-0.5 / 3600 / 100 * dC_uM * 1e3
k_from_co2 <- function(F, dC_uM, T_C) F / (dC_uM * 1e3) * 3600 * 100 * (sc_co2(T_C) / 600)^0.5   # inverse of F_co2
camp_of <- function(d) ifelse(format(d, "%Y-%m") == "2022-10", "Oct 2022", ifelse(format(d, "%Y-%m") == "2023-03", "Mar 2023", NA))
campaigns <- c("Oct 2022", "Mar 2023")
pclass <- c(SRS5 = "intact", SRS6 = "intact", BL60 = "regenerating", CP40 = "ghost", FLM30 = "ghost", SE1 = "scrub")
pcol <- c(pal_class, scrub = "#9A8C7A")

# ---- inputs (as the analysis script) ----
cal <- read_csv("output/flux/03_fit/water_k_calibration.csv", show_col_types = FALSE)
est <- read_csv("output/flux/03_fit/water_flux_estimates.csv", show_col_types = FALSE)
xc  <- read_csv("output/flux/03_fit/water_flux_dissolved_crosscheck.csv", show_col_types = FALSE)
k_use <- cal$k600_cm_h[cal$used]; k_mid <- median(k_use); k_lo <- min(k_use); k_hi <- max(k_use)
aux <- read_csv("data/inputs/closures.csv", show_col_types = FALSE) %>% rename(UniqueID = flux_id, plot = site, start.time = field_start)
clos <- read_csv("output/flux/03_fit/CH4/fluxes.csv", show_col_types = FALSE) %>% select(UniqueID, F_ch4 = best.flux) %>%
  full_join(read_csv("output/flux/03_fit/CO2/fluxes.csv", show_col_types = FALSE) %>% select(UniqueID, F_co2 = best.flux), by = "UniqueID") %>%
  inner_join(aux %>% filter(component == "water") %>% select(UniqueID, plot, date, start.time), by = "UniqueID") %>%
  mutate(campaign = camp_of(date)) %>% filter(!is.na(campaign))
gc_raw <- read_csv("data/environmental/dissolved_gas/dissolved_gas_all_observations.csv", show_col_types = FALSE) %>%
  filter(sample_type == "surface_water", source == "GC") %>%
  mutate(campaign = ifelse(grepl("2022", season), "Oct 2022", "Mar 2023"),
         T_C = ifelse(campaign == "Oct 2022", 28, 26), S = 15, plot = site, date = as.Date(real_date),
         source = "own GC, plot surface water")
tr <- read_excel("data/environmental/aquatic/Everglades_Lateral_C_GHG_Dataset_export_2026-10-01.xlsx", "BlueFlux Transect Data") %>%
  transmute(station = Site, date = as.Date(`Date Sampled`), S = suppressWarnings(as.numeric(Salinity)),
            T_C = suppressWarnings(as.numeric(Temp)), pCH4 = suppressWarnings(as.numeric(pCH4_uatm)),
            pCO2 = suppressWarnings(as.numeric(pCO2_uatm))) %>%
  filter(station %in% STATION, !is.na(pCH4), !is.na(T_C), !is.na(S)) %>%
  mutate(plot = names(STATION)[match(station, STATION)], campaign = camp_of(date)) %>% filter(!is.na(campaign)) %>%
  mutate(CH4_uM = pCH4 * 1e-6 * K0(T_C, S) * 1e6, CO2_uM = pCO2 * 1e-6 * K0_co2(T_C, S) * 1e6,
         source = paste0("aquatic transect, station ", station, " (", date, ")"))
samp <- bind_rows(gc_raw %>% select(plot, campaign, date, T_C, S, CH4_uM, CO2_uM, source),
                  tr %>% select(plot, campaign, date, T_C, S, CH4_uM, CO2_uM, source)) %>%
  mutate(Ceq_nM = Ceq_nM(T_C, S), Ceq_co2_uM = ATM_CO2_UATM * 1e-6 * K0_co2(T_C, S) * 1e6)

# ---- pairings: plot x campaign (as in code), per closure, same day ----
pairs <- cal %>% select(plot, campaign, T_C, S, source, Cw_nM, dC_nM, n_chamber, F_chamber, k600_cm_h, used)
sdates <- samp %>% group_by(plot, campaign, source) %>% summarise(sample_dates = paste(sort(unique(format(date, "%d %b"))), collapse = ", "), .groups = "drop")
cdates <- clos %>% filter(!is.na(F_ch4)) %>% group_by(plot, campaign) %>%
  summarise(chamber_dates = paste(sort(unique(format(as.Date(date), "%d %b"))), collapse = ", "), .groups = "drop")
co2_pair <- samp %>% filter(!is.na(CO2_uM)) %>% group_by(plot, campaign, source, T_C, S) %>%
  summarise(dC_co2_uM = mean(CO2_uM) - mean(Ceq_co2_uM), .groups = "drop") %>%
  inner_join(clos %>% filter(!is.na(F_co2)) %>% group_by(plot, campaign) %>% summarise(F_co2 = mean(F_co2), .groups = "drop"),
             by = c("plot", "campaign")) %>%
  mutate(k600_co2 = k_from_co2(F_co2, dC_co2_uM, T_C))
pairs <- pairs %>% left_join(sdates, by = c("plot", "campaign", "source")) %>% left_join(cdates, by = c("plot", "campaign")) %>%
  left_join(co2_pair %>% select(plot, campaign, source, dC_co2_uM, F_co2, k600_co2), by = c("plot", "campaign", "source"))
# closure-level k against the pair's mean gradient (CH4 and CO2)
kc <- pairs %>% select(plot, campaign, source, T_C, dC_nM, dC_co2_uM, used) %>%
  inner_join(clos, by = c("plot", "campaign"), relationship = "many-to-many") %>%
  mutate(k_ch4 = k_from(F_ch4, dC_nM, T_C), k_co2 = k_from_co2(F_co2, dC_co2_uM, T_C))
# same-day: GC samples taken on the day of the closures (transect: station sample on the closure day)
sd <- samp %>% inner_join(clos %>% filter(!is.na(F_ch4)) %>% mutate(date = as.Date(date)) %>% distinct(plot, campaign, date),
                          by = c("plot", "campaign", "date")) %>%
  group_by(plot, campaign, date, source, T_C, S) %>%
  summarise(n_samples = n(), dC_nM = mean(CH4_uM) * 1e3 - mean(Ceq_nM), dC_co2_uM = mean(CO2_uM) - mean(Ceq_co2_uM), .groups = "drop") %>%
  inner_join(clos %>% mutate(date = as.Date(date)) %>% group_by(plot, campaign, date) %>%
               summarise(n_ch = sum(!is.na(F_ch4)), F_ch4 = mean(F_ch4, na.rm = TRUE), F_co2 = mean(F_co2, na.rm = TRUE), .groups = "drop"),
             by = c("plot", "campaign", "date")) %>%
  mutate(k_ch4 = k_from(F_ch4, dC_nM, T_C), k_co2 = k_from_co2(F_co2, dC_co2_uM, T_C))
cat("\nSame-day pairs:\n"); print(as.data.frame(sd %>% mutate(across(where(is.numeric), ~ signif(.x, 3)))))

ok <- function(k) k[is.finite(k) & k > 0 & k <= K600_MAX]
alt <- tibble(
  label = c("used: plot × campaign median", "per-closure median (retained pairs)", "same-day pairs median", "CO2-implied, plot × campaign median"),
  short = c("used", "per closure", "same day", "CO2-implied"),
  k600 = c(k_mid, median(ok(kc$k_ch4[kc$used])), median(ok(sd$k_ch4)), median(ok(pairs$k600_co2))),
  n = c(length(k_use), length(ok(kc$k_ch4[kc$used])), length(ok(sd$k_ch4)), length(ok(pairs$k600_co2))))
cat("\nAlternative k600 (cm h-1):\n"); print(as.data.frame(alt))
cat(sprintf("Per-closure implied CH4 k600 (retained pairs): median %.2f, IQR %.2f-%.2f, range %.2f-%.2f, n = %d; negative/other excluded: %d\n",
            median(ok(kc$k_ch4[kc$used])), quantile(ok(kc$k_ch4[kc$used]), 0.25), quantile(ok(kc$k_ch4[kc$used]), 0.75),
            min(ok(kc$k_ch4[kc$used])), max(ok(kc$k_ch4[kc$used])), length(ok(kc$k_ch4[kc$used])),
            sum(kc$used & !is.na(kc$k_ch4)) - length(ok(kc$k_ch4[kc$used]))))
cat("CO2-implied k600 by pair:\n"); print(as.data.frame(pairs %>% select(plot, campaign, source, dC_co2_uM, F_co2, k600_co2) %>% mutate(across(where(is.numeric), ~ signif(.x, 3)))))

# consequence for SRS5/SRS6 Oct 2022: water flux scales linearly with k600
summ <- read.csv("output/upscaling/summary_CH4_by_component.csv") %>%
  filter(site %in% c("SRS5", "SRS6"), campaign == "Oct 2022", scenario == "exponential")
cons <- est %>% filter(gas == "CH4") %>% select(site, F_used = flux_rate) %>% crossing(alt %>% select(short, k600)) %>%
  mutate(F_alt = F_used * k600 / k_mid) %>%
  left_join(summ %>% select(site, water_mg = water, total_mg = total), by = "site") %>%
  mutate(total_alt_mg = total_mg + water_mg * (k600 / k_mid - 1), change_pct = 100 * (total_alt_mg / total_mg - 1))
cat("\nConsequence (CH4, Oct 2022, tide-weighted site totals, mg CH4 m-2 d-1):\n"); print(as.data.frame(cons %>% mutate(across(where(is.numeric), ~ round(.x, 3)))))
write.csv(pairs, "output/figures/other/si_dissolved_gas_k_pairs.csv", row.names = FALSE)
print(as.data.frame(pairs %>% select(plot, campaign, source, sample_dates, chamber_dates, k600_cm_h, used, k600_co2) %>%
                      mutate(source = substr(source, 1, 22), across(where(is.numeric), ~ round(.x, 2)))))

# ---- plotting helpers ----
pc_lab <- function(d) d %>% mutate(pc = paste(plot, sub(" 20", " ’", campaign)), cls = factor(pclass[plot], names(pcol)))
th <- theme_fig() + theme(legend.position = "none")

# (a) dissolved CH4
sa <- pc_lab(samp) %>% mutate(src = ifelse(grepl("^own", source), "plot GC", "transect"),
                              target = plot %in% c("SRS5", "SRS6") & campaign == "Oct 2022")
ord <- sa %>% group_by(pc) %>% summarise(m = mean(CH4_uM)) %>% arrange(m) %>% pull(pc)
pa <- ggplot(sa %>% mutate(pc = factor(pc, ord)), aes(y = pc, x = CH4_uM * 1e3, colour = cls, shape = src)) +
  geom_vline(xintercept = Ceq_nM(28, 15), linetype = "dashed", colour = "grey40", linewidth = 0.3) +
  geom_point(size = 1.2, alpha = 0.85) +
  annotate("text", x = Ceq_nM(28, 15) * 1.2, y = 0.6, label = "C[eq]", parse = TRUE, hjust = 0, size = 2.2, colour = "grey30") +
  scale_x_log10(breaks = c(10, 100, 1000, 1e4), labels = c("10", "100", "1k", "10k")) +
  scale_colour_manual(values = pcol) + scale_shape_manual(values = c("plot GC" = 16, transect = 2), name = NULL) +
  guides(colour = "none") + labs(x = "surface-water CH₄ (nM)", y = NULL) + th +
  theme(legend.position = c(0.75, 0.2), legend.background = element_rect(fill = "white", colour = NA),
        legend.key.size = unit(6, "pt"))

# (b) flux vs gradient
kl <- tibble(k = c(k_lo, k_mid, k_hi, K600_MAX), lab = c(sprintf("%.2f", k_lo), sprintf("%.2f", k_mid), sprintf("%.2f", k_hi), "50 (cut-off)"))
xg <- 10^seq(0.95, 4.6, length.out = 50)
kline <- crossing(kl, dC = xg) %>% mutate(F = F_from(k, dC, 27)) %>% filter(F >= 0.1, F <= 150)
pts_b <- pc_lab(kc %>% filter(!is.na(F_ch4)))
mean_b <- pc_lab(pairs)
tg <- est %>% filter(gas == "CH4") %>% left_join(xc %>% filter(is.na(n_chamber)) %>% select(site = plot, dissolved_source, T_C, S, Cw_nM2 = Cw_nM),
                                                 by = c("site", "dissolved_source")) %>%
  mutate(dC = Cw_nM - Ceq_nM(T_C, S), plot = site, campaign = "Oct 2022") %>% pc_lab()
kline <- kline %>% mutate(lab = factor(lab, kl$lab))
pb <- ggplot() +
  geom_line(data = kline, aes(dC, F, group = lab, linetype = lab), colour = "grey45", linewidth = 0.3) +
  geom_point(data = pts_b, aes(dC_nM, F_ch4, colour = cls), size = 0.7, alpha = 0.6, stroke = 0) +
  geom_point(data = mean_b, aes(dC_nM, F_chamber, colour = cls, shape = used), size = 2) +
  geom_errorbar(data = tg, aes(x = dC, ymin = ci_lo, ymax = ci_hi), width = 0.04, colour = pal_class[["intact"]], linewidth = 0.3) +
  geom_point(data = tg, aes(dC, flux_rate), shape = 23, fill = "white", colour = pal_class[["intact"]], size = 2) +
  scale_x_log10(limits = c(8, 1.5e5), breaks = c(10, 100, 1000, 1e4, 1e5), labels = c("10", "100", "1k", "10k", "100k")) +
  scale_y_log10(limits = c(0.1, 150), breaks = c(0.1, 1, 10, 100), labels = c("0.1", "1", "10", "100")) +
  scale_colour_manual(values = pcol, guide = "none") + scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 4), guide = "none") +
  scale_linetype_manual(values = setNames(c("dotdash", "solid", "dashed", "dotted"), kl$lab),
                        name = expression(k[600] ~ "(cm h"^-1 * ")")) +
  labs(x = expression(C[w] - C[eq] ~ "(nM)"), y = expression("chamber CH"[4] ~ "flux (nmol m"^-2 ~ "s"^-1 * ")")) +
  theme_fig() + theme(legend.position = c(0.74, 0.2), legend.key.width = unit(12, "pt"), legend.text = element_text(size = 6),
                      legend.title = element_text(size = 6.5), legend.background = element_rect(fill = "white", colour = NA))

# (c) implied k600
kk <- bind_rows(
  pc_lab(pairs) %>% transmute(pc, cls, gas = "CH₄", level = "plot × campaign", k = k600_cm_h),
  pc_lab(kc) %>% transmute(pc, cls, gas = "CH₄", level = "closure", k = k_ch4),
  pc_lab(sd) %>% transmute(pc, cls, gas = "CH₄", level = "same day", k = k_ch4),
  pc_lab(pairs) %>% transmute(pc, cls, gas = "CO₂", level = "plot × campaign", k = k600_co2)) %>%
  filter(is.finite(k))
neg <- kk %>% filter(k <= 0)
kk_pos <- kk %>% filter(k > 0)
ord_c <- pc_lab(pairs) %>% arrange(k600_cm_h) %>% pull(pc) %>% unique()
kk_pos <- kk_pos %>% mutate(pc = factor(pc, ord_c), level = factor(level, c("closure", "plot × campaign", "same day")))
pcc <- ggplot(kk_pos, aes(y = pc, x = k)) +
  annotate("rect", xmin = k_lo, xmax = k_hi, ymin = -Inf, ymax = Inf, fill = "grey90") +
  geom_vline(xintercept = k_mid, colour = "grey20", linewidth = 0.4) +
  geom_vline(xintercept = K600_MAX, colour = "grey40", linetype = "dotted", linewidth = 0.3) +
  geom_point(data = kk_pos %>% filter(level == "closure"), aes(colour = cls), size = 0.7, alpha = 0.5, stroke = 0,
             position = position_nudge(y = -0.18)) +
  geom_point(data = kk_pos %>% filter(level != "closure"), aes(colour = cls, shape = interaction(gas, level, sep = ": ")),
             size = 1.7, stroke = 0.5) +
  scale_shape_manual(values = c("CH₄: plot × campaign" = 16, "CH₄: same day" = 5, "CO₂: plot × campaign" = 2), name = NULL) +
  scale_colour_manual(values = pcol, guide = "none") +
  scale_x_log10(breaks = c(0.1, 1, 10, 100, 1000), labels = c("0.1", "1", "10", "100", "1,000")) +
  labs(x = expression("implied k"[600] ~ "(cm h"^-1 * ")"), y = NULL) + theme_fig() +
  theme(legend.position = c(0.7, 0.08), legend.key.size = unit(6, "pt"), legend.text = element_text(size = 6),
        legend.background = element_rect(fill = "white", colour = NA), legend.margin = margin(1, 1, 1, 1))

# (d, e) water fluxes: chambers vs dissolved estimates
wcmp <- function(gas, fcol, unit_lab) {
  ch <- pc_lab(clos %>% mutate(F = .data[[fcol]]) %>% filter(!is.na(F)))
  m <- ch %>% group_by(pc, plot, campaign, cls) %>% summarise(F = mean(F), .groups = "drop") %>% mutate(kind = "chamber")
  e <- est %>% filter(gas == !!gas) %>% transmute(plot = site, campaign, F = flux_rate, lo = ci_lo, hi = ci_hi) %>% pc_lab() %>%
    mutate(kind = "dissolved")
  ea <- est %>% filter(gas == !!gas) %>% transmute(plot = site, campaign, F0 = flux_rate) %>% pc_lab() %>%
    crossing(alt %>% filter(short != "used") %>% select(short, k600)) %>% mutate(F = F0 * k600 / k_mid)
  lev <- c(m %>% arrange(cls, plot, campaign) %>% pull(pc) %>% unique(), e$pc)
  lev <- unique(lev)
  ggplot(mapping = aes(y = factor(pc, rev(lev)), x = F)) +
    geom_point(data = ch, aes(colour = cls), size = 0.7, alpha = 0.5, stroke = 0, position = position_nudge(y = -0.2)) +
    geom_point(data = m, aes(colour = cls), size = 1.8) +
    geom_errorbar(data = e, aes(xmin = lo, xmax = hi, colour = cls), width = 0.25, linewidth = 0.35, orientation = "y") +
    geom_point(data = e, aes(colour = cls), shape = 23, fill = "white", size = 2) +
    geom_point(data = ea, aes(shape = short), colour = "grey30", size = 1.1, stroke = 0.4, position = position_nudge(y = 0.25)) +
    scale_shape_manual(values = c("per closure" = 3, "same day" = 5, "CO2-implied" = 2),
                       labels = c("per closure" = "k per closure", "same day" = "k same day", "CO2-implied" = "k from CO₂"), name = NULL) +
    scale_colour_manual(values = pcol, guide = "none") +
    labs(x = unit_lab, y = NULL) + theme_fig() +
    theme(legend.position = c(0.84, 0.72), legend.key.size = unit(6, "pt"), legend.margin = margin(1, 1, 1, 1),
          legend.background = element_rect(fill = "white", colour = NA))
}
pd <- wcmp("CH4", "F_ch4", expression("water CH"[4] ~ "flux (nmol m"^-2 ~ "s"^-1 * ")")) +
  scale_x_log10(breaks = c(0.1, 1, 10, 100), labels = c("0.1", "1", "10", "100"))
pe <- wcmp("CO2", "F_co2", expression("water CO"[2] ~ "flux (µmol m"^-2 ~ "s"^-1 * ")")) +
  scale_x_continuous(limits = c(-0.2, NA)) + theme(legend.position = "none")

p <- (pa | pb | pcc) / (pd | pe) + plot_layout(heights = c(1.1, 1)) + plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 11))
ggsave("output/figures/other/si_dissolved_gas.png", p, width = 7.2, height = 6.4, dpi = 300, bg = "white")
ggsave("output/figures/other/si_dissolved_gas.pdf", p, width = 7.2, height = 6.4, device = cairo_pdf)
