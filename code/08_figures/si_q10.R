# =============================================================================
# Fig. S | Daytime-to-24 h scaling of chamber respiration (Methods M13/M14).
#   (a) Within-month temperature sensitivity of night-time ecosystem respiration
#       at US-Skr (as 07_upscaling/01_tower_gpp.R): night NEE (SW_IN < 10,
#       0 < NEE < 30, u* > 0.2; 2004-2023), lm(log NEE ~ TA + year-month fixed
#       effect). Shown as deviations from each year-month mean (the fixed-effect
#       slope is the slope of these deviations); line: fitted slope b,
#       Q10 = exp(10 b) (output/gpp/US-Skr_Q10_within_month.csv).
#   (b) Chamber measurement temperatures (tower TA at chamber time, `air_temp`)
#       by time of day against the tower's diel TA cycle in each campaign month;
#       dashed: 24-h mean TA (TA_use in the GPP half-hourly file, as 03_upscale_co2.R).
#   (c) Correction factor mean_24h(Q10^(T/10)) / mean_chambers(Q10^(T/10)) per
#       campaign x component (output/upscaling/co2_temperature_correction.csv),
#       with the factor at the Q10 95 % CI.
# Writes output/figures/other/si_q10.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(patchwork); library(data.table)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))
campaigns <- c("Oct 2022", "Mar 2023"); tls_sites <- c("CP40", "FLM30", "SRS5", "SRS6")
resp_comps <- c("stem", "root", "soil", "cwd")
comp_lab <- c(stem = "stem", root = "prop root", soil = "soil", cwd = "downed wood")

# ---- (a) tower night respiration, as 01_tower_gpp.R ----
dt <- fread("data/tower/AMF_US-Skr_BASE_HH_2-5.csv",
            select = c("TIMESTAMP_START", "TIMESTAMP_END", "NEE_PI", "FC", "SC", "TA_1_1_1", "SW_IN", "USTAR"))
for (nm in names(dt)) if (is.numeric(dt[[nm]])) set(dt, which(dt[[nm]] <= -9990), nm, NA_real_)
ts <- function(x) as.POSIXct(as.character(x), format = "%Y%m%d%H%M", tz = "UTC")
dt[, mid := ts(TIMESTAMP_START) + as.numeric(difftime(ts(TIMESTAMP_END), ts(TIMESTAMP_START), units = "secs")) / 2]
dt[, `:=`(year = as.integer(format(mid, "%Y")), month = as.integer(format(mid, "%m")))]
dt[, NEE_OBS := fifelse(is.finite(NEE_PI), NEE_PI, fifelse(is.finite(FC) & is.finite(SC), FC + SC, FC))]
qn <- dt[is.finite(NEE_OBS) & is.finite(TA_1_1_1) & is.finite(SW_IN) & SW_IN < 10 &
           NEE_OBS > 0 & NEE_OBS < 30 & is.finite(USTAR) & USTAR > 0.2]
qn[, ym := paste(year, month)]
fit <- lm(log(NEE_OBS) ~ TA_1_1_1 + factor(ym), data = qn)
b <- coef(fit)[["TA_1_1_1"]]; se <- summary(fit)$coefficients["TA_1_1_1", 2]
q10_csv <- read.csv("output/gpp/US-Skr_Q10_within_month.csv")
cat(sprintf("Refit: Q10 = %.3f [%.3f-%.3f], n = %d; CSV: %.3f [%.3f-%.3f], n = %d\n", exp(10 * b), exp(10 * (b - 1.96 * se)),
            exp(10 * (b + 1.96 * se)), nrow(qn), q10_csv$Q10, q10_csv$Q10_lo, q10_csv$Q10_hi, q10_csv$n))
qn[, `:=`(dT = TA_1_1_1 - mean(TA_1_1_1), dlnR = log(NEE_OBS) - mean(log(NEE_OBS))), by = ym]
bins <- qn[, .(m = mean(dlnR), n = .N), by = .(x = round(dT))][n >= 30][order(x)]
pa <- ggplot(qn, aes(dT, dlnR)) +
  geom_bin2d(binwidth = c(0.5, 0.1)) +
  scale_fill_gradient(low = "grey90", high = "grey20", trans = "log10", name = "half-hours") +
  geom_point(data = bins, aes(x, m), inherit.aes = FALSE, size = 1.2, colour = "#D55E00") +
  geom_abline(slope = b, intercept = 0, colour = "#D55E00", linewidth = 0.5) +
  annotate("text", x = -10.5, y = 3.0, hjust = 0, vjust = 1, size = 2.4,
           label = sprintf("Q[10] == %.2f~(%.2f*'–'*%.2f)", q10_csv$Q10, q10_csv$Q10_lo, q10_csv$Q10_hi), parse = TRUE) +
  annotate("text", x = -10.5, y = 2.62, hjust = 0, vjust = 1, size = 2.2, colour = "grey30",
           label = sprintf("n = %s night half-hours, %s", format(nrow(qn), big.mark = ","), q10_csv$years)) +
  coord_cartesian(xlim = c(-10, 8), ylim = c(-2, 2.9)) +
  labs(x = "air temperature − year-month mean (°C)", y = "ln night NEE − year-month mean") +
  theme_fig() + theme(legend.position = "right", legend.key.height = unit(14, "pt"))

# ---- (b) chamber-time vs 24-h temperature ----
gpp <- read.csv("output/gpp/US-Skr_GPP_halfhourly_Mar2022_Oct2022_Mar2023.csv") %>%
  mutate(campaign = case_when(year == 2022 & month == 10 ~ "Oct 2022", year == 2023 & month == 3 ~ "Mar 2023"),
         TA_use = ifelse(!is.na(TA) & TA > -900, TA, TA_model), hod = hour + minute / 60) %>% filter(!is.na(campaign))
diel <- gpp %>% group_by(campaign, hod) %>%
  summarise(m = mean(TA_use), lo = quantile(TA_use, 0.1), hi = quantile(TA_use, 0.9), .groups = "drop")
tc <- read.csv("output/upscaling/co2_temperature_correction.csv")
t24 <- tc %>% distinct(campaign, T24_mean)
ch <- read.csv("output/data_products/combined_gas_flux_dataset.csv") %>%
  filter(plot %in% tls_sites, month_year %in% c("2022-10", "2023-03"), component %in% resp_comps,
         !is.na(CO2_best.flux), !is.na(air_temp)) %>%
  mutate(campaign = ifelse(month_year == "2022-10", "Oct 2022", "Mar 2023"),
         hod = as.numeric(substr(start_time, 1, 2)) + as.numeric(substr(start_time, 4, 5)) / 60,
         comp = factor(comp_lab[component], comp_lab))
cat(sprintf("Chamber times: %d closures, median %.1f h (range %.1f-%.1f)\n", nrow(ch), median(ch$hod, na.rm = TRUE),
            min(ch$hod, na.rm = TRUE), max(ch$hod, na.rm = TRUE)))
fc <- function(d) d %>% mutate(campaign = factor(campaign, campaigns))
cc <- setNames(pal_comp[c("stem", "prop root", "soil", "downed wood")], comp_lab); cc["stem"] <- "#C9A85C"
pb <- ggplot(fc(diel), aes(hod)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey88") + geom_line(aes(y = m), colour = "grey35", linewidth = 0.4) +
  geom_hline(data = fc(t24), aes(yintercept = T24_mean), linetype = "dashed", colour = "grey20", linewidth = 0.3) +
  geom_point(data = fc(ch), aes(y = air_temp, colour = comp), size = 0.6, alpha = 0.7, stroke = 0) +
  facet_wrap(~ campaign) + scale_colour_manual(values = cc, name = NULL) +
  scale_x_continuous(breaks = seq(0, 24, 6), limits = c(0, 24)) +
  labs(x = "hour of day", y = "air temperature (°C)") + theme_fig() +
  guides(colour = guide_legend(override.aes = list(size = 1.8, alpha = 1)))

# ---- (c) correction factors ----
# factors at the Q10 CI bounds: recompute with the chamber temperatures and tower 24-h series at Q10_lo / Q10_hi
f_at <- function(q) {
  bq <- log(q) / 10
  f24 <- gpp %>% group_by(campaign) %>% summarise(f24 = mean(exp(bq * TA_use), na.rm = TRUE), .groups = "drop")
  ch %>% group_by(campaign, component) %>% summarise(fm = mean(exp(bq * air_temp)), .groups = "drop") %>%
    left_join(f24, by = "campaign") %>% transmute(campaign, component, f = f24 / fm)
}
ci <- f_at(tc$Q10_lo[1]) %>% rename(f_lo = f) %>% left_join(f_at(tc$Q10_hi[1]) %>% rename(f_hi = f), by = c("campaign", "component"))
chk <- f_at(tc$Q10[1]) %>% left_join(tc %>% select(campaign, component, factor), by = c("campaign", "component"))
cat(sprintf("Max |recomputed - CSV factor| = %.2g\n", max(abs(chk$f - chk$factor))))
tcp <- tc %>% left_join(ci, by = c("campaign", "component")) %>%
  mutate(comp = factor(comp_lab[component], rev(comp_lab)), campaign = factor(campaign, campaigns),
         lab = sprintf("%.3f  (%.1f vs %.1f °C)", factor, T_meas_mean, T24_mean))
print(tcp %>% select(campaign, component, n, T_meas_mean, T24_mean, factor, f_lo, f_hi) %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
pc <- ggplot(tcp, aes(y = comp, colour = comp)) +
  geom_vline(xintercept = 1, colour = "grey50", linewidth = 0.3) +
  geom_errorbar(aes(xmin = pmin(f_lo, f_hi), xmax = pmax(f_lo, f_hi)), width = 0.25, linewidth = 0.4, orientation = "y") +
  geom_point(aes(x = factor), size = 1.8) +
  geom_text(aes(x = 1.035, label = lab), hjust = 0, size = 2.1, colour = "grey25") +
  facet_wrap(~ campaign) + scale_colour_manual(values = cc, guide = "none") +
  scale_x_continuous(limits = c(0.94, 1.115), breaks = c(0.95, 1, 1.05)) +
  labs(x = "24-h / chamber-time respiration factor", y = NULL) + theme_fig()

p <- (pa | pb) / pc + plot_layout(heights = c(1.25, 1)) + plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 11))
ggsave("output/figures/other/si_q10.png", p, width = 7.2, height = 4.6, dpi = 300, bg = "white")
ggsave("output/figures/other/si_q10.pdf", p, width = 7.2, height = 4.6, device = cairo_pdf)
