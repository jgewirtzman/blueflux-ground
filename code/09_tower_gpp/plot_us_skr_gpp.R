library(data.table)
library(ggplot2)
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

infile <- "output/gpp/US-Skr_GPP_halfhourly_Mar2022_Oct2022_Mar2023.csv"
plot_dir <- "output/gpp/plots"
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

dt <- fread(infile)
dt[, timestamp_mid := as.POSIXct(timestamp_mid, tz = "UTC")]
dt[, date := as.Date(timestamp_mid)]
dt[, month_label := factor(
  sprintf("%04d-%02d", year, month),
  levels = c("2022-03", "2022-10", "2023-03")
)]
dt[, time_of_day := as.POSIXct(
  sprintf("2000-01-01 %02d:%02d:00", hour, minute),
  tz = "UTC"
)]

carbon_factor <- 1800 * 12.0108 / 1e6
daily <- dt[, .(
  GPP_gC_m2_day = sum(GPP * carbon_factor, na.rm = TRUE),
  GPP_q025_gC_m2_day = sum(GPP_q025 * carbon_factor, na.rm = TRUE),
  GPP_q975_gC_m2_day = sum(GPP_q975 * carbon_factor, na.rm = TRUE),
  gapfilled_fraction = mean(NEE_gapfill_flag)
), by = .(month_label, date)]

diurnal <- dt[, .(
  GPP = mean(GPP, na.rm = TRUE),
  GPP_q025 = mean(GPP_q025, na.rm = TRUE),
  GPP_q975 = mean(GPP_q975, na.rm = TRUE)
), by = .(month_label, time_of_day)]

theme_flux <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

p_ts <- ggplot(dt, aes(timestamp_mid, GPP)) +
  geom_ribbon(aes(ymin = GPP_q025, ymax = GPP_q975), fill = "#8bb8a8", alpha = 0.35) +
  geom_line(color = "#1f6f5b", linewidth = 0.25) +
  facet_wrap(~ month_label, scales = "free_x", ncol = 1) +
  labs(
    title = "US-Skr Half-Hourly GPP",
    x = NULL,
    y = expression(GPP~(mu*mol~CO[2]~m^{-2}~s^{-1})),
    caption = "Ribbon: bootstrap 95% interval"
  ) +
  theme_flux()

p_daily <- ggplot(daily, aes(date, GPP_gC_m2_day)) +
  geom_ribbon(aes(ymin = GPP_q025_gC_m2_day, ymax = GPP_q975_gC_m2_day),
              fill = "#91a8d0", alpha = 0.35) +
  geom_col(fill = "#315f9f", width = 0.9) +
  facet_wrap(~ month_label, scales = "free_x", ncol = 1) +
  labs(
    title = "US-Skr Daily GPP Totals",
    x = NULL,
    y = expression(GPP~(gC~m^{-2}~day^{-1})),
    caption = "Bars: daily sum; ribbon: summed bootstrap 95% interval"
  ) +
  theme_flux()

p_diurnal <- ggplot(diurnal, aes(time_of_day, GPP, color = month_label, fill = month_label)) +
  geom_ribbon(aes(ymin = GPP_q025, ymax = GPP_q975), alpha = 0.18, color = NA) +
  geom_line(linewidth = 0.8) +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "3 hours") +
  labs(
    title = "US-Skr Mean Diurnal GPP",
    x = "Time of day",
    y = expression(GPP~(mu*mol~CO[2]~m^{-2}~s^{-1})),
    color = NULL,
    fill = NULL,
    caption = "Lines: monthly mean half-hourly GPP; ribbons: mean bootstrap 95% interval"
  ) +
  theme_flux()

ggsave(file.path(plot_dir, "US-Skr_GPP_halfhourly_timeseries.png"), p_ts,
       width = 11, height = 8.5, dpi = 300)
ggsave(file.path(plot_dir, "US-Skr_GPP_daily_totals.png"), p_daily,
       width = 11, height = 8.5, dpi = 300)
ggsave(file.path(plot_dir, "US-Skr_GPP_mean_diurnal_cycle.png"), p_diurnal,
       width = 10, height = 6, dpi = 300)

cat("Wrote plots to", plot_dir, "\n")
