# =============================================================================
# Chamber air temperature: how much do the options matter? (diagnostic)
#
# Compares, for every included measurement with geometry:
#   current       field-sheet reading; gaps filled by same-plot readings, then
#                 the tower calibrated per plot x campaign (04_build_auxfile.R)
#   tower_raw_fill as current, but gaps filled with the uncalibrated tower
#   tower_all     tower TA_1_1_1 for every measurement (no handheld readings)
# Flux scales with 1 / T(K), so the flux change of an option relative to
# current is (T_current + 273.15) / (T_option + 273.15) - 1.
#
# Writes output/rebuild/air_temperature_options.csv and .png.
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(ggplot2); library(patchwork)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

a <- read_csv("output/rebuild/auxfile.csv", show_col_types = FALSE) %>%
  filter(!excluded, !is.na(Area), !is.na(Tcham)) %>%
  mutate(campaign = format(date, "%b %Y"),
         tower_filled = grepl("^tower", Tcham_source),
         T_current = Tcham,
         T_tower_raw_fill = if_else(tower_filled & !is.na(tower_TA_raw), Tcham - tower_TA_bias, Tcham),
         T_tower_all = coalesce(tower_TA_raw, Tcham),
         flux_pct_tower_raw_fill = 100 * ((T_current + 273.15) / (T_tower_raw_fill + 273.15) - 1),
         flux_pct_tower_all      = 100 * ((T_current + 273.15) / (T_tower_all + 273.15) - 1))

by_plot <- a %>% group_by(campaign, plot) %>%
  summarise(n = n(), n_handheld = sum(Tcham_source == "field sheet"), n_tower_filled = sum(tower_filled),
            field_minus_tower_C = median(Tcham[Tcham_source == "field sheet"] -
                                         tower_TA_raw[Tcham_source == "field sheet"], na.rm = TRUE),
            flux_pct_tower_all_min = min(flux_pct_tower_all), flux_pct_tower_all_max = max(flux_pct_tower_all),
            flux_pct_tower_raw_fill = median(flux_pct_tower_raw_fill),
            flux_pct_tower_all = median(flux_pct_tower_all),
            .groups = "drop") %>%
  mutate(across(where(is.double), ~ round(.x, 2)))
write_csv(by_plot, "output/rebuild/air_temperature_options.csv")

cat("Measurements:", nrow(a), "| handheld readings:", sum(a$Tcham_source == "field sheet"),
    "| tower-filled:", sum(a$tower_filled), "\n")
cat("Flux change vs current (%), all measurements [10th, 50th, 90th pct]:\n")
q <- function(x) round(quantile(x, c(.1, .5, .9), na.rm = TRUE), 2)
cat("  raw tower for gaps :", q(a$flux_pct_tower_raw_fill), "\n")
cat("  tower for all      :", q(a$flux_pct_tower_all), "\n")
print(as.data.frame(by_plot), row.names = FALSE)

# ---- Figure ---------------------------------------------------------------------
ink <- "#2a6f97"; grid <- "#e6e6e6"
lab <- by_plot %>% mutate(row = paste(plot, campaign, sep = " · ")) %>%
  arrange(campaign, field_minus_tower_C) %>% mutate(row = factor(row, levels = unique(row)))
# rows without handheld readings stay as empty rows in the left panel
pts <- a %>% filter(Tcham_source == "field sheet", !is.na(tower_TA_raw)) %>%
  mutate(row = factor(paste(plot, campaign, sep = " · "), levels = levels(lab$row)),
         d = Tcham - tower_TA_raw)
theme_rb <- theme_minimal(base_size = 10) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = grid, linewidth = 0.3),
        axis.title = element_text(colour = "#444444"), plot.title = element_text(face = "bold", size = 10))
p1 <- ggplot(pts, aes(d, row)) +
  geom_vline(xintercept = 0, colour = "#999999", linewidth = 0.4) +
  geom_point(colour = ink, alpha = 0.25, size = 1.2, position = position_jitter(height = 0.15, width = 0, seed = 1)) +
  geom_point(data = lab %>% filter(!is.na(field_minus_tower_C)), aes(field_minus_tower_C, row),
             colour = ink, size = 2.6, shape = 21, fill = "white", stroke = 1) +
  scale_y_discrete(drop = FALSE) +
  labs(title = "Handheld air minus tower air", x = "deg C (dots: readings; ring: median)", y = NULL) + theme_rb
p2 <- ggplot(lab, aes(y = row)) +
  geom_vline(xintercept = 0, colour = "#999999", linewidth = 0.4) +
  geom_segment(aes(x = flux_pct_tower_all_min, xend = flux_pct_tower_all_max, yend = row),
               colour = ink, alpha = 0.35, linewidth = 1.6, lineend = "round") +
  geom_point(aes(x = flux_pct_tower_all), colour = ink, size = 2.6) +
  scale_y_discrete(drop = FALSE) +
  labs(title = "Flux change if tower air is used for all", x = "% vs current (dot: median; bar: range)", y = NULL) +
  theme_rb + theme(axis.text.y = element_blank())
ggsave("output/rebuild/air_temperature_options.png", p1 + p2 + plot_layout(widths = c(1.2, 1)),
       width = 9, height = 6, dpi = 150, bg = "white")
cat("Wrote output/rebuild/air_temperature_options.{csv,png}\n")
