#!/usr/bin/env Rscript
# Site characterization figures: depth profiles + salinity vs CH4

library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(ggpubr)

# =============================================
# Helpers
# =============================================
calc_dissolved_uM <- function(ppm, Vw = 0.180, Vg = 0.020, T = 25, P = 1, KH = 1.4e-3) {
  R <- 0.082057; TK <- T + 273.15; p <- ppm / 1e6 * P
  ((p * Vg / (R * TK)) + (KH * p * Vw)) / Vw * 1e6
}

theme_pub <- function(base_size = 11) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      axis.title = element_text(size = base_size, face = "bold"),
      axis.text = element_text(size = base_size - 1),
      strip.text = element_text(size = base_size - 1, face = "bold"),
      strip.background = element_rect(fill = "grey95", color = "grey70"),
      legend.title = element_text(size = base_size - 1, face = "bold"),
      legend.text = element_text(size = base_size - 2),
      panel.grid.minor = element_blank()
    )
}

season_colors <- c("wet (Oct 2022)" = "#4682B4", "dry (Mar 2023)" = "#D2691E", "Nov 2025" = "#9370DB")
disturbance_colors <- c("healthy" = "#228B22", "regenerating" = "#808080", "ghost" = "#8B4513")
site_disturbance <- c(BL60 = "regenerating", CP40 = "ghost", FLM30 = "ghost",
                      SRS5 = "healthy", SRS6 = "healthy", SE1 = "scrub", MI = "ghost")
core_sites <- c("BL60", "CP40", "FLM30", "SRS5", "SRS6")

# =============================================
# 1. GC data (Oct 2022 + Mar 2023)
# =============================================
cat("Loading GC data...\n")
d1c <- read_excel("data/environmental/porewater_gas/GC Run_Dec_2023_Peterman_Gewirtzman (1).xlsx",
                  sheet = "Run 1 Compiled")
d2c <- read_excel("data/environmental/porewater_gas/GC Run_Dec_2023_Peterman_Gewirtzman (1).xlsx",
                  sheet = "Run 2 Compiled")

gc <- bind_rows(d1c, d2c) %>%
  filter(Project == "Everglades") %>%
  mutate(
    CH4_ppm = as.numeric(Concentration...16),
    real_date = as.Date(as.numeric(Date...6), origin = "1899-12-30"),
    CH4_uM = calc_dissolved_uM(CH4_ppm),
    site = case_when(
      grepl("BL.?60", `Sample ID`, ignore.case = TRUE) ~ "BL60",
      grepl("^CP|CP.?4", `Sample ID`, ignore.case = TRUE) ~ "CP40",
      grepl("FLM|FML", `Sample ID`, ignore.case = TRUE) ~ "FLM30",
      grepl("SRS.?5|SRS 5", `Sample ID`) ~ "SRS5",
      grepl("SRS.?6|SRS 6", `Sample ID`) ~ "SRS6",
      grepl("^MI\\b", `Sample ID`) ~ "MI",
      grepl("SE.?1", `Sample ID`) ~ "SE1",
      TRUE ~ NA_character_),
    sample_type = case_when(
      grepl("pore|pour", `Sample ID`, ignore.case = TRUE) ~ "porewater",
      grepl("surface", `Sample ID`, ignore.case = TRUE) ~ "surface_water",
      `Sample ID` == "CP 40" ~ "porewater",
      `Sample ID` == "FML 30" ~ "surface_water",
      TRUE ~ NA_character_),
    depth_cm = case_when(
      grepl("100 cm", `Sample ID`) ~ 100,
      grepl("40 cm", `Sample ID`) ~ 40,
      grepl("15 cm", `Sample ID`) ~ 15,
      sample_type == "surface_water" ~ -5,
      sample_type == "porewater" ~ 40,
      TRUE ~ NA_real_),
    season = ifelse(real_date < as.Date("2023-01-01"), "wet (Oct 2022)", "dry (Mar 2023)")
  ) %>%
  filter(!is.na(site), !is.na(sample_type)) %>%
  select(site, season, sample_type, depth_cm, CH4_uM)

# =============================================
# 2. Nov 2025 Picarro data
# =============================================
cat("Loading Picarro data...\n")
pw <- read_csv("/Users/jongewirtzman/My Drive/Research/Blueflux/microbes/merged_porewater_all_parameters.csv",
               show_col_types = FALSE) %>%
  mutate(
    site = Site,
    season = "Nov 2025",
    sample_type = ifelse(Depth_cm == "Surface", "surface_water", "porewater"),
    depth_cm = case_when(Depth_cm == "Surface" ~ -5, TRUE ~ as.numeric(Depth_cm)),
    CH4_uM = CH4_mean_uM,
    PSU_val = PSU
  ) %>%
  select(site, season, sample_type, depth_cm, CH4_uM, PSU_val)

# =============================================
# 3. Salinity data
# =============================================
cat("Loading salinity data...\n")
sal_terr <- read_excel("data/environmental/salinity/Blueflux Salinity.xlsx",
                       sheet = "Terrestrial Data (Jon)") %>%
  mutate(
    PSU_val = as.numeric(`Salinity (PSU) - Final`),
    site = case_when(
      Location == "SRS5" ~ "SRS5", Location == "SRS6" ~ "SRS6",
      Location == "BL60" ~ "BL60", Location == "CP40" ~ "CP40",
      Location == "FLM30" ~ "FLM30", Location == "SE1" ~ "SE1",
      TRUE ~ NA_character_),
    sample_type = ifelse(grepl("Pore", `Sample Type`), "porewater", "surface_water"),
    depth_cm = case_when(
      sample_type == "surface_water" ~ -5,
      !is.na(`Depth (cm)`) ~ as.numeric(`Depth (cm)`),
      TRUE ~ 40),
    season = ifelse(grepl("2022", as.character(Date)), "wet (Oct 2022)", "dry (Mar 2023)")
  ) %>%
  filter(!is.na(site), !is.na(PSU_val)) %>%
  select(site, season, sample_type, depth_cm, PSU_val)

# =============================================
# Combine and summarize
# =============================================
ch4_all <- bind_rows(
  gc %>% select(site, season, sample_type, depth_cm, CH4_uM),
  pw %>% select(site, season, sample_type, depth_cm, CH4_uM)
)

sal_all <- bind_rows(
  sal_terr,
  pw %>% filter(!is.na(PSU_val)) %>% select(site, season, sample_type, depth_cm, PSU_val)
)

ch4_sum <- ch4_all %>%
  filter(site %in% core_sites) %>%
  group_by(site, season, depth_cm) %>%
  summarise(CH4_mean = mean(CH4_uM, na.rm = TRUE),
            CH4_sd = sd(CH4_uM, na.rm = TRUE),
            n = n(), .groups = "drop") %>%
  mutate(disturbance = site_disturbance[site])

sal_sum <- sal_all %>%
  filter(site %in% core_sites) %>%
  group_by(site, season, depth_cm) %>%
  summarise(PSU_mean = mean(PSU_val, na.rm = TRUE),
            PSU_sd = sd(PSU_val, na.rm = TRUE),
            n = n(), .groups = "drop") %>%
  mutate(disturbance = site_disturbance[site])

cat("CH4 summary:", nrow(ch4_sum), "rows\n")
cat("Salinity summary:", nrow(sal_sum), "rows\n")

# =============================================
# LAYOUT 1: Paired line profiles
# =============================================
cat("Plotting layout 1: paired line profiles...\n")

p1_ch4 <- ch4_sum %>%
  ggplot(aes(x = CH4_mean, y = depth_cm, color = season, shape = season)) +
  geom_point(size = 2.5) +
  geom_path(aes(group = season), linewidth = 0.7) +
  geom_errorbarh(aes(xmin = pmax(0, CH4_mean - CH4_sd), xmax = CH4_mean + CH4_sd),
                 height = 3, alpha = 0.4) +
  facet_wrap(~ site, nrow = 1, scales = "free_x") +
  scale_y_reverse() +
  scale_color_manual(values = season_colors, name = "Campaign") +
  scale_shape_manual(values = c(16, 17, 15), name = "Campaign") +
  labs(x = expression(Dissolved~CH[4]~(mu*M)), y = "Depth (cm)", tag = "a") +
  theme_pub(base_size = 10) +
  theme(legend.position = "none")

p1_sal <- sal_sum %>%
  ggplot(aes(x = PSU_mean, y = depth_cm, color = season, shape = season)) +
  geom_point(size = 2.5) +
  geom_path(aes(group = season), linewidth = 0.7) +
  geom_errorbarh(aes(xmin = pmax(0, PSU_mean - PSU_sd), xmax = PSU_mean + PSU_sd),
                 height = 3, alpha = 0.4) +
  facet_wrap(~ site, nrow = 1, scales = "free_x") +
  scale_y_reverse() +
  scale_color_manual(values = season_colors, name = "Campaign") +
  scale_shape_manual(values = c(16, 17, 15), name = "Campaign") +
  labs(x = "Salinity (PSU)", y = "Depth (cm)", tag = "b") +
  theme_pub(base_size = 10) +
  theme(legend.position = "bottom")

layout1 <- ggarrange(p1_ch4, p1_sal, ncol = 1, heights = c(1, 1.15))
ggsave("output/figures/pub_SI_depth_profiles_lines.pdf", layout1, width = 12, height = 7)
ggsave("output/figures/pub_SI_depth_profiles_lines.png", layout1, width = 12, height = 7, dpi = 300)
cat("  Saved layout 1\n")

# =============================================
# LAYOUT 2: Facet grid (site x variable)
# =============================================
cat("Plotting layout 2: facet grid...\n")

ch4_long <- ch4_sum %>% mutate(variable = "CH4 (uM)", value = CH4_mean)
sal_long <- sal_sum %>% mutate(variable = "Salinity (PSU)", value = PSU_mean)
both_long <- bind_rows(ch4_long %>% select(site, season, depth_cm, variable, value, disturbance),
                       sal_long %>% select(site, season, depth_cm, variable, value, disturbance))

p2 <- both_long %>%
  ggplot(aes(x = value, y = depth_cm, color = season, shape = season)) +
  geom_point(size = 2) +
  geom_path(aes(group = season), linewidth = 0.6) +
  facet_grid(site ~ variable, scales = "free_x") +
  scale_y_reverse() +
  scale_color_manual(values = season_colors, name = "Campaign") +
  scale_shape_manual(values = c(16, 17, 15), name = "Campaign") +
  labs(x = NULL, y = "Depth (cm)") +
  theme_pub(base_size = 10) +
  theme(legend.position = "bottom")

ggsave("output/figures/pub_SI_depth_profiles_grid.pdf", p2, width = 7, height = 10)
ggsave("output/figures/pub_SI_depth_profiles_grid.png", p2, width = 7, height = 10, dpi = 300)
cat("  Saved layout 2\n")

# =============================================
# LAYOUT 3: Heatmap tiles
# =============================================
cat("Plotting layout 3: heatmap...\n")

ch4_heat <- ch4_sum %>%
  mutate(site_depth = paste0(site, " (", depth_cm, " cm)"),
         site_depth = factor(site_depth, levels = rev(sort(unique(site_depth))))) %>%
  ggplot(aes(x = season, y = site_depth, fill = log10(CH4_mean + 0.01))) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(CH4_mean, 1)), size = 2.5) +
  scale_fill_viridis_c(name = expression(log[10]~CH[4]~(mu*M)), option = "inferno") +
  labs(x = NULL, y = NULL, title = expression(Dissolved~CH[4]~(mu*M))) +
  theme_pub(base_size = 10) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

sal_heat <- sal_sum %>%
  mutate(site_depth = paste0(site, " (", depth_cm, " cm)"),
         site_depth = factor(site_depth, levels = rev(sort(unique(site_depth))))) %>%
  ggplot(aes(x = season, y = site_depth, fill = PSU_mean)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(PSU_mean, 1)), size = 2.5) +
  scale_fill_viridis_c(name = "PSU", option = "mako", direction = -1) +
  labs(x = NULL, y = NULL, title = "Salinity (PSU)") +
  theme_pub(base_size = 10) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

layout3 <- ggarrange(ch4_heat, sal_heat, ncol = 2, widths = c(1, 1))
ggsave("output/figures/pub_SI_depth_profiles_heatmap.pdf", layout3, width = 14, height = 8)
ggsave("output/figures/pub_SI_depth_profiles_heatmap.png", layout3, width = 14, height = 8, dpi = 300)
cat("  Saved layout 3\n")

# =============================================
# LAYOUT 4: Bubble plot (sites as columns, time as x)
# =============================================
cat("Plotting layout 4: bubbles...\n")

season_short_map <- c("wet (Oct 2022)" = "Oct 22", "dry (Mar 2023)" = "Mar 23", "Nov 2025" = "Nov 25")
season_short_levels <- c("Oct 22", "Mar 23", "Nov 25")

# Order sites by mean salinity (lowest to highest)
site_sal_order <- sal_sum %>%
  group_by(site) %>%
  summarise(mean_sal = mean(PSU_mean, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_sal) %>%
  pull(site)

# Order seasons chronologically
season_chron <- c("wet (Oct 2022)", "dry (Mar 2023)", "Nov 2025")

ch4_sum_bubble <- ch4_sum %>%
  filter(depth_cm >= 0) %>%
  mutate(site = factor(site, levels = site_sal_order),
         season = factor(season, levels = season_chron))
sal_sum_bubble <- sal_sum %>%
  filter(depth_cm >= 0) %>%
  mutate(site = factor(site, levels = site_sal_order),
         season = factor(season, levels = season_chron))

p4_ch4 <- ch4_sum_bubble %>%
  mutate(season_short = factor(season_short_map[season], levels = season_short_levels)) %>%
  ggplot(aes(x = season_short, y = depth_cm, size = CH4_mean, color = CH4_mean)) +
  geom_point() +
  facet_wrap(~ site, nrow = 1) +
  scale_y_reverse() +
  scale_size_continuous(range = c(1, 8), name = expression(CH[4]~(mu*M))) +
  scale_color_gradient(low = "blue", high = "red", name = expression(CH[4]~(mu*M))) +
  labs(x = NULL, y = "Depth (cm)", tag = "a") +
  theme_pub(base_size = 10) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))

p4_sal <- sal_sum_bubble %>%
  mutate(season_short = factor(season_short_map[season], levels = season_short_levels)) %>%
  ggplot(aes(x = season_short, y = depth_cm, size = PSU_mean, color = PSU_mean)) +
  geom_point() +
  facet_wrap(~ site, nrow = 1) +
  scale_y_reverse() +
  scale_size_continuous(range = c(1, 8), name = "PSU") +
  scale_color_gradient(low = "blue", high = "red", name = "PSU") +
  labs(x = NULL, y = "Depth (cm)", tag = "b") +
  theme_pub(base_size = 10) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))

layout4 <- ggarrange(p4_ch4, p4_sal, ncol = 1)
ggsave("output/figures/pub_SI_depth_profiles_bubbles.pdf", layout4, width = 12, height = 8)
ggsave("output/figures/pub_SI_depth_profiles_bubbles.png", layout4, width = 12, height = 8, dpi = 300)
cat("  Saved layout 4\n")

# =============================================
# SALINITY vs CH4 scatter
# =============================================
cat("Plotting salinity vs CH4...\n")

merged <- ch4_sum %>%
  inner_join(sal_sum %>% select(site, season, depth_cm, PSU_mean),
             by = c("site", "season", "depth_cm"))

# Compute per-class Spearman stats manually (on raw values, matching geom_smooth)
# Use Pearson to match geom_smooth(method="lm") trendlines
# Filter to rows that have BOTH CH4 and PSU (non-NA)
merged_complete <- merged %>% filter(!is.na(PSU_mean), !is.na(CH4_mean))

scatter_stats <- merged_complete %>%
  group_by(disturbance) %>%
  summarise(
    n = n(),
    r = cor(PSU_mean, log1p(CH4_mean), use = "complete.obs"),
    p_val = cor.test(PSU_mean, log1p(CH4_mean))$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    p_label = ifelse(p_val < 0.001, "italic(p) < 0.001",
                     sprintf("italic(p) == %.3f", p_val)),
    label = sprintf("italic(r) == %.3f*','~%s*','~italic(n) == %d",
                    r, p_label, n)
  )

# Position labels in top right, stacked by class
scatter_stats <- scatter_stats %>%
  arrange(disturbance) %>%
  mutate(y_npc = seq(0.98, by = -0.07, length.out = n()))

p_scatter <- merged_complete %>%
  ggplot(aes(x = PSU_mean, y = CH4_mean, color = disturbance, shape = season)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(aes(group = disturbance, fill = disturbance),
              method = "lm", se = TRUE, alpha = 0.15, linewidth = 0.7) +
  scale_y_continuous(trans = "log1p", breaks = c(0, 1, 5, 10, 25, 50, 100)) +
  scale_color_manual(values = disturbance_colors, name = "Disturbance") +
  scale_fill_manual(values = disturbance_colors, guide = "none") +
  scale_shape_manual(values = c(16, 17, 15), name = "Campaign") +
  geom_text(data = scatter_stats,
            aes(x = Inf, y = Inf, label = label, color = disturbance),
            inherit.aes = FALSE, parse = TRUE,
            hjust = 1.05, vjust = c(1.2, 2.5, 3.8), size = 3) +
  scale_color_manual(values = disturbance_colors, name = "Disturbance") +
  labs(x = "Salinity (PSU)",
       y = expression(Dissolved~CH[4]~(mu*M))) +
  theme_pub(base_size = 11)

ggsave("output/figures/pub_SI_salinity_vs_ch4.pdf", p_scatter, width = 8, height = 6)
ggsave("output/figures/pub_SI_salinity_vs_ch4.png", p_scatter, width = 8, height = 6, dpi = 300)
cat("  Saved salinity vs CH4 scatter\n")

# Faceted by site
p_scatter_site <- merged %>%
  ggplot(aes(x = PSU_mean, y = CH4_mean, color = season, shape = season)) +
  geom_point(size = 3, alpha = 0.8) +
  facet_wrap(~ site, scales = "free", nrow = 1) +
  scale_color_manual(values = season_colors, name = "Campaign") +
  scale_shape_manual(values = c(16, 17, 15), name = "Campaign") +
  labs(x = "Salinity (PSU)", y = expression(Dissolved~CH[4]~(mu*M))) +
  theme_pub(base_size = 10) +
  theme(legend.position = "bottom")

ggsave("output/figures/pub_SI_salinity_vs_ch4_bysite.pdf", p_scatter_site, width = 12, height = 4)
ggsave("output/figures/pub_SI_salinity_vs_ch4_bysite.png", p_scatter_site, width = 12, height = 4, dpi = 300)
cat("  Saved salinity vs CH4 by site\n")

# =============================================
# COMBINED SI FIGURE: Bubbles (a,b) + Scatter (c)
# =============================================
cat("Plotting combined SI figure...\n")

# Rebuild bubble plots with updated tags
p_comb_ch4 <- ch4_sum_bubble %>%
  mutate(season_short = factor(season_short_map[season], levels = season_short_levels)) %>%
  ggplot(aes(x = season_short, y = depth_cm, size = CH4_mean, color = CH4_mean)) +
  geom_point() +
  facet_wrap(~ site, nrow = 1) +
  scale_y_reverse() +
  scale_size_continuous(range = c(1, 8), name = expression(CH[4]~(mu*M))) +
  scale_color_gradient(low = "blue", high = "red", name = expression(CH[4]~(mu*M))) +
  guides(size = guide_legend(), color = guide_colorbar()) +
  labs(x = NULL, y = "Depth (cm)", tag = "a") +
  theme_pub(base_size = 10) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))

p_comb_sal <- sal_sum_bubble %>%
  mutate(season_short = factor(season_short_map[season], levels = season_short_levels)) %>%
  ggplot(aes(x = season_short, y = depth_cm, size = PSU_mean, color = PSU_mean)) +
  geom_point() +
  facet_wrap(~ site, nrow = 1) +
  scale_y_reverse() +
  scale_size_continuous(range = c(1, 8), name = "PSU") +
  scale_color_gradient(low = "blue", high = "red", name = "PSU") +
  guides(size = guide_legend(), color = guide_colorbar()) +
  labs(x = NULL, y = "Depth (cm)", tag = "b") +
  theme_pub(base_size = 10) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))

p_comb_scatter <- merged_complete %>%
  ggplot(aes(x = PSU_mean, y = CH4_mean, color = disturbance, shape = season)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(aes(group = disturbance, fill = disturbance),
              method = "lm", se = TRUE, alpha = 0.15, linewidth = 0.7) +
  scale_y_continuous(trans = "log1p", breaks = c(0, 1, 5, 10, 25, 50, 100)) +
  scale_color_manual(values = disturbance_colors, name = "Disturbance") +
  scale_fill_manual(values = disturbance_colors, guide = "none") +
  scale_shape_manual(values = c(16, 17, 15), name = "Campaign") +
  geom_text(data = scatter_stats,
            aes(x = Inf, y = Inf, label = label, color = disturbance),
            inherit.aes = FALSE, parse = TRUE,
            hjust = 1.05, vjust = c(1.2, 2.5, 3.8), size = 3) +
  labs(x = "Salinity (PSU)",
       y = expression(Dissolved~CH[4]~(mu*M)),
       tag = "c") +
  theme_pub(base_size = 10)

combined_si <- ggarrange(
  ggarrange(p_comb_ch4, p_comb_sal, ncol = 1, heights = c(1, 1)),
  p_comb_scatter,
  ncol = 1, heights = c(2, 1.3)
)

ggsave("output/figures/pub_SI_porewater_characterization.pdf", combined_si,
       width = 12, height = 14)
ggsave("output/figures/pub_SI_porewater_characterization.png", combined_si,
       width = 12, height = 14, dpi = 300)
cat("  Saved combined SI figure\n")

cat("\n=== DONE ===\n")
