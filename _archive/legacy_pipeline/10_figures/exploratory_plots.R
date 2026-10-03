# =============================================================================
# Exploratory Plots — Tree Flux Relationships
# =============================================================================
# Quick exploratory figures to evaluate candidate relationships for publication.
# Saves PNGs to output/figures/other/exploratory/
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(forcats)

# --- Data Loading (reuse same preprocessing as publication_figures.R) --------
df <- read.csv("output/data_products/combined_gas_flux_dataset.csv")

selected_plots <- c("BL60", "CP40", "FLM30", "MI", "SE1", "SRS5", "SRS6", "RB10")

df <- df %>%
  filter(plot %in% selected_plots) %>%
  filter(is.na(CO2_best.flux) | CO2_best.flux >= -10) %>%
  mutate(
    disturbance_level = factor(disturbance_level,
                               levels = c("healthy", "regenerating", "ghost", "scrub")),
    season_agg = factor(case_when(
      season == "wet" ~ "Wet", season == "dry" ~ "Dry", TRUE ~ NA_character_
    ), levels = c("Wet", "Dry")),
    component = tolower(component),
    plot = factor(plot, levels = c("RB10", "SRS5", "SRS6", "BL60",
                                    "CP40", "FLM30", "MI", "SE1")),
    species = ifelse(species == "COPE", "COER", species)
  )

dir.create("output/figures/other/exploratory", showWarnings = FALSE, recursive = TRUE)

# Shared theme
theme_expl <- function(base_size = 10) {
  theme_bw(base_size = base_size) +
    theme(panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "grey95"),
          plot.title = element_text(size = base_size + 1, face = "bold"))
}

disturbance_colors <- c("healthy" = "#2D6A4F", "regenerating" = "#95D5B2",
                         "ghost" = "#BC6C25", "scrub" = "#606C38")

cat("Data loaded:", nrow(df), "rows\n\n")


# =============================================================================
# 1. CH4 vs CO2 — all components, colored by component
# =============================================================================
cat("--- 1. CH4 vs CO2 ---\n")

p1_data <- df %>%
  filter(!is.na(CH4_best.flux), !is.na(CO2_best.flux))

component_colors <- c("soil" = "#8B4513", "stem" = "#228B22", "water" = "#4682B4",
                       "root" = "#D2691E", "cwd" = "#654321",
                       "pneumatophore" = "#32CD32", "leaves" = "#90EE90")

p1 <- p1_data %>%
  ggplot(aes(x = CO2_best.flux, y = CH4_best.flux, color = component)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, alpha = 0.7) +
  scale_x_continuous(trans = "asinh",
                     breaks = c(-10, 0, 10, 100, 1000),
                     labels = c("-10", "0", "10", "100", "1000")) +
  scale_y_continuous(trans = "asinh",
                     breaks = c(-1, 0, 1, 10, 100, 1000),
                     labels = c("-1", "0", "1", "10", "100", "1000")) +
  scale_color_manual(values = component_colors, name = "Component") +
  labs(x = expression(CO[2]~Flux~(mu*mol~m^{-2}~s^{-1})),
       y = expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1})),
       title = "CH4 vs CO2 flux — all components (trendlines by component)") +
  theme_expl() +
  facet_wrap(~ disturbance_level, nrow = 1)

ggsave("output/figures/other/exploratory/expl_ch4_vs_co2.png", p1,
       width = 12, height = 4, dpi = 200)
cat("Saved: expl_ch4_vs_co2.png\n")


# =============================================================================
# 2. CH4 by stem diameter
# =============================================================================
cat("\n--- 2. CH4 vs stem diameter ---\n")

p2_data <- df %>%
  filter(component == "stem",
         !is.na(CH4_best.flux), !is.na(diameter), diameter > 0)

cat("  n =", nrow(p2_data), "stem measurements with diameter\n")
cat("  diameter range:", range(p2_data$diameter), "\n")

p2 <- p2_data %>%
  ggplot(aes(x = diameter, y = CH4_best.flux, color = disturbance_level)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "loess", se = TRUE, color = "black", linewidth = 0.8) +
  scale_y_continuous(trans = "asinh",
                     breaks = c(-1, 0, 1, 10, 100, 1000),
                     labels = c("-1", "0", "1", "10", "100", "1000")) +
  scale_color_manual(values = disturbance_colors, name = "Disturbance") +
  labs(x = "Stem diameter at measurement point (cm)",
       y = expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1})),
       title = "Stem CH4 flux vs diameter") +
  theme_expl()

# Also faceted by species
p2b <- p2_data %>%
  filter(species %in% c("AVGE", "RHMA", "LARA", "COER")) %>%
  ggplot(aes(x = diameter, y = CH4_best.flux, color = disturbance_level)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "loess", se = TRUE, color = "black", linewidth = 0.8) +
  scale_y_continuous(trans = "asinh",
                     breaks = c(-1, 0, 1, 10, 100, 1000),
                     labels = c("-1", "0", "1", "10", "100", "1000")) +
  scale_color_manual(values = disturbance_colors, name = "Disturbance") +
  facet_wrap(~ species, scales = "free_x") +
  labs(x = "Stem diameter at measurement point (cm)",
       y = expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1})),
       title = "Stem CH4 flux vs diameter — by species") +
  theme_expl()

ggsave("output/figures/other/exploratory/expl_ch4_vs_diameter.png", p2,
       width = 8, height = 5, dpi = 200)
ggsave("output/figures/other/exploratory/expl_ch4_vs_diameter_species.png", p2b,
       width = 10, height = 6, dpi = 200)
cat("Saved: expl_ch4_vs_diameter.png, expl_ch4_vs_diameter_species.png\n")


# =============================================================================
# 3. Root vs stem flux per plot × season (RHMA only)
# =============================================================================
cat("\n--- 3. Root vs stem (RHMA) ---\n")

p3_data <- df %>%
  filter(component %in% c("stem", "root"),
         species == "RHMA",
         !is.na(CH4_best.flux)) %>%
  mutate(component = factor(component, levels = c("root", "stem")))

cat("  Root RHMA:", sum(p3_data$component == "root"), "\n")
cat("  Stem RHMA:", sum(p3_data$component == "stem"), "\n")
cat("  Plots with root data:", paste(unique(p3_data$plot[p3_data$component == "root"]),
                                      collapse = ", "), "\n")

# Only keep plots that have root data
plots_with_roots <- p3_data %>%
  filter(component == "root") %>%
  pull(plot) %>% unique()

p3_data <- p3_data %>% filter(plot %in% plots_with_roots)

p3 <- p3_data %>%
  ggplot(aes(x = component, y = CH4_best.flux, fill = component)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_jitter(aes(color = component), width = 0.15, alpha = 0.4, size = 1.5) +
  scale_y_continuous(trans = "asinh",
                     breaks = c(-1, 0, 1, 10, 100, 1000),
                     labels = c("-1", "0", "1", "10", "100", "1000")) +
  scale_fill_manual(values = c("root" = "#D2691E", "stem" = "#228B22"), guide = "none") +
  scale_color_manual(values = c("root" = "#D2691E", "stem" = "#228B22"), guide = "none") +
  facet_grid(season_agg ~ plot) +
  labs(x = NULL,
       y = expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1})),
       title = "Root vs stem CH4 flux — R. mangle only (plots with root data)") +
  theme_expl() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("output/figures/other/exploratory/expl_root_vs_stem_rhma.png", p3,
       width = 12, height = 5, dpi = 200)
cat("Saved: expl_root_vs_stem_rhma.png\n")


# =============================================================================
# 4. Soil CH4 and CO2 vs pneumatophore density
# =============================================================================
cat("\n--- 4. Soil flux vs pneumatophore density ---\n")

p4_data <- df %>%
  filter(component %in% c("soil", "water"),
         !is.na(pneumatophore_density))

cat("  n =", nrow(p4_data), "measurements with pneumatophore_density\n")
cat("  Density range:", range(p4_data$pneumatophore_density, na.rm = TRUE), "\n")

# CH4
p4a <- p4_data %>%
  filter(!is.na(CH4_best.flux)) %>%
  ggplot(aes(x = pneumatophore_density, y = CH4_best.flux,
             color = disturbance_level, shape = component)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  scale_y_continuous(trans = "asinh",
                     breaks = c(-10, 0, 10, 100, 1000),
                     labels = c("-10", "0", "10", "100", "1000")) +
  scale_color_manual(values = disturbance_colors, name = "Disturbance") +
  scale_shape_manual(values = c("soil" = 16, "water" = 17), name = "Surface") +
  labs(x = expression(Pneumatophore~density~(m^{-2})),
       y = expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1})),
       title = expression(Soil/water~CH[4]~vs~pneumatophore~density)) +
  theme_expl()

# CO2
p4b <- p4_data %>%
  filter(!is.na(CO2_best.flux)) %>%
  ggplot(aes(x = pneumatophore_density, y = CO2_best.flux,
             color = disturbance_level, shape = component)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  scale_color_manual(values = disturbance_colors, name = "Disturbance") +
  scale_shape_manual(values = c("soil" = 16, "water" = 17), name = "Surface") +
  labs(x = expression(Pneumatophore~density~(m^{-2})),
       y = expression(CO[2]~Flux~(mu*mol~m^{-2}~s^{-1})),
       title = expression(Soil/water~CO[2]~vs~pneumatophore~density)) +
  theme_expl()

library(patchwork)
p4 <- p4a + theme(legend.position = "none") +
  p4b + theme(legend.position = "right") +
  plot_layout(ncol = 1)

ggsave("output/figures/other/exploratory/expl_flux_vs_pneumatophore_density.png", p4,
       width = 9, height = 8, dpi = 200)
cat("Saved: expl_flux_vs_pneumatophore_density.png\n")


# =============================================================================
# 5. CH4 by above water vs above sediment
# =============================================================================
cat("\n--- 5. CH4 by above water vs sediment ---\n")

p5_data <- df %>%
  filter(component %in% c("stem", "root"),
         !is.na(CH4_best.flux),
         !is.na(above), above != "") %>%
  mutate(above = factor(above, levels = c("sediment", "water")))

cat("  Above sediment:", sum(p5_data$above == "sediment"), "\n")
cat("  Above water:", sum(p5_data$above == "water"), "\n")

# Overall
p5a <- p5_data %>%
  ggplot(aes(x = above, y = CH4_best.flux, fill = above)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_jitter(alpha = 0.3, width = 0.15, size = 1) +
  scale_y_continuous(trans = "asinh",
                     breaks = c(-1, 0, 1, 10, 100, 1000),
                     labels = c("-1", "0", "1", "10", "100", "1000")) +
  scale_fill_manual(values = c("sediment" = "#C4A882", "water" = "#4682B4"),
                    guide = "none") +
  labs(x = "Measurement above...", y = expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1})),
       title = "Stem + root CH4: above sediment vs water") +
  theme_expl()

# By component and disturbance
p5b <- p5_data %>%
  ggplot(aes(x = above, y = CH4_best.flux, fill = above)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_jitter(alpha = 0.3, width = 0.15, size = 1) +
  scale_y_continuous(trans = "asinh",
                     breaks = c(-1, 0, 1, 10, 100, 1000),
                     labels = c("-1", "0", "1", "10", "100", "1000")) +
  scale_fill_manual(values = c("sediment" = "#C4A882", "water" = "#4682B4"),
                    guide = "none") +
  facet_grid(component ~ disturbance_level) +
  labs(x = "Measurement above...", y = expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1})),
       title = "CH4 by inundation — component × disturbance") +
  theme_expl()

ggsave("output/figures/other/exploratory/expl_ch4_above_water_vs_sediment.png", p5a,
       width = 5, height = 5, dpi = 200)
ggsave("output/figures/other/exploratory/expl_ch4_above_water_vs_sediment_faceted.png", p5b,
       width = 10, height = 5, dpi = 200)
cat("Saved: expl_ch4_above_water_vs_sediment.png, _faceted.png\n")

cat("\n===== All exploratory plots saved to output/figures/other/exploratory/ =====\n")
