# =============================================================================
# Publication Figures: Porewater Depth Profiles & Geochemistry
# =============================================================================
# Data source: merged_porewater_all_parameters.csv from microbes/ directory
# Sites: SRS5, SRS6 (healthy), BL60 (regenerating), CP40 (ghost)
# Depths: Surface, 0, 15, 45, 90 cm
#
# Variables: ORP, pH, DO, sulfide, iron, SO4, Cl, NO3, PO4, DOC, alkalinity,
#            dissolved CH4/CO2, d13C-CH4/CO2
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(patchwork)
library(forcats)
library(ggrepel)
library(scales)

# --- Shared Definitions (match publication_figures.R) -------------------------

disturbance_colors <- c(
  "healthy" = "#228B22", "regenerating" = "#808080",
  "ghost" = "#8B4513", "scrub" = "#DAA520"
)

site_colors <- c(
  "SRS5" = "#228B22", "SRS6" = "#2E8B57",
  "BL60" = "#808080", "CP40" = "#8B4513"
)

theme_pub <- function(base_size = 11) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      axis.title        = element_text(size = base_size, face = "bold"),
      axis.text         = element_text(size = base_size - 1),
      strip.text        = element_text(size = base_size - 1, face = "bold"),
      strip.background  = element_rect(fill = "grey95", color = "grey70"),
      legend.title      = element_text(size = base_size - 1, face = "bold"),
      legend.text       = element_text(size = base_size - 2),
      panel.grid.minor  = element_blank(),
      plot.tag          = element_text(size = base_size + 3, face = "bold"),
      plot.title        = element_text(size = base_size + 1, face = "bold"),
      plot.subtitle     = element_text(size = base_size - 1, color = "grey40")
    )
}

save_pub <- function(plot, name, width, height, units = "mm") {
  ggsave(paste0("output/figures/pub_", name, ".pdf"), plot,
         width = width, height = height, units = units)
  ggsave(paste0("output/figures/pub_", name, ".png"), plot,
         width = width, height = height, units = units, dpi = 300)
  cat("Saved: pub_", name, ".pdf/.png\n", sep = "")
}


# =============================================================================
# LOAD DATA
# =============================================================================
cat("--- Loading merged porewater data ---\n")

microbes_path <- "/Users/jongewirtzman/My Drive/Research/Blueflux/microbes/"
df <- read_csv(paste0(microbes_path, "merged_porewater_all_parameters.csv"),
               show_col_types = FALSE)

# Add disturbance classification and clean up
df <- df %>%
  mutate(
    disturbance = case_when(
      Site %in% c("SRS5", "SRS6") ~ "healthy",
      Site == "BL60" ~ "regenerating",
      Site == "CP40" ~ "ghost"
    ),
    disturbance = factor(disturbance, levels = c("healthy", "regenerating", "ghost")),
    Site = factor(Site, levels = c("SRS5", "SRS6", "BL60", "CP40")),
    site_label = paste0(Site, " (", disturbance, ")"),
    Depth_cm = factor(Depth_cm, levels = c("Surface", "0", "15", "45", "90")),
    Depth_numeric = case_when(
      Depth_cm == "Surface" ~ -5,
      Depth_cm == "0" ~ 0,
      Depth_cm == "15" ~ 15,
      Depth_cm == "45" ~ 45,
      Depth_cm == "90" ~ 90
    )
  )

cat("  Loaded:", nrow(df), "rows,", n_distinct(df$Site), "sites\n")
cat("  Variables:", ncol(df), "columns\n")


# =============================================================================
# FIGURE 1: Redox & Geochemistry Depth Profiles (multi-panel)
# ORP, pH, sulfide, dissolved O2 as depth profiles by site
# =============================================================================
cat("\n--- Figure 1: Redox & Geochemistry Depth Profiles ---\n")

# Helper for depth profile panels
# x = depth (becomes y after coord_flip), y = response variable
# Lines connect means by depth within each site
make_depth_panel <- function(data, var, y_lab, tag_label, log_scale = FALSE) {
  d <- data %>% filter(!is.na(.data[[var]])) %>% mutate(Site = droplevels(Site))

  # Compute means by site × depth (for line connections)
  d_means <- d %>%
    group_by(Site, Depth_numeric) %>%
    summarise(mean_val = mean(.data[[var]], na.rm = TRUE), .groups = "drop")

  p <- ggplot() +
    # Lines connecting means by depth
    geom_line(data = d_means,
              aes(x = Depth_numeric, y = mean_val, color = Site, group = Site),
              linewidth = 0.7, alpha = 0.7) +
    # Individual points
    geom_point(data = d,
               aes(x = Depth_numeric, y = .data[[var]], color = Site, shape = Site),
               size = 3) +
    scale_x_continuous(breaks = c(-5, 0, 15, 45, 90),
                       labels = c("Surf.", "0", "15", "45", "90"),
                       trans = "reverse") +
    scale_color_manual(values = site_colors, name = "Site") +
    scale_shape_manual(values = c("SRS5" = 16, "SRS6" = 17, "BL60" = 15, "CP40" = 18),
                       name = "Site") +
    labs(y = y_lab, x = "Depth (cm)", tag = tag_label) +
    coord_flip() +
    theme_pub(base_size = 10) +
    theme(legend.position = "none")
  if (log_scale) p <- p + scale_y_log10()
  p
}

fig1a <- make_depth_panel(df, "ORP", "ORP (mV)", "(a)")
fig1b <- make_depth_panel(df, "pH", "pH", "(b)")
fig1c <- make_depth_panel(df, "Sulfide", expression(Sulfide~(mg~L^{-1})), "(c)")
fig1d <- make_depth_panel(df, "ppmDO",  expression(DO~(mg~L^{-1})), "(d)")

# Shared legend from one panel
fig1_legend <- df %>%
  filter(!is.na(ORP)) %>%
  ggplot(aes(x = Depth_numeric, y = ORP, color = Site, shape = Site)) +
  geom_point(size = 3) +
  scale_color_manual(values = site_colors, name = "Site") +
  scale_shape_manual(values = c("SRS5" = 16, "SRS6" = 17, "BL60" = 15, "CP40" = 18),
                     name = "Site") +
  theme_pub() +
  theme(legend.position = "bottom")
legend_grob <- cowplot::get_legend(fig1_legend)

fig1 <- (fig1a | fig1b) / (fig1c | fig1d) /
  patchwork::wrap_elements(legend_grob) +
  plot_layout(heights = c(1, 1, 0.1))

save_pub(fig1, "soil_redox_profiles", width = 220, height = 200)


# =============================================================================
# FIGURE 2: Dissolved Carbon Depth Profiles
# DOC, alkalinity, dissolved CH4, dissolved CO2
# =============================================================================
cat("\n--- Figure 2: Dissolved Carbon Depth Profiles ---\n")

fig2a <- make_depth_panel(df, "DOC_mg_L", expression(DOC~(mg~L^{-1})), "(a)")
fig2b <- make_depth_panel(df, "Alkalinity_uM", expression(Alkalinity~(mu*M)), "(b)")
fig2c <- make_depth_panel(df, "CH4_mean_uM", expression(Dissolved~CH[4]~(mu*M)), "(c)")
fig2d <- make_depth_panel(df, "CO2_mean_uM", expression(Dissolved~CO[2]~(mu*M)), "(d)")

fig2 <- (fig2a | fig2b) / (fig2c | fig2d) /
  patchwork::wrap_elements(legend_grob) +
  plot_layout(heights = c(1, 1, 0.1))

save_pub(fig2, "soil_carbon_profiles", width = 220, height = 200)


# =============================================================================
# FIGURE 3: Isotope Depth Profiles (d13C-CH4 and d13C-CO2)
# =============================================================================
cat("\n--- Figure 3: Isotope Depth Profiles ---\n")

fig3a <- make_depth_panel(df, "d13C_CH4_mean",
                          expression(delta^{13}*C-CH[4]~("\u2030")), "(a)")
fig3b <- make_depth_panel(df, "d13C_CO2_mean",
                          expression(delta^{13}*C-CO[2]~("\u2030")), "(b)")

fig3 <- (fig3a | fig3b) /
  patchwork::wrap_elements(legend_grob) +
  plot_layout(heights = c(1, 0.08))

save_pub(fig3, "soil_isotope_profiles", width = 220, height = 130)


# =============================================================================
# FIGURE 4: Anion Depth Profiles (SO4, Cl, NO3, PO4)
# =============================================================================
cat("\n--- Figure 4: Anion Depth Profiles ---\n")

fig4a <- make_depth_panel(df, "SO4_ppm",  expression(SO[4]^{"2-"}~(ppm)), "(a)")
fig4b <- make_depth_panel(df, "Cl_ppm",   expression(Cl^{"-"}~(ppm)), "(b)")
fig4c <- make_depth_panel(df, "NO3_N_ppm", expression(NO[3]*"-N"~(ppm)), "(c)")
fig4d <- make_depth_panel(df, "PO4_P_ppm", expression(PO[4]*"-P"~(ppm)), "(d)")

fig4 <- (fig4a | fig4b) / (fig4c | fig4d) /
  patchwork::wrap_elements(legend_grob) +
  plot_layout(heights = c(1, 1, 0.1))

save_pub(fig4, "soil_anion_profiles", width = 220, height = 200)


# =============================================================================
# FIGURE 5: PCA of Porewater Geochemistry
# =============================================================================
cat("\n--- Figure 5: PCA of Porewater Geochemistry ---\n")

# Select numeric columns for PCA, exclude metadata and SD columns
remove_cols <- c("Lat", "Long", "Depth_numeric", "SpCond", "TempC",
                 "n_replicates", "Tds ppt", "ppmDO", "Br_ppm", "F_ppm")
sd_cols <- names(df)[grepl("_sd$|_sd_", names(df))]
co2_cols <- names(df)[grepl("CO2", names(df))]

pca_vars <- df %>%
  select(where(is.numeric)) %>%
  select(-any_of(c(remove_cols, sd_cols, co2_cols)))

# Keep only variables with <= 20% missing
keep_vars <- names(pca_vars)[colMeans(is.na(pca_vars)) <= 0.20]

pca_df <- df %>%
  select(Site, Depth_cm, Depth_numeric, disturbance, all_of(keep_vars)) %>%
  drop_na()

if (nrow(pca_df) >= 3 && length(keep_vars) >= 2) {
  pca_mat <- pca_df %>% select(all_of(keep_vars))
  pca_res <- prcomp(pca_mat, center = TRUE, scale. = TRUE)

  # Scores
  scores <- as.data.frame(pca_res$x[, 1:2])
  scores <- bind_cols(pca_df %>% select(Site, Depth_cm, Depth_numeric, disturbance), scores)

  var_exp <- summary(pca_res)$importance[2, 1:2] * 100

  # Loadings (scaled for biplot)
  load_scale <- 4
  loadings <- as.data.frame(pca_res$rotation[, 1:2]) %>%
    mutate(variable = rownames(pca_res$rotation),
           PC1 = PC1 * load_scale,
           PC2 = PC2 * load_scale)

  # Clean variable names for display
  loadings <- loadings %>%
    mutate(var_display = case_when(
      variable == "ORP" ~ "ORP",
      variable == "pH" ~ "pH",
      variable == "%DO" ~ "%DO",
      variable == "PSU" ~ "Salinity",
      variable == "Sulfide" ~ "Sulfide",
      variable == "Total Iron" ~ "Fe",
      variable == "Cl_ppm" ~ "Cl",
      variable == "NO2_N_ppm" ~ "NO2",
      variable == "NO3_N_ppm" ~ "NO3",
      variable == "PO4_P_ppm" ~ "PO4",
      variable == "SO4_ppm" ~ "SO4",
      variable == "Alkalinity_uM" ~ "Alk",
      variable == "DOC_mg_L" ~ "DOC",
      variable == "CH4_mean_uM" ~ "CH4",
      variable == "d13C_CH4_mean" ~ paste0("d13C-CH4"),
      TRUE ~ variable
    ))

  fig5 <- ggplot(scores, aes(x = PC1, y = PC2)) +
    # Reference lines
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey80") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey80") +
    # Cluster ellipses by site
    stat_ellipse(aes(group = Site, linetype = Site),
                 level = 0.68, color = "grey50", linewidth = 0.5) +
    # Loading arrows
    geom_segment(data = loadings,
                 aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 inherit.aes = FALSE,
                 color = "grey50", linewidth = 0.4,
                 arrow = arrow(length = unit(0.15, "cm"))) +
    geom_text_repel(data = loadings,
                    aes(x = PC1, y = PC2, label = var_display),
                    inherit.aes = FALSE,
                    color = "grey30", size = 3, fontface = "italic",
                    box.padding = 0.3, segment.color = NA) +
    # Sample points — depth as discrete color ramp, site as shape
    geom_point(aes(color = Depth_cm, shape = Site),
               size = 3.5, stroke = 0.8) +
    geom_text_repel(aes(label = paste0(Site, "-", Depth_cm)),
                    size = 2.5, max.overlaps = 20, segment.size = 0.3) +
    scale_color_viridis_d(name = "Depth (cm)", option = "viridis",
                          direction = 1,
                          guide = guide_legend(reverse = TRUE)) +
    scale_shape_manual(values = c("SRS5" = 16, "SRS6" = 17,
                                  "BL60" = 15, "CP40" = 18),
                       name = "Site") +
    scale_linetype_manual(values = c("SRS5" = "solid", "SRS6" = "dashed",
                                     "BL60" = "dotted", "CP40" = "dotdash"),
                          name = "Site") +
    labs(x = sprintf("PC1 (%.1f%%)", var_exp[1]),
         y = sprintf("PC2 (%.1f%%)", var_exp[2]),
         tag = "(a)") +
    theme_pub() +
    theme(legend.position = "right",
          panel.grid = element_blank())

  save_pub(fig5, "soil_pca", width = 220, height = 170)

  cat("  PCA vars used:", paste(keep_vars, collapse = ", "), "\n")
  cat("  Variance explained: PC1 =", round(var_exp[1], 1), "%, PC2 =", round(var_exp[2], 1), "%\n")
} else {
  cat("  Skipping PCA: insufficient complete cases\n")
}


# =============================================================================
# FIGURE 6: Heatmap of all variables by site x depth
# =============================================================================
cat("\n--- Figure 6: Geochemistry Heatmap ---\n")

# Select key variables to display
heat_vars <- c("ORP", "pH", "Sulfide", "Total Iron",
               "SO4_ppm", "Cl_ppm", "DOC_mg_L", "Alkalinity_uM",
               "CH4_mean_uM", "d13C_CH4_mean")

heat_labels <- c("ORP (mV)", "pH", "Sulfide (mg/L)", "Fe (mg/L)",
                 "SO4 (ppm)", "Cl (ppm)", "DOC (mg/L)", "Alk (uM)",
                 "CH4 (uM)", "d13C-CH4")

df_heat <- df %>%
  select(Site, Depth_cm, Depth_numeric, all_of(heat_vars)) %>%
  pivot_longer(cols = all_of(heat_vars), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(variable = factor(variable, levels = heat_vars, labels = heat_labels))

# Scale within each variable for consistent color mapping
df_heat <- df_heat %>%
  group_by(variable) %>%
  mutate(value_scaled = (value - mean(value, na.rm = TRUE)) / sd(value, na.rm = TRUE)) %>%
  ungroup()

fig6 <- df_heat %>%
  ggplot(aes(x = Site, y = Depth_cm, fill = value_scaled)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = signif(value, 3)), size = 2.2, color = "black") +
  facet_wrap(~ variable, nrow = 2, scales = "free") +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       midpoint = 0, name = "Z-score") +
  scale_y_discrete(limits = rev(c("Surface", "0", "15", "45", "90"))) +
  labs(x = NULL, y = "Depth (cm)") +
  theme_pub(base_size = 9) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
        legend.position = "right",
        strip.text = element_text(size = 8))

save_pub(fig6, "soil_heatmap", width = 280, height = 180)


# =============================================================================
# DONE
# =============================================================================
cat("\n===== All soil profile figures saved to output/figures/ =====\n")
