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
library(readxl)
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
site_shapes <- c(
  "SRS5" = 16, "SRS6" = 17,
  "BL60" = 15, "CP40" = 18
)

theme_pub <- function(base_size = 14) {
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
         width = width, height = height, units = units,
         device = cairo_pdf)
  ggsave(paste0("output/figures/pub_", name, ".png"), plot,
         width = width, height = height, units = units, dpi = 300,
         type = "cairo")
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

# Apply DO sensor offset correction (-1.8 mg/L from measured values)
# Then shift up by 0.13 to floor minimum at 0 (avoids negative DO)
df <- df %>%
  mutate(ppmDO = ppmDO - 1.8 + 0.13)
cat("  Applied DO correction: -1.67 mg/L (sensor offset -1.8, floor shift +0.13)\n")

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
    scale_color_manual(values = site_colors, name = "Site",
                       labels = c("SRS5
(healthy)", "SRS6
(healthy)",
                                  "BL60
(regen.)", "CP40
(ghost)")) +
    scale_shape_manual(values = site_shapes, name = "Site",
                       labels = c("SRS5
(healthy)", "SRS6
(healthy)",
                                  "BL60
(regen.)", "CP40
(ghost)")) +
    labs(y = y_lab, x = "Depth (cm)", tag = tag_label) +
    coord_flip() +
    theme_pub(base_size = 14) +
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
  scale_color_manual(values = site_colors, name = "Site",
                     labels = c("SRS5
(healthy)", "SRS6
(healthy)",
                                "BL60
(regen.)", "CP40
(ghost)")) +
  scale_shape_manual(values = site_shapes, name = "Site",
                     labels = c("SRS5
(healthy)", "SRS6
(healthy)",
                                "BL60
(regen.)", "CP40
(ghost)")) +
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
# FIGURE 3: Isotope Depth Profile (d13C-CH4 only; d13C-CO2 excluded due to
# H2S interference in analysis)
# =============================================================================
cat("\n--- Figure 3: Isotope Depth Profile ---\n")

fig3a <- make_depth_panel(df, "d13C_CH4_mean",
                          expression(delta^{13}*C-CH[4]~("\u2030")), "(a)")

fig3 <- fig3a /
  patchwork::wrap_elements(legend_grob) +
  plot_layout(heights = c(1, 0.08))

save_pub(fig3, "soil_isotope_profiles", width = 140, height = 130)


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
                 "n_replicates", "Tds ppt", "%DO", "Br_ppm", "F_ppm")
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
      variable == "ppmDO" ~ "DO",
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
    stat_ellipse(aes(group = Site),
                 level = 0.68, color = "grey50", linewidth = 0.5,
                 linetype = "dashed") +
    # Label ellipse groups by disturbance category
    geom_label(data = tibble(
                 label = c("healthy", "regenerating", "ghost"),
                 color = c(disturbance_colors[["healthy"]],
                           disturbance_colors[["regenerating"]],
                           disturbance_colors[["ghost"]]),
                 PC1 = c(mean(scores$PC1[scores$Site %in% c("SRS5", "SRS6")]),
                         mean(scores$PC1[scores$Site == "BL60"]),
                         mean(scores$PC1[scores$Site == "CP40"])),
                 PC2 = c(max(scores$PC2[scores$Site %in% c("SRS5", "SRS6")]) + 1.2,
                         min(scores$PC2[scores$Site == "BL60"]) - 1.2,
                         max(scores$PC2[scores$Site == "CP40"]) + 1.2)
               ),
               aes(x = PC1, y = PC2, label = label),
               inherit.aes = FALSE,
               size = 5.5, fontface = "bold", color = c(disturbance_colors[["healthy"]],
                                                       disturbance_colors[["regenerating"]],
                                                       disturbance_colors[["ghost"]]),
               fill = alpha("white", 0.7), label.size = 0,
               label.padding = unit(0.2, "lines")) +
    # Loading arrows — CH4 in red, others grey
    geom_segment(data = loadings %>% filter(var_display != "CH4"),
                 aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 inherit.aes = FALSE,
                 color = "grey50", linewidth = 0.4,
                 arrow = arrow(length = unit(0.15, "cm"))) +
    geom_segment(data = loadings %>% filter(var_display == "CH4"),
                 aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 inherit.aes = FALSE,
                 color = "firebrick", linewidth = 0.5,
                 arrow = arrow(length = unit(0.15, "cm"))) +
    geom_text_repel(data = loadings %>% filter(var_display != "CH4"),
                    aes(x = PC1, y = PC2, label = var_display),
                    inherit.aes = FALSE,
                    color = "grey30", size = 5, fontface = "italic",
                    box.padding = 0.35, point.padding = 0.15,
                    min.segment.length = 0, segment.color = "grey70",
                    segment.size = 0.3, max.overlaps = Inf,
                    force = 1.5, force_pull = 0.3) +
    geom_text_repel(data = loadings %>% filter(var_display == "CH4"),
                    aes(x = PC1, y = PC2, label = var_display),
                    inherit.aes = FALSE,
                    color = "firebrick", size = 5.5, fontface = "bold.italic",
                    box.padding = 0.35, point.padding = 0.15,
                    min.segment.length = 0, segment.color = "firebrick",
                    segment.size = 0.3, force = 1.5, force_pull = 0.3) +
    # Sample points — depth as discrete color ramp, site as shape
    geom_point(aes(color = Depth_cm, shape = Site),
               size = 5, stroke = 1) +
    # Individual point labels removed for clarity

    scale_color_viridis_d(name = "Depth (cm)", option = "viridis",
                          direction = 1,
                          guide = guide_legend(reverse = TRUE, byrow = TRUE)) +
    scale_shape_manual(values = site_shapes, name = "Site",
                       labels = c("SRS5
(healthy)", "SRS6
(healthy)",
                                  "BL60
(regen.)", "CP40
(ghost)")) +
    labs(x = sprintf("PC1 (%.1f%%)", var_exp[1]),
         y = sprintf("PC2 (%.1f%%)", var_exp[2]),
         tag = "(a)") +
    theme_pub() +
    theme(legend.position = c(0.85, 0.65),
          legend.justification = c(0, 0.5),
          legend.background = element_rect(fill = alpha("white", 0.85), color = NA),
          legend.key.size = unit(3.5, "mm"),
          legend.spacing.y = unit(1, "mm"),
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
  theme_pub(base_size = 14) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
        legend.position = "right",
        strip.text = element_text(size = 8))

save_pub(fig6, "soil_heatmap", width = 280, height = 180)


# =============================================================================
# =============================================================================
# FIGURE 7: Comprehensive Porewater Depth Profiles (2 rows × 5 panels)
# dissolved CH4, CO2, salinity, sulfate, ORP, DO, pH, DOC, alkalinity,
# d13C-CH4 — excluding "Surface" depth and d13C-CO2 (H2S interference)
# =============================================================================
cat("\n--- Figure 7: Comprehensive Porewater Depth Profiles ---\n")

# Filter out Surface
df_nosurface <- df %>% filter(Depth_cm != "Surface") %>%
  mutate(Depth_cm = droplevels(Depth_cm))

# Slim depth panel helper — no tags, compact for 11-across layout
make_slim_panel <- function(data, var, y_lab, log_scale = FALSE) {
  d <- data %>% filter(!is.na(.data[[var]])) %>% mutate(Site = droplevels(Site))
  d_means <- d %>%
    group_by(Site, Depth_numeric) %>%
    summarise(mean_val = mean(.data[[var]], na.rm = TRUE),
              se_val = sd(.data[[var]], na.rm = TRUE) / sqrt(n()),
              .groups = "drop") %>%
    mutate(ymin = mean_val - se_val, ymax = mean_val + se_val)

  p <- ggplot() +
    geom_line(data = d_means,
              aes(x = Depth_numeric, y = mean_val, color = Site, group = Site),
              linewidth = 0.6, alpha = 0.7) +
    geom_point(data = d,
               aes(x = Depth_numeric, y = .data[[var]], color = Site, shape = Site),
               size = 3.5) +
    scale_x_continuous(breaks = c(0, 15, 45, 90),
                       labels = c("0", "15", "45", "90"),
                       trans = "reverse") +
    scale_color_manual(values = site_colors, name = "Site",
                       labels = c("SRS5
(healthy)", "SRS6
(healthy)",
                                  "BL60
(regen.)", "CP40
(ghost)")) +
    scale_shape_manual(values = site_shapes, name = "Site",
                       labels = c("SRS5
(healthy)", "SRS6
(healthy)",
                                  "BL60
(regen.)", "CP40
(ghost)")) +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 3)) +
    labs(y = y_lab, x = NULL) +
    coord_flip() +
    theme_bw(base_size = 14) +
    theme(legend.position = "none",
          axis.title = element_text(size = 7.5, face = "plain"),
          axis.text.y = element_text(size = 14),
          axis.text.x = element_text(size = 13, angle = 45, hjust = 1),
          panel.grid.minor = element_blank(),
          plot.margin = margin(2, 3, 2, 2))
  if (log_scale) p <- p + scale_y_log10(breaks = scales::breaks_log(n = 4))
  p
}

p_ch4  <- make_slim_panel(df_nosurface, "CH4_mean_uM",
                           expression(CH[4]~(mu*M)))
p_co2  <- make_slim_panel(df_nosurface, "CO2_mean_uM",
                           expression(CO[2]~(mu*M)))
p_sal  <- make_slim_panel(df_nosurface, "PSU",
                           expression(Salinity~(PSU)))
p_so4  <- make_slim_panel(df_nosurface, "SO4_ppm",
                           expression(SO[4]^{"2-"}~(ppm)))
p_orp  <- make_slim_panel(df_nosurface, "ORP",
                           expression(ORP~(mV)))
p_do   <- make_slim_panel(df_nosurface, "ppmDO",
                           expression(DO~(mg~L^{-1})))
p_ph   <- make_slim_panel(df_nosurface, "pH",
                           expression(pH))
p_doc  <- make_slim_panel(df_nosurface, "DOC_mg_L",
                           expression(DOC~(mg~L^{-1})))
p_alk  <- make_slim_panel(df_nosurface, "Alkalinity_uM",
                           expression(Alk.~(mu*M)))
p_d13ch4 <- make_slim_panel(df_nosurface, "d13C_CH4_mean",
                              "\u03B4\u00B9\u00B3CH\u2084 (\u2030)")
# d13C-CO2 excluded due to H2S interference in analysis

# Add depth label to first panel in each row (keeps rows equal height)
p_ch4 <- p_ch4 + labs(x = "Depth (cm)")
p_do  <- p_do  + labs(x = "Depth (cm)")

# Legend on last panel of row 1 (right side, spans both rows visually)
p_orp_leg <- p_orp +
  guides(color = guide_legend(byrow = TRUE),
         shape = guide_legend(byrow = TRUE)) +
  theme(legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        legend.key.size = unit(3.5, "mm"),
        legend.spacing.y = unit(9, "mm"))
p_d13ch4_leg <- p_d13ch4
p_do_leg <- p_do + labs(x = "Depth (cm)")

# 2 rows × 5 panels: row 1 = CH4, CO2, salinity, sulfate, ORP (with legend)
#                     row 2 = DO, pH, DOC, alkalinity, d13C-CH4
fig7_row1 <- p_ch4 + p_co2 + p_sal + p_so4 + p_orp_leg + plot_layout(nrow = 1)
fig7_row2 <- p_do + p_ph + p_doc + p_alk + p_d13ch4 + plot_layout(nrow = 1)

fig7 <- fig7_row1 / fig7_row2 +
  plot_layout(heights = c(1, 1))

save_pub(fig7, "porewater_depth_profiles", width = 210, height = 110)


# =============================================================================
# FIGURE 8: PCA + Porewater Depth Profiles Composite
# PCA on top, porewater profiles on bottom
# =============================================================================
cat("\n--- Figure 8: PCA + Porewater Composite ---\n")

if (exists("fig5")) {
  # Use cowplot for consistent (a)/(b) labels across sub-layouts
  fig5_clean <- fig5 + labs(tag = NULL)  # remove PCA's own tag

  fig8_row1 <- p_ch4 + labs(x = "Depth (cm)")
  fig8_row1 <- fig8_row1 + p_co2 + p_sal + p_so4 + p_orp_leg + plot_layout(nrow = 1)
  fig8_row2 <- p_do + p_ph + p_doc + p_alk + p_d13ch4 + plot_layout(nrow = 1)
  profiles <- fig8_row1 / fig8_row2

  # Add top margin to profiles so (b) label doesn't overlap depth axis
  profiles_padded <- profiles + plot_annotation(theme = theme(plot.margin = margin(20, 0, 0, 0)))

  fig8 <- ggpubr::ggarrange(
    fig5_clean, profiles_padded,
    ncol = 1, heights = c(1.4, 2),
    labels = c("(a)", "(b)"),
    font.label = list(size = 14, face = "bold")
  )

  save_pub(fig8, "pca_porewater_composite", width = 210, height = 220)
} else {
  cat("  Skipping composite: PCA not available\n")
}

# Also save the PCA + profiles as left panel object for the combined figure
fig8_left <- if (exists("fig8")) fig8 else NULL


# =============================================================================
# FIGURE 9: Combined layout — PCA+depth profiles (left) + bubbles+scatter (right)
# Side-by-side of fig8 (pub_pca_porewater_composite) and
# pub_SI_porewater_characterization
# =============================================================================
cat("\n--- Figure 9: Combined PCA/profiles + multi-campaign characterization ---\n")

if (!is.null(fig8_left)) {
  # Source the site characterization figures script to get the right-side panels
  # (bubbles + scatter). We need p_comb_ch4, p_comb_sal, p_comb_scatter from it.
  source("code/07_ebullition/site_characterization_figures.R")

  # Right side: bubbles stacked + scatter below (from site_characterization_figures.R)
  # These objects were created by sourcing that script: p_comb_ch4, p_comb_sal, p_comb_scatter
  # But they may not exist if that script doesn't export them. Build inline instead.

  # Read the saved porewater characterization figure? No — we need ggplot objects.
  # The site_characterization_figures.R already built and saved these.
  # Let's just build the right panel here from the same data.

  calc_dissolved_uM <- function(ppm, Vw = 0.180, Vg = 0.020, T = 25, P = 1, KH = 1.4e-3) {
    R <- 0.082057; TK <- T + 273.15; p <- ppm / 1e6 * P
    ((p * Vg / (R * TK)) + (KH * p * Vw)) / Vw * 1e6
  }

  season_short_map <- c("wet (Oct 2022)" = "Oct 22", "dry (Mar 2023)" = "Mar 23", "Nov 2025" = "Nov 25")
  season_short_levels <- c("Oct 22", "Mar 23", "Nov 25")
  site_disturbance_mc <- c(BL60 = "regenerating", CP40 = "ghost", FLM30 = "ghost",
                           SRS5 = "healthy", SRS6 = "healthy")
  core_sites_mc <- c("BL60", "CP40", "FLM30", "SRS5", "SRS6")

  # GC data
  d1c <- read_excel("data/environmental/porewater_gas/GC Run_Dec_2023_Peterman_Gewirtzman (1).xlsx",
                    sheet = "Run 1 Compiled")
  d2c <- read_excel("data/environmental/porewater_gas/GC Run_Dec_2023_Peterman_Gewirtzman (1).xlsx",
                    sheet = "Run 2 Compiled")
  gc <- bind_rows(d1c, d2c) %>%
    filter(Project == "Everglades") %>%
    mutate(CH4_ppm = as.numeric(Concentration...16),
           real_date = as.Date(as.numeric(Date...6), origin = "1899-12-30"),
           CH4_uM = calc_dissolved_uM(CH4_ppm),
           site = case_when(
             grepl("BL.?60", `Sample ID`, ignore.case = TRUE) ~ "BL60",
             grepl("^CP|CP.?4", `Sample ID`, ignore.case = TRUE) ~ "CP40",
             grepl("FLM|FML", `Sample ID`, ignore.case = TRUE) ~ "FLM30",
             grepl("SRS.?5|SRS 5", `Sample ID`) ~ "SRS5",
             grepl("SRS.?6|SRS 6", `Sample ID`) ~ "SRS6",
             TRUE ~ NA_character_),
           sample_type = case_when(
             grepl("pore|pour", `Sample ID`, ignore.case = TRUE) ~ "porewater",
             grepl("surface", `Sample ID`, ignore.case = TRUE) ~ "surface_water",
             `Sample ID` == "CP 40" ~ "porewater",
             `Sample ID` == "FML 30" ~ "surface_water",
             TRUE ~ NA_character_),
           depth_cm = case_when(
             grepl("100 cm", `Sample ID`) ~ 100, grepl("40 cm", `Sample ID`) ~ 40,
             grepl("15 cm", `Sample ID`) ~ 15, sample_type == "surface_water" ~ -5,
             sample_type == "porewater" ~ 40, TRUE ~ NA_real_),
           season = ifelse(real_date < as.Date("2023-01-01"), "wet (Oct 2022)", "dry (Mar 2023)")
    ) %>%
    filter(!is.na(site), !is.na(sample_type), site %in% core_sites_mc)

  pw_mc <- read_csv("/Users/jongewirtzman/My Drive/Research/Blueflux/microbes/merged_porewater_all_parameters.csv",
                    show_col_types = FALSE) %>%
    mutate(site = Site, season = "Nov 2025",
           sample_type = ifelse(Depth_cm == "Surface", "surface_water", "porewater"),
           depth_cm = case_when(Depth_cm == "Surface" ~ -5, TRUE ~ as.numeric(Depth_cm)),
           CH4_uM = CH4_mean_uM, PSU_val = PSU)

  sal_terr <- read_excel("data/environmental/salinity/Blueflux Salinity.xlsx",
                         sheet = "Terrestrial Data (Jon)") %>%
    mutate(PSU_val = as.numeric(`Salinity (PSU) - Final`),
           site = case_when(Location == "SRS5" ~ "SRS5", Location == "SRS6" ~ "SRS6",
                            Location == "BL60" ~ "BL60", Location == "CP40" ~ "CP40",
                            Location == "FLM30" ~ "FLM30", TRUE ~ NA_character_),
           sample_type = ifelse(grepl("Pore", `Sample Type`), "porewater", "surface_water"),
           depth_cm = case_when(sample_type == "surface_water" ~ -5,
                                !is.na(`Depth (cm)`) ~ as.numeric(`Depth (cm)`), TRUE ~ 40),
           season = ifelse(grepl("2022", as.character(Date)), "wet (Oct 2022)", "dry (Mar 2023)")
    ) %>% filter(!is.na(site), !is.na(PSU_val))

  # All data (including surface water) for scatter
  ch4_sum_all <- bind_rows(gc %>% select(site, season, depth_cm, CH4_uM),
                           pw_mc %>% select(site, season, depth_cm, CH4_uM)) %>%
    filter(site %in% core_sites_mc) %>%
    group_by(site, season, depth_cm) %>%
    summarise(CH4_mean = mean(CH4_uM, na.rm = TRUE), .groups = "drop") %>%
    mutate(disturbance = site_disturbance_mc[site])

  sal_sum_all <- bind_rows(sal_terr %>% select(site, season, depth_cm, PSU_val),
                           pw_mc %>% filter(!is.na(PSU_val)) %>% select(site, season, depth_cm, PSU_val)) %>%
    filter(site %in% core_sites_mc) %>%
    group_by(site, season, depth_cm) %>%
    summarise(PSU_mean = mean(PSU_val, na.rm = TRUE), .groups = "drop") %>%
    mutate(disturbance = site_disturbance_mc[site])

  # Porewater only (depth >= 0) for bubble plots
  ch4_sum_mc <- ch4_sum_all %>% filter(depth_cm >= 0)
  sal_sum_mc <- sal_sum_all %>% filter(depth_cm >= 0)

  site_sal_order <- sal_sum_all %>%
    group_by(site) %>% summarise(mean_sal = mean(PSU_mean, na.rm = TRUE), .groups = "drop") %>%
    arrange(mean_sal) %>% pull(site)
  ch4_sum_mc <- ch4_sum_mc %>% mutate(site = factor(site, levels = site_sal_order))
  sal_sum_mc <- sal_sum_mc %>% mutate(site = factor(site, levels = site_sal_order))

  # Scatter uses ALL data (including surface water)
  merged_mc <- ch4_sum_all %>%
    inner_join(sal_sum_all %>% select(site, season, depth_cm, PSU_mean),
               by = c("site", "season", "depth_cm")) %>%
    filter(!is.na(PSU_mean), !is.na(CH4_mean))

  scatter_stats_mc <- merged_mc %>%
    group_by(disturbance) %>%
    summarise(n = n(),
              r = cor(PSU_mean, log1p(CH4_mean), use = "complete.obs"),
              p_val = cor.test(PSU_mean, log1p(CH4_mean))$p.value,
              .groups = "drop") %>%
    mutate(p_label = ifelse(p_val < 0.001, "italic(p) < 0.001",
                            sprintf("italic(p) == %.2f", p_val)),
           label = sprintf("italic(r) == %.2f*','~%s", r, p_label))

  # Right panels
  p_r_ch4 <- ch4_sum_mc %>%
    mutate(season_short = factor(season_short_map[as.character(season)], levels = season_short_levels)) %>%
    ggplot(aes(x = season_short, y = depth_cm, size = CH4_mean, color = CH4_mean)) +
    geom_point() +
    facet_wrap(~ site, nrow = 1) +
    scale_y_reverse() +
    scale_color_gradientn(colours = c("#4575B4", "#91BFDB", "#FEE090", "#FC8D59", "#D73027"),
                         name = expression(CH[4]~(mu*M)),
                         breaks = c(1, 20, 40, 60, 80),
                         limits = c(0, 90),
                         guide = guide_legend()) +
    scale_size_continuous(range = c(1, 7), name = expression(CH[4]~(mu*M)),
                          breaks = c(1, 20, 40, 60, 80),
                          limits = c(0, 90),
                          guide = guide_legend()) +
    labs(x = NULL, y = "Depth (cm)") +
    theme_pub(base_size = 14) +
    theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1),
          strip.background = element_blank())

  p_r_sal <- sal_sum_mc %>%
    mutate(season_short = factor(season_short_map[as.character(season)], levels = season_short_levels)) %>%
    ggplot(aes(x = season_short, y = depth_cm, size = PSU_mean, color = PSU_mean)) +
    geom_point() +
    facet_wrap(~ site, nrow = 1) +
    scale_y_reverse() +
    scale_color_gradientn(colours = c("#4575B4", "#91BFDB", "#FEE090", "#FC8D59", "#D73027"),
                         name = "Salinity (PSU)",
                         breaks = c(10, 20, 30, 40, 50, 60),
                         limits = c(5, 65),
                         guide = guide_legend()) +
    scale_size_continuous(range = c(1, 7), name = "Salinity (PSU)",
                          breaks = c(10, 20, 30, 40, 50, 60),
                          limits = c(5, 65),
                          guide = guide_legend()) +
    labs(x = NULL, y = "Depth (cm)") +
    theme_pub(base_size = 14) +
    theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1),
          strip.background = element_blank())

  p_r_scatter <- merged_mc %>%
    ggplot(aes(x = PSU_mean, y = CH4_mean, color = disturbance, shape = season)) +
    geom_point(size = 2.5, alpha = 0.8) +
    geom_smooth(aes(group = disturbance, fill = disturbance),
                method = "lm", se = TRUE, alpha = 0.15, linewidth = 0.7) +
    scale_y_continuous(trans = "log1p", breaks = c(0, 1, 5, 10, 25, 50, 100)) +
    scale_color_manual(values = disturbance_colors, name = "Disturbance") +
    scale_fill_manual(values = disturbance_colors, guide = "none") +
    scale_shape_manual(values = c(16, 17, 15), name = "Campaign") +
    geom_text(data = scatter_stats_mc,
              aes(x = Inf, y = Inf, label = label, color = disturbance),
              inherit.aes = FALSE, parse = TRUE,
              hjust = 1.05, vjust = c(1.2, 2.5, 3.8), size = 3) +
    labs(x = "Salinity (PSU)", y = expression(Dissolved~CH[4]~(mu*M))) +
    theme_pub(base_size = 14)

  # Build entire composite with patchwork — ALL panels flat (no nesting)
  #
  # Layout (14 rows x 15 columns):
  #   Left col (9 wide): PCA (rows 1-6) then 2 rows x 5 depth panels (rows 7-14)
  #   Right col (6 wide): CH4 bubbles (rows 1-4), sal bubbles (rows 5-9), scatter (rows 10-14)
  #
  # Panels: A=PCA, B-F=depth row1 (ch4,co2,sal,so4,orp),
  #         G-K=depth row2 (do,ph,doc,alk,d13ch4),
  #         L=CH4 bubbles, M=sal bubbles, N=scatter

  # Unified theme for ALL panels
  unified <- theme(
    axis.title   = element_text(size = 14),
    axis.text    = element_text(size = 12),
    legend.title = element_text(size = 12, face = "plain"),
    legend.text  = element_text(size = 11),
    strip.text   = element_text(size = 12, face = "bold"),
    plot.tag     = element_text(size = 18, face = "bold"),
    plot.margin  = margin(2, 2, 2, 2)
  )

  # PCA panel — legend right, within column width
  p_A <- fig5 + labs(tag = "(a)") + unified +
    theme(legend.position = "right",
          legend.key.size = unit(3.5, "mm"))

  # Depth panels — remove existing tags, add (b) to first only
  dp <- list(p_ch4 + labs(tag = "(b)"),
             p_co2, p_sal, p_so4, p_orp_leg,
             p_do_leg, p_ph, p_doc, p_alk, p_d13ch4_leg)
  # Note: legend shows on p_orp_leg (last panel, row 1 right side)
  dp <- lapply(dp, function(p) p + unified)

  # Right panels
  p_L <- p_r_ch4 + labs(tag = "(c)") + unified
  p_M <- p_r_sal + labs(tag = "(d)") + unified
  p_N <- p_r_scatter + labs(tag = "(e)") + unified +
    theme(plot.margin = margin(2, 2, 2, 2))

  # Scatter panel with theme_classic (no gridlines, no shading)
  # Add stat labels directly with fixed colors (not via aes, to avoid scale conflicts)
  scatter_stats_mc <- scatter_stats_mc %>%
    mutate(text_color = disturbance_colors[as.character(disturbance)])

  p_N <- merged_mc %>%
    ggplot(aes(x = PSU_mean, y = CH4_mean, color = disturbance, shape = season)) +
    geom_point(size = 2.5, alpha = 0.8) +
    geom_smooth(aes(group = disturbance, fill = disturbance),
                method = "lm", se = TRUE, alpha = 0.15, linewidth = 0.7) +
    scale_y_continuous(trans = "log1p", breaks = c(0, 1, 5, 10, 25, 50, 100)) +
    scale_color_manual(values = disturbance_colors, name = "Disturbance") +
    scale_fill_manual(values = disturbance_colors, guide = "none") +
    scale_shape_manual(values = c(16, 17, 15), name = "Campaign") +
    # Expand y-axis down to make room for annotations below data
    expand_limits(y = -0.5) +
    annotate("text", x = max(merged_mc$PSU_mean, na.rm=TRUE),
             y = -0.45, label = scatter_stats_mc$label[1],
             parse = TRUE, hjust = 1, vjust = 2.2, size = 5,
             color = scatter_stats_mc$text_color[1]) +
    annotate("text", x = max(merged_mc$PSU_mean, na.rm=TRUE),
             y = -0.45, label = scatter_stats_mc$label[2],
             parse = TRUE, hjust = 1, vjust = 0.9, size = 5,
             color = scatter_stats_mc$text_color[2]) +
    annotate("text", x = max(merged_mc$PSU_mean, na.rm=TRUE),
             y = -0.45, label = scatter_stats_mc$label[3],
             parse = TRUE, hjust = 1, vjust = -0.4, size = 5,
             color = scatter_stats_mc$text_color[3]) +
    labs(x = "Salinity (PSU)", y = expression(Dissolved~CH[4]~(mu*M)),
         tag = "(e)") +
    theme_classic(base_size = 14) +
    theme(plot.tag = element_text(size = 18, face = "bold"))

  # Grid: 30 rows x 25 cols
  # Left: A (PCA) rows 1-12, depth row 1 rows 13-21, depth row 2 rows 22-30
  # Right: L rows 1-10, M rows 11-20, N rows 21-30
  # Depth panels: 5 per row, each 3 cols wide (cols 1-15)
  # Right panels: cols 16-25
  fig9_layout <- c(
    area(t = 1,  b = 12, l = 1,  r = 15),   # A: PCA
    area(t = 13, b = 21, l = 1,  r = 3),    # B: CH4 depth
    area(t = 13, b = 21, l = 4,  r = 6),    # C: CO2 depth
    area(t = 13, b = 21, l = 7,  r = 9),    # D: Sal depth
    area(t = 13, b = 21, l = 10, r = 12),   # E: SO4 depth
    area(t = 13, b = 21, l = 13, r = 15),   # F: ORP depth
    area(t = 22, b = 30, l = 1,  r = 3),    # G: DO depth
    area(t = 22, b = 30, l = 4,  r = 6),    # H: pH depth
    area(t = 22, b = 30, l = 7,  r = 9),    # I: DOC depth
    area(t = 22, b = 30, l = 10, r = 12),   # J: Alk depth
    area(t = 22, b = 30, l = 13, r = 15),   # K: d13CH4 depth
    area(t = 1,  b = 10, l = 16, r = 25),   # L: CH4 bubbles
    area(t = 11, b = 20, l = 16, r = 25),   # M: Sal bubbles
    area(t = 21, b = 30, l = 16, r = 25)    # N: Scatter
  )

  # Build left and right columns separately with patchwork, then combine with cowplot
  # This avoids the area() height issues

  # Left column: PCA over 2 rows of depth panels
  left_col <- p_A /
    (dp[[1]] + dp[[2]] + dp[[3]] + dp[[4]] + dp[[5]] + plot_layout(nrow = 1)) /
    (dp[[6]] + dp[[7]] + dp[[8]] + dp[[9]] + dp[[10]] + plot_layout(nrow = 1)) +
    plot_layout(heights = c(4, 3, 3))

  # Right column: CH4 bubbles, sal bubbles, scatter (equal heights)
  right_col <- p_L / p_M / p_N + plot_layout(heights = c(1, 1, 1))

  # Combine with cowplot for precise width control
  fig9 <- cowplot::plot_grid(left_col, right_col, ncol = 2, rel_widths = c(3, 2))

  save_pub(fig9, "pca_porewater_full_composite", width = 420, height = 300)
  cat("  Saved combined layout\n")
} else {
  cat("  Skipping: fig8 not available\n")
}

# DONE
# =============================================================================
cat("\n===== All soil profile figures saved to output/figures/ =====\n")
