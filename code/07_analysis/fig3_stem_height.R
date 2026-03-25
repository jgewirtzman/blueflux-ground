# =============================================================================
# Figure 3: Stem height x species CH4/CO2 composite
# Output: pub_stem_height_composite_combined
# =============================================================================
source("code/07_analysis/publication_figures_common.R")

cat("\n--- Figure 4: Stem Height Profile ---\n")

stem_height <- df %>%
  filter(component == "stem", CH4_flux_status == "valid",
         !is.na(height_category),
         plot %in% c("SRS5", "SRS6", "BL60", "FLM30", "CP40")) %>%
  mutate(disturbance_level = droplevels(disturbance_level))

# Pad x_range in asinh space so density kernels aren't clipped at boundaries
x_range_raw <- range(stem_height$CH4_best.flux, na.rm = TRUE)
x_range <- c(sinh(asinh(x_range_raw[1]) - 1.5),
             sinh(asinh(x_range_raw[2]) + 1.5))

# (a) Overlapping ridges — height_category on y-axis, disturbance as fill
fig4a <- stem_height %>%
  ggplot(aes(x = CH4_best.flux, y = height_category, fill = disturbance_level)) +
  geom_density_ridges(alpha = 0.6, scale = 0.9, bandwidth = 1.0) +
  scale_x_continuous(trans = "asinh", limits = x_range,
                     breaks = asinh_brk_pos, labels = asinh_labels) +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.45))) +
  scale_fill_manual(values = disturbance_colors, name = "Disturbance\nLevel") +
  labs(y = "Height Category") +
  theme_pub() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), legend.position = "right",
        plot.margin = margin(10, 5, 15, 5))

# (b) Box/jitter faceted by height category
fig4b <- stem_height %>%
  mutate(disturbance_level = factor(disturbance_level,
                                     levels = rev(c("healthy", "regenerating", "ghost")))) %>%
  ggplot(aes(x = CH4_best.flux,
             y = interaction(disturbance_level, height_category, sep = " - "),
             fill = disturbance_level)) +
  geom_jitter(aes(color = disturbance_level),
             height = 0.2, width = 0, alpha = 0.5, size = 2) +
  geom_boxplot(width = 0.4, alpha = 0.7, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 23,
               size = 2.5, fill = alpha("white", 0.6), color = alpha("black", 0.6), stroke = 0.7, alpha = 0.7) +
  scale_x_continuous(trans = "asinh", limits = x_range,
                     breaks = asinh_brk_pos, labels = asinh_labels) +
  scale_fill_manual(values = disturbance_colors, guide = "none") +
  scale_color_manual(values = disturbance_colors, guide = "none") +
  scale_y_discrete(labels = function(x) sapply(strsplit(x, " - "), `[`, 1)) +
  labs(x = expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1})), y = NULL) +
  facet_grid(rows = vars(factor(height_category,
                                levels = rev(c("0-50 cm", "50-100 cm", "100-150 cm", ">150 cm")))),
             scales = "free_y", space = "free_y", switch = "y") +
  theme_pub() +
  theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0, size = 9),
        legend.position = "none", plot.margin = margin(0, 5, 10, 5))

aligned <- align_plots(fig4a, fig4b, align = "v", axis = "lr")
fig4 <- plot_grid(ggdraw(aligned[[1]]), ggdraw(aligned[[2]]),
                  ncol = 1, rel_heights = c(1, 1.5),
                  labels = c("(a)", "(b)"), label_size = 14, label_fontface = "bold")

# save_pub(fig4, "height_profile", width = 200, height = 200)


# =============================================================================
# FIGURE 5: Disturbance Gradient — CH4 across top row, CO2 across bottom row
# Each row has 4 component facets side by side, legend at bottom
# =============================================================================
cat("\n--- Figure 5: Disturbance Gradient ---\n")

top_components <- c("Stem", "Soil", "Water", "Root")

df_spatial <- df %>% filter(component_display %in% top_components)

# CH4 row: components across columns
fig5_ch4 <- df_spatial %>%
  filter(CH4_flux_status == "valid") %>%
  ggplot(aes(x = site_label, y = CH4_best.flux, fill = disturbance_level)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.7) +
  geom_jitter(alpha = 0.3, size = 0.7, width = 0.15) +
  mean_diamond() +
  scale_y_continuous(trans = "asinh", breaks = asinh_brk_pos, labels = asinh_labels) +
  scale_fill_manual(values = disturbance_colors, name = "Disturbance") +
  facet_wrap(~ component_display, nrow = 1) +
  labs(x = NULL, y = expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1})), tag = "(a)") +
  theme_pub(base_size = 10) +
  theme(axis.text.x = element_text(angle = 55, hjust = 1, size = 6),
        legend.position = "none")

# CO2 row: components across columns
fig5_co2 <- df_spatial %>%
  filter(CO2_flux_status == "valid") %>%
  ggplot(aes(x = site_label, y = CO2_best.flux, fill = disturbance_level)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.7) +
  geom_jitter(alpha = 0.3, size = 0.7, width = 0.15) +
  mean_diamond() +
  scale_y_continuous(trans = "asinh", breaks = asinh_brk, labels = asinh_labels) +
  scale_fill_manual(values = disturbance_colors, name = "Disturbance") +
  facet_wrap(~ component_display, nrow = 1) +
  labs(x = NULL, y = expression(CO[2]~Flux~(mu*mol~m^{-2}~s^{-1})), tag = "(b)") +
  theme_pub(base_size = 10) +
  theme(axis.text.x = element_text(angle = 55, hjust = 1, size = 6),
        legend.position = "bottom")

fig5 <- fig5_ch4 / fig5_co2 + plot_layout(heights = c(1, 1.15))
# save_pub(fig5, "disturbance_gradient", width = 280, height = 200)


# =============================================================================
# FIGURE 6: Environmental Drivers (water depth + temperature)
# =============================================================================
cat("\n--- Figure 6: Environmental Drivers ---\n")

df_env <- df %>% filter(component_display %in% top_components)

fig6a <- df_env %>%
  filter(CH4_flux_status == "valid", !is.na(water_depth)) %>%
  ggplot(aes(x = water_depth, y = CH4_best.flux, color = component_display)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.15, linewidth = 0.8) +
  scale_y_continuous(trans = "asinh", breaks = asinh_brk_pos, labels = asinh_labels) +
  scale_color_manual(values = component_colors, name = "Component") +
  labs(x = "Water Depth (cm)", y = expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1})), tag = "(a)") +
  theme_pub() + theme(legend.position = "bottom")

fig6b <- df_env %>%
  filter(CO2_flux_status == "valid", !is.na(water_depth)) %>%
  ggplot(aes(x = water_depth, y = CO2_best.flux, color = component_display)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.15, linewidth = 0.8) +
  scale_y_continuous(trans = "asinh", breaks = asinh_brk, labels = asinh_labels) +
  scale_color_manual(values = component_colors, name = "Component") +
  labs(x = "Water Depth (cm)", y = expression(CO[2]~Flux~(mu*mol~m^{-2}~s^{-1})), tag = "(b)") +
  theme_pub() + theme(legend.position = "bottom")

fig6c <- df_env %>%
  filter(CH4_flux_status == "valid", !is.na(temp)) %>%
  ggplot(aes(x = temp, y = CH4_best.flux, color = component_display)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.15, linewidth = 0.8) +
  scale_y_continuous(trans = "asinh", breaks = asinh_brk_pos, labels = asinh_labels) +
  scale_color_manual(values = component_colors, name = "Component") +
  labs(x = expression(Temperature~(degree*C)), y = expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1})), tag = "(c)") +
  theme_pub() + theme(legend.position = "bottom")

fig6d <- df_env %>%
  filter(CO2_flux_status == "valid", !is.na(temp)) %>%
  ggplot(aes(x = temp, y = CO2_best.flux, color = component_display)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.15, linewidth = 0.8) +
  scale_y_continuous(trans = "asinh", breaks = asinh_brk, labels = asinh_labels) +
  scale_color_manual(values = component_colors, name = "Component") +
  labs(x = expression(Temperature~(degree*C)), y = expression(CO[2]~Flux~(mu*mol~m^{-2}~s^{-1})), tag = "(d)") +
  theme_pub() + theme(legend.position = "bottom")

fig6 <- (fig6a | fig6b) / (fig6c | fig6d) + plot_layout(guides = "collect")
# save_pub(fig6, "environmental_drivers", width = 240, height = 200)


# =============================================================================
# FIGURE 7: CH4 vs CO2 Covariation
# =============================================================================
cat("\n--- Figure 7: CH4 vs CO2 Covariation ---\n")

df_covar <- df %>%
  filter(CH4_flux_status == "valid", CO2_flux_status == "valid",
         component_display %in% top_components)

fig7 <- df_covar %>%
  ggplot(aes(x = CO2_best.flux, y = CH4_best.flux, color = disturbance_level)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.15, linewidth = 0.7) +
  scale_x_continuous(trans = "asinh", breaks = asinh_brk, labels = asinh_labels) +
  scale_y_continuous(trans = "asinh", breaks = asinh_brk_pos, labels = asinh_labels) +
  scale_color_manual(values = disturbance_colors, name = "Disturbance Level") +
  facet_wrap(~ component_display, scales = "free", ncol = 2) +
  labs(x = expression(CO[2]~Flux~(mu*mol~m^{-2}~s^{-1})),
       y = expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1}))) +
  theme_pub() + theme(legend.position = "bottom")

# save_pub(fig7, "ch4_co2_covariation", width = 210, height = 180)


# =============================================================================
# FIGURE 8: Heatmap Summary (Component x Site x Season)
# =============================================================================
cat("\n--- Figure 8: Heatmap Summary ---\n")

heatmap_data_ch4 <- df %>%
  filter(CH4_flux_status == "valid", !is.na(season_agg)) %>%
  group_by(component_display, site_label, season_agg) %>%
  summarise(median_flux = median(CH4_best.flux, na.rm = TRUE),
            n = n(), .groups = "drop")

heatmap_data_co2 <- df %>%
  filter(CO2_flux_status == "valid", !is.na(season_agg)) %>%
  group_by(component_display, site_label, season_agg) %>%
  summarise(median_flux = median(CO2_best.flux, na.rm = TRUE),
            n = n(), .groups = "drop")

fig8a <- heatmap_data_ch4 %>%
  ggplot(aes(x = site_label, y = component_display, fill = median_flux)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = n), size = 2.5, color = "white", fontface = "bold") +
  facet_wrap(~ season_agg, nrow = 1) +
  scale_fill_viridis_c(name = expression(atop(Median~CH[4]~Flux, (nmol~m^{-2}~s^{-1}))),
                       trans = "asinh", breaks = c(0, 1, 10, 100, 1000),
                       na.value = "grey90", option = "D") +
  labs(x = NULL, y = NULL, tag = "(a)") +
  theme_pub(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        legend.position = "bottom", legend.key.width = unit(20, "mm"))

fig8b <- heatmap_data_co2 %>%
  ggplot(aes(x = site_label, y = component_display, fill = median_flux)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = n), size = 2.5, color = "white", fontface = "bold") +
  facet_wrap(~ season_agg, nrow = 1) +
  scale_fill_viridis_c(name = expression(atop(Median~CO[2]~Flux, (mu*mol~m^{-2}~s^{-1}))),
                       trans = "asinh", breaks = c(-10, 0, 10),
                       na.value = "grey90", option = "C") +
  labs(x = NULL, y = NULL, tag = "(b)") +
  theme_pub(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        legend.position = "bottom", legend.key.width = unit(20, "mm"))

fig8 <- fig8a / fig8b
# save_pub(fig8, "heatmap_summary", width = 270, height = 220)


# =============================================================================
# Figure 9: CH4 Flux by Mangrove Species (emmeans)
# =============================================================================
# Estimated marginal means from mixed model:
#   asinh(CH4) ~ species + height + season + (1|plot)
# Restricted to sites where multiple species co-occur (BL60, SRS5, SRS6)
#   to reduce species × site confounding.
#   BL60 (regenerating): C. erectus, L. racemosa, R. mangle
#   SRS5 (healthy):      A. germinans, L. racemosa, R. mangle
#   SRS6 (healthy):      A. germinans, L. racemosa, R. mangle
# Note: C. erectus only at BL60; A. germinans only at SRS5/SRS6.
#       L. racemosa and R. mangle co-occur at all three sites.
#
# Two panels:
#   (a) Overall emmeans (averaging over height & season)
#   (b) Emmeans per height category (0-50, 50-100, 100-150 cm)
# Species included: those with >= 5 stem observations at shared sites
# =============================================================================

library(lme4)
library(lmerTest)
library(emmeans)

# --- Prepare stem data for species analysis ---
# Restrict to shared sites where within-site species comparisons are possible
shared_sites <- c("BL60", "SRS5", "SRS6")

stem_species <- df %>%
  filter(component == "stem",
         plot %in% shared_sites,
         !is.na(species), species != "UNKN", species != "",
         !is.na(CH4_best.flux),
         !is.na(height_corrected),
         height_corrected >= 0) %>%
  mutate(
    # Merge COER and COPE — both are Conocarpus erectus
    species = ifelse(species == "COPE", "COER", species),
    species = factor(species),
    height_cat = factor(
      case_when(
        height_corrected < 50  ~ "0-50 cm",
        height_corrected < 100 ~ "50-100 cm",
        height_corrected < 150 ~ "100-150 cm",
        TRUE                   ~ ">150 cm"
      ),
      levels = c("0-50 cm", "50-100 cm", "100-150 cm", ">150 cm")
    ),
    y_ch4 = asinh(CH4_best.flux)
  )

# Keep species with >= 5 observations at shared sites
sp_counts <- table(stem_species$species)
keep_spp <- names(sp_counts[sp_counts >= 5])
stem_species <- stem_species %>%
  filter(species %in% keep_spp) %>%
  mutate(species = droplevels(species))

cat("Species analysis (shared sites only: BL60, SRS5, SRS6)\n")
cat("n per species:\n")
print(table(stem_species$species))
cat("\nSpecies × site:\n")
print(table(stem_species$species, stem_species$plot))

# --- Model 1: Overall (height as continuous covariate) ---
m_overall <- lmer(y_ch4 ~ species + height_corrected + season_agg + (1 | plot),
                  data = stem_species)
cat("\n--- Overall model summary ---\n")
print(summary(m_overall))

emm_overall <- emmeans(m_overall, "species")
emm_overall_df <- as.data.frame(summary(emm_overall))

# Pairwise contrasts
pairs_overall <- as.data.frame(pairs(emm_overall))
cat("\n--- Overall pairwise contrasts ---\n")
print(pairs_overall)

# Get sample sizes for labeling
n_per_species <- stem_species %>%
  group_by(species) %>%
  summarise(n = n(), .groups = "drop")
emm_overall_df <- left_join(emm_overall_df, n_per_species, by = "species")

# --- Model 2: Species × height_cat interaction (0-50, 50-100, 100-150 only) ---
stem_ht <- stem_species %>%
  filter(height_cat %in% c("0-50 cm", "50-100 cm", "100-150 cm")) %>%
  mutate(height_cat = droplevels(height_cat))

m_byheight <- lmer(y_ch4 ~ species * height_cat + season_agg + (1 | plot),
                   data = stem_ht)
cat("\n--- Species × height model summary ---\n")
print(summary(m_byheight))

emm_byheight <- emmeans(m_byheight, ~ species | height_cat)
emm_byheight_df <- as.data.frame(summary(emm_byheight))

# Drop non-estimable rows
emm_byheight_df <- emm_byheight_df %>% filter(!is.na(emmean))

# Sample sizes per species × height
n_sp_ht <- stem_ht %>%
  group_by(species, height_cat) %>%
  summarise(n = n(), .groups = "drop")
emm_byheight_df <- left_join(emm_byheight_df, n_sp_ht,
                             by = c("species", "height_cat"))

# Pairwise contrasts within each height
pairs_byheight <- as.data.frame(pairs(emm_byheight))
cat("\n--- Pairwise contrasts by height ---\n")
print(pairs_byheight)

# --- Species display names ---
species_labels <- c(
  "AVGE" = "A. germinans",
  "COER" = "C. erectus",
  "LARA" = "L. racemosa",
  "RHMA" = "R. mangle"
)

emm_overall_df$species_label <- species_labels[as.character(emm_overall_df$species)]
emm_byheight_df$species_label <- species_labels[as.character(emm_byheight_df$species)]

# Species color palette — named for common names:
#   A. germinans = black mangrove → black
#   R. mangle    = red mangrove   → red
#   L. racemosa  = white mangrove → grey (white not visible on white bg)
#   C. erectus   = buttonwood     → blue
species_colors <- c(
  "A. germinans" = "#1a1a1a",
  "C. erectus"   = "#2166AC",
  "L. racemosa"  = "#878787",
  "R. mangle"    = "#B2182B"
)

# --- Panel (a): Overall emmeans ---
fig9a <- emm_overall_df %>%
  mutate(species_label = fct_reorder(species_label, emmean)) %>%
  ggplot(aes(x = emmean, y = species_label, color = species_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_errorbar(aes(xmin = lower.CL, xmax = upper.CL),
                width = 0.25, linewidth = 0.7, orientation = "y") +
  geom_point(size = 3) +
  geom_text(aes(label = paste0("n=", n)),
            hjust = -0.3, vjust = -0.8, size = 3, color = "grey30") +
  scale_color_manual(values = species_colors, guide = "none") +
  scale_x_continuous(
    name = expression(Estimated~Marginal~Mean~CH[4]~Flux~(asinh~scale)),
    sec.axis = sec_axis(~ sinh(.),
                        name = expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1})),
                        breaks = c(0, 0.5, 1, 2, 5))
  ) +
  labs(y = NULL, tag = "(a)",
       subtitle = "Controlled for height & season; shared sites only") +
  theme_pub(base_size = 10) +
  theme(
    axis.text.y = element_text(face = "italic", size = 10),
    plot.margin = margin(5, 10, 5, 5)
  )

# --- Panel (b): Emmeans by height category — single panel, species dodged by color ---
# Height on y-axis (0-50 at bottom → 100-150 at top), flux on x-axis
# Pre-compute label positions to avoid position_dodge issues with geom_text
pd <- position_dodge(width = 0.6)

fig9b <- emm_byheight_df %>%
  mutate(
    species_label = factor(species_label, levels = names(species_colors)),
    height_cat = factor(height_cat, levels = c("0-50 cm", "50-100 cm", "100-150 cm"))
  ) %>%
  ggplot(aes(x = emmean, y = height_cat, color = species_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_errorbar(aes(xmin = lower.CL, xmax = upper.CL),
                width = 0.2, linewidth = 0.7, orientation = "y",
                position = pd) +
  geom_point(size = 3, position = pd) +
  scale_color_manual(values = species_colors,
                     name = "Species",
                     labels = function(x) parse(text = paste0("italic('", x, "')"))) +
  scale_x_continuous(
    name = expression(Estimated~Marginal~Mean~CH[4]~Flux~(asinh~scale))
  ) +
  labs(y = "Measurement Height", tag = "(b)",
       subtitle = "Species x height interaction; shared sites only") +
  theme_pub(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.text.align = 0,
    plot.margin = margin(5, 5, 5, 5)
  )

# --- Combine: (a) top, (b) bottom ---
fig9 <- fig9a / fig9b + plot_layout(heights = c(1, 1.2))
# save_pub(fig9, "species_emmeans", width = 200, height = 200)

cat("\n--- Species emmeans figure saved ---\n")


# =============================================================================
# Figure 10: Stem Height Composite (4-quadrant)
# =============================================================================
# Layout:
#   Top left  (a): Density ridges by disturbance (from fig4a)
#   Bot left  (b): Box/jitter by disturbance × height (from fig4b)
#   Top right (c): Placeholder — surface area by height (TBD)
#   Bot right (d): Species emmeans by height (from fig9b)
# Disturbance legend shared at bottom.
# =============================================================================
cat("\n--- Figure 10: Stem Height Composite ---\n")

# (a) Ridges — legend at top, x-axis hidden (aligned with b via cowplot)
fig10a <- stem_height %>%
  ggplot(aes(x = CH4_best.flux, y = height_category, fill = disturbance_level)) +
  geom_density_ridges(alpha = 0.6, scale = 0.9, bandwidth = 1.0) +
  scale_x_continuous(trans = "asinh", limits = x_range,
                     breaks = asinh_brk_pos, labels = asinh_labels) +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.45))) +
  scale_fill_manual(values = disturbance_colors, name = "Disturbance Level") +
  labs(y = "Height Category", tag = "(a)") +
  theme_pub(base_size = 9) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 8, face = "bold"),
        legend.text = element_text(size = 7),
        plot.margin = margin(5, 5, 0, 5))

# (b) Box/jitter — rebuild without legend
fig10b <- stem_height %>%
  mutate(disturbance_level = factor(disturbance_level,
                                     levels = rev(c("healthy", "regenerating", "ghost")))) %>%
  ggplot(aes(x = CH4_best.flux,
             y = interaction(disturbance_level, height_category, sep = " - "),
             fill = disturbance_level)) +
  geom_jitter(aes(color = disturbance_level),
             height = 0.2, width = 0, alpha = 0.5, size = 2) +
  geom_boxplot(width = 0.4, alpha = 0.7, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 23,
               size = 2.5, fill = alpha("white", 0.6), color = alpha("black", 0.6), stroke = 0.7, alpha = 0.7) +
  scale_x_continuous(trans = "asinh", limits = x_range,
                     breaks = asinh_brk_pos, labels = asinh_labels) +
  scale_fill_manual(values = disturbance_colors, guide = "none") +
  scale_color_manual(values = disturbance_colors, guide = "none") +
  scale_y_discrete(labels = function(x) sapply(strsplit(x, " - "), `[`, 1)) +
  labs(x = expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1})), y = NULL, tag = "(b)") +
  facet_grid(rows = vars(factor(height_category,
                                levels = rev(c("0-50 cm", "50-100 cm", "100-150 cm", ">150 cm")))),
             scales = "free_y", space = "free_y", switch = "y") +
  theme_pub(base_size = 9) +
  theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0, size = 8),
        legend.position = "none", plot.margin = margin(0, 5, 5, 5))

# Align (a) and (b) on x-axis using cowplot
ab_aligned <- align_plots(fig10a, fig10b, align = "v", axis = "lr")
fig10a_aligned <- ab_aligned[[1]]
fig10b_aligned <- ab_aligned[[2]]

# (c) Placeholder — surface area by height profiles (TBD)
fig10c <- ggplot(data.frame(x = 0.5, y = 0.5), aes(x, y)) +
  annotate("text", x = 0.5, y = 0.5, label = "Surface area\nby height\n(TBD)",
           size = 5, color = "grey50", fontface = "italic") +
  labs(tag = "(c)") +
  theme_void(base_size = 9) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
    plot.tag = element_text(size = 12, face = "bold"),
    plot.margin = margin(5, 5, 10, 5)
  )

# (d) Species × status emmeans by height
# Group: COER total, LARA total, AVGE alive, AVGE dead, RHMA alive, RHMA dead
# Use all sites with both alive & dead trees (BL60, CP40, FLM30, SRS5, SRS6)
ad_sites <- c("BL60", "CP40", "FLM30", "SRS5", "SRS6")

stem_ad_ht <- df %>%
  filter(component == "stem", plot %in% ad_sites,
         !is.na(species), species != "UNKN", species != "",
         !is.na(CH4_best.flux), !is.na(height_corrected), height_corrected >= 0,
         !is.na(status), status != "CWD") %>%
  mutate(
    species = ifelse(species == "COPE", "COER", species),
    alive = ifelse(status %in% c("alive", "Alive"), "Alive", "Dead"),
    height_cat = factor(case_when(
      height_corrected < 50 ~ "0-50 cm", height_corrected < 100 ~ "50-100 cm",
      height_corrected < 150 ~ "100-150 cm", TRUE ~ ">150 cm"
    ), levels = c("0-50 cm", "50-100 cm", "100-150 cm", ">150 cm")),
    y_ch4 = asinh(CH4_best.flux),
    plot = factor(plot)
  ) %>%
  filter(height_cat %in% c("0-50 cm", "50-100 cm", "100-150 cm"),
         species %in% c("AVGE", "COER", "LARA", "RHMA")) %>%
  mutate(
    # Create combined species_status grouping:
    # COER/LARA = pooled (no alive/dead split), AVGE/RHMA = split
    sp_status = factor(case_when(
      species == "COER" ~ "COER",
      species == "LARA" ~ "LARA",
      species == "AVGE" & alive == "Alive" ~ "AVGE_alive",
      species == "AVGE" & alive == "Dead"  ~ "AVGE_dead",
      species == "RHMA" & alive == "Alive" ~ "RHMA_alive",
      species == "RHMA" & alive == "Dead"  ~ "RHMA_dead"
    )),
    height_cat = droplevels(height_cat)
  )

cat("\n--- Panel (d): species × status × height ---\n")
cat("n per group:\n")
print(table(stem_ad_ht$sp_status, stem_ad_ht$height_cat))

# Model: sp_status + height_cat + season + (1|plot)
m_d <- lmer(y_ch4 ~ sp_status + height_cat + season_agg + (1 | plot),
            data = stem_ad_ht)

emm_d <- emmeans(m_d, ~ sp_status | height_cat)
emm_d_df <- as.data.frame(summary(emm_d)) %>% filter(!is.na(emmean))

# Add display labels and colors
emm_d_df <- emm_d_df %>%
  mutate(
    label = case_when(
      sp_status == "AVGE_alive" ~ "A. germinans (alive)",
      sp_status == "AVGE_dead"  ~ "A. germinans (dead)",
      sp_status == "COER"       ~ "C. erectus",
      sp_status == "LARA"       ~ "L. racemosa",
      sp_status == "RHMA_alive" ~ "R. mangle (alive)",
      sp_status == "RHMA_dead"  ~ "R. mangle (dead)"
    ),
    label = factor(label, levels = c(
      "A. germinans (dead)", "A. germinans (alive)",
      "C. erectus", "L. racemosa",
      "R. mangle (dead)", "R. mangle (alive)"
    ))
  )

# Colors: same color per species, regardless of alive/dead
spst_colors <- c(
  "A. germinans (alive)" = "#1a1a1a",
  "A. germinans (dead)"  = "#1a1a1a",
  "C. erectus"           = "#2166AC",
  "L. racemosa"          = "#878787",
  "R. mangle (alive)"    = "#B2182B",
  "R. mangle (dead)"     = "#B2182B"
)

# Shapes: filled circle for alive/total, open circle for dead
spst_shapes <- c(
  "A. germinans (alive)" = 19,
  "A. germinans (dead)"  = 1,
  "C. erectus"           = 19,
  "L. racemosa"          = 19,
  "R. mangle (alive)"    = 19,
  "R. mangle (dead)"     = 1
)

pd10 <- position_dodge(width = 0.7)

# Back-transform asinh emmeans to real flux units for x-axis labels
emm_breaks <- asinh(c(-1, 0, 1, 2, 5, 10))
emm_labels <- c("-1", "0", "1", "2", "5", "10")

fig10d <- emm_d_df %>%
  ggplot(aes(x = emmean, y = height_cat, color = label, shape = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_errorbar(aes(xmin = lower.CL, xmax = upper.CL),
                width = 0.3, linewidth = 0.5, orientation = "y",
                position = pd10) +
  geom_point(size = 2.5, stroke = 0.8, position = pd10) +
  scale_color_manual(values = spst_colors, name = NULL) +
  scale_shape_manual(values = spst_shapes, name = NULL) +
  scale_x_continuous(
    breaks = emm_breaks, labels = emm_labels,
    name = expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1}))
  ) +
  labs(y = "Measurement Height", tag = "(d)") +
  guides(color = guide_legend(ncol = 3, byrow = TRUE,
                              override.aes = list(size = 3)),
         shape = guide_legend(ncol = 3, byrow = TRUE,
                              override.aes = list(size = 3))) +
  theme_pub(base_size = 9) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 7, face = "italic"),
        plot.margin = margin(0, 5, 5, 5))

# --- Assemble 4-quadrant layout ---
# Top: (a) ridges + (c) placeholder | Bottom: (b) box/jitter + (d) emmeans
# (a) and (b) are pre-aligned on x-axis; legend is at top of (a), bottom of (d)
top_row <- plot_grid(fig10a_aligned, fig10c, nrow = 1, rel_widths = c(1, 1))
bot_row <- plot_grid(fig10b_aligned, fig10d, nrow = 1, rel_widths = c(1, 1))

fig10 <- plot_grid(top_row, bot_row,
                   ncol = 1, rel_heights = c(1, 1))

# save_pub(fig10, "stem_height_composite", width = 260, height = 200)

cat("\n--- Stem height composite figure saved ---\n")


# =============================================================================
# Figure 10b: CO2 Stem Height Composite (4-quadrant, same layout as CH4)
# =============================================================================
cat("\n--- Figure 10b: CO2 Stem Height Composite ---\n")

# Prepare CO2 stem height data (same sites as CH4)
stem_height_co2 <- df %>%
  filter(component == "stem", CO2_flux_status == "valid",
         !is.na(height_category), !is.na(CO2_best.flux),
         plot %in% c("SRS5", "SRS6", "BL60", "FLM30", "CP40")) %>%
  mutate(disturbance_level = droplevels(disturbance_level))

x_range_co2_raw <- range(stem_height_co2$CO2_best.flux, na.rm = TRUE)
x_range_co2 <- c(sinh(asinh(x_range_co2_raw[1]) - 1.5),
                 sinh(asinh(x_range_co2_raw[2]) + 1.5))
co2_brk <- c(-10, 0, 10, 100, 1000)

# (a) Ridges
fig10b_a <- stem_height_co2 %>%
  ggplot(aes(x = CO2_best.flux, y = height_category, fill = disturbance_level)) +
  geom_density_ridges(alpha = 0.6, scale = 0.9, bandwidth = 0.8) +
  scale_x_continuous(trans = "asinh", limits = x_range_co2,
                     breaks = co2_brk, labels = asinh_labels) +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.45))) +
  scale_fill_manual(values = disturbance_colors, name = "Disturbance Level") +
  labs(y = "Height Category", tag = "(a)") +
  theme_pub(base_size = 9) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 8, face = "bold"),
        legend.text = element_text(size = 7),
        plot.margin = margin(5, 5, 0, 5))

# (b) Box/jitter
fig10b_b <- stem_height_co2 %>%
  mutate(disturbance_level = factor(disturbance_level,
                                     levels = rev(c("healthy", "regenerating", "ghost")))) %>%
  ggplot(aes(x = CO2_best.flux,
             y = interaction(disturbance_level, height_category, sep = " - "),
             fill = disturbance_level)) +
  geom_jitter(aes(color = disturbance_level),
             height = 0.2, width = 0, alpha = 0.5, size = 2) +
  geom_boxplot(width = 0.4, alpha = 0.7, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 23,
               size = 2.5, fill = alpha("white", 0.6), color = alpha("black", 0.6), stroke = 0.7, alpha = 0.7) +
  scale_x_continuous(trans = "asinh", limits = x_range_co2,
                     breaks = co2_brk, labels = asinh_labels) +
  scale_fill_manual(values = disturbance_colors, guide = "none") +
  scale_color_manual(values = disturbance_colors, guide = "none") +
  scale_y_discrete(labels = function(x) sapply(strsplit(x, " - "), `[`, 1)) +
  labs(x = expression(CO[2]~Flux~(mu*mol~m^{-2}~s^{-1})), y = NULL, tag = "(b)") +
  facet_grid(rows = vars(factor(height_category,
                                levels = rev(c("0-50 cm", "50-100 cm", "100-150 cm", ">150 cm")))),
             scales = "free_y", space = "free_y", switch = "y") +
  theme_pub(base_size = 9) +
  theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0, size = 8),
        legend.position = "none", plot.margin = margin(0, 5, 5, 5))

# Align (a) and (b) on x-axis
ab_co2_aligned <- align_plots(fig10b_a, fig10b_b, align = "v", axis = "lr")

# (c) Placeholder
fig10b_c <- ggplot(data.frame(x = 0.5, y = 0.5), aes(x, y)) +
  annotate("text", x = 0.5, y = 0.5, label = "Surface area\nby height\n(TBD)",
           size = 5, color = "grey50", fontface = "italic") +
  labs(tag = "(c)") +
  theme_void(base_size = 9) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
    plot.tag = element_text(size = 12, face = "bold"),
    plot.margin = margin(5, 5, 10, 5)
  )

# (d) Species × status CO2 emmeans by height
stem_ad_ht_co2 <- df %>%
  filter(component == "stem", plot %in% ad_sites,
         !is.na(species), species != "UNKN", species != "",
         !is.na(CO2_best.flux), !is.na(height_corrected), height_corrected >= 0,
         !is.na(status), status != "CWD") %>%
  mutate(
    species = ifelse(species == "COPE", "COER", species),
    alive = ifelse(status %in% c("alive", "Alive"), "Alive", "Dead"),
    height_cat = factor(case_when(
      height_corrected < 50 ~ "0-50 cm", height_corrected < 100 ~ "50-100 cm",
      height_corrected < 150 ~ "100-150 cm", TRUE ~ ">150 cm"
    ), levels = c("0-50 cm", "50-100 cm", "100-150 cm", ">150 cm")),
    y_co2 = asinh(CO2_best.flux),
    plot = factor(plot)
  ) %>%
  filter(height_cat %in% c("0-50 cm", "50-100 cm", "100-150 cm"),
         species %in% c("AVGE", "COER", "LARA", "RHMA")) %>%
  mutate(
    sp_status = factor(case_when(
      species == "COER" ~ "COER",
      species == "LARA" ~ "LARA",
      species == "AVGE" & alive == "Alive" ~ "AVGE_alive",
      species == "AVGE" & alive == "Dead"  ~ "AVGE_dead",
      species == "RHMA" & alive == "Alive" ~ "RHMA_alive",
      species == "RHMA" & alive == "Dead"  ~ "RHMA_dead"
    )),
    height_cat = droplevels(height_cat)
  )

cat("\n--- Panel (d) CO2: species × status × height ---\n")
cat("n per group:\n")
print(table(stem_ad_ht_co2$sp_status, stem_ad_ht_co2$height_cat))

m_d_co2 <- lmer(y_co2 ~ sp_status + height_cat + season_agg + (1 | plot),
                data = stem_ad_ht_co2)

emm_d_co2 <- emmeans(m_d_co2, ~ sp_status | height_cat)
emm_d_co2_df <- as.data.frame(summary(emm_d_co2)) %>% filter(!is.na(emmean))

emm_d_co2_df <- emm_d_co2_df %>%
  mutate(
    label = case_when(
      sp_status == "AVGE_alive" ~ "A. germinans (alive)",
      sp_status == "AVGE_dead"  ~ "A. germinans (dead)",
      sp_status == "COER"       ~ "C. erectus",
      sp_status == "LARA"       ~ "L. racemosa",
      sp_status == "RHMA_alive" ~ "R. mangle (alive)",
      sp_status == "RHMA_dead"  ~ "R. mangle (dead)"
    ),
    label = factor(label, levels = c(
      "A. germinans (dead)", "A. germinans (alive)",
      "C. erectus", "L. racemosa",
      "R. mangle (dead)", "R. mangle (alive)"
    ))
  )

# CO2 emmeans axis breaks (in µmol m-2 s-1)
emm_co2_breaks <- asinh(c(0, 1, 2, 5, 10, 20))
emm_co2_labels <- c("0", "1", "2", "5", "10", "20")

fig10b_d <- emm_d_co2_df %>%
  ggplot(aes(x = emmean, y = height_cat, color = label, shape = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_errorbar(aes(xmin = lower.CL, xmax = upper.CL),
                width = 0.3, linewidth = 0.5, orientation = "y",
                position = pd10) +
  geom_point(size = 2.5, stroke = 0.8, position = pd10) +
  scale_color_manual(values = spst_colors, name = NULL) +
  scale_shape_manual(values = spst_shapes, name = NULL) +
  scale_x_continuous(
    breaks = emm_co2_breaks, labels = emm_co2_labels,
    name = expression(CO[2]~Flux~(mu*mol~m^{-2}~s^{-1}))
  ) +
  labs(y = "Measurement Height", tag = "(d)") +
  guides(color = guide_legend(ncol = 3, byrow = TRUE,
                              override.aes = list(size = 3)),
         shape = guide_legend(ncol = 3, byrow = TRUE,
                              override.aes = list(size = 3))) +
  theme_pub(base_size = 9) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 7, face = "italic"),
        plot.margin = margin(0, 5, 5, 5))

# --- Assemble CO2 4-quadrant layout ---
top_row_co2 <- plot_grid(ab_co2_aligned[[1]], fig10b_c, nrow = 1, rel_widths = c(1, 1))
bot_row_co2 <- plot_grid(ab_co2_aligned[[2]], fig10b_d, nrow = 1, rel_widths = c(1, 1))

fig10_co2 <- plot_grid(top_row_co2, bot_row_co2,
                       ncol = 1, rel_heights = c(1, 1))

# save_pub(fig10_co2, "stem_height_composite_co2", width = 260, height = 200)

cat("\n--- CO2 stem height composite figure saved ---\n")


# =============================================================================
# Figure 10c: Combined CH4 + CO2 Height Composite (3 rows × 2 columns)
# =============================================================================
# Left col: CH4 (ridges, box/jitter, species emmeans)
# Right col: CO2 (ridges, box/jitter, species emmeans)
# Shared disturbance legend (top) and species legend (bottom)
# =============================================================================
cat("\n--- Figure 10c: Combined CH4 + CO2 Height Composite ---\n")

no_legend <- theme(legend.position = "none")

# --- CH4 column (rebuild without individual legends) ---
# --- CH4 column (same aesthetics as individual composites, base_size=9) ---
ch4_ridges <- stem_height %>%
  ggplot(aes(x = CH4_best.flux, y = height_category, fill = disturbance_level)) +
  geom_density_ridges(alpha = 0.6, scale = 0.9, bandwidth = 1.0) +
  scale_x_continuous(trans = "asinh", limits = x_range,
                     breaks = asinh_brk_pos, labels = asinh_labels) +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.45))) +
  scale_fill_manual(values = disturbance_colors, name = "Disturbance Level") +
  labs(y = "Height Category") +
  theme_pub(base_size = 9) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(5, 5, 0, 5)) +
  no_legend

ch4_box <- stem_height %>%
  mutate(disturbance_level = factor(disturbance_level,
                                     levels = rev(c("healthy", "regenerating", "ghost")))) %>%
  ggplot(aes(x = CH4_best.flux,
             y = interaction(disturbance_level, height_category, sep = " - "),
             fill = disturbance_level)) +
  geom_jitter(aes(color = disturbance_level),
             height = 0.2, width = 0, alpha = 0.5, size = 2) +
  geom_boxplot(width = 0.4, alpha = 0.7, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 23,
               size = 2.5, fill = alpha("white", 0.6), color = alpha("black", 0.6), stroke = 0.7, alpha = 0.7) +
  scale_x_continuous(trans = "asinh", limits = x_range,
                     breaks = asinh_brk_pos, labels = asinh_labels) +
  scale_fill_manual(values = disturbance_colors, guide = "none") +
  scale_color_manual(values = disturbance_colors, guide = "none") +
  scale_y_discrete(labels = function(x) sapply(strsplit(x, " - "), `[`, 1)) +
  labs(x = expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1})), y = NULL) +
  facet_grid(rows = vars(factor(height_category,
                                levels = rev(c("0-50 cm", "50-100 cm", "100-150 cm", ">150 cm")))),
             scales = "free_y", space = "free_y", switch = "y") +
  theme_pub(base_size = 9) +
  theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0, size = 8),
        plot.margin = margin(0, 5, 5, 5)) +
  no_legend

ch4_emm <- emm_d_df %>%
  ggplot(aes(x = emmean, y = height_cat, color = label, shape = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_errorbar(aes(xmin = lower.CL, xmax = upper.CL),
                width = 0.3, linewidth = 0.5, orientation = "y",
                position = pd10) +
  geom_point(size = 2.5, stroke = 0.8, position = pd10) +
  scale_color_manual(values = spst_colors, name = NULL) +
  scale_shape_manual(values = spst_shapes, name = NULL) +
  scale_x_continuous(breaks = emm_breaks, labels = emm_labels,
                     name = expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1}))) +
  labs(y = "Measurement Height") +
  theme_pub(base_size = 9) +
  theme(plot.margin = margin(0, 5, 5, 5)) +
  no_legend

# --- CO2 column (same aesthetics, drop redundant y-axis labels) ---
co2_ridges <- stem_height_co2 %>%
  ggplot(aes(x = CO2_best.flux, y = height_category, fill = disturbance_level)) +
  geom_density_ridges(alpha = 0.6, scale = 0.9, bandwidth = 0.8) +
  scale_x_continuous(trans = "asinh", limits = x_range_co2,
                     breaks = co2_brk, labels = asinh_labels) +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.45))) +
  scale_fill_manual(values = disturbance_colors, name = "Disturbance Level") +
  labs(y = NULL) +
  theme_pub(base_size = 9) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.margin = margin(5, 5, 0, 5)) +
  no_legend

co2_box <- stem_height_co2 %>%
  mutate(disturbance_level = factor(disturbance_level,
                                     levels = rev(c("healthy", "regenerating", "ghost")))) %>%
  ggplot(aes(x = CO2_best.flux,
             y = interaction(disturbance_level, height_category, sep = " - "),
             fill = disturbance_level)) +
  geom_jitter(aes(color = disturbance_level),
             height = 0.2, width = 0, alpha = 0.5, size = 2) +
  geom_boxplot(width = 0.4, alpha = 0.7, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 23,
               size = 2.5, fill = alpha("white", 0.6), color = alpha("black", 0.6), stroke = 0.7, alpha = 0.7) +
  scale_x_continuous(trans = "asinh", limits = x_range_co2,
                     breaks = co2_brk, labels = asinh_labels) +
  scale_fill_manual(values = disturbance_colors, guide = "none") +
  scale_color_manual(values = disturbance_colors, guide = "none") +
  scale_y_discrete(labels = function(x) sapply(strsplit(x, " - "), `[`, 1)) +
  labs(x = expression(CO[2]~Flux~(mu*mol~m^{-2}~s^{-1})), y = NULL) +
  facet_grid(rows = vars(factor(height_category,
                                levels = rev(c("0-50 cm", "50-100 cm", "100-150 cm", ">150 cm")))),
             scales = "free_y", space = "free_y", switch = "y") +
  theme_pub(base_size = 9) +
  theme(strip.placement = "outside", strip.text.y.left = element_blank(),
        plot.margin = margin(0, 5, 5, 5)) +
  no_legend

co2_emm <- emm_d_co2_df %>%
  ggplot(aes(x = emmean, y = height_cat, color = label, shape = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_errorbar(aes(xmin = lower.CL, xmax = upper.CL),
                width = 0.3, linewidth = 0.5, orientation = "y",
                position = pd10) +
  geom_point(size = 2.5, stroke = 0.8, position = pd10) +
  scale_color_manual(values = spst_colors, name = NULL) +
  scale_shape_manual(values = spst_shapes, name = NULL) +
  scale_x_continuous(breaks = emm_co2_breaks, labels = emm_co2_labels,
                     name = expression(CO[2]~Flux~(mu*mol~m^{-2}~s^{-1}))) +
  labs(y = NULL) +
  theme_pub(base_size = 9) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.margin = margin(0, 5, 5, 5)) +
  no_legend

# --- Build combined figure with patchwork ---
library(patchwork)

# Add disturbance legend back to CH4 ridges (top of left column)
ch4_ridges_leg <- ch4_ridges +
  scale_fill_manual(values = disturbance_colors, name = "Disturbance") +
  theme(legend.position = "top",
        legend.title = element_text(size = 8, face = "bold"),
        legend.text = element_text(size = 7))

# Add species legend back to CH4 emmeans (bottom of left column)
ch4_emm_leg <- ch4_emm +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 7, face = "italic")) +
  guides(color = guide_legend(ncol = 3, byrow = TRUE,
                              override.aes = list(size = 2.5)),
         shape = guide_legend(ncol = 3, byrow = TRUE,
                              override.aes = list(size = 2.5)))

# Column titles as plot_annotation
ch4_col <- (ch4_ridges_leg / ch4_box / ch4_emm_leg) +
  plot_layout(heights = c(0.8, 1.2, 0.8)) +
  plot_annotation(title = expression(CH[4]~Flux),
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11)))

co2_col <- (co2_ridges / co2_box / co2_emm) +
  plot_layout(heights = c(0.8, 1.2, 0.8)) +
  plot_annotation(title = expression(CO[2]~Flux),
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11)))

# Combine columns side by side, add sequential tags
fig10_combined <- (ch4_col | co2_col) +
  plot_layout(widths = c(1, 0.85)) +
  plot_annotation(tag_levels = list(c("a", "b", "c", "d", "e", "f")),
                  theme = theme(plot.tag = element_text(size = 10, face = "bold")))

save_pub(fig10_combined, "stem_height_composite_combined", width = 260, height = 280)

cat("\n--- Combined CH4+CO2 height composite figure saved ---\n")
