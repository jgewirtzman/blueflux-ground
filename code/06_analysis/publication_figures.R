# =============================================================================
# Publication Figures for Mangrove GHG Flux Manuscript
# =============================================================================
# Generates publication-quality figures from the combined gas flux dataset.
# Each figure is saved as both PDF (vector) and PNG (300 dpi).
#
# Figures:
#   1.  Component flux overview (CH4 + CO2) — all data
#   1b. Component flux overview split by season (wet vs dry, core sites)
#   2.  Component jitterbox by plot x campaign — all 8 sites (+ core-only version)
#   3.  Seasonal variation by component (wet vs dry, core sites only)
#   4.  Stem height profile (ridges + box/jitter)
#   5.  Spatial/disturbance gradient — CH4 row, CO2 row
#   6.  Environmental drivers (water depth + temperature)
#   7.  CH4 vs CO2 covariation
#   8.  Heatmap summary (component x site x season)
#
# Site groupings:
#   SE1: scrub | BL60: regenerating
#   CP40, FLM30, MI: ghost | RB10, SRS5, SRS6: healthy
#
# Core sites (sampled in both wet & dry): BL60, CP40, FLM30, SRS5, SRS6
# =============================================================================

# --- Libraries ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(patchwork)
library(scales)
library(forcats)
library(cowplot)
library(ggh4x)  # for facet_nested

# --- Data Loading & Preprocessing ---------------------------------------------
df <- read.csv("output/combined_gas_flux_dataset.csv")

# Main study plots
selected_plots <- c("BL60", "CP40", "FLM30", "MI", "SE1", "SRS5", "SRS6", "RB10")

# Core sites sampled in BOTH wet and dry seasons
core_sites <- c("BL60", "CP40", "FLM30", "SRS5", "SRS6")

df <- df %>%
  filter(plot %in% selected_plots) %>%
  mutate(
    # Disturbance level (ordered factor)
    disturbance_level = factor(
      disturbance_level,
      levels = c("healthy", "regenerating", "ghost", "scrub")
    ),
    # Season aggregation (wet vs dry only)
    season_agg = factor(
      case_when(
        season == "wet" ~ "Wet",
        season == "dry" ~ "Dry",
        TRUE ~ NA_character_
      ),
      levels = c("Wet", "Dry")
    ),
    # Title-case component for display
    component_display = factor(
      tools::toTitleCase(component),
      levels = c("Soil", "Root", "Pneumatophore", "Stem", "Leaves", "Cwd", "Water")
    ),
    # Site label with disturbance annotation
    site_label = paste0(plot, " (", disturbance_level, ")"),
    # Height category for stem data
    height_category = case_when(
      component == "stem" & !is.na(height_corrected) & height_corrected >= 0 &
        height_corrected < 50 ~ "0-50 cm",
      component == "stem" & !is.na(height_corrected) &
        height_corrected >= 50 & height_corrected < 100 ~ "50-100 cm",
      component == "stem" & !is.na(height_corrected) &
        height_corrected >= 100 & height_corrected < 150 ~ "100-150 cm",
      component == "stem" & !is.na(height_corrected) &
        height_corrected >= 150 ~ ">150 cm",
      TRUE ~ NA_character_
    ),
    height_category = factor(height_category,
                             levels = c("0-50 cm", "50-100 cm", "100-150 cm", ">150 cm")),
    # Nicer month_year labels
    campaign = factor(month_year,
                      levels = c("2022-03", "2022-10", "2023-03"),
                      labels = c("Mar 2022 (dry)", "Oct 2022 (wet)", "Mar 2023 (dry)")),
    # Plot ordered by disturbance for facet_nested
    plot = factor(plot, levels = c("RB10", "SRS5", "SRS6",
                                   "BL60",
                                   "CP40", "FLM30", "MI",
                                   "SE1"))
  )

# Order site_label by disturbance group, then alphabetically within
site_order <- df %>%
  distinct(plot, disturbance_level, site_label) %>%
  arrange(disturbance_level, plot) %>%
  pull(site_label)
df$site_label <- factor(df$site_label, levels = site_order)

# Unified temperature column (component-appropriate)
df <- df %>%
  mutate(
    temp = case_when(
      component == "stem" ~ stem_temp,
      component == "soil" ~ soil_temp,
      component == "root" ~ stem_temp,
      component == "water" ~ water_temp,
      TRUE ~ coalesce(stem_temp, soil_temp)
    )
  )

cat("Dataset loaded:", nrow(df), "rows,", ncol(df), "columns\n")
cat("Core sites for seasonal comparison:", paste(core_sites, collapse = ", "), "\n")

# --- Shared Definitions -------------------------------------------------------

# Color palettes
component_colors <- c(
  "Soil" = "#8B4513", "Stem" = "#228B22", "Water" = "#4682B4",
  "Root" = "#D2691E", "Cwd" = "#654321", "Pneumatophore" = "#32CD32",
  "Leaves" = "#90EE90"
)

component_colors_lc <- c(
  "soil" = "#8B4513", "stem" = "#228B22", "water" = "#4682B4",
  "root" = "#D2691E", "cwd" = "#654321", "pneumatophore" = "#32CD32",
  "leaves" = "#90EE90"
)

disturbance_colors <- c(
  "healthy" = "#228B22", "regenerating" = "#808080",
  "ghost" = "#8B4513", "scrub" = "#DAA520"
)

season_colors <- c("Wet" = "#4682B4", "Dry" = "#D2691E")

# Shared publication theme
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

# Asinh scale breaks — drop 0.1 to avoid overlap with 0
# Gap analysis: asinh(0)->0, asinh(0.1)->0.10, asinh(1)->0.88 — 0 and 0.1 overlap
asinh_brk     <- c(-100, -10, 0, 10, 100, 1000)  # full range (no -1/0.1)
asinh_brk_pos <- c(0, 1, 10, 100, 1000)           # positive only

# Clean numeric labels for asinh axes
asinh_labels <- function(x) {
  ifelse(x == 0, "0", format(x, scientific = FALSE, big.mark = "", drop0trailing = TRUE))
}

# Mean diamond geom (reusable)
mean_diamond <- function(...) {
  stat_summary(fun = mean, geom = "point", shape = 23,
               size = 2.5, fill = "white", color = "black", stroke = 0.7, ...)
}

# Save helper (PDF + PNG)
save_pub <- function(plot, name, width, height, units = "mm") {
  ggsave(paste0("output/figures/pub_", name, ".pdf"), plot,
         width = width, height = height, units = units)
  ggsave(paste0("output/figures/pub_", name, ".png"), plot,
         width = width, height = height, units = units, dpi = 300)
  cat("Saved: pub_", name, ".pdf/.png\n", sep = "")
}


# =============================================================================
# FIGURE 1: Component Flux Overview (CH4 + CO2) — all data pooled
# =============================================================================
cat("\n--- Figure 1: Component Flux Overview ---\n")

df_ch4 <- df %>% filter(CH4_flux_status == "valid", !is.na(component_display))
ch4_n <- df_ch4 %>% group_by(component_display) %>%
  summarise(n = n(), med = median(CH4_best.flux), .groups = "drop")

fig1a <- df_ch4 %>%
  ggplot(aes(x = CH4_best.flux,
             y = fct_reorder(component_display, CH4_best.flux, .fun = median),
             fill = component_display, color = component_display)) +
  geom_jitter(alpha = 0.4, size = 1, height = 0.25) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA, color = "black", width = 0.5) +
  mean_diamond() +
  geom_text(data = ch4_n %>% mutate(y = fct_reorder(component_display, med)),
            aes(x = Inf, y = y, label = paste0("n=", n)),
            inherit.aes = FALSE, hjust = 1.1, size = 3, color = "grey40") +
  scale_x_continuous(trans = "asinh", breaks = asinh_brk_pos, labels = asinh_labels) +
  scale_fill_manual(values = component_colors, guide = "none") +
  scale_color_manual(values = component_colors, guide = "none") +
  labs(x = expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1})), y = NULL, tag = "(a)") +
  theme_pub() +
  theme(plot.margin = margin(5, 12, 5, 5))

df_co2 <- df %>% filter(CO2_flux_status == "valid", !is.na(component_display))
co2_n <- df_co2 %>% group_by(component_display) %>%
  summarise(n = n(), med = median(CO2_best.flux), .groups = "drop")

fig1b <- df_co2 %>%
  ggplot(aes(x = CO2_best.flux,
             y = fct_reorder(component_display, CO2_best.flux, .fun = median),
             fill = component_display, color = component_display)) +
  geom_jitter(alpha = 0.4, size = 1, height = 0.25) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA, color = "black", width = 0.5) +
  mean_diamond() +
  geom_text(data = co2_n %>% mutate(y = fct_reorder(component_display, med)),
            aes(x = Inf, y = y, label = paste0("n=", n)),
            inherit.aes = FALSE, hjust = 1.1, size = 3, color = "grey40") +
  scale_x_continuous(trans = "asinh", breaks = asinh_brk, labels = asinh_labels) +
  scale_fill_manual(values = component_colors, guide = "none") +
  scale_color_manual(values = component_colors, guide = "none") +
  labs(x = expression(CO[2]~Flux~(mu*mol~m^{-2}~s^{-1})), y = NULL, tag = "(b)") +
  theme_pub() +
  theme(plot.margin = margin(5, 12, 5, 5))

fig1 <- fig1a + fig1b
save_pub(fig1, "component_overview", width = 240, height = 110)


# =============================================================================
# FIGURE 1b: Component Flux Overview split by Season (core sites only)
# Layout: rows = Dry/Wet, columns = CH4/CO2
# =============================================================================
cat("\n--- Figure 1b: Component Overview by Season ---\n")

df_season_core <- df %>%
  filter(!is.na(component_display), !is.na(season_agg), plot %in% core_sites)

# Compute a single component ordering (by overall CH4 median) for all panels
comp_order_season <- df_season_core %>%
  filter(CH4_flux_status == "valid") %>%
  group_by(component_display) %>%
  summarise(med = median(CH4_best.flux, na.rm = TRUE), .groups = "drop") %>%
  arrange(med) %>%
  pull(component_display)

df_season_core <- df_season_core %>%
  mutate(component_display = factor(component_display, levels = comp_order_season))

# Helper to build one panel (fixed component ordering)
make_season_panel <- function(data, gas, season_val, tag_label) {
  if (gas == "CH4") {
    d <- data %>% filter(CH4_flux_status == "valid", season_agg == season_val)
    flux_var <- "CH4_best.flux"
    brk <- asinh_brk_pos
    x_lab <- expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1}))
  } else {
    d <- data %>% filter(CO2_flux_status == "valid", season_agg == season_val)
    flux_var <- "CO2_best.flux"
    brk <- asinh_brk
    x_lab <- expression(CO[2]~Flux~(mu*mol~m^{-2}~s^{-1}))
  }
  d %>%
    ggplot(aes(x = .data[[flux_var]],
               y = component_display,
               fill = component_display, color = component_display)) +
    geom_jitter(alpha = 0.4, size = 1, height = 0.25) +
    geom_boxplot(alpha = 0.5, outlier.shape = NA, color = "black", width = 0.5) +
    mean_diamond() +
    scale_x_continuous(trans = "asinh", breaks = brk, labels = asinh_labels) +
    scale_fill_manual(values = component_colors, guide = "none", drop = FALSE) +
    scale_color_manual(values = component_colors, guide = "none", drop = FALSE) +
    scale_y_discrete(drop = FALSE) +
    labs(x = x_lab, y = NULL, tag = tag_label,
         title = paste0(season_val, " Season")) +
    theme_pub() +
    theme(plot.title = element_text(size = 11, face = "bold"))
}

fig1b_dry_ch4 <- make_season_panel(df_season_core, "CH4", "Dry", "(a)")
fig1b_dry_co2 <- make_season_panel(df_season_core, "CO2", "Dry", "(b)")
fig1b_wet_ch4 <- make_season_panel(df_season_core, "CH4", "Wet", "(c)")
fig1b_wet_co2 <- make_season_panel(df_season_core, "CO2", "Wet", "(d)")

fig1b_combined <- (fig1b_dry_ch4 | fig1b_dry_co2) / (fig1b_wet_ch4 | fig1b_wet_co2)
save_pub(fig1b_combined, "component_overview_by_season", width = 260, height = 180)


# =============================================================================
# FIGURE 2: Component Jitterbox by Plot x Campaign (classic grid)
# Uses facet_nested to show disturbance grouping on plot rows
# Two versions: all 8 sites, and core 5 sites only
# =============================================================================
cat("\n--- Figure 2: Component Jitterbox by Plot x Campaign ---\n")

# Helper to build campaign grid figure
make_campaign_grid <- function(data, gas = "CH4", tag_label = "(a)") {
  if (gas == "CH4") {
    flux_var <- "CH4_best.flux"
    status_var <- "CH4_flux_status"
    brk <- asinh_brk_pos
    x_lab <- expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1}))
  } else {
    flux_var <- "CO2_best.flux"
    status_var <- "CO2_flux_status"
    brk <- asinh_brk
    x_lab <- expression(CO[2]~Flux~(mu*mol~m^{-2}~s^{-1}))
  }
  data %>%
    filter(.data[[status_var]] == "valid", !is.na(component), !is.na(campaign)) %>%
    ggplot(aes(x = .data[[flux_var]], y = component, fill = component, color = component)) +
    geom_jitter(alpha = 0.5, size = 1, height = 0.3) +
    geom_boxplot(alpha = 0.3, outlier.shape = NA, color = "black", width = 0.5) +
    mean_diamond() +
    facet_nested(disturbance_level + plot ~ campaign,
                 nest_line = element_line(linewidth = 0.5),
                 scales = "free_y",
                 strip = strip_nested(size = "variable")) +
    scale_x_continuous(trans = "asinh", breaks = brk, labels = asinh_labels) +
    scale_fill_manual(values = component_colors_lc) +
    scale_color_manual(values = component_colors_lc) +
    labs(x = x_lab, y = NULL, fill = "Component", color = "Component", tag = tag_label) +
    theme_pub(base_size = 9) +
    theme(legend.position = "bottom",
          axis.text.y = element_text(size = 7),
          strip.text = element_text(size = 7, face = "bold"))
}

# All sites version
fig2_ch4_all <- make_campaign_grid(df, "CH4", "(a)")
fig2_co2_all <- make_campaign_grid(df, "CO2", "(b)")
save_pub(fig2_ch4_all, "component_by_plot_campaign_ch4", width = 260, height = 300)
save_pub(fig2_co2_all, "component_by_plot_campaign_co2", width = 260, height = 300)

# Core sites only version
df_core <- df %>% filter(plot %in% core_sites)
fig2_ch4_core <- make_campaign_grid(df_core, "CH4", "(a)")
fig2_co2_core <- make_campaign_grid(df_core, "CO2", "(b)")
save_pub(fig2_ch4_core, "component_by_plot_campaign_ch4_core", width = 260, height = 220)
save_pub(fig2_co2_core, "component_by_plot_campaign_co2_core", width = 260, height = 220)


# --- Condensed version: proportional row heights, uniform boxplot widths ------
cat("\n--- Figure 2c: Condensed Component by Plot x Campaign (CH4) ---\n")

make_campaign_grid_condensed <- function(data, gas = "CH4", tag_label = "(a)") {
  if (gas == "CH4") {
    flux_var <- "CH4_best.flux"
    status_var <- "CH4_flux_status"
    brk <- asinh_brk_pos
    x_lab <- expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1}))
  } else {
    flux_var <- "CO2_best.flux"
    status_var <- "CO2_flux_status"
    brk <- asinh_brk
    x_lab <- expression(CO[2]~Flux~(mu*mol~m^{-2}~s^{-1}))
  }

  d <- data %>%
    filter(.data[[status_var]] == "valid", !is.na(component), !is.na(campaign))

  d %>%
    ggplot(aes(x = .data[[flux_var]], y = component,
               fill = component, color = component)) +
    geom_jitter(alpha = 0.45, size = 2, height = 0.2, stroke = 0) +
    geom_boxplot(alpha = 0.4, outlier.shape = NA, color = "black",
                 width = 0.6, linewidth = 0.3) +
    stat_summary(fun = mean, geom = "point", shape = 23,
                 size = 1.5, fill = "white", color = "black", stroke = 0.5) +
    facet_nested(disturbance_level + plot ~ campaign,
                 nest_line = element_line(linewidth = 0.4),
                 scales = "free_y",
                 space = "free_y",
                 strip = strip_nested(size = "variable")) +
    scale_x_continuous(trans = "asinh", breaks = brk, labels = asinh_labels) +
    scale_fill_manual(values = component_colors_lc, name = "Component") +
    scale_color_manual(values = component_colors_lc, name = "Component") +
    labs(x = x_lab, y = NULL, tag = tag_label) +
    theme_pub(base_size = 8) +
    theme(
      legend.position    = "top",
      legend.key.size    = unit(3, "mm"),
      legend.text        = element_text(size = 7),
      legend.title       = element_text(size = 7, face = "bold"),
      legend.margin      = margin(0, 0, 2, 0),
      legend.box.margin  = margin(0, 0, 0, 0),
      axis.text.y        = element_text(size = 6.5),
      axis.text.x        = element_text(size = 7),
      axis.title.x       = element_text(size = 8, face = "bold"),
      strip.text         = element_text(size = 6.5, face = "bold",
                                        margin = margin(1.5, 1.5, 1.5, 1.5)),
      strip.background   = element_rect(fill = "grey95", color = "grey70",
                                         linewidth = 0.3),
      panel.spacing.y    = unit(1.5, "mm"),
      panel.spacing.x    = unit(2, "mm"),
      plot.margin        = margin(3, 5, 3, 3)
    )
}

fig2c_ch4 <- make_campaign_grid_condensed(df, "CH4")
save_pub(fig2c_ch4, "component_by_plot_campaign_ch4_condensed", width = 190, height = 200)


# --- Raincloud version: half-boxplot above + dots below ----------------------
cat("\n--- Figure 2d: Raincloud Component by Plot x Campaign (CH4) ---\n")

library(ggdist)

make_campaign_grid_raincloud <- function(data, gas = "CH4", tag_label = "(a)") {
  if (gas == "CH4") {
    flux_var <- "CH4_best.flux"
    status_var <- "CH4_flux_status"
    brk <- asinh_brk_pos
    x_lab <- expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1}))
  } else {
    flux_var <- "CO2_best.flux"
    status_var <- "CO2_flux_status"
    brk <- asinh_brk
    x_lab <- expression(CO[2]~Flux~(mu*mol~m^{-2}~s^{-1}))
  }

  d <- data %>%
    filter(.data[[status_var]] == "valid", !is.na(component), !is.na(campaign))

  d %>%
    ggplot(aes(x = .data[[flux_var]], y = component,
               fill = component, color = component)) +
    # Dots jittered below the center line
    stat_dots(side = "bottom", scale = 0.4, alpha = 0.5,
              dotsize = 0.8, overflow = "compress") +
    # Half-boxplot nudged above the center line
    geom_boxplot(alpha = 0.5, outlier.shape = NA, color = "black",
                 width = 0.3, linewidth = 0.3,
                 position = position_nudge(y = 0.18)) +
    stat_summary(fun = mean, geom = "point", shape = 23,
                 size = 1.5, fill = "white", color = "black", stroke = 0.5,
                 position = position_nudge(y = 0.18)) +
    facet_nested(disturbance_level + plot ~ campaign,
                 nest_line = element_line(linewidth = 0.4),
                 scales = "free_y",
                 space = "free_y",
                 strip = strip_nested(size = "variable")) +
    scale_x_continuous(trans = "asinh", breaks = brk, labels = asinh_labels) +
    scale_fill_manual(values = component_colors_lc, name = "Component") +
    scale_color_manual(values = component_colors_lc, name = "Component") +
    labs(x = x_lab, y = NULL, tag = tag_label) +
    theme_pub(base_size = 8) +
    theme(
      legend.position    = "top",
      legend.key.size    = unit(3, "mm"),
      legend.text        = element_text(size = 7),
      legend.title       = element_text(size = 7, face = "bold"),
      legend.margin      = margin(0, 0, 2, 0),
      legend.box.margin  = margin(0, 0, 0, 0),
      axis.text.y        = element_text(size = 6.5),
      axis.text.x        = element_text(size = 7),
      axis.title.x       = element_text(size = 8, face = "bold"),
      strip.text         = element_text(size = 6.5, face = "bold",
                                        margin = margin(1.5, 1.5, 1.5, 1.5)),
      strip.background   = element_rect(fill = "grey95", color = "grey70",
                                         linewidth = 0.3),
      panel.spacing.y    = unit(1.5, "mm"),
      panel.spacing.x    = unit(2, "mm"),
      plot.margin        = margin(3, 5, 3, 3)
    )
}

fig2d_ch4 <- make_campaign_grid_raincloud(df, "CH4")
save_pub(fig2d_ch4, "component_by_plot_campaign_ch4_raincloud", width = 190, height = 220)


# =============================================================================
# FIGURE 3: Seasonal Variation (Wet vs Dry) — CORE SITES ONLY
# =============================================================================
cat("\n--- Figure 3: Seasonal Variation (core sites) ---\n")

main_components <- c("Stem", "Soil", "Water", "Root", "Cwd")

df_seasonal <- df %>%
  filter(!is.na(season_agg), component_display %in% main_components,
         plot %in% core_sites)

cat("  Core site seasonal data:", nrow(df_seasonal), "rows\n")

fig3a <- df_seasonal %>%
  filter(CH4_flux_status == "valid") %>%
  ggplot(aes(x = CH4_best.flux, y = component_display, fill = season_agg)) +
  geom_point(aes(color = season_agg), alpha = 0.3, size = 1,
             position = position_jitterdodge(jitter.height = 0.15, jitter.width = 0,
                                              dodge.width = 0.75)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, color = "black", width = 0.6,
               position = position_dodge(width = 0.75)) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2.5,
               fill = "white", color = "black", stroke = 0.7,
               position = position_dodge(width = 0.75)) +
  scale_x_continuous(trans = "asinh", breaks = asinh_brk_pos, labels = asinh_labels) +
  scale_fill_manual(values = season_colors, name = "Season") +
  scale_color_manual(values = season_colors, guide = "none") +
  labs(x = expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1})), y = NULL, tag = "(a)",
       subtitle = "Core sites only: BL60, CP40, FLM30, SRS5, SRS6") +
  theme_pub() +
  theme(legend.position = "bottom",
        plot.subtitle = element_text(size = 9, color = "grey50"))

fig3b <- df_seasonal %>%
  filter(CO2_flux_status == "valid") %>%
  ggplot(aes(x = CO2_best.flux, y = component_display, fill = season_agg)) +
  geom_point(aes(color = season_agg), alpha = 0.3, size = 1,
             position = position_jitterdodge(jitter.height = 0.15, jitter.width = 0,
                                              dodge.width = 0.75)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, color = "black", width = 0.6,
               position = position_dodge(width = 0.75)) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2.5,
               fill = "white", color = "black", stroke = 0.7,
               position = position_dodge(width = 0.75)) +
  scale_x_continuous(trans = "asinh", breaks = asinh_brk, labels = asinh_labels) +
  scale_fill_manual(values = season_colors, name = "Season") +
  scale_color_manual(values = season_colors, guide = "none") +
  labs(x = expression(CO[2]~Flux~(mu*mol~m^{-2}~s^{-1})), y = NULL, tag = "(b)") +
  theme_pub() +
  theme(legend.position = "bottom")

fig3 <- fig3a + fig3b + plot_layout(guides = "collect")
save_pub(fig3, "seasonal_variation", width = 240, height = 130)


# =============================================================================
# FIGURE 4: Stem Height Profile (Ridges + Box/Jitter)
# =============================================================================
cat("\n--- Figure 4: Stem Height Profile ---\n")

stem_height <- df %>%
  filter(component == "stem", CH4_flux_status == "valid",
         !is.na(height_category),
         plot %in% c("SRS5", "SRS6", "BL60", "FLM30", "CP40")) %>%
  mutate(disturbance_level = droplevels(disturbance_level))

x_range <- range(stem_height$CH4_best.flux, na.rm = TRUE)

fig4a <- stem_height %>%
  ggplot(aes(x = CH4_best.flux, y = height_category, fill = disturbance_level)) +
  geom_density_ridges(alpha = 0.6, scale = 0.9, bandwidth = 0.5) +
  scale_x_continuous(trans = "asinh", limits = x_range,
                     breaks = asinh_brk_pos, labels = asinh_labels) +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.3))) +
  scale_fill_manual(values = disturbance_colors, name = "Disturbance\nLevel") +
  labs(y = "Height Category") +
  theme_pub() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), legend.position = "right",
        plot.margin = margin(10, 5, 0, 5))

fig4b <- stem_height %>%
  mutate(disturbance_level = factor(disturbance_level,
                                     levels = rev(c("healthy", "regenerating", "ghost")))) %>%
  ggplot(aes(x = CH4_best.flux,
             y = interaction(disturbance_level, height_category, sep = " - "),
             fill = disturbance_level)) +
  geom_jitter(height = 0.2, width = 0, alpha = 0.4, size = 1) +
  geom_boxplot(width = 0.4, alpha = 0.7, outlier.shape = NA) +
  mean_diamond() +
  scale_x_continuous(trans = "asinh", limits = x_range,
                     breaks = asinh_brk_pos, labels = asinh_labels) +
  scale_fill_manual(values = disturbance_colors, guide = "none") +
  scale_y_discrete(labels = function(x) sapply(strsplit(x, " - "), `[`, 1)) +
  labs(x = expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1})), y = "Disturbance Level") +
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

save_pub(fig4, "height_profile", width = 200, height = 200)


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
save_pub(fig5, "disturbance_gradient", width = 280, height = 200)


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
save_pub(fig6, "environmental_drivers", width = 240, height = 200)


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

save_pub(fig7, "ch4_co2_covariation", width = 210, height = 180)


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
save_pub(fig8, "heatmap_summary", width = 270, height = 220)


# =============================================================================
# DONE
# =============================================================================
cat("\n===== All publication figures saved to output/figures/ =====\n")
cat("Diamond markers (shape 23) indicate mean; boxplot line indicates median.\n")
