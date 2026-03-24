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
  # Exclude observations with CO2 flux < -10 (suspect values)
  filter(is.na(CO2_best.flux) | CO2_best.flux >= -10) %>%
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
    geom_jitter(alpha = 0.45, size = 2, height = 0.12, stroke = 0) +
    geom_boxplot(alpha = 0.4, outlier.shape = NA, color = "black",
                 width = 0.5, linewidth = 0.3) +
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
      legend.position    = "bottom",
      legend.key.size    = unit(3, "mm"),
      legend.text        = element_text(size = 7),
      legend.title       = element_text(size = 7, face = "bold"),
      legend.margin      = margin(0, 0, 0, 0),
      legend.box.margin  = margin(0, 0, 0, 0),
      axis.text.y        = element_text(size = 6, margin = margin(0, 1, 0, 0)),
      axis.text.x        = element_text(size = 7),
      axis.title.x       = element_text(size = 8, face = "bold"),
      strip.text         = element_text(size = 8, face = "bold",
                                        margin = margin(0.3, 0.5, 0.3, 0.5)),
      strip.text.y       = element_text(size = 8, face = "bold", angle = 0,
                                        margin = margin(0.3, 0.3, 0.3, 0.3)),
      strip.background   = element_blank(),
      panel.spacing.y    = unit(0.3, "mm"),
      panel.spacing.x    = unit(1.5, "mm"),
      plot.margin        = margin(2, 14, 2, 3)
    )
}

fig2c_ch4 <- make_campaign_grid_condensed(df, "CH4")
save_pub(fig2c_ch4, "component_by_plot_campaign_ch4_condensed", width = 150, height = 120)

fig2c_co2 <- make_campaign_grid_condensed(df, "CO2")
save_pub(fig2c_co2, "component_by_plot_campaign_co2_condensed", width = 150, height = 120)


# --- Fig 2c-boot: Condensed with bootstrapped mean + 95% CI -----------------
cat("\n--- Figure 2c-boot: Condensed with Bootstrapped Mean + CI ---\n")

boot_mean_ci <- function(x, R = 5000, conf = 0.95) {
  x <- x[!is.na(x) & is.finite(x)]
  n <- length(x)
  if (n < 3) return(data.frame(y = mean(x), ymin = NA_real_, ymax = NA_real_))
  set.seed(42)
  boot_means <- replicate(R, mean(sample(x, n, replace = TRUE)))
  alpha <- (1 - conf) / 2
  data.frame(
    y = mean(boot_means),
    ymin = unname(quantile(boot_means, alpha)),
    ymax = unname(quantile(boot_means, 1 - alpha))
  )
}

make_campaign_grid_condensed_boot <- function(data, gas = "CH4", tag_label = "(a)") {
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
    geom_jitter(alpha = 0.45, size = 2, height = 0.12, stroke = 0) +
    geom_boxplot(alpha = 0.4, outlier.shape = NA, color = "black",
                 width = 0.5, linewidth = 0.3) +
    # Bootstrapped mean + 95% CI (horizontal error bar + diamond)
    stat_summary(
      fun.data = function(x) boot_mean_ci(x),
      geom = "pointrange", shape = 23,
      size = 0.4, linewidth = 0.5,
      fill = "white", color = "black", stroke = 0.5,
      fatten = 4
    ) +
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
      legend.position    = "bottom",
      legend.key.size    = unit(3, "mm"),
      legend.text        = element_text(size = 7),
      legend.title       = element_text(size = 7, face = "bold"),
      legend.margin      = margin(0, 0, 0, 0),
      legend.box.margin  = margin(0, 0, 0, 0),
      axis.text.y        = element_text(size = 6, margin = margin(0, 1, 0, 0)),
      axis.text.x        = element_text(size = 7),
      axis.title.x       = element_text(size = 8, face = "bold"),
      strip.text         = element_text(size = 8, face = "bold",
                                        margin = margin(0.3, 0.5, 0.3, 0.5)),
      strip.text.y       = element_text(size = 8, face = "bold", angle = 0,
                                        margin = margin(0.3, 0.3, 0.3, 0.3)),
      strip.background   = element_blank(),
      panel.spacing.y    = unit(0.3, "mm"),
      panel.spacing.x    = unit(1.5, "mm"),
      plot.margin        = margin(2, 14, 2, 3)
    )
}

fig2c_ch4_boot <- make_campaign_grid_condensed_boot(df, "CH4")
save_pub(fig2c_ch4_boot, "component_by_plot_campaign_ch4_condensed_boot", width = 150, height = 120)

fig2c_co2_boot <- make_campaign_grid_condensed_boot(df, "CO2")
save_pub(fig2c_co2_boot, "component_by_plot_campaign_co2_condensed_boot", width = 150, height = 120)


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
               size = 2.5, fill = "white", color = "black", stroke = 0.7, alpha = 0.7) +
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
save_pub(fig9, "species_emmeans", width = 200, height = 200)

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
               size = 2.5, fill = "white", color = "black", stroke = 0.7, alpha = 0.7) +
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

save_pub(fig10, "stem_height_composite", width = 260, height = 200)

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

x_range_co2 <- range(stem_height_co2$CO2_best.flux, na.rm = TRUE)
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
               size = 2.5, fill = "white", color = "black", stroke = 0.7, alpha = 0.7) +
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

save_pub(fig10_co2, "stem_height_composite_co2", width = 260, height = 200)

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
               size = 2.5, fill = "white", color = "black", stroke = 0.7, alpha = 0.7) +
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
               size = 2.5, fill = "white", color = "black", stroke = 0.7, alpha = 0.7) +
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

# --- Column titles ---
title_ch4 <- ggdraw() + draw_label(expression(CH[4]~Flux), fontface = "bold", size = 11)
title_co2 <- ggdraw() + draw_label(expression(CO[2]~Flux), fontface = "bold", size = 11)

# --- Align ridges and box within each column ---
ch4_aligned <- align_plots(ch4_ridges, ch4_box, align = "v", axis = "lr")
co2_aligned <- align_plots(co2_ridges, co2_box, align = "v", axis = "lr")

# --- Build columns (no legends — all stripped) ---
ch4_col <- plot_grid(ch4_aligned[[1]], ch4_aligned[[2]], ch4_emm,
                     ncol = 1, rel_heights = c(0.8, 1.2, 0.8),
                     labels = c("(a)", "(c)", "(e)"), label_size = 10, label_fontface = "bold")

co2_col <- plot_grid(co2_aligned[[1]], co2_aligned[[2]], co2_emm,
                     ncol = 1, rel_heights = c(0.8, 1.2, 0.8),
                     labels = c("(b)", "(d)", "(f)"), label_size = 10, label_fontface = "bold")

# --- Centered shared legends (extract via ggplotGrob) ---
# Helper: extract legend grob from a ggplot
extract_legend <- function(p) {
  g <- ggplotGrob(p + theme(legend.position = "bottom"))
  leg <- g$grobs[which(sapply(g$grobs, function(x) x$name) == "guide-box")]
  if (length(leg) > 0) leg[[1]] else grid::nullGrob()
}

# Disturbance legend
leg_dist_grob <- extract_legend(
  ggplot(stem_height, aes(x = CH4_best.flux, fill = disturbance_level)) +
    geom_density() +
    scale_fill_manual(values = disturbance_colors, name = "Disturbance Level") +
    guides(fill = guide_legend(nrow = 1)) +
    theme_pub(base_size = 9) +
    theme(legend.title = element_text(size = 9, face = "bold"),
          legend.text = element_text(size = 8))
)

# Species/status legend
leg_spp_grob <- extract_legend(
  ggplot(emm_d_df, aes(x = emmean, y = height_cat,
                        color = label, shape = label)) +
    geom_point(size = 2.5, stroke = 0.8) +
    scale_color_manual(values = spst_colors, name = NULL) +
    scale_shape_manual(values = spst_shapes, name = NULL) +
    guides(color = guide_legend(ncol = 3, byrow = TRUE,
                                override.aes = list(size = 3)),
           shape = guide_legend(ncol = 3, byrow = TRUE,
                                override.aes = list(size = 3))) +
    theme_pub(base_size = 9) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 8, face = "italic"))
)

# --- Title row ---
title_row <- plot_grid(title_ch4, title_co2, nrow = 1)

# --- Main body (equal-height columns) ---
body <- plot_grid(ch4_col, co2_col, nrow = 1, rel_widths = c(1, 0.85))

# --- Full figure: title, disturbance legend, body, species legend ---
fig10_combined <- plot_grid(
  title_row,
  leg_dist_grob,
  body,
  leg_spp_grob,
  ncol = 1, rel_heights = c(0.04, 0.04, 1, 0.06)
)

save_pub(fig10_combined, "stem_height_composite_combined", width = 260, height = 280)

cat("\n--- Combined CH4+CO2 height composite figure saved ---\n")


# =============================================================================
# Figure 11: Living vs Dead Tree CH4 Flux (emmeans)
# =============================================================================
# Compares alive vs dead stems, controlling for species, height, season.
# Restricted to sites where both alive and dead trees have n >= 5.
# Sites included: BL60 (regen), CP40 (ghost), FLM30 (ghost), SRS5 (healthy),
#   SRS6 (healthy).
# Model with alive × species interaction reveals species-specific patterns.
# =============================================================================
cat("\n--- Figure 11: Living vs Dead Tree Analysis ---\n")

# --- Prepare data ---
stem_ad <- df %>%
  filter(component == "stem",
         !is.na(status), status != "CWD",
         !is.na(CH4_best.flux),
         !is.na(height_corrected), height_corrected >= 0) %>%
  mutate(
    alive = factor(ifelse(status %in% c("alive", "Alive"), "Alive", "Dead")),
    species = ifelse(species == "COPE", "COER", species),
    species = factor(species),
    y_ch4 = asinh(CH4_best.flux)
  )

# Restrict to sites where both alive AND dead exist with n >= 5
ad_by_site <- stem_ad %>%
  group_by(plot, alive) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = alive, values_from = n, values_fill = 0) %>%
  filter(Alive >= 5, Dead >= 5)

stem_ad <- stem_ad %>%
  filter(plot %in% ad_by_site$plot) %>%
  mutate(plot = droplevels(plot))

# Keep species with >= 5 obs
sp_counts_ad <- table(stem_ad$species)
keep_spp_ad <- names(sp_counts_ad[sp_counts_ad >= 5])
stem_ad <- stem_ad %>%
  filter(species %in% keep_spp_ad) %>%
  mutate(species = droplevels(species))

cat("Living vs dead analysis — sites:", paste(levels(stem_ad$plot), collapse = ", "), "\n")
cat("n per status:\n")
print(table(stem_ad$alive))
cat("\nAlive/Dead × species:\n")
print(table(stem_ad$alive, stem_ad$species))

# --- Model: alive × species interaction ---
m_ad <- lmer(y_ch4 ~ alive * species + height_corrected + season_agg + (1 | plot),
             data = stem_ad)
cat("\n--- Alive × Species model ---\n")
print(summary(m_ad))

# Emmeans: overall alive vs dead
emm_ad_overall <- emmeans(m_ad, "alive")
cat("\n--- Overall Alive vs Dead ---\n")
print(summary(emm_ad_overall))
print(pairs(emm_ad_overall))

# Emmeans: alive vs dead by species
emm_ad_sp <- emmeans(m_ad, ~ alive | species)
emm_ad_sp_df <- as.data.frame(summary(emm_ad_sp))
emm_ad_sp_df <- emm_ad_sp_df %>% filter(!is.na(emmean))

# Add species display names
emm_ad_sp_df$species_label <- species_labels[as.character(emm_ad_sp_df$species)]

# Pairwise contrasts
pairs_ad_sp <- as.data.frame(pairs(emm_ad_sp))
cat("\n--- Alive vs Dead contrasts by species ---\n")
print(pairs_ad_sp)

# Sample sizes
n_ad_sp <- stem_ad %>%
  group_by(species, alive) %>%
  summarise(n = n(), .groups = "drop")
emm_ad_sp_df <- left_join(emm_ad_sp_df, n_ad_sp, by = c("species", "alive"))

# Status colors
status_colors <- c("Alive" = "#228B22", "Dead" = "#8B4513")

# --- Figure: Alive vs Dead emmeans by species ---
pd_ad <- position_dodge(width = 0.5)

fig11 <- emm_ad_sp_df %>%
  mutate(species_label = factor(species_label, levels = rev(names(species_colors)))) %>%
  ggplot(aes(x = emmean, y = species_label, color = alive, shape = alive)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_errorbar(aes(xmin = lower.CL, xmax = upper.CL),
                width = 0.3, linewidth = 0.7, orientation = "y",
                position = pd_ad) +
  geom_point(size = 3.5, position = pd_ad) +
  geom_text(aes(label = paste0("n=", n)),
            hjust = -0.3, vjust = -0.6, size = 2.8, color = "grey40",
            position = pd_ad, show.legend = FALSE) +
  scale_color_manual(values = status_colors, name = "Status") +
  scale_shape_manual(values = c("Alive" = 16, "Dead" = 17), name = "Status") +
  scale_x_continuous(
    name = expression(Estimated~Marginal~Mean~CH[4]~Flux~(asinh~scale)),
    sec.axis = sec_axis(~ sinh(.),
                        name = expression(CH[4]~Flux~(nmol~m^{-2}~s^{-1})),
                        breaks = c(-0.5, 0, 0.5, 1, 2, 5))
  ) +
  labs(y = NULL,
       subtitle = "Controlled for height & season; sites with both alive & dead stems") +
  theme_pub(base_size = 10) +
  theme(
    axis.text.y = element_text(face = "italic", size = 10),
    legend.position = "bottom",
    plot.margin = margin(5, 10, 5, 5)
  )

save_pub(fig11, "alive_vs_dead_emmeans", width = 180, height = 120)

cat("\n--- Living vs dead figure saved ---\n")


# =============================================================================
# DONE
# =============================================================================
cat("\n===== All publication figures saved to output/figures/ =====\n")
cat("Diamond markers (shape 23) indicate mean; boxplot line indicates median.\n")
