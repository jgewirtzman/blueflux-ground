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
asinh_brk     <- c(-100, -10, -1, 0, 1, 10, 100)  # CO2 full range
asinh_brk_pos <- c(0, 1, 10, 100, 1000)              # CH4 positive breaks

# Clean numeric labels for asinh axes
asinh_labels <- function(x) {
  ifelse(x == 0, "0", format(x, scientific = FALSE, big.mark = "", drop0trailing = TRUE))
}

# Mean diamond geom (reusable)
mean_diamond <- function(...) {
  stat_summary(fun = mean, geom = "point", shape = 23,
               size = 2.5, fill = alpha("white", 0.6), color = alpha("black", 0.6),
               stroke = 0.7, ...)
}

# Save helper (PDF + PNG)
save_pub <- function(plot, name, width, height, units = "mm") {
  ggsave(paste0("output/figures/pub_", name, ".pdf"), plot,
         width = width, height = height, units = units)
  ggsave(paste0("output/figures/pub_", name, ".png"), plot,
         width = width, height = height, units = units, dpi = 300)
  cat("Saved: pub_", name, ".pdf/.png\n", sep = "")
}
