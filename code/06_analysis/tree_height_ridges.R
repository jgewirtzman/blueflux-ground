# Load required libraries
library(dplyr)
library(ggplot2)
library(ggridges)
library(cowplot)
library(scales)

# Load data if not already in memory (normally created by tree_height_plots.R)
if (!exists("stem_data_final") || !"height_category" %in% names(stem_data_final)) {
  df <- read.csv("output/combined_gas_flux_dataset.csv")
  stem_data_final <- df %>%
    filter(component == "stem",
           !is.na(CH4_best.flux),
           !is.na(CO2_best.flux),
           !is.na(height_corrected)) %>%
    mutate(height_corrected = as.numeric(height_corrected)) %>%
    filter(!is.na(height_corrected), height_corrected >= 0) %>%
    mutate(
      height_category = case_when(
        height_corrected >= 0 & height_corrected < 50 ~ "0-50cm",
        height_corrected >= 50 & height_corrected < 100 ~ "50-100cm",
        height_corrected >= 100 & height_corrected < 150 ~ "100-150cm",
        height_corrected >= 150 ~ ">150cm",
        TRUE ~ NA_character_
      ),
      height_category = factor(height_category,
                               levels = c("0-50cm", "50-100cm", "100-150cm", ">150cm"))
    ) %>%
    filter(!is.na(height_category))
}

# Create site bins and filter data
stem_data_ridge <- stem_data_final %>%
  mutate(
    site_bin = case_when(
      plot %in% c("SRS5", "SRS6") ~ "healthy",
      plot %in% c("BL60") ~ "regenerating", 
      plot %in% c("FLM30", "CP40") ~ "ghost",
      TRUE ~ NA_character_
    )
  ) %>%
  # Filter to only include the specified sites
  filter(!is.na(site_bin)) %>%
  # Convert site_bin to factor with desired order
  mutate(site_bin = factor(site_bin, levels = c("healthy", "regenerating", "ghost")))

# Define colors
site_colors <- c("healthy" = "#228B22", "regenerating" = "#808080", "ghost" = "#8B4513")

# Define shared x-axis limits and breaks
x_limits <- range(stem_data_ridge$CH4_best.flux, na.rm = TRUE)
x_breaks <- c(0, 0.1, 1, 10, 100, 1000)

# Create overlapping ridges plot (top portion)
ridges_plot <- stem_data_ridge %>%
  ggplot(aes(x = CH4_best.flux, y = height_category, fill = site_bin)) +
  geom_density_ridges(alpha = 0.7, scale = 0.9, bandwidth = 0.5) +
  scale_x_continuous(trans = "asinh", limits = x_limits, breaks = x_breaks) +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.33))) +  # Add more space at top
  scale_fill_manual(values = site_colors, name = "Site\nCondition") +
  labs(
    title = expression(Stem~CH[4]~Flux~Distribution~by~Height~Category~and~Site~Condition),
    y = "Height Category"
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    legend.position = "right",
    plot.margin = margin(t = 10, r = 5, b = 0, l = 5)
  )

# Create box and jitter plots underneath each height category
box_plots <- stem_data_ridge %>%
  # Reorder site_bin to control vertical order within each height group
  mutate(site_bin = factor(site_bin, levels = c("ghost", "regenerating", "healthy"))) %>%
  ggplot(aes(x = CH4_best.flux, y = interaction(site_bin, height_category, sep = " - "), fill = site_bin)) +
  geom_jitter(height = 0.2, width = 0, alpha = 0.4, size = 1) +
  geom_boxplot(width = 0.4, alpha = 0.7) +
  scale_x_continuous(trans = "asinh", limits = x_limits, breaks = x_breaks) +
  scale_fill_manual(values = site_colors) +
  scale_y_discrete(labels = function(x) {
    # Extract just the site_bin part for labeling
    sapply(strsplit(x, " - "), function(parts) parts[1])
  }) +
  labs(
    x = expression(CH[4]~Flux~Rate~(nmol~m^-2~s^-1)~-~Asinh~Scale),
    y = "Site Condition"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 11, face = "bold"),
    legend.position = "none",
    plot.margin = margin(t = 0, r = 5, b = 10, l = 5),
    # Add spacing between height category groups
    panel.grid.major.y = element_line(color = "grey90", size = 0.5),
    strip.text = element_text(size = 9)
  ) +
  # Add faceting to separate height categories
  facet_grid(
    rows = vars(factor(height_category, levels = rev(c("0-50cm", "50-100cm", "100-150cm", ">150cm")))), 
    scales = "free_y", 
    space = "free_y",
    switch = "y"
  ) +
  theme(
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0)
  )

# Align and combine the plots
aligned <- align_plots(ridges_plot, box_plots, align = "v", axis = "lr")
p1 <- ggdraw(aligned[[1]])
p2 <- ggdraw(aligned[[2]])

# Create final rainfall plot
rainfall_plot <- plot_grid(p1, p2, ncol = 1, align = "v", rel_heights = c(1, 1.5))

print(rainfall_plot)
ggsave("output/figures/tree_height_ridges_rainfall.png", rainfall_plot, width = 12, height = 10, dpi = 300)