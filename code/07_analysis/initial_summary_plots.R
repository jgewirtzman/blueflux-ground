# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyverse)

# Read the data (if not already loaded)
 df <- read.csv("output/combined_gas_flux_dataset.csv")

# Define the plots to include
selected_plots <- c("BL60", "CP40", "FLM30", "MI", "SE1", "SRS5", "SRS6", "RB10")

# Filter data for selected plots only
# (component is already standardized lowercase by assemble_clean_dataset.R)
filtered_data <- df %>%
  filter(plot %in% selected_plots,
         !is.na(CH4_best.flux), 
         !is.na(component), 
         !is.na(month_year),
         CH4_best.flux >= 0)  # Keep zero and positive values

# Check what components we have
print("Components in filtered data:")
print(table(filtered_data$component))

# Define meaningful colors for each component
component_colors <- c(
  "soil" = "#8B4513",          # Brown - earth/soil
  "water" = "#4682B4",         # Steel blue - water
  "stem" = "#228B22",          # Forest green - woody plant parts
  "root" = "#D2691E",          # Chocolate - underground parts
  "CWD" = "#654321",           # Dark brown - coarse woody debris
  "pneumatophore" = "#32CD32", # Lime green - specialized root structures
  "leaves" = "#90EE90"         # Light green - foliage
)

# Create the plot colored by component
p1 <- filtered_data %>%
  ggplot(aes(x = component, y = CH4_best.flux, color = component)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_boxplot(alpha = 0.3, outlier.shape = NA) +
  facet_grid(plot ~ month_year, scales = "free_x") +
  scale_y_continuous(trans = "asinh") +
  scale_color_manual(values = component_colors) +
  labs(
    title = "CH4 Flux by Component across Selected Plots",
    subtitle = "Rows = Plot, Columns = Month-Year (Season), Colored by Component",
    x = "Component",
    y = "CH4 Flux (CH4_best.flux) - Asinh Scale",
    color = "Component"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom"
  )

print(p1)
ggsave("output/figures/ch4_component_boxplot_grid.png", p1, width = 14, height = 12, dpi = 300)

# Alternative with jittered points
p2 <- filtered_data %>%
  ggplot(aes(x = component, y = CH4_best.flux, color = component)) +
  geom_jitter(alpha = 0.7, size = 2, width = 0.3) +
  stat_summary(fun = median, geom = "crossbar", width = 0.5, alpha = 0.8, color = "black") +
  facet_grid(plot ~ month_year, scales = "free_x") +
  scale_y_continuous(trans = "asinh") +
  scale_color_manual(values = component_colors) +
  labs(
    title = "CH4 Flux by Component across Selected Plots",
    subtitle = "Rows = Plot, Columns = Month-Year, Colored by Component, with Medians",
    x = "Component",
    y = "CH4 Flux (CH4_best.flux) - Asinh Scale",
    color = "Component"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom"
  )

print(p2)

# Version without redundant x-axis labels (since color already shows component)
p3 <- filtered_data %>%
  ggplot(aes(x = component, y = CH4_best.flux, color = component)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_boxplot(alpha = 0.3, outlier.shape = NA) +
  facet_grid(plot ~ month_year, scales = "free_x") +
  scale_y_continuous(trans = "asinh") +
  scale_color_manual(values = component_colors) +
  labs(
    title = "CH4 Flux by Component across Selected Plots",
    subtitle = "Rows = Plot, Columns = Month-Year, Colored by Component",
    x = "Component",
    y = "CH4 Flux (CH4_best.flux) - Asinh Scale",
    color = "Component"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis text since color legend shows components
    axis.ticks.x = element_blank(), # Remove x-axis ticks
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom"
  )

print(p3)

# Create a violin plot version for better distribution visualization
p4 <- filtered_data %>%
  ggplot(aes(x = component, y = CH4_best.flux, fill = component)) +
  geom_violin(alpha = 0.7) +
  geom_point(alpha = 0.5, size = 1, position = position_jitter(width = 0.2)) +
  facet_grid(plot ~ month_year, scales = "free_x") +
  scale_y_continuous(trans = "asinh") +
  scale_fill_manual(values = component_colors) +
  labs(
    title = "CH4 Flux Distribution by Component across Selected Plots",
    subtitle = "Violin plots showing distribution shape, colored by component",
    x = "Component",
    y = "CH4 Flux (CH4_best.flux) - Asinh Scale",
    fill = "Component"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom"
  )

print(p4)


# Print the color scheme for reference
print("Component color scheme:")
for(comp in names(component_colors)) {
  print(paste(comp, ":", component_colors[comp]))
}

# Summary statistics by component
component_summary <- filtered_data %>%
  group_by(component) %>%
  summarise(
    n = n(),
    mean_flux = mean(CH4_best.flux, na.rm = TRUE),
    median_flux = median(CH4_best.flux, na.rm = TRUE),
    sd_flux = sd(CH4_best.flux, na.rm = TRUE),
    min_flux = min(CH4_best.flux, na.rm = TRUE),
    max_flux = max(CH4_best.flux, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(median_flux))

print("Component summary (ordered by median flux):")
print(component_summary)







# Create p2 with switched axes, log scale, and proper superscripts/subscripts
p2_switched <- filtered_data %>%
  ggplot(aes(x = CH4_best.flux, y = component, color = component)) +
  geom_jitter(alpha = 0.7, size = 2, height = 0.3) +
  stat_summary(fun = median, geom = "crossbar", height = 0.5, alpha = 0.8, color = "black") +
  facet_grid(plot ~ month_year, scales = "free_y") +
  scale_x_continuous(
    trans = "asinh",
    name = expression(CH[4]~Flux~Rate~(nmol~m^-2~s^-1)),
    breaks = c(0, 0.1, 1, 10, 100, 1000)
  ) +
  scale_color_manual(values = component_colors) +
  labs(
    title = expression(Methane~(CH[4])~Flux~Rates~by~Ecosystem~Component),
    subtitle = "Study plots and sampling periods, showing median values with black bars",
    y = "Ecosystem Component",
    color = "Component"
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom"
  )

print(p2_switched)

# Alternative version with simple labels and units
p2_switched_simple <- filtered_data %>%
  ggplot(aes(x = CH4_best.flux, y = component, color = component)) +
  geom_jitter(alpha = 0.7, size = 2, height = 0.3) +
  stat_summary(fun = median, geom = "crossbar", height = 0.5, alpha = 0.8, color = "black") +
  facet_grid(plot ~ month_year, scales = "free_y") +
  scale_x_continuous(
    trans = "asinh",
    name = expression(CH[4]~Flux~Rate~(nmol~m^-2~s^-1)),
    breaks = c(0, 0.1, 1, 10, 100, 1000)
  ) +
  scale_color_manual(values = component_colors) +
  labs(
    #title = expression(Methane~(CH[4])~Flux~Rates~by~Ecosystem~Component),
    #subtitle = "Study plots and sampling periods, showing median values with black bars",
    y = "Ecosystem Component",
    color = "Component"
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom"
  )

print(p2_switched_simple)
ggsave("output/figures/ch4_component_by_plot_month.png", p2_switched_simple, width = 14, height = 12, dpi = 300)







# Alternative Ways to Visualize Methane Flux Data

## 1. **Ridge Plot / Density Plot**

library(ggridges)

filtered_data %>%
  ggplot(aes(x = CH4_best.flux, y = component, fill = component)) +
  geom_density_ridges(alpha = 0.7, scale = 0.9) +
  scale_x_continuous(trans = "asinh") +
  facet_grid(plot ~ month_year) +
  theme_ridges()


## 2. **Box Plot with Better Statistical Summary**

filtered_data %>%
  ggplot(aes(x = component, y = CH4_best.flux, fill = component)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
  scale_y_continuous(trans = "asinh") +
  facet_grid(plot ~ month_year) +
  coord_flip() +
  theme_bw()


## 3. **Violin Plot for Distribution Shape**

filtered_data %>%
  ggplot(aes(x = component, y = CH4_best.flux, fill = component)) +
  geom_violin(alpha = 0.7, trim = FALSE) +
  geom_boxplot(width = 0.1, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.05, alpha = 0.3, size = 0.8) +
  scale_y_continuous(trans = "asinh") +
  facet_grid(plot ~ month_year) +
  coord_flip()


## 4. **Heatmap/Tile Plot**

# Summarize data first
summary_data <- filtered_data %>%
  group_by(plot, month_year, component) %>%
  summarise(median_flux = median(CH4_best.flux, na.rm = TRUE),
            .groups = 'drop')

summary_data %>%
  ggplot(aes(x = month_year, y = component, fill = median_flux)) +
  geom_tile(color = "white", size = 0.5) +
  facet_wrap(~plot, ncol = 2) +
  scale_fill_viridis_c(name = "CH4 Flux", trans = "asinh") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## 5. **Small Multiples by Component**
filtered_data %>%
  ggplot(aes(x = month_year, y = CH4_best.flux, color = plot)) +
  geom_jitter(alpha = 0.6, width = 0.2) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.3) +
  scale_y_continuous(trans = "asinh") +
  facet_wrap(~component, scales = "free_y", ncol = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## 6. **Interactive Plot with Plotly**
library(plotly)

p <- filtered_data %>%
  ggplot(aes(x = CH4_best.flux, y = component, 
             color = component, text = paste("Plot:", plot, 
                                             "<br>Date:", month_year))) +
  geom_jitter(alpha = 0.7, size = 2) +
  scale_x_continuous(trans = "asinh") +
  facet_grid(plot ~ month_year) +
  theme_bw()

tryCatch(ggplotly(p, tooltip = "text"), error = function(e) message("ggplotly skipped (non-interactive): ", e$message))


## 7. **Slope Graph for Temporal Changes**
# Calculate medians by time period
slope_data <- filtered_data %>%
  group_by(plot, component, month_year) %>%
  summarise(median_flux = median(CH4_best.flux, na.rm = TRUE),
            .groups = 'drop')

slope_data %>%
  ggplot(aes(x = month_year, y = median_flux,
             color = component, group = component)) +
  geom_point(size = 3) +
  geom_line(size = 1.2, alpha = 0.8) +
  facet_wrap(~plot) +
  theme_minimal()


## 8. **Raincloud Plot**
# You'll need the ggdist package
library(ggdist)

filtered_data %>%
  ggplot(aes(x = component, y = CH4_best.flux, fill = component)) +
  stat_halfeye(alpha = 0.7, width = 0.6) +
  stat_dots(side = "left", alpha = 0.4) +
  scale_y_continuous(trans = "asinh") +
  facet_grid(plot ~ month_year) +
  coord_flip() +
  theme_bw()









# Ridge plot with modified colors for better differentiation
library(ggridges)

# Modified component colors for better differentiation
component_colors_ridges <- component_colors
component_colors_ridges["CWD"] <- "#D2B48C"  # Light tan
component_colors_ridges["Soil"] <- "#8B4513"  # Dark brown

p_ridges <- filtered_data %>%
  ggplot(aes(x = CH4_best.flux, y = component, fill = component)) +
  geom_density_ridges(alpha = 0.7, scale = 0.9) +
  facet_grid(plot ~ month_year, scales = "free_y") +
  scale_x_continuous(
    trans = "asinh",
    name = expression(CH[4]~Flux~Rate~(nmol~m^-2~s^-1)),
    breaks = c(0, 0.1, 1, 10, 100, 1000)
  ) +
  scale_fill_manual(values = component_colors_ridges) +
  labs(
    y = "Ecosystem Component",
    fill = "Component"
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom"
  )

print(p_ridges)
ggsave("output/figures/ch4_component_ridges.png", p_ridges, width = 14, height = 12, dpi = 300)

# Heatmap with viridis scale in the same grid format
# First summarize the data
summary_data <- filtered_data %>%
  group_by(plot, month_year, component) %>%
  summarise(median_flux = median(CH4_best.flux, na.rm = TRUE),
            .groups = 'drop')

p_heatmap <- summary_data %>%
  ggplot(aes(x = month_year, y = component, fill = median_flux)) +
  geom_tile(color = "white", size = 0.5) +
  facet_wrap(~plot, scales = "free_y") +
  scale_fill_viridis_c(
    name = expression(CH[4]~Flux~Rate~(nmol~m^-2~s^-1)),
    trans = "asinh",
    breaks = c(0, 0.1, 1, 10, 100, 1000)
  ) +
  labs(
    x = "Time Period",
    y = "Ecosystem Component"
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom"
  )

print(p_heatmap)
ggsave("output/figures/ch4_component_heatmap.png", p_heatmap, width = 12, height = 10, dpi = 300)


p_jitter_box_ch4 <- filtered_data %>%
  ggplot(aes(x = CH4_best.flux, y = component, fill = component, color = component)) +
  geom_jitter(alpha = 0.6, size = 2, height = 0.3) +
  geom_boxplot(alpha = 0.3, outlier.shape = NA, color = "black", width = 0.5) +
  facet_grid(plot ~ month_year, scales = "free_y") +
  scale_x_continuous(
    trans = "asinh",
    name = expression(CH[4]~Flux~Rate~(nmol~m^-2~s^-1)),
    breaks = c(0, 0.1, 1, 10, 100, 1000)
  ) +
  scale_fill_manual(values = component_colors_ridges) +
  scale_color_manual(values = component_colors_ridges) +
  labs(
    y = "Ecosystem Component",
    fill = "Component",
    color = "Component"
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom"
  )
print(p_jitter_box_ch4)
ggsave("output/figures/ch4_component_jitterbox.png", p_jitter_box_ch4, width = 14, height = 12, dpi = 300)




p_jitter_box_co2 <- filtered_data %>%
  ggplot(aes(x = CO2_best.flux, y = component, fill = component, color = component)) +
  geom_jitter(alpha = 0.6, size = 2, height = 0.3) +
  geom_boxplot(alpha = 0.3, outlier.shape = NA, color = "black", width = 0.5) +
  facet_grid(plot ~ month_year, scales = "free_y") +
  scale_x_continuous(
    trans = "asinh",
    name = expression(CO[2]~Flux~Rate~(mu*mol~m^-2~s^-1)),
    breaks = c(0, 0.1, 1, 10, 100, 1000)
  ) +
  scale_fill_manual(values = component_colors_ridges) +
  scale_color_manual(values = component_colors_ridges) +
  labs(
    y = "Ecosystem Component",
    fill = "Component",
    color = "Component"
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom"
  )
print(p_jitter_box_co2)
ggsave("output/figures/co2_component_jitterbox.png", p_jitter_box_co2, width = 14, height = 12, dpi = 300)





# Simple CO2 flux plot by component only - all observations combined
p_co2_simple <- filtered_data %>%
  ggplot(aes(x = CO2_best.flux, y = component, color = component)) +
  geom_jitter(alpha = 0.7, size = 2, height = 0.3) +
  stat_summary(fun = median, geom = "crossbar", height = 0.5, alpha = 0.8, color = "black") +
  scale_color_manual(values = component_colors) +
  labs(
    x = expression(CO[2]~Flux~Rate~(mu*mol~m^-2~s^-1)),
    y = "Ecosystem Component",
    color = "Component"
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    legend.position = "bottom"
  )

print(p_co2_simple)
ggsave("output/figures/co2_component_simple.png", p_co2_simple, width = 8, height = 6, dpi = 300)



# Combined CO2 and CH4 flux plot by component - all observations
library(dplyr)

# Reshape data to long format for both gases
combined_data <- filtered_data %>%
  select(component, CO2_best.flux, CH4_best.flux) %>%
  pivot_longer(cols = c(CO2_best.flux, CH4_best.flux), 
               names_to = "gas_type", 
               values_to = "flux_value") %>%
  mutate(gas_type = case_when(
    gas_type == "CO2_best.flux" ~ "CO2",
    gas_type == "CH4_best.flux" ~ "CH4"
  ))

p_combined_flux <- combined_data %>%
  ggplot(aes(x = flux_value, y = component, color = component)) +
  geom_jitter(alpha = 0.7, size = 2, height = 0.3) +
  stat_summary(fun = median, geom = "crossbar", height = 0.5, alpha = 0.8, color = "black") +
  facet_wrap(~gas_type, scales = "free_x") +
  scale_color_manual(values = component_colors) +
  labs(
    x = expression(Flux~Rate~(CO[2]:~mu*mol~m^-2~s^-1 ~CH[4]:~nmol~m^-2~s^-1)),
    y = "Ecosystem Component",
    color = "Component"
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom"
  )

print(p_combined_flux)





# Combined CO2 and CH4 flux plot by component - pseudo log scale
library(dplyr)
library(scales)

# Reshape data to long format for both gases
combined_data <- filtered_data %>%
  select(component, CO2_best.flux, CH4_best.flux) %>%
  pivot_longer(cols = c(CO2_best.flux, CH4_best.flux), 
               names_to = "gas_type", 
               values_to = "flux_value") %>%
  mutate(gas_type = case_when(
    gas_type == "CO2_best.flux" ~ "CO2",
    gas_type == "CH4_best.flux" ~ "CH4"
  ))

p_combined_flux_pseudolog <- combined_data %>%
  ggplot(aes(x = flux_value, y = component, color = component)) +
  geom_jitter(alpha = 0.7, size = 2, height = 0.3) +
  stat_summary(fun = median, geom = "crossbar", height = 0.5, alpha = 0.8, color = "black") +
  facet_wrap(~gas_type, scales = "free_x") +
  scale_x_continuous(trans = "asinh") +
  scale_color_manual(values = component_colors) +
  labs(
    x = expression(Flux~Rate~(CO[2]:~mu*mol~m^-2~s^-1~CH[4]:~nmol~m^-2~s^-1)),
    y = "Ecosystem Component",
    color = "Component"
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom"
  )

print(p_combined_flux_pseudolog)




# Combined CO2 and CH4 flux plot by component - pseudo log scale, faceted by plot
library(dplyr)
library(scales)

# Reshape data to long format for both gases
combined_data <- filtered_data %>%
  select(component, plot, CO2_best.flux, CH4_best.flux) %>%
  pivot_longer(cols = c(CO2_best.flux, CH4_best.flux), 
               names_to = "gas_type", 
               values_to = "flux_value") %>%
  mutate(gas_type = case_when(
    gas_type == "CO2_best.flux" ~ "CO2",
    gas_type == "CH4_best.flux" ~ "CH4"
  ))

p_combined_flux_plot <- combined_data %>%
  ggplot(aes(x = flux_value, y = component, color = component)) +
  geom_jitter(alpha = 0.7, size = 2, height = 0.3) +
  stat_summary(fun = median, geom = "crossbar", height = 0.5, alpha = 0.8, color = "black") +
  facet_grid(plot ~ gas_type, scales = "free_x") +
  scale_x_continuous(trans = "asinh") +
  scale_color_manual(values = component_colors) +
  labs(
    x = expression(Flux~Rate~(CO[2]:~mu*mol~m^-2~s^-1~CH[4]:~nmol~m^-2~s^-1)),
    y = "Ecosystem Component",
    color = "Component"
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom"
  )

print(p_combined_flux_plot)


p_combined_flux_pseudolog <- combined_data %>%
  ggplot(aes(x = flux_value, y = component, color = component, fill = component)) +
  geom_jitter(alpha = 0.6, size = 1.5, height = 0.3) +
  geom_boxplot(alpha = 0.3, outlier.shape = NA, color = "black", width = 0.4) +
  stat_summary(fun = median, geom = "crossbar", height = 0.5, alpha = 0.8, color = "black", width = 0.4) +
  stat_summary(fun = mean, geom = "point", size = 3, color = "black", shape = 23, fill = "white") +
  facet_wrap(~gas_type, scales = "free_x") +
  #scale_x_continuous(trans = "asinh") +
  scale_color_manual(values = component_colors) +
  scale_fill_manual(values = component_colors) +
  labs(
    x = expression(Flux~Rate~(CO[2]:~mu*mol~m^-2~s^-1~CH[4]:~nmol~m^-2~s^-1)),
    y = "Ecosystem Component",
    color = "Component",
    fill = "Component"
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom"
  )+xlim(0,10)

print(p_combined_flux_pseudolog)





###this one is to see the co2 more easily — asinh scale
p_combined_flux_asinh <- combined_data %>%
  ggplot(aes(x = flux_value, y = component, color = component, fill = component)) +
  geom_jitter(alpha = 0.6, size = 1.5, height = 0.3) +
  geom_boxplot(alpha = 0.3, outlier.shape = NA, color = "black", width = 0.4) +
  stat_summary(fun = median, geom = "crossbar", height = 0.5, alpha = 0.8, color = "black", width = 0.4) +
  stat_summary(fun = mean, geom = "point", size = 3, color = "black", shape = 23, fill = "white") +
  facet_wrap(~gas_type, scales = "free_x") +
  scale_x_continuous(trans = "asinh") +
  scale_color_manual(values = component_colors) +
  scale_fill_manual(values = component_colors) +
  labs(
    x = expression(Flux~Rate~(CO[2]:~mu*mol~m^-2~s^-1~CH[4]:~nmol~m^-2~s^-1)),
    y = "Ecosystem Component",
    color = "Component",
    fill = "Component"
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom"
  )

print(p_combined_flux_asinh)
ggsave("output/figures/combined_ch4_co2_by_component.png", p_combined_flux_asinh, width = 12, height = 8, dpi = 300)

