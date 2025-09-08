# Load required libraries
library(dplyr)
library(ggplot2)

# Read the data (if not already loaded)
# df <- read.csv("combined_gas_flux_dataset_with_month_year.csv")

# Define the plots to include
selected_plots <- c("BL60", "CP40", "FLM30", "MI", "SE1", "SRS5", "SRS6")

# Update component based on surface column
df_updated <- df %>%
  mutate(component = case_when(
    surface == "Soil" ~ "soil",
    surface == "Water" ~ "water",
    TRUE ~ component  # Keep original component value for all other cases
  ))

# Filter data for selected plots only
filtered_data <- df_updated %>%
  filter(plot %in% selected_plots,
         !is.na(CH4_best.flux), 
         !is.na(component), 
         !is.na(month_year),
         CH4_best.flux > 0)  # Remove zero/negative values for log scale

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
  scale_y_log10() +
  scale_color_manual(values = component_colors) +
  labs(
    title = "CH4 Flux by Component across Selected Plots",
    subtitle = "Rows = Plot, Columns = Month-Year (Season), Colored by Component",
    x = "Component",
    y = "CH4 Flux (CH4_best.flux) - Log Scale",
    color = "Component"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom"
  )

print(p1)

# Alternative with jittered points
p2 <- filtered_data %>%
  ggplot(aes(x = component, y = CH4_best.flux, color = component)) +
  geom_jitter(alpha = 0.7, size = 2, width = 0.3) +
  stat_summary(fun = median, geom = "crossbar", width = 0.5, alpha = 0.8, color = "black") +
  facet_grid(plot ~ month_year, scales = "free_x") +
  scale_y_log10() +
  scale_color_manual(values = component_colors) +
  labs(
    title = "CH4 Flux by Component across Selected Plots",
    subtitle = "Rows = Plot, Columns = Month-Year, Colored by Component, with Medians",
    x = "Component",
    y = "CH4 Flux (CH4_best.flux) - Log Scale",
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
  scale_y_log10() +
  scale_color_manual(values = component_colors) +
  labs(
    title = "CH4 Flux by Component across Selected Plots",
    subtitle = "Rows = Plot, Columns = Month-Year, Colored by Component",
    x = "Component",
    y = "CH4 Flux (CH4_best.flux) - Log Scale",
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
  scale_y_log10() +
  scale_fill_manual(values = component_colors) +
  labs(
    title = "CH4 Flux Distribution by Component across Selected Plots",
    subtitle = "Violin plots showing distribution shape, colored by component",
    x = "Component",
    y = "CH4 Flux (CH4_best.flux) - Log Scale",
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
  scale_x_log10(
    name = expression(CH[4]~Flux~Rate~(nmol~m^-2~s^-1)),
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    breaks = scales::trans_breaks("log10", function(x) 10^x, n = 4)
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
  scale_x_log10(
    name = expression(CH[4]~Flux~Rate~(nmol~m^-2~s^-1)),
    breaks = c(0.01, 0.1, 1, 10, 100, 1000),
    labels = c("0.01", "0.1", "1", "10", "100", "1000")
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

