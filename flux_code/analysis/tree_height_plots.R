# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyverse)

# Read the data (assumes you have this loaded)
# df <- read.csv("combined_gas_flux_dataset_with_month_year.csv")

# Update component based on surface column (from your existing code)
df_updated <- df %>%
  mutate(component = case_when(
    surface == "Soil" ~ "soil",
    surface == "Water" ~ "water",
    TRUE ~ component  # Keep original component value for all other cases
  ))

# First, let's check what the height values look like
print("Sample height values (character):")
print(head(df_updated$height[df_updated$component == "stem"], 20))

# Filter for stems only and convert height to numeric, filtering out negative heights
stem_data <- df_updated %>%
  filter(component == "stem",
         !is.na(CH4_best.flux), 
         !is.na(CO2_best.flux),
         !is.na(height)) %>%  # Check for non-missing height first
  mutate(height_numeric = as.numeric(height)) %>%  # Convert height to numeric
  filter(!is.na(height_numeric),  # Remove rows where height conversion failed
         height_numeric >= 0,      # Filter out negative and zero heights
         CH4_best.flux >= 0,
         CO2_best.flux >= 0) %>%
  select(-height) %>%  # Remove original height column
  rename(height = height_numeric)  # Rename numeric version to height

# Check for any remaining negative heights (quality control)
print("Checking for negative heights after filtering:")
print(paste("Minimum height:", min(stem_data$height, na.rm = TRUE)))
print(paste("Any negative heights remaining?", any(stem_data$height < 0, na.rm = TRUE)))

# Filter to include only plots with 3 or more measurements
plot_counts <- stem_data %>%
  count(plot) %>%
  filter(n >= 3)

print("Plots with 3+ measurements:")
print(plot_counts)

# Filter stem_data to only include plots with sufficient measurements
stem_data <- stem_data %>%
  filter(plot %in% plot_counts$plot)

# Check the height data structure after conversion and filtering
print("Height data summary for stems after conversion and filtering:")
print(summary(stem_data$height))
print("Number of stem measurements:")
print(nrow(stem_data))
print("Final plots with stem data:")
print(table(stem_data$plot))

# Define color for stems
stem_color <- "#228B22"  # Forest green

# Re-run just the CH4 plot
p_ch4_height <- stem_data %>%
  ggplot(aes(x = height, y = CH4_best.flux)) +
  geom_point(alpha = 0.7, size = 2, color = stem_color) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.3, color = "darkgreen", span = 0.8) +
  #scale_y_log10() +
  coord_flip() +
  labs(
    title = expression(Stem~CH[4]~Flux~by~Height),
    x = "Height (cm)",
    y = expression(CH[4]~Flux~Rate~(nmol~m^-2~s^-1))
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 11, face = "bold")
  )

print(p_ch4_height)

# 2. CO2 Flux by Height - Scatter plot for stems (height on y-axis after coord_flip)
p_co2_height <- stem_data %>%
  ggplot(aes(x = height, y = CO2_best.flux)) +
  geom_point(alpha = 0.7, size = 2, color = stem_color) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.3, color = "darkgreen", span = 0.8) +
  scale_y_log10() +
  coord_flip() +
  labs(
    title = expression(Stem~CO[2]~Flux~by~Height),
    x = "Height (cm)",
    y = expression(CO[2]~Flux~Rate~(μmol~m^-2~s^-1))
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 11, face = "bold")
  )

print(p_co2_height)

# 3. Combined CH4 and CO2 flux by height for stems (using coord_flip with less sensitive loess)
# Reshape data for combined plot
stem_combined <- stem_data %>%
  select(height, plot, month_year, CH4_best.flux, CO2_best.flux) %>%
  pivot_longer(cols = c(CH4_best.flux, CO2_best.flux), 
               names_to = "gas_type", 
               values_to = "flux_value") %>%
  mutate(gas_type = case_when(
    gas_type == "CH4_best.flux" ~ "CH4",
    gas_type == "CO2_best.flux" ~ "CO2"
  ))

p_combined_height <- stem_combined %>%
  ggplot(aes(x = height, y = flux_value)) +
  geom_point(alpha = 0.7, size = 2, color = stem_color) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.3, color = "darkgreen", span = 0.8) +
  facet_wrap(~gas_type, scales = "free_y") +
  scale_y_log10() +
  coord_flip() +
  labs(
    title = "Stem Gas Flux by Height",
    x = "Height (cm)",
    y = expression(Flux~Rate~(CO[2]:~μmol~m^-2~s^-1~CH[4]:~nmol~m^-2~s^-1))
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 10, face = "bold")
  )

print(p_combined_height)

# 4. Height binned analysis for stems (filter out NA height categories)
# Create height bins appropriate for 0-185 cm range
stem_data_binned <- stem_data %>%
  mutate(height_bin = cut(height, 
                          breaks = c(0, 25, 50, 75, 100, 125, 150, 185),
                          labels = c("0-25cm", "25-50cm", "50-75cm", "75-100cm", "100-125cm", "125-150cm", "150-185cm"),
                          include.lowest = TRUE)) %>%
  filter(!is.na(height_bin))  # Remove NA height categories

p_height_bins <- stem_data_binned %>%
  ggplot(aes(x = height_bin, y = CH4_best.flux)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5, fill = stem_color, color = "darkgreen") +
  geom_jitter(alpha = 0.4, size = 1.5, width = 0.2, color = "darkgreen") +
  scale_y_log10() +
  coord_flip() +
  labs(
    title = expression(Stem~CH[4]~Flux~by~Height~Categories),
    x = "Height Category",
    y = expression(CH[4]~Flux~Rate~(nmol~m^-2~s^-1))
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 11, face = "bold")
  )

print(p_height_bins)

# 5. Faceted by plot for stems (using less sensitive loess)
p_height_faceted <- stem_data %>%
  ggplot(aes(x = height, y = CH4_best.flux)) +
  geom_point(alpha = 0.7, size = 2, color = stem_color) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.3, color = "darkgreen") +
  facet_wrap(~plot, scales = "free") +
  scale_y_log10() +
  coord_flip() +
  labs(
    title = expression(Stem~CH[4]~Flux~by~Height~(by~Plot)),
    x = "Height (cm)",
    y = expression(CH[4]~Flux~Rate~(nmol~m^-2~s^-1))
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 10, face = "bold"),
    strip.text = element_text(size = 9, face = "bold")
  )

print(p_height_faceted)

# 6. Stem flux by height colored by plot (using less sensitive loess)
p_height_by_plot <- stem_data %>%
  ggplot(aes(x = height, y = CH4_best.flux, color = plot)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "loess", se = FALSE, alpha = 0.8, span = 0.8) +
  scale_y_log10() +
  coord_flip() +
  labs(
    title = expression(Stem~CH[4]~Flux~by~Height~(Colored~by~Plot)),
    x = "Height (cm)",
    y = expression(CH[4]~Flux~Rate~(nmol~m^-2~s^-1)),
    color = "Plot"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 11, face = "bold"),
    legend.position = "bottom"
  )

print(p_height_by_plot)

# Summary statistics by height ranges for stems
height_summary <- stem_data_binned %>%
  group_by(height_bin) %>%
  summarise(
    n = n(),
    mean_height = mean(height, na.rm = TRUE),
    mean_ch4 = mean(CH4_best.flux, na.rm = TRUE),
    median_ch4 = median(CH4_best.flux, na.rm = TRUE),
    sd_ch4 = sd(CH4_best.flux, na.rm = TRUE),
    mean_co2 = mean(CO2_best.flux, na.rm = TRUE),
    median_co2 = median(CO2_best.flux, na.rm = TRUE),
    sd_co2 = sd(CO2_best.flux, na.rm = TRUE),
    .groups = 'drop'
  )

print("Summary statistics by height category for stems:")
print(height_summary)

# Correlation analysis for stems
correlation_results <- stem_data %>%
  summarise(
    ch4_height_cor = cor(height, log10(CH4_best.flux), use = "complete.obs"),
    co2_height_cor = cor(height, log10(CO2_best.flux), use = "complete.obs"),
    ch4_height_cor_linear = cor(height, CH4_best.flux, use = "complete.obs"),
    co2_height_cor_linear = cor(height, CO2_best.flux, use = "complete.obs"),
    n_obs = n()
  )

print("Correlation between height and flux for stems:")
print(correlation_results)

# Correlation by plot (only for plots with 5+ observations for reliable correlations)
correlation_by_plot <- stem_data %>%
  group_by(plot) %>%
  summarise(
    ch4_height_cor = cor(height, log10(CH4_best.flux), use = "complete.obs"),
    co2_height_cor = cor(height, log10(CO2_best.flux), use = "complete.obs"),
    n_obs = n(),
    .groups = 'drop'
  ) %>%
  filter(n_obs >= 5)  # Only show plots with at least 5 observations for reliable correlations

print("Correlation between height and flux by plot (plots with 5+ observations):")
print(correlation_by_plot)







# Load required libraries
library(dplyr)
library(ggplot2)
library(scales)  # For log scale labels

# Create height categories with your specified bins
stem_data_categorized <- stem_data %>%
  mutate(
    height_category = case_when(
      height >= 0 & height < 50 ~ "0-50cm",
      height >= 50 & height < 100 ~ "50-100cm", 
      height >= 100 & height < 150 ~ "100-150cm",
      height >= 150 ~ ">150cm",
      TRUE ~ NA_character_
    ),
    # Convert to factor with proper ordering
    height_category = factor(height_category, 
                             levels = c("0-50cm", "50-100cm", "100-150cm", ">150cm"))
  ) %>%
  # Remove rows with missing height categories
  filter(!is.na(height_category))

# Filter for plots with >15 measurements
plots_with_sufficient_data <- stem_data_categorized %>%
  count(plot) %>%
  filter(n > 15) %>%
  pull(plot)

# Filter the data to only include plots with >15 measurements
stem_data_final <- stem_data_categorized %>%
  filter(plot %in% plots_with_sufficient_data)

# Check how many plots and measurements we have
cat("Plots with >15 measurements:", length(plots_with_sufficient_data), "\n")
cat("Total measurements in filtered data:", nrow(stem_data_final), "\n")

# Create the faceted plot
p_stem_flux_faceted <- stem_data_final %>%
  ggplot(aes(x = height_category, y = CH4_best.flux)) +
  geom_boxplot(aes(fill = height_category), alpha = 0.7, outlier.alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
  facet_wrap(~ plot, scales = "free_y", ncol = 3) +
  scale_fill_viridis_d(name = "Height\nCategory") +
  labs(
    title = expression(Stem~CH[4]~Flux~by~Height~Category~(Plots~with~">15"~Measurements)),
    x = "Height Category",
    y = expression(CH[4]~Flux~Rate~(nmol~m^-2~s^-1)),
    caption = paste("Data from", length(plots_with_sufficient_data), "plots with >15 measurements each")
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom"
  )

print(p_stem_flux_faceted)

# Summary statistics by height category and plot
summary_stats <- stem_data_final %>%
  group_by(plot, height_category) %>%
  summarise(
    n = n(),
    mean_flux = mean(CH4_best.flux, na.rm = TRUE),
    median_flux = median(CH4_best.flux, na.rm = TRUE),
    sd_flux = sd(CH4_best.flux, na.rm = TRUE),
    .groups = "drop"
  )

print("Summary statistics by plot and height category:")
print(summary_stats)

# Optional: Create a summary plot showing overall patterns across height categories
p_overall_summary <- stem_data_final %>%
  ggplot(aes(x = height_category, y = CH4_best.flux)) +
  geom_boxplot(aes(fill = height_category), alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.8) +
  scale_fill_viridis_d(name = "Height\nCategory") +
  labs(
    title = expression(Overall~Stem~CH[4]~Flux~by~Height~Category),
    x = "Height Category", 
    y = expression(CH[4]~Flux~Rate~(nmol~m^-2~s^-1)),
    subtitle = paste("Combined data from", length(plots_with_sufficient_data), "plots")
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )

print(p_overall_summary)



# Load required libraries
library(dplyr)
library(ggplot2)
library(scales)  # For log scale labels

# Create height categories with your specified bins
stem_data_categorized <- stem_data %>%
  mutate(
    height_category = case_when(
      height >= 0 & height < 50 ~ "0-50cm",
      height >= 50 & height < 100 ~ "50-100cm", 
      height >= 100 & height < 150 ~ "100-150cm",
      height >= 150 ~ ">150cm",
      TRUE ~ NA_character_
    ),
    # Convert to factor with proper ordering
    height_category = factor(height_category, 
                             levels = c("0-50cm", "50-100cm", "100-150cm", ">150cm"))
  ) %>%
  # Remove rows with missing height categories
  filter(!is.na(height_category))

# Filter for plots with >15 measurements
plots_with_sufficient_data <- stem_data_categorized %>%
  count(plot) %>%
  filter(n > 15) %>%
  pull(plot)

# Filter the data to only include plots with >15 measurements
stem_data_final <- stem_data_categorized %>%
  filter(plot %in% plots_with_sufficient_data) %>%
  # Filter out zero and negative values for log transformation
  filter(CH4_best.flux > 0)

# Check how many plots and measurements we have
cat("Plots with >15 measurements:", length(plots_with_sufficient_data), "\n")
cat("Total measurements in filtered data:", nrow(stem_data_final), "\n")

# Create the faceted plot with height on y-axis and log scale for flux
p_stem_flux_faceted <- stem_data_final %>%
  ggplot(aes(x = CH4_best.flux, y = height_category)) +
  geom_boxplot(aes(fill = height_category), alpha = 0.7, outlier.alpha = 0.5) +
  geom_jitter(height = 0.2, alpha = 0.5, size = 1) +
  facet_wrap(~ plot, scales = "free_x", ncol = 3) +
  scale_x_log10(labels = label_log()) +
  scale_fill_viridis_d(name = "Height\nCategory") +
  labs(
    title = expression(Stem~CH[4]~Flux~by~Height~Category~(Plots~with~">15"~Measurements)),
    y = "Height Category",
    x = expression(CH[4]~Flux~Rate~(nmol~m^-2~s^-1)~-~Log~Scale),
    caption = paste("Data from", length(plots_with_sufficient_data), "plots with >15 measurements each")
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "bottom"
  )

print(p_stem_flux_faceted)


# Create the faceted plot with height on y-axis and log scale for flux
p_stem_flux_faceted <- stem_data_final %>%
  ggplot(aes(x = CH4_best.flux, y = height_category)) +
  geom_boxplot(aes(fill = height_category), alpha = 0.7, outlier.alpha = 0.5) +
  geom_jitter(height = 0.2, alpha = 0.5, size = 1) +
  facet_wrap(~ plot, ncol = 3) +
  scale_x_log10(labels = label_log()) +
  scale_fill_viridis_d(name = "Height\nCategory") +
  labs(
    #title = expression(Stem~CH[4]~Flux~by~Height~Category~(Plots~with~">15"~Measurements)),
    y = "Height Category",
    x = expression(CH[4]~Flux~Rate~(nmol~m^-2~s^-1)~-~Log~Scale),
    #caption = paste("Data from", length(plots_with_sufficient_data), "plots with >15 measurements each")
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "none"
  )

print(p_stem_flux_faceted)

# Summary statistics by height category and plot
summary_stats <- stem_data_final %>%
  group_by(plot, height_category) %>%
  summarise(
    n = n(),
    mean_flux = mean(CH4_best.flux, na.rm = TRUE),
    median_flux = median(CH4_best.flux, na.rm = TRUE),
    sd_flux = sd(CH4_best.flux, na.rm = TRUE),
    .groups = "drop"
  )

print("Summary statistics by plot and height category:")
print(summary_stats)

# Optional: Create a summary plot showing overall patterns across height categories
p_overall_summary <- stem_data_final %>%
  ggplot(aes(x = CH4_best.flux, y = height_category)) +
  geom_boxplot(aes(fill = height_category), alpha = 0.7) +
  geom_jitter(height = 0.2, alpha = 0.3, size = 0.8) +
  scale_x_log10(labels = label_log()) +
  scale_fill_viridis_d(name = "Height\nCategory") +
  labs(
    title = expression(Overall~Stem~CH[4]~Flux~by~Height~Category),
    y = "Height Category", 
    x = expression(CH[4]~Flux~Rate~(nmol~m^-2~s^-1)~-~Log~Scale),
    subtitle = paste("Combined data from", length(plots_with_sufficient_data), "plots")
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )

print(p_overall_summary)