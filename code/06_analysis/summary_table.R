#!/usr/bin/env Rscript
# ===============================================================================
# Flux Statistics - Single Comprehensive Table
# FIXED: Converts height_corrected to numeric before binning
# ===============================================================================

# Read the data (assumes you have this loaded)
df <- read.csv("output/combined_gas_flux_dataset.csv")



library(dplyr)
library(ggplot2)
library(tidyr)
library(writexl)
library(stringr)

# Helper Functions --------------------------------------------------------------

# Geometric stats: computed on positive values only.
# For flux data, negative values indicate net uptake (real signal).
# Geometric mean/SD/SE are only meaningful for strictly positive data,
# so we filter to positive values and report the count (n_pos) alongside.
geom_mean <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x) & is.finite(x)]
  x_pos <- x[x > 0]
  if (length(x_pos) == 0) return(NA_real_)
  exp(mean(log(x_pos)))
}

geom_sd <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x) & is.finite(x)]
  x_pos <- x[x > 0]
  if (length(x_pos) <= 1) return(NA_real_)
  exp(sd(log(x_pos)))
}

geom_se <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x) & is.finite(x)]
  x_pos <- x[x > 0]
  n <- length(x_pos)
  if (n <= 1) return(NA_real_)
  exp(sd(log(x_pos)) / sqrt(n))
}

calc_stats <- function(data, var_name) {
  x <- data[[var_name]]
  x <- x[!is.na(x) & is.finite(x)]

  if (length(x) == 0) {
    return(tibble(
      n = 0L,
      n_pos = 0L,
      median = NA_real_,
      mean = NA_real_,
      geom_mean = NA_real_,
      sd = NA_real_,
      se = NA_real_,
      geom_sd = NA_real_,
      geom_se = NA_real_
    ))
  }

  n <- length(x)
  n_pos <- sum(x > 0)
  mean_val <- mean(x)
  sd_val <- sd(x)

  tibble(
    n = n,
    n_pos = n_pos,
    median = median(x),
    mean = mean_val,
    geom_mean = geom_mean(x),
    sd = sd_val,
    se = sd_val / sqrt(n),
    geom_sd = geom_sd(x),
    geom_se = geom_se(x)
  )
}

# Main Analysis -----------------------------------------------------------------

cat("\n===============================================================================\n")
cat("FLUX STATISTICS TABLE\n")
cat("===============================================================================\n\n")

# Process data ------------------------------------------------------------------
cat("Processing data...\n")

df_processed <- df %>%
  mutate(
    # Extract month
    month_extracted = case_when(
      !is.na(month) ~ as.integer(month),
      !is.na(month_year) ~ as.integer(str_sub(month_year, -2, -1)),
      TRUE ~ NA_integer_
    ),
    
    # Define season
    season = case_when(
      month_extracted == 3 ~ "dry",
      month_extracted == 10 ~ "wet",
      TRUE ~ NA_character_
    ),
    
    # Define disturbance level
    disturbance_level = case_when(
      plot %in% c("SRS5", "SRS6", "RB10") ~ "healthy",
      plot == "BL60" ~ "regenerating",
      plot %in% c("CP40", "FLM30", "MI") ~ "ghost",
      plot == "SE1" ~ "scrub",
      TRUE ~ NA_character_
    ),
    
    # Component
    component_base = case_when(
      !is.na(component) ~ component,
      #!is.na(surface) ~ surface,
      TRUE ~ "unknown"
    ),
    
    # CRITICAL FIX: Convert height_corrected to numeric first!
    height_corrected_numeric = as.numeric(height_corrected),
    
    # Height categories for stems/pneumatophores
    height_category = case_when(
      grepl("stem|pneum", component_base, ignore.case = TRUE) & !is.na(height_corrected_numeric) &
        height_corrected_numeric >= 0 ~
        as.character(cut(height_corrected_numeric,
                         breaks = c(0, 50, 100, 150, Inf),
                         labels = c("0-50cm", "50-100cm", "100-150cm", ">150cm"),
                         right = FALSE,
                         include.lowest = TRUE)),
      grepl("stem|pneum", component_base, ignore.case = TRUE) & !is.na(height_corrected_numeric) &
        height_corrected_numeric < 0 ~ "submerged",
      grepl("stem|pneum", component_base, ignore.case = TRUE) & is.na(height_corrected_numeric) ~ "unknown",
      TRUE ~ NA_character_
    )
  )

cat("✓ Converted height_corrected to numeric\n")
cat("✓ Created height categories\n\n")

# Filter data
df_analyzed <- df_processed %>%
  filter(
    !is.na(season),
    plot %in% c("BL60", "CP40", "FLM30", "MI", "RB10", "SE1", "SRS5", "SRS6")
  )

cat("Rows analyzed:", nrow(df_analyzed), "\n\n")

# Check height categories
cat("Height category distribution for stems:\n")
stem_heights <- df_analyzed %>%
  filter(component_base == "stem") %>%
  group_by(height_category) %>%
  summarise(
    n = n(),
    min_height_corrected = min(height_corrected_numeric, na.rm = TRUE),
    max_height_corrected = max(height_corrected_numeric, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(height_category)

print(stem_heights)
cat("\n")

# Calculate statistics ----------------------------------------------------------
cat("Calculating CH4 statistics...\n")

ch4_stats <- df_analyzed %>%
  group_by(season, plot, disturbance_level, component_base, height_category) %>%
  reframe(calc_stats(pick(everything()), "CH4_best.flux")) %>%
  mutate(gas = "CH4") %>%
  ungroup()

cat("Calculating CO2 statistics...\n")

co2_stats <- df_analyzed %>%
  group_by(season, plot, disturbance_level, component_base, height_category) %>%
  reframe(calc_stats(pick(everything()), "CO2_best.flux")) %>%
  mutate(gas = "CO2") %>%
  ungroup()

# Combine into one table --------------------------------------------------------
cat("Creating final table...\n\n")

final_table <- bind_rows(ch4_stats, co2_stats) %>%
  pivot_wider(
    names_from = gas,
    values_from = c(n, n_pos, median, mean, geom_mean, sd, se, geom_sd, geom_se),
    names_sep = "_"
  ) %>%
  # Remove empty groups where BOTH gases have n=0
  filter(!(n_CH4 == 0 & n_CO2 == 0)) %>%
  arrange(season, disturbance_level, plot, component_base, height_category) %>%
  select(
    season,
    plot,
    disturbance_level,
    component_base,
    height_category,
    n_CH4, n_pos_CH4, median_CH4, mean_CH4, geom_mean_CH4, sd_CH4, se_CH4, geom_sd_CH4, geom_se_CH4,
    n_CO2, n_pos_CO2, median_CO2, mean_CO2, geom_mean_CO2, sd_CO2, se_CO2, geom_sd_CO2, geom_se_CO2
  )

# Display results ---------------------------------------------------------------
cat("===============================================================================\n")
cat("FINAL TABLE\n")
cat("===============================================================================\n\n")

cat("Total rows:", nrow(final_table), "\n")
cat("Seasons:", paste(unique(final_table$season), collapse = ", "), "\n")
cat("Plots:", paste(sort(unique(final_table$plot)), collapse = ", "), "\n")
cat("Components:", paste(sort(unique(final_table$component_base)), collapse = ", "), "\n\n")

# Check height categories in final table
stem_rows <- final_table %>% filter(component_base == "stem")
cat("Stem rows by height category:\n")
height_summary <- stem_rows %>%
  group_by(height_category) %>%
  summarise(
    n_rows = n(),
    total_measurements_CH4 = sum(n_CH4, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(height_category)
print(height_summary)
cat("\n")

cat("Preview (first 50 rows):\n")
print(final_table, n = 50)

# Save --------------------------------------------------------------------------
cat("\n\nSaving results...\n")

write.csv(final_table, "output/flux_statistics_table.csv", row.names = FALSE)

cat("Done!\n")

# Return the table
invisible(final_table)




# Simple mean vs median plot
ggplot(final_table, aes(x = median_CH4, y = mean_CH4)) +
  geom_point(aes(color = component_base), size = 3, alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "Median CH4 Flux",
    y = "Mean CH4 Flux",
    color = "Component",
    title = "Mean vs Median CH4 Flux"
  ) +
  theme_bw()
