#!/usr/bin/env Rscript
# ===============================================================================
# Export tree flux data for Julio Salas-Rabaza collaboration
# "Tree-mediated GHG fluxes in mangroves" compilation
#
# Exports aggregated (mean +/- SE) flux data from healthy sites only (SRS5, SRS6)
# for tree tissues: stems, prop roots (R. mangle), and soil with/without pneumatophores.
#
# QC approach (following Gewirtzman et al. ch4-data-filtering methodology):
#   - ALL measurements are retained (no exclusions)
#   - QC flags indicate data quality per group:
#     * n_below_MDF: count of measurements below goFlux minimal detectable flux
#     * n_flagged: count with goFlux quality.check flags (SE, AICc, g-factor, etc.)
#     * median_SNR: median signal-to-noise ratio (|flux| / SE) across measurements
#   - Both mean and median are reported so users can assess skewness/outlier influence
# ===============================================================================

library(dplyr)
library(readr)
library(tidyr)

cat("=== EXPORT FOR JULIO COLLABORATION ===\n\n")

# Read the combined dataset
df <- read_csv("output/combined_gas_flux_dataset.csv", show_col_types = FALSE)

# Site metadata
site_meta <- tibble(
  plot = c("SRS5", "SRS6"),
  site_name = c("Shark River Slough Site 5", "Shark River Slough Site 6"),
  lat = c(25.3640, 25.3650),
  lon = c(-81.0770, -81.0780),
  ecotype = c("riverine", "riverine")
)

species_labels <- c(RHMA = "Rhizophora mangle", AVGE = "Avicennia germinans", LARA = "Laguncularia racemosa")

# =============================================================================
# PART 1: TREE TISSUE DATA (stems + prop roots)
# =============================================================================

cat("Processing tree tissue data...\n")

trees <- df %>%
  filter(
    plot %in% c("SRS5", "SRS6"),
    component %in% c("stem", "root"),
    !is.na(species)
  ) %>%
  mutate(
    season = case_when(
      grepl("-03$", month_year) ~ "dry",
      grepl("-10$", month_year) ~ "wet",
      TRUE ~ NA_character_
    ),
    # Tissue label for Julio's template
    tissue = case_when(
      component == "root" & species == "RHMA" ~ "prop root",
      component == "root" ~ "root",
      TRUE ~ "stem"
    ),
    species_full = species_labels[species],

    # --- QC flags per measurement (following ch4-data-filtering approach) ---

    # goFlux quality.check: NA or "" = clean (no flags triggered)
    ch4_flagged = !is.na(CH4_quality.check) & CH4_quality.check != "",
    co2_flagged = !is.na(CO2_quality.check) & CO2_quality.check != "",

    # Below MDF (manufacturer precision-based minimal detectable flux from goFlux)
    ch4_below_mdf = !is.na(CH4_best.flux) & !is.na(CH4_MDF) & abs(CH4_best.flux) <= CH4_MDF,
    co2_below_mdf = !is.na(CO2_best.flux) & !is.na(CO2_MDF) & abs(CO2_best.flux) <= CO2_MDF,

    # Signal-to-noise ratio: |flux| / SE (SE-based, per measurement)
    ch4_snr = ifelse(!is.na(CH4_best.flux) & !is.na(CH4_LM.SE) & CH4_LM.SE > 0,
                     abs(CH4_best.flux) / CH4_LM.SE, NA_real_),
    co2_snr = ifelse(!is.na(CO2_best.flux) & !is.na(CO2_LM.SE) & CO2_LM.SE > 0,
                     abs(CO2_best.flux) / CO2_LM.SE, NA_real_)
  ) %>%
  filter(!is.na(season))

# Aggregate by site x species x tissue x season
tree_agg <- trees %>%
  group_by(plot, species, species_full, tissue, season) %>%
  summarise(
    # CH4 flux statistics
    n_CH4 = sum(!is.na(CH4_best.flux)),
    CH4_mean = mean(CH4_best.flux, na.rm = TRUE),
    CH4_se = ifelse(n_CH4 > 1, sd(CH4_best.flux, na.rm = TRUE) / sqrt(n_CH4), NA_real_),
    CH4_median = median(CH4_best.flux, na.rm = TRUE),
    CH4_min = ifelse(n_CH4 > 0, min(CH4_best.flux, na.rm = TRUE), NA_real_),
    CH4_max = ifelse(n_CH4 > 0, max(CH4_best.flux, na.rm = TRUE), NA_real_),

    # CH4 QC columns
    CH4_n_below_MDF = sum(ch4_below_mdf, na.rm = TRUE),
    CH4_n_flagged = sum(ch4_flagged & !is.na(CH4_best.flux), na.rm = TRUE),
    CH4_median_SNR = round(median(ch4_snr, na.rm = TRUE), 1),

    # CO2 flux statistics
    n_CO2 = sum(!is.na(CO2_best.flux)),
    CO2_mean = mean(CO2_best.flux, na.rm = TRUE),
    CO2_se = ifelse(n_CO2 > 1, sd(CO2_best.flux, na.rm = TRUE) / sqrt(n_CO2), NA_real_),
    CO2_median = median(CO2_best.flux, na.rm = TRUE),
    CO2_min = ifelse(n_CO2 > 0, min(CO2_best.flux, na.rm = TRUE), NA_real_),
    CO2_max = ifelse(n_CO2 > 0, max(CO2_best.flux, na.rm = TRUE), NA_real_),

    # CO2 QC columns
    CO2_n_below_MDF = sum(co2_below_mdf, na.rm = TRUE),
    CO2_n_flagged = sum(co2_flagged & !is.na(CO2_best.flux), na.rm = TRUE),
    CO2_median_SNR = round(median(co2_snr, na.rm = TRUE), 1),

    # Metadata
    mean_height_cm = mean(height_corrected, na.rm = TRUE),
    years = paste(sort(unique(format(as.Date(paste0(month_year, "-01")), "%Y"))), collapse = ", "),
    lenticels = paste(unique(lenticels[!is.na(lenticels) & lenticels != ""]), collapse = "/"),
    .groups = "drop"
  ) %>%
  filter(n_CH4 > 0 | n_CO2 > 0) %>%
  left_join(site_meta, by = "plot")

cat("  Tree tissue rows:", nrow(tree_agg), "\n")

# =============================================================================
# PART 2: SOIL WITH/WITHOUT PNEUMATOPHORES
# =============================================================================

cat("Processing soil pneumatophore data...\n")

soil <- df %>%
  filter(
    plot %in% c("SRS5", "SRS6"),
    component == "soil",
    !is.na(pneumatophore_count)
  ) %>%
  mutate(
    season = case_when(
      grepl("-03$", month_year) ~ "dry",
      grepl("-10$", month_year) ~ "wet",
      TRUE ~ NA_character_
    ),
    pneum_count = as.numeric(pneumatophore_count),
    # Convert count to density (pneu/m²) using chamber footprint area
    pneum_area_m2 = surface_area_cm2 / 10000,  # cm² → m²
    pneum_density = pneum_count / pneum_area_m2,
    pneum_status = ifelse(pneum_count > 0, "with", "without"),
    ch4_flagged = !is.na(CH4_quality.check) & CH4_quality.check != "",
    co2_flagged = !is.na(CO2_quality.check) & CO2_quality.check != "",
    ch4_below_mdf = !is.na(CH4_best.flux) & !is.na(CH4_MDF) & abs(CH4_best.flux) <= CH4_MDF,
    co2_below_mdf = !is.na(CO2_best.flux) & !is.na(CO2_MDF) & abs(CO2_best.flux) <= CO2_MDF,
    ch4_snr = ifelse(!is.na(CH4_best.flux) & !is.na(CH4_LM.SE) & CH4_LM.SE > 0,
                     abs(CH4_best.flux) / CH4_LM.SE, NA_real_),
    co2_snr = ifelse(!is.na(CO2_best.flux) & !is.na(CO2_LM.SE) & CO2_LM.SE > 0,
                     abs(CO2_best.flux) / CO2_LM.SE, NA_real_)
  ) %>%
  filter(!is.na(season))

soil_agg <- soil %>%
  group_by(plot, pneum_status, season) %>%
  summarise(
    n_plots = n(),
    mean_pneum_count = mean(pneum_count, na.rm = TRUE),
    mean_pneum_density = round(mean(pneum_density, na.rm = TRUE), 0),
    n_CH4 = sum(!is.na(CH4_best.flux)),
    CH4_mean = mean(CH4_best.flux, na.rm = TRUE),
    CH4_se = ifelse(sum(!is.na(CH4_best.flux)) > 1,
                    sd(CH4_best.flux, na.rm = TRUE) / sqrt(sum(!is.na(CH4_best.flux))), NA_real_),
    CH4_median = median(CH4_best.flux, na.rm = TRUE),
    CH4_min = ifelse(sum(!is.na(CH4_best.flux)) > 0, min(CH4_best.flux, na.rm = TRUE), NA_real_),
    CH4_max = ifelse(sum(!is.na(CH4_best.flux)) > 0, max(CH4_best.flux, na.rm = TRUE), NA_real_),
    CH4_n_below_MDF = sum(ch4_below_mdf, na.rm = TRUE),
    CH4_n_flagged = sum(ch4_flagged & !is.na(CH4_best.flux), na.rm = TRUE),
    CH4_median_SNR = round(median(ch4_snr, na.rm = TRUE), 1),
    n_CO2 = sum(!is.na(CO2_best.flux)),
    CO2_mean = mean(CO2_best.flux, na.rm = TRUE),
    CO2_se = ifelse(sum(!is.na(CO2_best.flux)) > 1,
                    sd(CO2_best.flux, na.rm = TRUE) / sqrt(sum(!is.na(CO2_best.flux))), NA_real_),
    CO2_median = median(CO2_best.flux, na.rm = TRUE),
    CO2_min = ifelse(sum(!is.na(CO2_best.flux)) > 0, min(CO2_best.flux, na.rm = TRUE), NA_real_),
    CO2_max = ifelse(sum(!is.na(CO2_best.flux)) > 0, max(CO2_best.flux, na.rm = TRUE), NA_real_),
    CO2_n_below_MDF = sum(co2_below_mdf, na.rm = TRUE),
    CO2_n_flagged = sum(co2_flagged & !is.na(CO2_best.flux), na.rm = TRUE),
    CO2_median_SNR = round(median(co2_snr, na.rm = TRUE), 1),
    years = paste(sort(unique(format(as.Date(paste0(month_year, "-01")), "%Y"))), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(n_CH4 > 0 | n_CO2 > 0) %>%
  left_join(site_meta, by = "plot")

cat("  Soil pneumatophore rows:", nrow(soil_agg), "\n")

# =============================================================================
# PART 3: FORMAT FOR JULIO'S TEMPLATE
# =============================================================================

cat("\nFormatting for Julio's template...\n")

# Tree rows — Julio's template column order, then our extras
tree_rows <- tree_agg %>%
  transmute(
    # --- Julio's template columns (in his order) ---
    Contributor_name = "Jonathan Gewirtzman",
    Country = "USA",
    Location = "Everglades National Park, FL",
    Site = plot,
    Lat = lat, Long = lon,
    GeoSet_ecotype = ecotype,
    Species = species_full,
    Chamber_type = "rigid",
    Chamber_method = "closed chamber",
    Gas_sampling = "field",
    Dark_light = "light",
    Interface = "variable (tidal)",
    Tide = "variable",
    Tree_tissue_surface = tissue,
    Wood_density_g_cm3 = NA_real_,
    Lenticels_density_lent_cm2 = ifelse(lenticels != "", lenticels, NA_character_),
    Pneumatophore_density_pneu_m2 = NA_character_,
    Timescale = "seasonal",
    Season = season,
    Year = ifelse(season == "dry", years, "2022"),
    # CO2 first (Julio's order)
    CO2_mean = round(CO2_mean, 3),
    CO2_se = round(CO2_se, 3),
    CO2_mdn = round(CO2_median, 3),
    CO2_range = ifelse(n_CO2 > 0, paste0(round(CO2_min, 3), " to ", round(CO2_max, 3)), NA_character_),
    CO2_units = ifelse(n_CO2 > 0, "umol m-2 s-1", NA_character_),
    # CH4
    CH4_mean = round(CH4_mean, 4),
    CH4_se = round(CH4_se, 4),
    CH4_mdn = round(CH4_median, 4),
    CH4_range = ifelse(n_CH4 > 0, paste0(round(CH4_min, 4), " to ", round(CH4_max, 4)), NA_character_),
    CH4_units = ifelse(n_CH4 > 0, "umol m-2 s-1", NA_character_),
    # N2O (no data)
    N2O_mean = NA_real_,
    N2O_se = NA_real_,
    N2O_mdn = NA_real_,
    N2O_range = NA_character_,
    N2O_units = NA_character_,
    # Comments
    Comments = paste0(
      "n_CH4=", n_CH4, "; n_CO2=", n_CO2,
      "; CH4_below_MDF=", CH4_n_below_MDF, "/", n_CH4,
      "; CH4_flagged=", CH4_n_flagged, "/", n_CH4,
      "; CH4_median_SNR=", CH4_median_SNR,
      "; CO2_below_MDF=", CO2_n_below_MDF, "/", n_CO2,
      "; CO2_flagged=", CO2_n_flagged, "/", n_CO2,
      "; CO2_median_SNR=", CO2_median_SNR,
      ifelse(!is.na(mean_height_cm) & is.finite(mean_height_cm),
             paste0("; mean_height_cm=", round(mean_height_cm, 0)), ""),
      "; analyzers=LGR_UGGA_GLA131+Picarro_G4301",
      "; all_measurements_retained_with_QC_flags"
    )
  )

# Soil rows
soil_rows <- soil_agg %>%
  transmute(
    Contributor_name = "Jonathan Gewirtzman",
    Country = "USA",
    Location = "Everglades National Park, FL",
    Site = plot,
    Lat = lat, Long = lon,
    GeoSet_ecotype = ecotype,
    Species = "mixed (R. mangle, A. germinans, L. racemosa)",
    Chamber_type = "rigid",
    Chamber_method = "closed chamber",
    Gas_sampling = "field",
    Dark_light = "light",
    Interface = "soil-air",
    Tide = "low",
    Tree_tissue_surface = paste0("soil (", pneum_status, " pneumatophores)"),
    Wood_density_g_cm3 = NA_real_,
    Lenticels_density_lent_cm2 = NA_character_,
    Pneumatophore_density_pneu_m2 = ifelse(pneum_status == "with",
                                            as.character(mean_pneum_density),
                                            "0"),
    Timescale = "seasonal",
    Season = season,
    Year = years,
    # CO2 first (Julio's order)
    CO2_mean = round(CO2_mean, 3),
    CO2_se = round(CO2_se, 3),
    CO2_mdn = round(CO2_median, 3),
    CO2_range = ifelse(n_CO2 > 0, paste0(round(CO2_min, 3), " to ", round(CO2_max, 3)), NA_character_),
    CO2_units = ifelse(n_CO2 > 0, "umol m-2 s-1", NA_character_),
    # CH4
    CH4_mean = round(CH4_mean, 4),
    CH4_se = round(CH4_se, 4),
    CH4_mdn = round(CH4_median, 4),
    CH4_range = ifelse(n_CH4 > 0, paste0(round(CH4_min, 4), " to ", round(CH4_max, 4)), NA_character_),
    CH4_units = ifelse(n_CH4 > 0, "umol m-2 s-1", NA_character_),
    # N2O (no data)
    N2O_mean = NA_real_,
    N2O_se = NA_real_,
    N2O_mdn = NA_real_,
    N2O_range = NA_character_,
    N2O_units = NA_character_,
    # Comments
    Comments = paste0(
      "n_CH4=", n_CH4, "; n_CO2=", n_CO2,
      "; CH4_below_MDF=", CH4_n_below_MDF, "/", n_CH4,
      "; CH4_flagged=", CH4_n_flagged, "/", n_CH4,
      "; CH4_median_SNR=", CH4_median_SNR,
      "; CO2_below_MDF=", CO2_n_below_MDF, "/", n_CO2,
      "; CO2_flagged=", CO2_n_flagged, "/", n_CO2,
      "; CO2_median_SNR=", CO2_median_SNR,
      "; pneum_counts_per_chamber_not_per_m2",
      "; analyzers=LGR_UGGA_GLA131+Picarro_G4301",
      "; all_measurements_retained_with_QC_flags"
    )
  )

# Combine
julio_export <- bind_rows(tree_rows, soil_rows)

# =============================================================================
# PART 4: SAVE AND REPORT
# =============================================================================

outfile <- "output/julio_collab_tree_fluxes.csv"
write_csv(julio_export, outfile)
cat("\nSaved:", outfile, "\n")
cat("Total rows:", nrow(julio_export), "\n\n")

# Print summary
cat("===============================================================================\n")
cat("SUMMARY\n")
cat("===============================================================================\n\n")

cat("Sites: SRS5 (intermediate stature), SRS6 (tall stature)\n")
cat("Seasons: dry (March 2022 + 2023 pooled), wet (October 2022)\n")
cat("Units: umol m-2 s-1\n")
cat("Analyzers: LGR UGGA (GLA131) and Picarro G4301, continuous in-situ\n\n")

cat("Tissue breakdown:\n")
julio_export %>%
  group_by(Tree_tissue_surface) %>%
  summarise(rows = n(), .groups = "drop") %>%
  print()

cat("\nColumn check — Julio's template columns present:\n")
julio_cols <- c("Contributor_name", "Country", "Location", "Site", "Lat", "Long",
                "GeoSet_ecotype", "Species", "Chamber_type", "Chamber_method",
                "Gas_sampling", "Dark_light", "Interface", "Tide",
                "Tree_tissue_surface", "Wood_density_g_cm3",
                "Lenticels_density_lent_cm2", "Pneumatophore_density_pneu_m2",
                "Timescale", "Season", "Year",
                "CO2_mean", "CO2_se", "CO2_mdn", "CO2_range", "CO2_units",
                "CH4_mean", "CH4_se", "CH4_mdn", "CH4_range", "CH4_units",
                "N2O_mean", "N2O_se", "N2O_mdn", "N2O_range", "N2O_units",
                "Comments")
cat("  All present:", all(julio_cols %in% names(julio_export)), "\n")
missing <- setdiff(julio_cols, names(julio_export))
if (length(missing) > 0) cat("  MISSING:", paste(missing, collapse = ", "), "\n")

cat("\n\nNOTES FOR JULIO:\n")
cat("- QC approach: ALL measurements retained; QC flags provided for transparency\n")
cat("  (following Gewirtzman et al. ch4-data-filtering methodology)\n")
cat("- n_below_MDF: measurements where |flux| <= minimal detectable flux (manufacturer precision)\n")
cat("  These are near the instrument detection limit but are NOT excluded\n")
cat("- n_flagged: measurements with goFlux quality flags (SE, AICc, g-factor, intercept issues)\n")
cat("  These indicate model-fit concerns but are NOT excluded\n")
cat("- median_SNR: median signal-to-noise ratio (|flux| / SE) across individual measurements\n")
cat("  Higher = better; SNR < 2 indicates group is near detection limit\n")
cat("- Both mean and median are reported; large divergence indicates skewed distributions\n")
cat("- Lenticels: presence/absence recorded, not density (lent/cm2)\n")
cat("- No N2O measurements\n")
cat("- Pneumatophore counts are per ~30cm-radius soil chamber, not per m2\n")
cat("- Height is mean chamber height above water/sediment surface (cm)\n")
cat("- All measurements used closed transparent/translucent chambers (light conditions)\n")
cat("- Soil pneumatophore data available for dry season only (counts not recorded in wet season)\n")

cat("\n=== DONE ===\n")
