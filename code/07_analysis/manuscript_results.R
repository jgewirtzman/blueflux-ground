# =============================================================================
# Manuscript Results: Numerical summaries for all manuscript sections
# =============================================================================
# Generates a text file (output/manuscript_results.txt) with all key statistics
# referenced in the manuscript. Each section maps to a Results subsection.
#
# Run from the blueflux-ground project root.
# =============================================================================

library(dplyr)
library(readr)
library(lme4)
library(lmerTest)
library(emmeans)
library(boot)
library(tidyr)

# --- Output sink -------------------------------------------------------------
sink("output/manuscript_results.txt", split = TRUE)
cat("Manuscript Results — generated", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n")

# --- Helper: bootstrapped mean + 95% CI -------------------------------------
boot_mean_ci <- function(x, R = 5000, conf = 0.95) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) < 3) return(data.frame(y = mean(x), ymin = NA, ymax = NA))
  b <- boot::boot(x, function(d, i) mean(d[i]), R = R)
  ci <- boot::boot.ci(b, conf = conf, type = "perc")
  data.frame(y = mean(x), ymin = ci$percent[4], ymax = ci$percent[5])
}

# --- Helper: format a boot_mean_ci result ------------------------------------
fmt_boot <- function(bci, digits = 3) {
  sprintf("%.${digits}f (95%% CI: %.${digits}f–%.${digits}f)" |>
            gsub("\\$\\{digits\\}", as.character(digits), x = _),
          bci$y, bci$ymin, bci$ymax)
}
# Simpler formatter
fmt_ci <- function(mean_val, lo, hi, digits = 3) {
  sprintf("%.*f (95%% CI: %.*f–%.*f)", digits, mean_val, digits, lo, digits, hi)
}

# =============================================================================
# LOAD DATA
# =============================================================================
cat("\n================================================================\n")
cat("DATA LOADING\n")
cat("================================================================\n\n")

df <- read_csv("output/combined_gas_flux_dataset.csv", show_col_types = FALSE)

# Main study plots
selected_plots <- c("BL60", "CP40", "FLM30", "MI", "SE1", "SRS5", "SRS6", "RB10")
core_sites <- c("BL60", "CP40", "FLM30", "SRS5", "SRS6")

df <- df %>%
  filter(plot %in% selected_plots) %>%
  filter(is.na(CO2_best.flux) | CO2_best.flux >= -10) %>%
  mutate(
    disturbance_level = factor(disturbance_level,
                               levels = c("healthy", "regenerating", "ghost", "scrub")),
    season_agg = factor(
      case_when(
        season == "wet" ~ "Wet",
        season == "dry" ~ "Dry",
        TRUE ~ NA_character_
      ),
      levels = c("Wet", "Dry")
    ),
    component = tolower(component)
  )

cat("  Loaded:", nrow(df), "observations after filtering\n")

# Also load flux statistics table
flux_stats <- read_csv("output/flux_statistics_table.csv", show_col_types = FALSE)
cat("  Flux statistics table:", nrow(flux_stats), "rows\n")


# =============================================================================
# STUDY OVERVIEW
# =============================================================================
cat("\n================================================================\n")
cat("STUDY OVERVIEW\n")
cat("================================================================\n\n")

cat("  Total observations: ", nrow(df), "\n")
cat("\n  Observations by component:\n")
comp_counts <- df %>% count(component) %>% arrange(desc(n))
for (i in seq_len(nrow(comp_counts))) {
  cat(sprintf("    %-20s n = %d\n", comp_counts$component[i], comp_counts$n[i]))
}

cat("\n  Observations by site:\n")
site_counts <- df %>% count(plot) %>% arrange(plot)
for (i in seq_len(nrow(site_counts))) {
  cat(sprintf("    %-10s n = %d\n", site_counts$plot[i], site_counts$n[i]))
}

cat("\n  Observations by season:\n")
season_counts <- df %>% count(season_agg) %>% arrange(season_agg)
for (i in seq_len(nrow(season_counts))) {
  cat(sprintf("    %-10s n = %d\n", season_counts$season_agg[i], season_counts$n[i]))
}

cat("\n  Observations by disturbance level:\n")
dist_counts <- df %>% count(disturbance_level) %>% arrange(disturbance_level)
for (i in seq_len(nrow(dist_counts))) {
  cat(sprintf("    %-20s n = %d\n", dist_counts$disturbance_level[i], dist_counts$n[i]))
}

# Species
species_valid <- df %>%
  filter(!is.na(species), species != "UNKN", species != "") %>%
  pull(species) %>%
  unique()
cat("\n  Number of species (excl. NA/UNKN/blank):", length(species_valid), "\n")
cat("    Species:", paste(sort(species_valid), collapse = ", "), "\n")

# Live vs dead
cat("\n  Live vs dead stems (from status column):\n")
stem_status <- df %>%
  filter(component == "stem", !is.na(status), status != "CWD") %>%
  mutate(alive = ifelse(status %in% c("alive", "Alive"), "Alive", "Dead")) %>%
  count(alive)
for (i in seq_len(nrow(stem_status))) {
  cat(sprintf("    %-10s n = %d\n", stem_status$alive[i], stem_status$n[i]))
}

# Core sites
cat("\n  Core sites (both seasons):", paste(core_sites, collapse = ", "), "\n")

# Date ranges per campaign
cat("\n  Date ranges per campaign:\n")
campaign_dates <- df %>%
  group_by(season_agg) %>%
  summarise(
    first_date = min(as.Date(date), na.rm = TRUE),
    last_date  = max(as.Date(date), na.rm = TRUE),
    n_days     = as.numeric(difftime(max(as.Date(date), na.rm = TRUE),
                                     min(as.Date(date), na.rm = TRUE), units = "days")) + 1,
    .groups = "drop"
  )
for (i in seq_len(nrow(campaign_dates))) {
  cat(sprintf("    %-5s: %s to %s (%d days)\n",
              campaign_dates$season_agg[i],
              campaign_dates$first_date[i],
              campaign_dates$last_date[i],
              campaign_dates$n_days[i]))
}


# =============================================================================
# R.1: AIRBORNE / TOWER (PLACEHOLDERS)
# =============================================================================
cat("\n================================================================\n")
cat("R.1: AIRBORNE / TOWER FLUXES\n")
cat("================================================================\n\n")

cat("  PLACEHOLDER: CARAFE CH4 by land cover class (awaiting from Erin)\n")
cat("  PLACEHOLDER: CARAFE wet vs dry season distributions (awaiting from Erin)\n")
cat("  PLACEHOLDER: SRS6 tower CH4 reference value\n")


# =============================================================================
# R.2: COMPONENT FLUX RATES (Fig 2)
# =============================================================================
cat("\n================================================================\n")
cat("R.2: COMPONENT FLUX RATES (Fig 2)\n")
cat("================================================================\n\n")

# --- CH4 ---
ch4_valid <- df %>% filter(CH4_flux_status == "valid")
cat("--- CH4 (valid fluxes, n =", nrow(ch4_valid), ") ---\n\n")

# Per component (pooled)
cat("  CH4 by component (pooled, boot mean + 95% CI):\n")
ch4_by_comp <- ch4_valid %>%
  group_by(component) %>%
  summarise(bci = list(boot_mean_ci(CH4_best.flux)),
            n = sum(!is.na(CH4_best.flux)), .groups = "drop") %>%
  mutate(y = sapply(bci, `[[`, "y"),
         ymin = sapply(bci, `[[`, "ymin"),
         ymax = sapply(bci, `[[`, "ymax"))
for (i in seq_len(nrow(ch4_by_comp))) {
  cat(sprintf("    %-18s %s, n = %d\n",
              ch4_by_comp$component[i],
              fmt_ci(ch4_by_comp$y[i], ch4_by_comp$ymin[i], ch4_by_comp$ymax[i]),
              ch4_by_comp$n[i]))
}

# Per component x disturbance
cat("\n  CH4 by component x disturbance:\n")
ch4_comp_dist <- ch4_valid %>%
  filter(!is.na(disturbance_level)) %>%
  group_by(component, disturbance_level) %>%
  summarise(bci = list(boot_mean_ci(CH4_best.flux)),
            n = sum(!is.na(CH4_best.flux)), .groups = "drop") %>%
  mutate(y = sapply(bci, `[[`, "y"),
         ymin = sapply(bci, `[[`, "ymin"),
         ymax = sapply(bci, `[[`, "ymax"))
for (i in seq_len(nrow(ch4_comp_dist))) {
  cat(sprintf("    %-15s %-15s %s, n = %d\n",
              ch4_comp_dist$component[i],
              as.character(ch4_comp_dist$disturbance_level[i]),
              fmt_ci(ch4_comp_dist$y[i], ch4_comp_dist$ymin[i], ch4_comp_dist$ymax[i]),
              ch4_comp_dist$n[i]))
}

# Per component x season
cat("\n  CH4 by component x season:\n")
ch4_comp_season <- ch4_valid %>%
  filter(!is.na(season_agg)) %>%
  group_by(component, season_agg) %>%
  summarise(bci = list(boot_mean_ci(CH4_best.flux)),
            n = sum(!is.na(CH4_best.flux)), .groups = "drop") %>%
  mutate(y = sapply(bci, `[[`, "y"),
         ymin = sapply(bci, `[[`, "ymin"),
         ymax = sapply(bci, `[[`, "ymax"))
for (i in seq_len(nrow(ch4_comp_season))) {
  cat(sprintf("    %-15s %-6s %s, n = %d\n",
              ch4_comp_season$component[i],
              as.character(ch4_comp_season$season_agg[i]),
              fmt_ci(ch4_comp_season$y[i], ch4_comp_season$ymin[i], ch4_comp_season$ymax[i]),
              ch4_comp_season$n[i]))
}

# --- CO2 ---
co2_valid <- df %>% filter(CO2_flux_status == "valid")
cat("\n--- CO2 (valid fluxes, n =", nrow(co2_valid), ") ---\n\n")

# Per component (pooled)
cat("  CO2 by component (pooled, boot mean + 95% CI):\n")
co2_by_comp <- co2_valid %>%
  group_by(component) %>%
  summarise(bci = list(boot_mean_ci(CO2_best.flux)),
            n = sum(!is.na(CO2_best.flux)), .groups = "drop") %>%
  mutate(y = sapply(bci, `[[`, "y"),
         ymin = sapply(bci, `[[`, "ymin"),
         ymax = sapply(bci, `[[`, "ymax"))
for (i in seq_len(nrow(co2_by_comp))) {
  cat(sprintf("    %-18s %s, n = %d\n",
              co2_by_comp$component[i],
              fmt_ci(co2_by_comp$y[i], co2_by_comp$ymin[i], co2_by_comp$ymax[i]),
              co2_by_comp$n[i]))
}

# Per component x disturbance
cat("\n  CO2 by component x disturbance:\n")
co2_comp_dist <- co2_valid %>%
  filter(!is.na(disturbance_level)) %>%
  group_by(component, disturbance_level) %>%
  summarise(bci = list(boot_mean_ci(CO2_best.flux)),
            n = sum(!is.na(CO2_best.flux)), .groups = "drop") %>%
  mutate(y = sapply(bci, `[[`, "y"),
         ymin = sapply(bci, `[[`, "ymin"),
         ymax = sapply(bci, `[[`, "ymax"))
for (i in seq_len(nrow(co2_comp_dist))) {
  cat(sprintf("    %-15s %-15s %s, n = %d\n",
              co2_comp_dist$component[i],
              as.character(co2_comp_dist$disturbance_level[i]),
              fmt_ci(co2_comp_dist$y[i], co2_comp_dist$ymin[i], co2_comp_dist$ymax[i]),
              co2_comp_dist$n[i]))
}

# Per component x season
cat("\n  CO2 by component x season:\n")
co2_comp_season <- co2_valid %>%
  filter(!is.na(season_agg)) %>%
  group_by(component, season_agg) %>%
  summarise(bci = list(boot_mean_ci(CO2_best.flux)),
            n = sum(!is.na(CO2_best.flux)), .groups = "drop") %>%
  mutate(y = sapply(bci, `[[`, "y"),
         ymin = sapply(bci, `[[`, "ymin"),
         ymax = sapply(bci, `[[`, "ymax"))
for (i in seq_len(nrow(co2_comp_season))) {
  cat(sprintf("    %-15s %-6s %s, n = %d\n",
              co2_comp_season$component[i],
              as.character(co2_comp_season$season_agg[i]),
              fmt_ci(co2_comp_season$y[i], co2_comp_season$ymin[i], co2_comp_season$ymax[i]),
              co2_comp_season$n[i]))
}

# --- Dead vs alive stem CH4 ratio ---
cat("\n  Dead vs alive stem CH4 flux ratio (boot CI):\n")
stem_ch4 <- ch4_valid %>%
  filter(component == "stem", !is.na(status), status != "CWD") %>%
  mutate(alive = ifelse(status %in% c("alive", "Alive"), "Alive", "Dead"))

dead_flux <- stem_ch4 %>% filter(alive == "Dead") %>% pull(CH4_best.flux)
alive_flux <- stem_ch4 %>% filter(alive == "Alive") %>% pull(CH4_best.flux)

dead_bci <- boot_mean_ci(dead_flux)
alive_bci <- boot_mean_ci(alive_flux)
cat(sprintf("    Dead stems:  %s, n = %d\n",
            fmt_ci(dead_bci$y, dead_bci$ymin, dead_bci$ymax), length(dead_flux)))
cat(sprintf("    Alive stems: %s, n = %d\n",
            fmt_ci(alive_bci$y, alive_bci$ymin, alive_bci$ymax), length(alive_flux)))
if (alive_bci$y != 0) {
  cat(sprintf("    Dead/Alive ratio: %.2f\n", dead_bci$y / alive_bci$y))
}

# --- % negative by component ---
cat("\n  Percent negative fluxes by component:\n")
cat("  CH4:\n")
ch4_neg <- ch4_valid %>%
  group_by(component) %>%
  summarise(n_total = n(),
            n_neg = sum(CH4_best.flux < 0, na.rm = TRUE),
            pct_neg = round(100 * n_neg / n_total, 1), .groups = "drop")
for (i in seq_len(nrow(ch4_neg))) {
  cat(sprintf("    %-18s %5.1f%% negative (%d / %d)\n",
              ch4_neg$component[i], ch4_neg$pct_neg[i], ch4_neg$n_neg[i], ch4_neg$n_total[i]))
}
cat("  CO2:\n")
co2_neg <- co2_valid %>%
  group_by(component) %>%
  summarise(n_total = n(),
            n_neg = sum(CO2_best.flux < 0, na.rm = TRUE),
            pct_neg = round(100 * n_neg / n_total, 1), .groups = "drop")
for (i in seq_len(nrow(co2_neg))) {
  cat(sprintf("    %-18s %5.1f%% negative (%d / %d)\n",
              co2_neg$component[i], co2_neg$pct_neg[i], co2_neg$n_neg[i], co2_neg$n_total[i]))
}


# --- R.2b: Ebullition subsection ---
cat("\n----------------------------------------------------------------\n")
cat("R.2b: EBULLITION\n")
cat("----------------------------------------------------------------\n\n")

ebull_site <- read_csv("output/ebullition/site_season_ebullition.csv", show_col_types = FALSE)
ebull_part <- read_csv("output/ebullition/partitioned_fluxes.csv", show_col_types = FALSE)

cat("  Site x season ebullition summary:\n")
for (i in seq_len(nrow(ebull_site))) {
  cat(sprintf("    %-6s %-5s: %d traces, %d with ebullition (%.1f%%)\n",
              ebull_site$site[i], ebull_site$season[i],
              ebull_site$n_traces[i], ebull_site$n_with_ebull[i],
              ebull_site$pct_with_ebull[i]))
}

cat(sprintf("\n  Total traces analyzed: %d\n", sum(ebull_site$n_traces)))
cat(sprintf("  Total with ebullition: %d\n", sum(ebull_site$n_with_ebull)))
cat(sprintf("  Overall %% with ebullition: %.1f%%\n",
            100 * sum(ebull_site$n_with_ebull) / sum(ebull_site$n_traces)))

# Partitioned fluxes: mean diffusive vs ebullitive
cat("\n  Partitioned flux rates (from individual traces):\n")
cat(sprintf("    Total traces in partitioned file: %d\n", nrow(ebull_part)))

has_ebull <- ebull_part %>% filter(n_jumps > 0)
cat(sprintf("    Traces with ebullition events: %d\n", nrow(has_ebull)))

if (nrow(has_ebull) > 0) {
  cat(sprintf("    Mean diffusive flux (nmol/m2/s): %.3f\n",
              mean(has_ebull$diffusive_flux_nmol, na.rm = TRUE)))
  cat(sprintf("    Mean ebullitive flux (nmol/m2/s): %.3f\n",
              mean(has_ebull$ebull_flux_nmol, na.rm = TRUE)))
  cat(sprintf("    Mean total flux (nmol/m2/s): %.3f\n",
              mean(has_ebull$total_flux_nmol, na.rm = TRUE)))
  cat(sprintf("    Mean ebullitive fraction: %.1f%%\n",
              100 * mean(has_ebull$ebullitive_fraction, na.rm = TRUE)))
}

# Wet-only ebullitive %
ebull_wet <- ebull_site %>% filter(season == "wet")
if (nrow(ebull_wet) > 0) {
  cat(sprintf("\n  Wet season %% ebullitive (site-level mean): %.1f%%\n",
              mean(ebull_wet$pct_CH4_ebullitive, na.rm = TRUE)))
}

cat("\n  Note: 0 ebullition events detected in dry season\n")


# =============================================================================
# R.3: STEM HEIGHT & SPECIES (Fig 3)
# =============================================================================
cat("\n================================================================\n")
cat("R.3: STEM HEIGHT & SPECIES (Fig 3)\n")
cat("================================================================\n\n")

# --- Prepare stem data (replicating publication_figures.R lines 1164-1249) ---
shared_sites <- c("BL60", "SRS5", "SRS6")

stem_species <- df %>%
  filter(component == "stem",
         plot %in% shared_sites,
         !is.na(species), species != "UNKN", species != "",
         !is.na(CH4_best.flux),
         !is.na(height_corrected),
         height_corrected >= 0) %>%
  mutate(
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
    y_ch4 = asinh(CH4_best.flux),
    season_agg = factor(season_agg, levels = c("Wet", "Dry")),
    plot = factor(plot)
  )

# Keep species with >= 5 observations
sp_counts <- table(stem_species$species)
keep_spp <- names(sp_counts[sp_counts >= 5])
stem_species <- stem_species %>%
  filter(species %in% keep_spp) %>%
  mutate(species = droplevels(species))

cat("Species analysis (shared sites: BL60, SRS5, SRS6)\n")
cat("n per species:\n")
print(table(stem_species$species))
cat("\nSpecies x site:\n")
print(table(stem_species$species, stem_species$plot))

# --- CH4 Model 1: Overall (height as continuous covariate) ---
cat("\n--- CH4 Model 1: species + height + season + (1|plot) ---\n")
m_ch4_overall <- lmer(y_ch4 ~ species + height_corrected + season_agg + (1 | plot),
                      data = stem_species)

cat("\nType III ANOVA:\n")
print(anova(m_ch4_overall, type = 3))

emm_ch4_overall <- emmeans(m_ch4_overall, "species")
cat("\nEmmeans for species:\n")
print(summary(emm_ch4_overall))

cat("\nPairwise contrasts:\n")
print(as.data.frame(pairs(emm_ch4_overall)))

# Height coefficient
coefs <- summary(m_ch4_overall)$coefficients
ht_row <- which(rownames(coefs) == "height_corrected")
if (length(ht_row) == 1) {
  ci_ht <- confint(m_ch4_overall, parm = "height_corrected", method = "Wald")
  cat(sprintf("\n  Height coefficient: %.5f +/- %.5f (95%% CI: %.5f–%.5f)\n",
              coefs[ht_row, "Estimate"], coefs[ht_row, "Std. Error"],
              ci_ht[1], ci_ht[2]))
}

# --- CH4 Model 2: Species x height_cat (0-150 cm) ---
cat("\n--- CH4 Model 2: species * height_cat + season + (1|plot) [0-150 cm] ---\n")
stem_ht <- stem_species %>%
  filter(height_cat %in% c("0-50 cm", "50-100 cm", "100-150 cm")) %>%
  mutate(height_cat = droplevels(height_cat))

m_ch4_byht <- lmer(y_ch4 ~ species * height_cat + season_agg + (1 | plot),
                    data = stem_ht)

cat("\nType III ANOVA:\n")
print(anova(m_ch4_byht, type = 3))

emm_ch4_byht <- emmeans(m_ch4_byht, ~ species | height_cat)
cat("\nEmmeans by species | height_cat:\n")
print(summary(emm_ch4_byht))

# --- CH4 Alive/Dead model ---
cat("\n--- CH4 Alive/Dead Model (AVGE + RHMA at shared sites) ---\n")
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

cat("\nn per sp_status x height_cat:\n")
print(table(stem_ad_ht$sp_status, stem_ad_ht$height_cat))

m_ch4_ad <- lmer(y_ch4 ~ sp_status + height_cat + season_agg + (1 | plot),
                 data = stem_ad_ht)

cat("\nType III ANOVA:\n")
print(anova(m_ch4_ad, type = 3))

emm_ch4_ad <- emmeans(m_ch4_ad, ~ sp_status)
cat("\nEmmeans for sp_status:\n")
print(summary(emm_ch4_ad))

cat("\nAlive vs Dead contrasts:\n")
# Custom contrasts: alive vs dead for AVGE and RHMA
contrast_list <- list(
  "AVGE alive-dead" = c(1, -1, 0, 0, 0, 0),
  "RHMA alive-dead" = c(0, 0, 0, 0, 1, -1)
)
# Get the order of levels to set contrasts correctly
emm_levels <- levels(as.factor(summary(emm_ch4_ad)$sp_status))
cat("  emmeans levels:", paste(emm_levels, collapse = ", "), "\n")

pairs_ch4_ad <- pairs(emm_ch4_ad)
cat("\nAll pairwise contrasts:\n")
print(as.data.frame(pairs_ch4_ad))


# ---- CO2 models (same structure) ----
cat("\n--- CO2 Model 1: species + height + season + (1|plot) ---\n")
stem_species_co2 <- df %>%
  filter(component == "stem",
         plot %in% shared_sites,
         !is.na(species), species != "UNKN", species != "",
         !is.na(CO2_best.flux),
         !is.na(height_corrected),
         height_corrected >= 0) %>%
  mutate(
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
    y_co2 = asinh(CO2_best.flux),
    season_agg = factor(season_agg, levels = c("Wet", "Dry")),
    plot = factor(plot)
  )

sp_counts_co2 <- table(stem_species_co2$species)
keep_spp_co2 <- names(sp_counts_co2[sp_counts_co2 >= 5])
stem_species_co2 <- stem_species_co2 %>%
  filter(species %in% keep_spp_co2) %>%
  mutate(species = droplevels(species))

m_co2_overall <- lmer(y_co2 ~ species + height_corrected + season_agg + (1 | plot),
                      data = stem_species_co2)

cat("\nType III ANOVA:\n")
print(anova(m_co2_overall, type = 3))

emm_co2_overall <- emmeans(m_co2_overall, "species")
cat("\nEmmeans for species (CO2):\n")
print(summary(emm_co2_overall))

cat("\nPairwise contrasts (CO2):\n")
print(as.data.frame(pairs(emm_co2_overall)))

coefs_co2 <- summary(m_co2_overall)$coefficients
ht_row_co2 <- which(rownames(coefs_co2) == "height_corrected")
if (length(ht_row_co2) == 1) {
  ci_ht_co2 <- confint(m_co2_overall, parm = "height_corrected", method = "Wald")
  cat(sprintf("\n  Height coefficient (CO2): %.5f +/- %.5f (95%% CI: %.5f–%.5f)\n",
              coefs_co2[ht_row_co2, "Estimate"], coefs_co2[ht_row_co2, "Std. Error"],
              ci_ht_co2[1], ci_ht_co2[2]))
}

# --- CO2 Model 2: Species x height_cat (0-150 cm) ---
cat("\n--- CO2 Model 2: species * height_cat + season + (1|plot) [0-150 cm] ---\n")
stem_ht_co2 <- stem_species_co2 %>%
  filter(height_cat %in% c("0-50 cm", "50-100 cm", "100-150 cm")) %>%
  mutate(height_cat = droplevels(height_cat))

m_co2_byht <- lmer(y_co2 ~ species * height_cat + season_agg + (1 | plot),
                    data = stem_ht_co2)

cat("\nType III ANOVA:\n")
print(anova(m_co2_byht, type = 3))

emm_co2_byht <- emmeans(m_co2_byht, ~ species | height_cat)
cat("\nEmmeans by species | height_cat (CO2):\n")
print(summary(emm_co2_byht))

# --- CO2 Alive/Dead model ---
cat("\n--- CO2 Alive/Dead Model (AVGE + RHMA) ---\n")

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

cat("\nn per sp_status x height_cat (CO2):\n")
print(table(stem_ad_ht_co2$sp_status, stem_ad_ht_co2$height_cat))

m_co2_ad <- lmer(y_co2 ~ sp_status + height_cat + season_agg + (1 | plot),
                 data = stem_ad_ht_co2)

cat("\nType III ANOVA (CO2):\n")
print(anova(m_co2_ad, type = 3))

emm_co2_ad <- emmeans(m_co2_ad, ~ sp_status)
cat("\nEmmeans for sp_status (CO2):\n")
print(summary(emm_co2_ad))

cat("\nAll pairwise contrasts (CO2 alive/dead):\n")
print(as.data.frame(pairs(emm_co2_ad)))


# =============================================================================
# R.4-R.5: TLS AND BOTTOM-UP / TOP-DOWN (PLACEHOLDERS)
# =============================================================================
cat("\n================================================================\n")
cat("R.4: TLS STEM SURFACE AREA\n")
cat("================================================================\n\n")

cat("  PLACEHOLDER: TLS-derived stem surface area by species and height (from Lizzy)\n")
cat("  PLACEHOLDER: Stand-level stem surface area scaling\n")

cat("\n================================================================\n")
cat("R.5: BOTTOM-UP / TOP-DOWN COMPARISON\n")
cat("================================================================\n\n")

cat("  PLACEHOLDER: Bottom-up ecosystem CH4 budget (TLS surface areas x flux rates)\n")
cat("  PLACEHOLDER: Top-down CARAFE comparison\n")
cat("  PLACEHOLDER: Net ecosystem exchange partitioning\n")


# =============================================================================
# R.6: POREWATER GEOCHEMISTRY (Fig 6)
# =============================================================================
cat("\n================================================================\n")
cat("R.6: POREWATER GEOCHEMISTRY (Fig 6)\n")
cat("================================================================\n\n")

pw <- read_csv("/Users/jongewirtzman/My Drive/Research/Blueflux/microbes/merged_porewater_all_parameters.csv",
               show_col_types = FALSE)

# Apply DO correction
pw <- pw %>%
  mutate(ppmDO = ppmDO - 1.67)
cat("  Applied DO correction: -1.67 mg/L\n")

# Add disturbance classification, Site factor, Depth_numeric
pw <- pw %>%
  mutate(
    disturbance = case_when(
      Site %in% c("SRS5", "SRS6") ~ "healthy",
      Site == "BL60" ~ "regenerating",
      Site == "CP40" ~ "ghost"
    ),
    disturbance = factor(disturbance, levels = c("healthy", "regenerating", "ghost")),
    Site = factor(Site, levels = c("SRS5", "SRS6", "BL60", "CP40")),
    Depth_numeric = case_when(
      Depth_cm == "Surface" ~ -5,
      Depth_cm == "0" ~ 0,
      Depth_cm == "15" ~ 15,
      Depth_cm == "45" ~ 45,
      Depth_cm == "90" ~ 90
    )
  )

cat("  Loaded:", nrow(pw), "porewater rows,", n_distinct(pw$Site), "sites\n\n")

# Mean +/- SD by site x depth for key variables
pw_vars <- c("CH4_mean_uM", "PSU", "Sulfide", "ORP", "ppmDO", "DOC_mg_L")

cat("  Mean +/- SD by Site x Depth:\n")
for (v in pw_vars) {
  cat(sprintf("\n  --- %s ---\n", v))
  pw_summary <- pw %>%
    group_by(Site, Depth_cm) %>%
    summarise(
      mean_val = mean(.data[[v]], na.rm = TRUE),
      sd_val   = sd(.data[[v]], na.rm = TRUE),
      n        = sum(!is.na(.data[[v]])),
      .groups = "drop"
    ) %>%
    arrange(Site, Depth_cm)
  for (j in seq_len(nrow(pw_summary))) {
    cat(sprintf("    %-6s %-10s %.2f +/- %.2f (n=%d)\n",
                as.character(pw_summary$Site[j]),
                as.character(pw_summary$Depth_cm[j]),
                pw_summary$mean_val[j],
                ifelse(is.na(pw_summary$sd_val[j]), 0, pw_summary$sd_val[j]),
                pw_summary$n[j]))
  }
}

# --- PCA ---
cat("\n--- Porewater PCA ---\n")

# Remove specified columns (same as soilprofile script)
remove_cols <- c("Lat", "Long", "Depth_numeric", "SpCond", "TempC",
                 "n_replicates", "Tds ppt", "%DO", "Br_ppm", "F_ppm")
sd_cols <- names(pw)[grepl("_sd$|_sd_", names(pw))]
co2_cols <- names(pw)[grepl("CO2", names(pw))]

pca_vars <- pw %>%
  select(where(is.numeric)) %>%
  select(-any_of(c(remove_cols, sd_cols, co2_cols)))

# Keep vars with <= 20% missing
keep_vars <- names(pca_vars)[colMeans(is.na(pca_vars)) <= 0.20]

pca_df <- pw %>%
  select(Site, Depth_cm, Depth_numeric, disturbance, all_of(keep_vars)) %>%
  drop_na()

cat(sprintf("  PCA variables (%d): %s\n", length(keep_vars), paste(keep_vars, collapse = ", ")))
cat(sprintf("  Complete cases for PCA: %d (of %d)\n", nrow(pca_df), nrow(pw)))

if (nrow(pca_df) >= 3 && length(keep_vars) >= 2) {
  pca_mat <- pca_df %>% select(all_of(keep_vars))
  pca_res <- prcomp(pca_mat, center = TRUE, scale. = TRUE)

  var_exp <- summary(pca_res)$importance[2, ] * 100

  cat(sprintf("\n  Variance explained:\n"))
  cat(sprintf("    PC1: %.1f%%\n", var_exp[1]))
  cat(sprintf("    PC2: %.1f%%\n", var_exp[2]))
  cat(sprintf("    PC3: %.1f%%\n", var_exp[3]))
  cat(sprintf("    Cumulative PC1-PC3: %.1f%%\n", sum(var_exp[1:3])))

  cat("\n  Loadings (PC1, PC2):\n")
  loadings_df <- data.frame(
    variable = rownames(pca_res$rotation),
    PC1 = pca_res$rotation[, 1],
    PC2 = pca_res$rotation[, 2]
  )
  loadings_df <- loadings_df %>% arrange(desc(abs(PC1)))
  for (j in seq_len(nrow(loadings_df))) {
    cat(sprintf("    %-20s PC1 = %+.3f   PC2 = %+.3f\n",
                loadings_df$variable[j], loadings_df$PC1[j], loadings_df$PC2[j]))
  }
} else {
  cat("  Skipping PCA: insufficient complete cases\n")
}


# =============================================================================
# R.7: NET CO2-EQUIVALENT FORCING
# =============================================================================
cat("\n================================================================\n")
cat("R.7: NET CO2-EQUIVALENT FORCING\n")
cat("================================================================\n\n")

# --- CH4: mean flux by disturbance class (all components pooled) ---
ch4_by_dist <- ch4_valid %>%
  filter(!is.na(disturbance_level)) %>%
  group_by(disturbance_level) %>%
  summarise(
    mean_nmol = mean(CH4_best.flux, na.rm = TRUE),
    n = sum(!is.na(CH4_best.flux)),
    .groups = "drop"
  ) %>%
  mutate(
    # Convert nmol/m2/s -> g CH4/m2/yr -> Mg CH4/ha/yr
    # nmol/m2/s * 3600 * 24 * 365 / 1e9 * 16 (g/mol CH4) = g CH4/m2/yr
    g_per_m2_yr = mean_nmol * 3600 * 24 * 365 / 1e9 * 16,
    Mg_per_ha_yr = g_per_m2_yr * 1e4 / 1e6,
    # GWP-weighted
    CO2eq_GWP100_g = g_per_m2_yr * 27.9,
    CO2eq_GWP20_g  = g_per_m2_yr * 81.2,
    CO2eq_GWP100_Mg_ha = Mg_per_ha_yr * 27.9,
    CO2eq_GWP20_Mg_ha  = Mg_per_ha_yr * 81.2
  )

cat("  CH4 flux by disturbance class:\n")
cat("  (all components pooled, valid CH4 only)\n\n")
cat(sprintf("  %-15s %10s %10s %12s %12s %12s\n",
            "Disturbance", "nmol/m2/s", "g/m2/yr", "Mg CH4/ha/yr",
            "CO2eq(100)", "CO2eq(20)"))
cat(sprintf("  %-15s %10s %10s %12s %12s %12s\n",
            "", "", "", "", "g/m2/yr", "g/m2/yr"))
for (i in seq_len(nrow(ch4_by_dist))) {
  cat(sprintf("  %-15s %10.3f %10.4f %12.6f %12.3f %12.3f\n",
              as.character(ch4_by_dist$disturbance_level[i]),
              ch4_by_dist$mean_nmol[i],
              ch4_by_dist$g_per_m2_yr[i],
              ch4_by_dist$Mg_per_ha_yr[i],
              ch4_by_dist$CO2eq_GWP100_g[i],
              ch4_by_dist$CO2eq_GWP20_g[i]))
}

# --- CO2: respiration from chambers by disturbance ---
cat("\n  CO2 respiration by disturbance class:\n")
cat("  (chamber-based, all components, valid CO2)\n\n")

# CO2 is in umol/m2/s; convert to g CO2/m2/yr
# umol/m2/s * 3600 * 24 * 365 / 1e6 * 44 = g CO2/m2/yr
co2_by_dist <- co2_valid %>%
  filter(!is.na(disturbance_level)) %>%
  group_by(disturbance_level) %>%
  summarise(
    mean_umol = mean(CO2_best.flux, na.rm = TRUE),
    n = sum(!is.na(CO2_best.flux)),
    .groups = "drop"
  ) %>%
  mutate(
    g_per_m2_yr = mean_umol * 3600 * 24 * 365 / 1e6 * 44,
    Mg_per_ha_yr = g_per_m2_yr * 1e4 / 1e6
  )

cat(sprintf("  %-15s %12s %12s %12s %5s\n",
            "Disturbance", "umol/m2/s", "g CO2/m2/yr", "Mg/ha/yr", "n"))
for (i in seq_len(nrow(co2_by_dist))) {
  cat(sprintf("  %-15s %12.3f %12.1f %12.4f %5d\n",
              as.character(co2_by_dist$disturbance_level[i]),
              co2_by_dist$mean_umol[i],
              co2_by_dist$g_per_m2_yr[i],
              co2_by_dist$Mg_per_ha_yr[i],
              co2_by_dist$n[i]))
}

cat("\n  PLACEHOLDER: SRS6 tower GPP for net CO2 budget\n")

# --- Net CO2eq table ---
cat("\n  Net CO2-equivalent table (chamber respiration + CH4 forcing):\n\n")
net_table <- ch4_by_dist %>%
  select(disturbance_level, ch4_g_m2_yr = g_per_m2_yr,
         ch4_CO2eq100 = CO2eq_GWP100_g, ch4_CO2eq20 = CO2eq_GWP20_g) %>%
  left_join(
    co2_by_dist %>% select(disturbance_level, co2_g_m2_yr = g_per_m2_yr),
    by = "disturbance_level"
  ) %>%
  mutate(
    total_CO2eq100 = co2_g_m2_yr + ch4_CO2eq100,
    total_CO2eq20  = co2_g_m2_yr + ch4_CO2eq20,
    ch4_pct_100 = 100 * ch4_CO2eq100 / total_CO2eq100,
    ch4_pct_20  = 100 * ch4_CO2eq20  / total_CO2eq20
  )

cat(sprintf("  %-15s %12s %12s %12s %12s %8s %8s\n",
            "Disturbance", "CO2 resp", "CH4 CO2eq100", "CH4 CO2eq20",
            "Total(100)", "CH4%100", "CH4%20"))
cat(sprintf("  %-15s %12s %12s %12s %12s %8s %8s\n",
            "", "g/m2/yr", "g/m2/yr", "g/m2/yr", "g/m2/yr", "", ""))
for (i in seq_len(nrow(net_table))) {
  cat(sprintf("  %-15s %12.1f %12.3f %12.3f %12.1f %7.1f%% %7.1f%%\n",
              as.character(net_table$disturbance_level[i]),
              net_table$co2_g_m2_yr[i],
              net_table$ch4_CO2eq100[i],
              net_table$ch4_CO2eq20[i],
              net_table$total_CO2eq100[i],
              net_table$ch4_pct_100[i],
              net_table$ch4_pct_20[i]))
}

cat("\n  Note: CO2 respiration here is chamber-based (soil + stem + pneumatophore etc.).\n")
cat("  Net ecosystem CO2 budget requires tower GPP (PLACEHOLDER).\n")


# =============================================================================
# END
# =============================================================================
cat("\n================================================================\n")
cat("END OF MANUSCRIPT RESULTS\n")
cat("================================================================\n")

sink()
cat("Results saved to: output/manuscript_results.txt\n")
