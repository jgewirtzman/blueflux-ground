# =============================================================================
# QA / handoff step 8: legacy -> rebuild comparison of the analysis datasets.
# Per component x disturbance class x campaign, for CH4 and CO2:
#   n, mean, median (legacy vs rebuild);
#   share below detection (legacy: goFlux datasheet MDF, CH4_below_MDF;
#   rebuild: empirical MDF, 1.96 sigma / t, centred sigma) and share QC-flagged
#   (legacy: CH4_flagged; rebuild: fluxqc qc_any);
#   rebuild detection class counts (emission / uptake / below detection).
# Plus the legacy vs rebuild manuscript_results.txt line diff of numbers.
# Writes output/qa/report_component_class_campaign.csv and
# output/qa/report_manuscript_results_diff.csv.
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(tidyr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

camp <- function(d) recode(format(as.Date(d), "%Y-%m"), "2022-03" = "Mar 2022", "2022-10" = "Oct 2022", "2023-03" = "Mar 2023",
                           .default = format(as.Date(d), "%Y-%m"))
leg <- read_csv("output/qa/baseline/output__data_products__combined_gas_flux_dataset.csv", show_col_types = FALSE, guess_max = 5000) %>%
  mutate(campaign = camp(date), component = tolower(component))
new <- read_csv("output/data_products/combined_gas_flux_dataset.csv", show_col_types = FALSE, guess_max = 5000) %>%
  mutate(campaign = camp(date), component = tolower(component))

summ <- function(d, gas, src, below, flagged, det = NULL) {
  f <- d[[paste0(gas, "_best.flux")]]
  d %>% mutate(.f = f, .b = .data[[below]] %in% TRUE, .q = .data[[flagged]] %in% TRUE,
               .det = if (!is.null(det)) .data[[det]] else NA_character_) %>%
    filter(!is.na(.f)) %>% group_by(component, disturbance_level, campaign) %>%
    summarise(n = n(), mean = mean(.f), median = median(.f), pct_below = 100 * mean(.b), pct_flagged = 100 * mean(.q),
              n_emission = sum(.det == "emission", na.rm = TRUE), n_uptake = sum(.det == "uptake", na.rm = TRUE),
              n_below = sum(.det == "below detection", na.rm = TRUE), .groups = "drop") %>%
    mutate(gas = gas, source = src)
}
tab <- bind_rows(
  summ(leg, "CH4", "legacy", "CH4_below_MDF", "CH4_flagged"), summ(leg, "CO2", "legacy", "CO2_below_MDF", "CO2_flagged"),
  summ(new, "CH4", "rebuild", "CH4_below_MDF", "CH4_flagged", "CH4_det_class_emp"),
  summ(new, "CO2", "rebuild", "CO2_below_MDF", "CO2_flagged", "CO2_det_class_emp"))
wide <- tab %>% select(gas, component, disturbance_level, campaign, source, n, mean, median, pct_below, pct_flagged) %>%
  pivot_wider(names_from = source, values_from = c(n, mean, median, pct_below, pct_flagged)) %>%
  left_join(tab %>% filter(source == "rebuild") %>% select(gas, component, disturbance_level, campaign, n_emission, n_uptake, n_below),
            by = c("gas", "component", "disturbance_level", "campaign")) %>%
  mutate(mean_change_pct = 100 * (mean_rebuild - mean_legacy) / abs(mean_legacy)) %>%
  arrange(gas, component, disturbance_level, campaign)
write_csv(wide, "output/qa/report_component_class_campaign.csv")

# manuscript_results.txt: legacy (frozen baseline) vs rebuild, lines whose numbers differ
lo <- readLines("output/qa/baseline/manuscript__text__manuscript_results.txt")
ln <- readLines("manuscript/text/manuscript_results.txt")
key <- function(x) trimws(gsub("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?", "#", x))
ko <- key(lo); kn <- key(ln)
m <- match(kn, ko)
d <- tibble(line_rebuild = seq_along(ln), text_rebuild = ln, text_legacy = ifelse(is.na(m), NA, lo[m])) %>%
  filter(grepl("[0-9]", text_rebuild), is.na(text_legacy) | text_legacy != text_rebuild, !grepl("generated", text_rebuild))
write_csv(d, "output/qa/report_manuscript_results_diff.csv")

cat("Component x class x campaign rows:", nrow(wide), "| manuscript_results lines that changed:", nrow(d), "\n")
print(as.data.frame(wide %>% filter(gas == "CH4") %>%
  transmute(component, class = disturbance_level, campaign, n = paste(n_legacy, "->", n_rebuild),
            mean = sprintf("%.2f -> %.2f", mean_legacy, mean_rebuild), below = sprintf("%.0f%% -> %.0f%%", pct_below_legacy, pct_below_rebuild),
            flagged = sprintf("%.0f%% -> %.0f%%", pct_flagged_legacy, pct_flagged_rebuild))), row.names = FALSE)
