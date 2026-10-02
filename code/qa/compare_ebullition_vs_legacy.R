# =============================================================================
# QA: stage-04 floating-chamber water CH4 against the legacy pipeline.
#   1. per placement: stage-04 diffusive / ebullitive / total beside the legacy
#      total and the stage-03 window fit;
#   2. per site x campaign: n and mean water CH4, legacy vs stage 04 (the legacy
#      set includes its 40 "added" ebullition placements and the time slices of
#      the long runs).
# Writes output/qa/ebullition_vs_legacy_placements.csv and
# output/qa/ebullition_vs_legacy_site_campaign.csv.
# =============================================================================
suppressMessages({library(dplyr); library(readr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

new <- read_csv("output/data_products/flux_measurements_all.csv", show_col_types = FALSE, guess_max = 5000)
leg <- read_csv("output/qa/baseline/output__data_products__combined_gas_flux_dataset.csv", show_col_types = FALSE, guess_max = 5000)
camp <- function(d) recode(format(as.Date(d), "%Y-%m"), "2022-10" = "Oct 2022", "2023-03" = "Mar 2023", "2022-03" = "Mar 2022")

pl <- new %>% filter(component == "water") %>%
  transmute(flux_id, plot, campaign = camp(date), analyzer = analyzer_source, use_in_analysis, exclusion_reason,
            placement_duration_min = round(placement_duration_s / 60, 1), diffusive_rule,
            CH4_diffusive_flux, CH4_ebull_flux, CH4_n_ebull_events, CH4_total = CH4_best.flux,
            CH4_stage03_best.flux, legacy_CH4_best.flux, ebullition_flag)
write_csv(pl, "output/qa/ebullition_vs_legacy_placements.csv")

sc <- full_join(
  leg %>% filter(component == "water") %>% group_by(plot, campaign = camp(date)) %>%
    summarise(n_legacy = n(), legacy_mean = mean(CH4_best.flux, na.rm = TRUE),
              legacy_ebull_share = sum(CH4_ebull_flux, na.rm = TRUE) / sum(CH4_best.flux, na.rm = TRUE), .groups = "drop"),
  new %>% filter(component == "water", use_in_analysis) %>% group_by(plot, campaign = camp(date)) %>%
    summarise(n_stage04 = n(), stage04_mean = mean(CH4_best.flux), stage04_diffusive_mean = mean(CH4_diffusive_flux, na.rm = TRUE),
              stage04_ebull_share = sum(CH4_ebull_flux) / sum(CH4_best.flux), .groups = "drop"),
  by = c("plot", "campaign")) %>% arrange(campaign, plot)
write_csv(sc, "output/qa/ebullition_vs_legacy_site_campaign.csv")
print(as.data.frame(sc %>% mutate(across(where(is.double), ~ round(.x, 2)))), row.names = FALSE)
cat("\nPlacements with |stage 04 - legacy| > 25 % of legacy:\n")
print(as.data.frame(pl %>% filter(use_in_analysis, !is.na(legacy_CH4_best.flux),
                                  abs(CH4_total - legacy_CH4_best.flux) > 0.25 * abs(legacy_CH4_best.flux)) %>%
                      transmute(flux_id, min = placement_duration_min, diff = round(CH4_diffusive_flux, 2),
                                ebull = round(CH4_ebull_flux, 2), total = round(CH4_total, 2),
                                legacy = round(legacy_CH4_best.flux, 2), stage03 = round(CH4_stage03_best.flux, 2))), row.names = FALSE)
