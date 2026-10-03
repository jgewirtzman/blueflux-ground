# =============================================================================
# Porewater alkalinity and sulfate drawdown (October 2025 profiles; 0-90 cm).
# Expected sulfate from salinity (seawater 2712 mg/L at S = 35, conservative
# mixing with a sulfate-free freshwater end-member); deficit = expected -
# observed (mM, % of expected). Excess TA over the seawater-scaled value
# (2.3 mM at S = 35); sulfate-reduction stoichiometry gives 2 TA per SO4
# reduced, so excess TA / (2 x SO4 deficit) > 1 means alkalinity beyond
# sulfate reduction. Writes output/analysis/porewater_carbonate_summary.csv.
# =============================================================================
suppressMessages(library(dplyr))
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
d <- read.csv("output/data_products/porewater_all_parameters.csv") %>%
  filter(Depth_cm != "Surface", !is.na(SO4_ppm), !is.na(PSU)) %>%
  mutate(SO4_exp = 2712 * PSU / 35, SO4_def_mM = (SO4_exp - SO4_ppm) / 96.06,
         SO4_def_pct = 100 * (SO4_exp - SO4_ppm) / SO4_exp, SO4_mM = SO4_ppm / 96.06,
         TA_mM = Alkalinity_uM / 1000, TA_x_seawater = TA_mM / 2.3,
         TA_excess_mM = TA_mM - 2.3 * PSU / 35, TA_excess_per_SR = TA_excess_mM / (2 * SO4_def_mM))
out <- d %>% group_by(Site) %>%
  summarise(n = n(), TA_mM = median(TA_mM), TA_x_seawater = median(TA_x_seawater), SO4_mM = median(SO4_mM),
            SO4_def_pct_median = median(SO4_def_pct), SO4_def_pct_max = max(SO4_def_pct),
            TA_excess_mM = median(TA_excess_mM), SO4_def_mM = median(SO4_def_mM),
            TA_excess_per_SR = median(TA_excess_per_SR), .groups = "drop")
dir.create("output/analysis", showWarnings = FALSE)
write.csv(out, "output/analysis/porewater_carbonate_summary.csv", row.names = FALSE)
print(as.data.frame(out %>% mutate(across(where(is.double), ~ signif(.x, 3)))), row.names = FALSE)
