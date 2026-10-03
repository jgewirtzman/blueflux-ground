# =============================================================================
# QA: why the healthy-class bottom-up CO2 respiration fell between the legacy
# pipeline and the rebuild. Per closure (stems, roots at SRS5/SRS6): legacy vs
# rebuild CO2 flux, with the change attributed to the fit window (saved /
# scripted / trimmed vs legacy), the model (HM -> LM, >= 30 points rule),
# chamber temperature (tower vs handheld) and membership (closures only in one
# set). Writes output/qa/co2_shift_closures.csv.
# =============================================================================
suppressMessages({library(dplyr); library(readr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
new <- read_csv("output/data_products/flux_measurements_all.csv", show_col_types = FALSE, guess_max = 5000)
leg <- read_csv("output/qa/baseline/output__data_products__combined_gas_flux_dataset.csv", show_col_types = FALSE, guess_max = 5000)
camp <- function(d) recode(format(as.Date(d), "%Y-%m"), "2022-10" = "Oct 2022", "2023-03" = "Mar 2023", "2022-03" = "Mar 2022")
x <- full_join(
  leg %>% filter(plot %in% c("SRS5", "SRS6"), component %in% c("stem", "root")) %>%
    transmute(flux_id, plot_l = plot, camp_l = camp(date), comp_l = component, co2_legacy = CO2_best.flux,
              model_legacy = CO2_model, Tcham_legacy = air_temp),
  new %>% filter(plot %in% c("SRS5", "SRS6"), component %in% c("stem", "root")) %>%
    transmute(flux_id, plot, campaign = camp(date), component, co2_new = CO2_best.flux, model_new = CO2_model,
              use_in_analysis, exclusion_reason, window_source, Tcham_new = air_temp, Tcham_handheld = air_temp_handheld,
              nb_obs = CO2_nb.obs, analyzer = analyzer_source),
  by = "flux_id") %>%
  mutate(plot = coalesce(plot, plot_l), campaign = coalesce(campaign, camp_l), component = coalesce(component, comp_l),
         status = case_when(is.na(co2_legacy) ~ "only rebuild", is.na(co2_new) | !use_in_analysis ~ "only legacy / excluded now",
                            TRUE ~ "both"),
         ratio = co2_new / co2_legacy,
         temp_factor = (Tcham_legacy + 273.15) / (Tcham_new + 273.15)) %>%   # flux term scales with 1 / T
  select(-plot_l, -camp_l, -comp_l)
write_csv(x, "output/qa/co2_shift_closures.csv")
cat("Means by site x campaign x component (closures in each set):\n")
print(as.data.frame(x %>% group_by(plot, campaign, component) %>%
  summarise(n_leg = sum(!is.na(co2_legacy)), mean_leg = mean(co2_legacy, na.rm = TRUE),
            n_new = sum(use_in_analysis %in% TRUE), mean_new = mean(co2_new[use_in_analysis %in% TRUE], na.rm = TRUE),
            both_n = sum(status == "both"), both_ratio_med = median(ratio[status == "both"], na.rm = TRUE),
            temp_factor_med = median(temp_factor, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)
cat("\nBy window source and model change (closures in both, |ratio - 1| > 0.2):\n")
print(as.data.frame(x %>% filter(status == "both", abs(ratio - 1) > 0.2) %>%
  count(plot, campaign, component, analyzer, window_source, model = paste(model_legacy, "->", model_new))), row.names = FALSE)
cat("\nOnly in one set:\n")
print(as.data.frame(x %>% filter(status != "both") %>% count(plot, campaign, component, status, reason = substr(coalesce(exclusion_reason, ""), 1, 50))), row.names = FALSE)
