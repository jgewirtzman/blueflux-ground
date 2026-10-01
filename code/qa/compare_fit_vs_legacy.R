# =============================================================================
# Refit vs legacy CH4 flux per closure, by window source (diagnostic, step 4).
# Writes output/qa/fit_vs_legacy_CH4.csv.
# =============================================================================
suppressMessages(library(dplyr))
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
n <- read.csv("output/flux/03_fit/CH4/fluxes.csv"); o <- read.csv("output/qa/baseline/output__data_products__combined_gas_flux_dataset.csv")
w <- read.csv("output/flux/02_windows/windows.csv")
x <- n %>% select(UniqueID, new = best.flux, new_model = model, nb.obs, det_class_emp, qc_any) %>%
  inner_join(o %>% select(UniqueID = flux_id, old = CH4_best.flux, old_model = CH4_model, data_source), by = "UniqueID") %>%
  left_join(w %>% select(UniqueID, window_source, analyzer, campaign), by = "UniqueID") %>%
  filter(!is.na(old), !is.na(new)) %>% mutate(r = new / old, lr = log10(abs(r)), sign_flip = sign(new) != sign(old))
q <- function(v) paste(round(quantile(v, c(.1, .25, .5, .75, .9), na.rm = TRUE), 3), collapse = " / ")
cat("n compared:", nrow(x), "\n")
print(x %>% group_by(window_source) %>% summarise(n = n(), ratio_q10_25_50_75_90 = q(r), within_5pct = mean(abs(r - 1) < 0.05),
      within_20pct = mean(abs(r - 1) < 0.2), sign_flips = sum(sign_flip), same_model = mean(new_model == old_model)) %>% as.data.frame())
print(x %>% filter(window_source == "saved manual window") %>% group_by(analyzer, campaign) %>%
      summarise(n = n(), med_ratio = round(median(r), 3), within_5pct = round(mean(abs(r - 1) < 0.05), 2), .groups = "drop") %>% as.data.frame())
write.csv(x, "output/qa/fit_vs_legacy_CH4.csv", row.names = FALSE)
