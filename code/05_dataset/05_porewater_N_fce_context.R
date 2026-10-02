# =============================================================================
# Context for our porewater inorganic N: FCE LTER long-term porewater nutrient
# monitoring at the same intact sites (knb-lter-fce.1171.16; Castaneda-Moya,
# Kominoski et al., "Monitoring of nutrient and sulfide concentrations in
# porewaters of mangrove forests from the Shark River Slough and Taylor Slough",
# December 2000 - ongoing; NH4 and NO3 in umol/L).
# Downloads and caches the FCE file, summarises NH4 and NO3 at SRS5 and SRS6
# (all years; 2022-2024; per year for hurricane context), and sets our values
# (04_porewater_nitrogen.R, mg N/L -> umol/L) beside them.
# Writes output/data_products/porewater_N_fce_context.csv.
# =============================================================================
suppressMessages({library(dplyr); library(readr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
f   <- "data/environmental/porewater_fce/FCE_LTER_1171_porewater_nutrients.csv"
url <- "https://pasta.lternet.edu/package/data/eml/knb-lter-fce/1171/16/d99ac76b47918ea4050e7931ca574246"
if (!file.exists(f)) {                                  # cached copy in the repo (EDI now requires login)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  download.file(url, f, mode = "wb", quiet = TRUE)
}
fce <- read_csv(f, show_col_types = FALSE, na = c("-9999", "-9999.000", "-9999.0")) %>%
  filter(SITENAME %in% c("SRS5", "SRS6")) %>%
  mutate(year = as.integer(substr(as.character(Date), 1, 4)))
summ <- function(d, period) d %>% group_by(site = SITENAME) %>%
  summarise(period = period, n = sum(!is.na(Porewater_NH4)),
            NH4_median = median(Porewater_NH4, na.rm = TRUE), NH4_q90 = quantile(Porewater_NH4, 0.9, na.rm = TRUE),
            NH4_max = max(Porewater_NH4, na.rm = TRUE), NO3_median = median(Porewater_NO3, na.rm = TRUE), .groups = "drop")
yrs <- range(fce$year, na.rm = TRUE)
fce_out <- bind_rows(summ(fce, paste0("FCE ", yrs[1], "-", yrs[2])),
                     summ(filter(fce, year %in% 2022:2024), "FCE 2022-2024"),
                     summ(filter(fce, year == 2017), "FCE 2017 (post-Irma)"),
                     summ(filter(fce, year == 2018), "FCE 2018"))

to_uM <- 1000 / 14.007                                   # mg N/L -> umol/L
ours <- read_csv("output/data_products/porewater_all_parameters.csv", show_col_types = FALSE) %>%
  filter(!is.na(NH4_N_mgL)) %>% group_by(site = Site) %>%
  summarise(period = "This study", n = n(),
            NH4_median = median(NH4_N_mgL) * to_uM, NH4_q90 = quantile(NH4_N_mgL, 0.9) * to_uM,
            NH4_max = max(NH4_N_mgL) * to_uM, NO3_median = median(NO3_N_mgL) * to_uM, .groups = "drop")
out <- bind_rows(fce_out, ours) %>% mutate(across(where(is.double), ~ signif(.x, 3)))
dir.create("output/data_products", showWarnings = FALSE, recursive = TRUE)
write_csv(out, "output/data_products/porewater_N_fce_context.csv")
print(as.data.frame(out), row.names = FALSE)
