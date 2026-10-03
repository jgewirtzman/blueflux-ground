# =============================================================================
# Supplementary tables built from the analysis outputs:
#   si_exclusions.csv      measurement exclusions by analysis criterion
#   si_sample_sizes.csv    analysed fluxes by site x campaign x component
#   si_component_rates.csv CH4 and CO2 rates by component x class x season
#                          (bootstrap mean and 95% percentile CI, 5,000 resamples)
# Rates use the same set as 06_analysis/02_manuscript_results.R (the eight named
# plots; closures with CO2 < -10 umol m-2 s-1 dropped).
# Writes output/analysis/si/.
# =============================================================================
suppressMessages(library(dplyr))
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
dir.create("output/analysis/si", showWarnings = FALSE, recursive = TRUE)

# ---- exclusions ----
all <- read.csv("output/data_products/flux_measurements_all.csv")
cat_of <- function(r) {
  r <- tolower(r)
  case_when(
    grepl("^duplicate", r) ~ "Duplicate record of another measurement (not an independent closure)",
    grepl("analyzer (artifact|fit failure)", r) ~ "Analyzer artefact (spurious concentration behaviour from the instrument)",
    grepl("placement artifact|leak|chamber center", r) ~ "Chamber placement or seal artefact",
    grepl("pilot chamber", r) ~ "Pilot chamber design without validated enclosed area",
    grepl("without dimensions|no chamber geometry", r) ~ "No chamber geometry",
    grepl("same floating-chamber placement", r) ~ "Second closure within one floating-chamber placement",
    grepl("no closure time|no field start time", r) ~ "No closure time",
    TRUE ~ "No usable analyzer record for the closure")
}
ex <- all %>% filter(excluded) %>% mutate(category = cat_of(exclusion_reason))
excl <- ex %>% group_by(category) %>%
  summarise(n = n(), components = paste(sprintf("%s %d", names(table(component)), as.integer(table(component))), collapse = ", "),
            .groups = "drop") %>% arrange(desc(n))
excl <- bind_rows(excl, data.frame(category = "Total excluded", n = nrow(ex), components = ""),
                  data.frame(category = "Analysed", n = sum(!all$excluded), components = ""))
write.csv(excl, "output/analysis/si/si_exclusions.csv", row.names = FALSE)

# ---- sample sizes ----
d <- read.csv("output/data_products/combined_gas_flux_dataset.csv")
camp <- function(date) { m <- format(as.Date(date), "%Y-%m")
  c(`2022-03` = "Mar 2022", `2022-10` = "Oct 2022", `2023-03` = "Mar 2023")[m] }
d$campaign <- camp(d$date)
ss <- d %>% count(plot, campaign, component) %>%
  tidyr::pivot_wider(names_from = component, values_from = n, values_fill = 0) %>%
  mutate(total = rowSums(across(where(is.numeric)))) %>% arrange(plot, campaign)
write.csv(ss, "output/analysis/si/si_sample_sizes.csv", row.names = FALSE)

# ---- component rates by class x season ----
plots8 <- c("BL60", "CP40", "FLM30", "MI", "RB10", "SE1", "SRS5", "SRS6")
r <- d %>% filter(plot %in% plots8, is.na(CO2_best.flux) | CO2_best.flux >= -10) %>%
  mutate(class = c(healthy = "intact", regenerating = "regenerating", ghost = "ghost", scrub = "scrub")[disturbance_level],
         season = ifelse(format(as.Date(date), "%m") == "10", "wet", "dry"))
set.seed(42)
boot <- function(x) { x <- x[is.finite(x)]; if (length(x) < 3) return(c(mean(x), NA, NA))
  set.seed(42); b <- replicate(5000, mean(sample(x, replace = TRUE))); c(mean(x), quantile(b, c(0.025, 0.975))) }
rates <- r %>% group_by(component, class, season) %>%
  summarise(n_CH4 = sum(is.finite(CH4_best.flux)), CH4 = list(boot(CH4_best.flux)),
            n_CO2 = sum(is.finite(CO2_best.flux)), CO2 = list(boot(CO2_best.flux)), .groups = "drop") %>%
  mutate(CH4_mean = sapply(CH4, `[`, 1), CH4_lo = sapply(CH4, `[`, 2), CH4_hi = sapply(CH4, `[`, 3),
         CO2_mean = sapply(CO2, `[`, 1), CO2_lo = sapply(CO2, `[`, 2), CO2_hi = sapply(CO2, `[`, 3)) %>%
  select(-CH4, -CO2) %>% arrange(component, class, season)
write.csv(rates, "output/analysis/si/si_component_rates.csv", row.names = FALSE)
cat(sprintf("exclusions %d categories; sample sizes %d rows; rates %d rows\n", nrow(excl) - 2, nrow(ss), nrow(rates)))
