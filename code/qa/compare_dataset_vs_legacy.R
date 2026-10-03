# =============================================================================
# QA: compiled dataset (stage 05) vs the frozen legacy combined dataset.
# Descriptive columns must agree for every closure present in both, except
# where a documented correction applies (date / analyzer corrections, Mar 2022
# chamber override). Writes output/qa/dataset_vs_legacy_columns.csv.
# =============================================================================
suppressMessages({library(dplyr); library(readr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
new <- read_csv("output/data_products/flux_measurements_all.csv", show_col_types = FALSE, guess_max = 5000)
old <- read_csv("output/qa/baseline/output__data_products__combined_gas_flux_dataset.csv", show_col_types = FALSE, guess_max = 5000) %>%
  filter(data_source != "ebullition_reprocessing")
cols <- c("plot", "date", "measurement_type", "component", "analyzer_source", "species", "status", "height",
          "height_corrected", "diameter", "lenticels", "above", "chamber_class", "surface_type", "collar_id",
          "collar_location", "chamber_id", "pneumatophore_count", "soil_temp", "stem_temp", "water_temp",
          "water_depth", "season", "month_year", "disturbance_level", "pneumatophore_density",
          "surface_area_cm2", "total_system_volume_cm3")
m <- inner_join(new, old, by = "flux_id", suffix = c(".new", ".old"))
same <- function(a, b) { if (is.numeric(a) && is.numeric(b)) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a - b) <= 1e-6 * pmax(1, abs(b)))
                         else (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & as.character(a) == as.character(b)) }
res <- bind_rows(lapply(cols, function(cc) {
  a <- m[[paste0(cc, ".new")]]; b <- m[[paste0(cc, ".old")]]
  if (cc == "analyzer_source") b <- ifelse(b == "LGR" & !is.na(m$source_file), sub("_.*", "", m$source_file), b)
  bad <- !same(a, b)
  tibble(column = cc, n_compared = nrow(m), n_differ = sum(bad),
         examples = paste(head(paste0(m$flux_id[bad], ": ", a[bad], " vs ", b[bad]), 4), collapse = " | "))
}))
write_csv(res, "output/qa/dataset_vs_legacy_columns.csv")
print(as.data.frame(res %>% mutate(examples = substr(examples, 1, 150))), row.names = FALSE)
