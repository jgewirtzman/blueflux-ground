# =============================================================================
# Porewater profiles + inorganic nitrogen (NO3-N, NH4-N; October 2025).
# Reads data/porewater/merged_porewater_all_parameters.csv and
# data/porewater/porewater_inorganic_N_Oct2025.csv (verbatim subset of the Yale
# inorganic-N run; see data/porewater/README_inorganic_N.md), and writes
# output/data_products/porewater_all_parameters.csv, the table read by the
# porewater figures and results.
#   - site and depth parsed from the sample ID ("surface" -> Surface; "C" at
#     CP40 -> 0 cm, the only CP40 depth otherwise missing)
#   - values <= 0 mg N/L are below detection: set to NA, flagged
# NH4/NO3 are added for profiles and description; they are not added to the
# porewater PCA (02_manuscript_results.R, publication_figures_soilprofile.R).
# =============================================================================
suppressMessages({library(dplyr); library(readr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
pw <- read_csv("data/porewater/merged_porewater_all_parameters.csv", show_col_types = FALSE)
n  <- read_csv("data/porewater/porewater_inorganic_N_Oct2025.csv", show_col_types = FALSE)
n <- n %>%
  mutate(id = sub("^EU PO 10/2025 _", "", `Sample ID`),
         Site = sub(" .*$", "", id), dep = sub("^\\S+ ", "", id),
         Depth_cm = case_when(tolower(dep) == "surface" ~ "Surface", dep == "C" ~ "0", TRUE ~ dep),
         NO3_N_mgL_raw = `mg N-NO3/L`, NH4_N_mgL_raw = `mg N-NH4/L`,
         NO3_N_mgL = ifelse(NO3_N_mgL_raw > 0, NO3_N_mgL_raw, NA), NH4_N_mgL = ifelse(NH4_N_mgL_raw > 0, NH4_N_mgL_raw, NA),
         NO3_N_bdl = NO3_N_mgL_raw <= 0, NH4_N_bdl = NH4_N_mgL_raw <= 0,
         N_depth_note = ifelse(dep == "C", "sample labelled 'C', taken as 0 cm", NA)) %>%
  select(Site, Depth_cm, NO3_N_mgL, NH4_N_mgL, NO3_N_mgL_raw, NH4_N_mgL_raw, NO3_N_bdl, NH4_N_bdl, N_depth_note)
stopifnot(!anyDuplicated(n[c("Site", "Depth_cm")]))
out <- pw %>% mutate(Depth_cm = as.character(Depth_cm)) %>% left_join(n, by = c("Site", "Depth_cm"))
stopifnot(sum(!is.na(out$NH4_N_mgL_raw)) == nrow(n))
dir.create("output/data_products", showWarnings = FALSE, recursive = TRUE)
write_csv(out, "output/data_products/porewater_all_parameters.csv")
cat("Porewater + inorganic N:", nrow(out), "rows;", sum(!out$NH4_N_bdl, na.rm = TRUE), "NH4 and",
    sum(!out$NO3_N_bdl, na.rm = TRUE), "NO3 values above detection\n")
print(as.data.frame(out %>% select(Site, Depth_cm, NH4_N_mgL, NO3_N_mgL)), row.names = FALSE)
