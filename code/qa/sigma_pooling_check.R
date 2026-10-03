# =============================================================================
# QA: empirical precision sigma per analyzer x campaign, three ways, on the
# stage-02 fit windows (Picarro: each gas's fresh readings only):
#   sigma_fluxqc  = fluxqc 0.2.3 style: first differences pooled uncentred;
#   sigma_centred = each closure's differences centred on its own median first
#                   (the stage-03 sigma);
#   per_closure_median = median of the per-closure MAD sigmas.
# Writes output/qa/sigma_pooling_check.csv.
# =============================================================================
suppressMessages(library(dplyr))
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/00_lib/lib_raw.R")
w <- readr::read_csv("output/flux/02_windows/windows.csv", show_col_types = FALSE) %>% filter(window_source != "none")
k <- 1.4826 / sqrt(2)
one <- function(gas) bind_rows(lapply(seq_len(nrow(w)), function(i) {
  r <- read_raw(w$analyzer[i], as.POSIXct(w$start[i], tz = "UTC"), as.POSIXct(w$end[i], tz = "UTC"))
  if (is.null(r) || nrow(r) < 5) return(NULL)
  r <- r[if (gas == "CH4dry_ppb") fresh_ch4(r, w$analyzer[i]) else fresh_co2(r, w$analyzer[i]), ]
  tibble(gas = gas, UniqueID = w$UniqueID[i], group = paste(w$analyzer[i], w$campaign[i]),
         dx = diff(r[[gas]]), dt = diff(as.numeric(r$POSIX.time)))
}))
d <- bind_rows(one("CH4dry_ppb"), one("CO2dry_ppm"))
out <- d %>% group_by(gas, group) %>% mutate(dx_c = dx - ave(dx, UniqueID, FUN = median)) %>%
  summarise(closures = n_distinct(UniqueID), step_s = round(median(dt), 1),
            sigma_fluxqc = mad(dx, constant = 1) * k, sigma_centred = mad(dx_c, constant = 1) * k,
            per_closure_median = median(tapply(dx, UniqueID, function(v) mad(v, constant = 1) * k)), .groups = "drop") %>%
  mutate(inflation = sigma_fluxqc / sigma_centred)
readr::write_csv(out, "output/qa/sigma_pooling_check.csv")
print(as.data.frame(out %>% mutate(across(where(is.double), ~ round(.x, 3)))), row.names = FALSE)
