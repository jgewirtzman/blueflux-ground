# =============================================================================
# Audit: did each saved manual-ID window come from the analyzer it is assigned
# to? (diagnostic, step 3/4)
#
# The legacy imports used goFlux::import2RData(merge = TRUE), which merges every
# .RData in the shared RData/ folder, so a closure could be fitted on another
# analyzer's record at the same clock time (found for 45000_CP40_Water_130 and
# Mar_23_166_CP40_stem on 2023-03-15, fitted on LGR2 data). For every saved
# window, the flagged rows (CH4 per second) are matched exactly (|dCH4| < 0.01
# ppb at the same second) against each analyzer's raw record.
#
# Writes output/qa/saved_trace_audit.csv.
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(purrr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/00_lib/lib_raw.R")

inv <- read_csv("output/qa/measurement_inventory.csv", show_col_types = FALSE) %>%
  filter(has_saved_window)
units <- c("LGR1", "LGR2", "LGR3", "Picarro")

audit_file <- function(f) {
  m <- read_csv(file.path("intermediate", f), show_col_types = FALSE,
                col_types = cols(.default = col_character())) %>%
    filter(flag == "1", UniqueID %in% inv$flux_id[inv$saved_window_file == f]) %>%
    transmute(UniqueID, t = round(as.numeric(as.POSIXct(sub("Z$", "", sub("T", " ", POSIX.time)), tz = "UTC"))),
              CH4 = as.numeric(CH4dry_ppb))
  map_dfr(split(m, m$UniqueID), function(s) {
    rng <- as.POSIXct(range(s$t), origin = "1970-01-01", tz = "UTC")
    hits <- vapply(units, function(u) {
      r <- read_raw(u, rng[1] - 2, rng[2] + 2)
      if (is.null(r) || !nrow(r)) return(0)
      rt <- round(as.numeric(r$POSIX.time))
      mean(vapply(seq_len(nrow(s)), function(i) any(abs(r$CH4dry_ppb[rt == s$t[i]] - s$CH4[i]) < 0.01), TRUE))
    }, 1)
    tibble(flux_id = s$UniqueID[1], saved_window_file = f, n_rows = nrow(s),
           !!!setNames(as.list(round(hits, 3)), paste0("match_", units)))
  })
}
aud <- map_dfr(unique(inv$saved_window_file), audit_file) %>%
  left_join(inv %>% select(flux_id, analyzer), by = "flux_id") %>%
  rowwise() %>%
  mutate(match_own = c_across(starts_with("match_"))[match(analyzer, units)],
         best_unit = units[which.max(c_across(starts_with("match_")))],
         best_match = max(c_across(starts_with("match_")))) %>% ungroup() %>%
  mutate(verdict = case_when(match_own >= 0.9 ~ "own analyzer",
                             best_match >= 0.9 & best_unit != analyzer ~ paste("other analyzer:", best_unit),
                             best_match < 0.5 ~ "matches no raw record",
                             TRUE ~ "partial"))
write_csv(aud, "output/qa/saved_trace_audit.csv")
print(table(aud$analyzer, aud$verdict))
print(as.data.frame(aud %>% filter(verdict != "own analyzer") %>%
  select(flux_id, analyzer, verdict, n_rows, starts_with("match_"))), row.names = FALSE)
