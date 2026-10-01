# =============================================================================
# QA / decision support (stage 04): Picarro floating-chamber closures (5-s
# logging). goAquaFlux could not resolve the confirmed bubbles on the Picarro
# traces (ebullition_window_comparison.R), so every approach is compared on all
# logged Picarro water closures, each fitted on its stage-02 window:
#   total        stage-03 goFlux fit of the whole window (no partition)
#   legacy       legacy hand-built jump partition (output/ebullition/partitioned_fluxes.csv)
#   A / D        released goFlux 0.4.0 goAquaFlux, detection window 30 / 6 obs
#   B / E        fork de-ebulliated goAquaFlux, detection window 15 / 6 obs
# Writes output/qa/picarro_water_comparison.csv.
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(tidyr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/00_lib/lib_raw.R")
fork_lib <- Sys.getenv("GOFLUX_FORK_LIB"); stopifnot(nzchar(fork_lib), dir.exists(fork_lib))

aux <- read_csv("output/flux/01_metadata/auxfile.csv", show_col_types = FALSE)
win <- read_csv("output/flux/02_windows/windows.csv", show_col_types = FALSE)
fit <- read_csv("output/flux/03_fit/CH4/fluxes.csv", show_col_types = FALSE)
legacy <- read_csv("output/ebullition/partitioned_fluxes.csv", show_col_types = FALSE)

cl <- aux %>% filter(analyzer == "Picarro", component == "water", !excluded) %>%
  inner_join(win %>% select(UniqueID, start, end), by = "UniqueID")
traces <- bind_rows(lapply(seq_len(nrow(cl)), function(i) {
  w <- cl[i, ]; s <- as.POSIXct(w$start, tz = "UTC"); e <- as.POSIXct(w$end, tz = "UTC")
  read_raw("Picarro", s, e) %>%
    mutate(UniqueID = w$UniqueID, Etime = as.numeric(POSIX.time - s, units = "secs"), flag = 1, H2O_ppm = 0,
           CO2_prec = 0.025, CH4_prec = 0.1, H2O_prec = 0, Area = w$Area, offset = 0, Vtot = w$Vtot,
           Vcham = w$Vcham / 1000, Tcham = w$Tcham, Pcham = w$Pcham) %>%
    select(-source_file)
}))
cat("Picarro water closures:", nrow(cl), "| samples per closure:",
    paste(table(traces$UniqueID), collapse = " "), "\n")

run_variant <- function(lib, args) callr::r(function(d, args, lib) {
  if (!is.null(lib)) .libPaths(c(lib, .libPaths()))
  suppressMessages(library(goFlux))
  one <- function(x) tryCatch({
    res <- suppressWarnings(do.call(goFlux::goAquaFlux, c(list(dataframe = x, gastype = "CH4dry_ppb"), args)))
    as.data.frame(if (!is.null(res$flux_summary)) res$flux_summary else res[[1]])
  }, error = function(e) data.frame(UniqueID = x$UniqueID[1], error = conditionMessage(e)))
  do.call(dplyr::bind_rows, lapply(split(d, d$UniqueID), one))
}, args = list(d = as.data.frame(traces), args = args, lib = lib))
tidy <- function(s, tag) tibble(UniqueID = s$UniqueID, approach = tag,
                                diffusive = if ("flux_diffusive" %in% names(s)) s$flux_diffusive else NA_real_,
                                ebullitive = if ("flux_ebullition" %in% names(s)) s$flux_ebullition else NA_real_,
                                total = if ("flux_total" %in% names(s)) s$flux_total else NA_real_,
                                error = if ("error" %in% names(s)) s$error else NA_character_)

res <- bind_rows(
  fit %>% filter(UniqueID %in% cl$UniqueID) %>%
    transmute(UniqueID, approach = "total (stage 03 fit)", diffusive = NA_real_, ebullitive = NA_real_, total = best.flux),
  legacy %>% filter(trace_type == "processed", matched_flux_id %in% cl$UniqueID) %>% group_by(matched_flux_id) %>% slice(1) %>%
    ungroup() %>% transmute(UniqueID = matched_flux_id, approach = "legacy jump partition", diffusive = diffusive_flux_nmol,
                            ebullitive = ebull_flux_nmol, total = total_flux_nmol),
  tidy(run_variant(NULL, list()), "A released, window 30"),
  tidy(run_variant(fork_lib, list(diffusion.window = "deebulliated", bubble.window.size = 15)), "B fork de-ebulliated, window 15"),
  tidy(run_variant(NULL, list(bubble.window.size = 6, diffusion.minimum_window = 6)), "D released, window 6"),
  tidy(run_variant(fork_lib, list(diffusion.window = "deebulliated", bubble.window.size = 6, diffusion.minimum_window = 6)),
       "E fork de-ebulliated, window 6")) %>%
  left_join(cl %>% select(UniqueID, plot, date), by = "UniqueID")
write_csv(res, "output/qa/picarro_water_comparison.csv")

cat("\nTotal CH4 flux per closure (nmol m-2 s-1):\n")
print(as.data.frame(res %>% select(UniqueID, approach, total) %>% mutate(total = round(total, 1)) %>%
                      pivot_wider(names_from = approach, values_from = total)), row.names = FALSE)
cat("\nEbullitive share (ebullitive / total):\n")
print(as.data.frame(res %>% filter(!is.na(ebullitive)) %>% mutate(share = round(ebullitive / total, 2)) %>%
                      select(UniqueID, approach, share) %>% pivot_wider(names_from = approach, values_from = share)), row.names = FALSE)
cat("\nSite x date mean total:\n")
print(as.data.frame(res %>% group_by(plot, date, approach) %>%
                      summarise(n = sum(!is.na(total)), mean_total = round(mean(total, na.rm = TRUE), 1), .groups = "drop") %>%
                      pivot_wider(names_from = approach, values_from = c(mean_total))), row.names = FALSE)
