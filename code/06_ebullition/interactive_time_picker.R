# =============================================================================
# Interactive Time Window Picker for Flux Traces
# =============================================================================
# Opens each trace in an interactive plot window. Click to set start and end
# points for the valid measurement window. Results are saved to a CSV.
#
# Usage: Source this file in RStudio, then call:
#   results <- pick_windows(flux_ids)
# =============================================================================

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

# ---- Load trace data (manual IDs + reconstructed) ----

load_trace <- function(flux_id) {
  # Try manual ID files first
  manual_files <- c(
    "intermediate/results_trees/lgr1_manual_identification_results.csv",
    "intermediate/results_trees/lgr3_manual_identification_results.csv",
    "intermediate/results_trees/lgr3_manual_identification_results_additional.csv",
    "intermediate/results_surface/lgr3_manual_identification_results_soil.csv",
    "intermediate/results_surface/lgr2_manual_identification_results_soil.csv",
    "intermediate/rescue/lgr2_manual_identification_results.csv"
  )

  for (f in manual_files) {
    if (!file.exists(f)) next
    d <- read_csv(f, show_col_types = FALSE) %>%
      filter(UniqueID == flux_id) %>%
      select(UniqueID, Etime, CH4dry_ppb, flag)
    if (nrow(d) > 0) return(d)
  }

  # Fall back to reconstructing from raw analyzer data
  df <- read_csv("output/data_products/combined_gas_flux_dataset.csv", show_col_types = FALSE)
  row <- df %>% filter(flux_id == !!flux_id)
  if (nrow(row) == 0) return(NULL)

  # Load auxfile
  aux_files <- c(
    "intermediate/auxfiles/tree_auxfile_lgr2_goflux.csv",
    "intermediate/auxfiles/tree_auxfile_lgr3_goflux.csv",
    "intermediate/auxfiles/soilwater_auxfile_lgr2_goflux.csv",
    "intermediate/auxfiles/soilwater_auxfile_lgr3_goflux.csv"
  )

  aux_row <- NULL
  for (af in aux_files) {
    if (!file.exists(af)) next
    a <- read_csv(af, show_col_types = FALSE) %>% filter(UniqueID == flux_id)
    if (nrow(a) > 0) { aux_row <- a[1,]; break }
  }
  if (is.null(aux_row)) return(NULL)

  # Parse raw analyzer data
  d <- as.Date(row$date[1])
  src <- row$source_file[1]
  analyzer_dir <- if (grepl("LGR3", src)) {
    paste0("data/analyzer/LGR_GLA131/LGR3/", d)
  } else if (grepl("LGR2", src)) {
    paste0("data/analyzer/LGR_GLA131/LGR2/", d)
  } else if (grepl("LGR1", src)) {
    paste0("data/analyzer/LGR_GLA131/LGR1/", d)
  } else return(NULL)

  if (!dir.exists(analyzer_dir)) return(NULL)

  files <- list.files(analyzer_dir, pattern = "_f\\d+\\.txt$",
                      full.names = TRUE, recursive = TRUE)
  if (length(files) == 0) return(NULL)

  parse_lgr <- function(filepath) {
    lines <- readLines(filepath, warn = FALSE)
    header_idx <- grep("\\[CH4\\]_ppm", lines)[1]
    if (is.na(header_idx)) return(NULL)
    header <- trimws(strsplit(lines[header_idx], ",")[[1]])
    data_lines <- lines[(header_idx + 1):length(lines)]
    data_lines <- data_lines[grepl("^\\d{2}/\\d{2}/\\d{4}", data_lines)]
    if (length(data_lines) == 0) return(NULL)
    df <- read.csv(text = paste(data_lines, collapse = "\n"),
                   header = FALSE, stringsAsFactors = FALSE)
    n_cols <- min(ncol(df), length(header))
    names(df)[1:n_cols] <- header[1:n_cols]
    time_col <- if ("SysTime" %in% names(df)) "SysTime" else names(df)[1]
    ch4_col <- grep("CH4.*d_ppm", names(df), value = TRUE)[1]
    tibble(datetime = mdy_hms(trimws(df[[time_col]])),
           CH4_ppm = as.numeric(trimws(df[[ch4_col]]))) %>%
      filter(!is.na(datetime)) %>% arrange(datetime)
  }

  raw <- bind_rows(lapply(files, parse_lgr)) %>%
    distinct(datetime, .keep_all = TRUE) %>% arrange(datetime)

  meas_start <- ymd_hms(aux_row$start.time)
  window <- raw %>%
    filter(datetime >= meas_start - 60, datetime <= meas_start + 600) %>%
    mutate(Etime = as.numeric(difftime(datetime, meas_start, units = "secs")),
           CH4dry_ppb = CH4_ppm * 1000,
           flag = ifelse(Etime >= 0, 1, 0),
           UniqueID = flux_id)

  window %>% select(UniqueID, Etime, CH4dry_ppb, flag)
}

# ---- Interactive picker ----

pick_windows <- function(flux_ids, output_file = "output/ebullition/corrected_time_windows.csv") {
  results <- tibble(
    flux_id = character(),
    new_start_etime = numeric(),
    new_end_etime = numeric(),
    action = character()  # "trim", "keep_original", "remove"
  )

  # Force base R graphics device so locator() works
  if (dev.cur() > 1) dev.off()

  for (fid in flux_ids) {
    cat("\n==============================\n")
    cat("Flux ID:", fid, "\n")

    d <- load_trace(fid)
    if (is.null(d) || nrow(d) == 0) {
      cat("  No trace data found. Skipping.\n")
      next
    }

    d_used <- d %>% filter(flag == 1)
    d_ctx  <- d %>% filter(flag == 0)

    # Use base R plot for locator() compatibility
    plot(d$Etime, d$CH4dry_ppb, type = "n",
         xlab = "Elapsed Time (s)", ylab = "CH4 (ppb)",
         main = fid)
    if (nrow(d_ctx) > 0) {
      points(d_ctx$Etime, d_ctx$CH4dry_ppb, col = "grey70", pch = 16, cex = 0.8)
    }
    points(d_used$Etime, d_used$CH4dry_ppb, col = "steelblue", pch = 16, cex = 1)
    if (nrow(d_used) > 2) {
      fit <- lm(CH4dry_ppb ~ Etime, data = d_used)
      abline(fit, col = "red", lwd = 1.5)
    }
    abline(v = 0, lty = 2, col = "grey50")
    legend("topright", legend = c("Used", "Context", "LM fit"),
           col = c("steelblue", "grey70", "red"),
           pch = c(16, 16, NA), lty = c(NA, NA, 1), cex = 0.8)

    cat("\nClick TWO points to set the new start and end time.\n")
    cat("(Right-click or press Esc to skip this trace)\n")

    pts <- tryCatch(locator(2, type = "l", col = "orange", lwd = 2),
                    error = function(e) NULL)

    if (is.null(pts) || length(pts$x) < 2) {
      cat("  Skipped.\n")
      next
    }

    new_start <- min(pts$x)
    new_end <- max(pts$x)

    # Draw the selected window
    abline(v = new_start, col = "orange", lwd = 2, lty = 2)
    abline(v = new_end, col = "orange", lwd = 2, lty = 2)
    rect(new_start, par("usr")[3], new_end, par("usr")[4],
         col = rgb(1, 0.6, 0, 0.15), border = NA)

    cat(sprintf("  New window: %.0f - %.0f s\n", new_start, new_end))

    results <- bind_rows(results, tibble(
      flux_id = fid,
      new_start_etime = round(new_start),
      new_end_etime = round(new_end),
      action = "trim"
    ))

    # Brief pause to see the selection
    Sys.sleep(0.5)
  }

  write_csv(results, output_file)
  cat("\nSaved results to:", output_file, "\n")
  results
}

cat("Interactive time picker loaded.\n")
cat("Usage:\n")
cat("  # IDs needing middle extraction:\n")
cat('  middle_ids <- c("Mar_23_243_SRS6_CWD", "Oct_22_29_SRS6_stem", ...)\n')
cat("  results <- pick_windows(middle_ids)\n")
cat("\n  # Or pick for all IDs at once:\n")
cat('  all_ids <- c(trim_end_ids, middle_ids, start_ids)\n')
cat("  results <- pick_windows(all_ids)\n")
