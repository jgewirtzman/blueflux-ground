#!/usr/bin/env Rscript
# ===============================================================================
# Recover Failed Measurements
# ===============================================================================
#
# This script attempts to recover the 55 measurements where goFlux never
# produced flux results. These measurements have field metadata (start/end
# times, chamber specs) but no flux output because their time windows were
# never identified during click.peak2 manual identification.
#
# WORKFLOW:
# 1. Identify the 55 no_data measurements from the clean dataset
# 2. Match each to its auxfile (for chamber specs) and raw analyzer data
# 3. Import raw data (detecting clock resets and interleaved streams)
# 4. Run click_flux_2d interactively: click a 2D box around the good data
#    to select the time range AND concentration band, then goFlux calculates
# 5. Run best.flux() on results
# 6. Save recovered results for integration via assemble_clean_dataset.R
#
# IMPORTANT: This script requires interactive use — click_flux_2d needs a
# graphics device and user input to select observation windows.
#
# After running this script, re-run assemble_clean_dataset.R to integrate
# any recovered measurements.
# ===============================================================================

library(goFlux)
library(dplyr)
library(readr)
library(lubridate)

# Set working directory to project root (two levels up from this script's location)
project_root <- normalizePath(file.path(dirname(
  if (interactive()) rstudioapi::getSourceEditorContext()$path
  else commandArgs(trailingOnly = FALSE)[grep("--file=", commandArgs(trailingOnly = FALSE))] |>
    sub("--file=", "", x = _)
), "..", ".."), mustWork = FALSE)

# Fallback: detect project root by looking for the output/ directory
if (!dir.exists(file.path(project_root, "output"))) {
  # Try working up from current directory
  wd <- getwd()
  while (nchar(wd) > 1 && !dir.exists(file.path(wd, "output"))) {
    wd <- dirname(wd)
  }
  if (dir.exists(file.path(wd, "output"))) {
    project_root <- wd
  } else {
    stop("Cannot find project root. Please setwd() to blueflux-ground/ before running.")
  }
}

setwd(project_root)
cat("Working directory:", getwd(), "\n\n")

cat("=== RECOVER FAILED MEASUREMENTS ===\n\n")

# =============================================================================
# STEP 1: Load the clean dataset and identify no_data measurements
# =============================================================================

cat("Step 1: Loading clean dataset and identifying no_data measurements...\n")

df <- read_csv("output/data_products/combined_gas_flux_dataset.csv", show_col_types = FALSE)
failed <- df %>% filter(flux_status == "no_data")

cat("  Total no_data measurements:", nrow(failed), "\n\n")

# Show summary
cat("  By analyzer:\n")
failed %>% count(analyzer_source) %>% print()
cat("\n  By measurement type:\n")
failed %>% count(measurement_type) %>% print()
cat("\n  By plot:\n")
failed %>% count(plot) %>% print()

# =============================================================================
# STEP 2: Map each failed measurement to its auxfile and raw data
# =============================================================================

cat("\nStep 2: Mapping failed measurements to auxfiles and raw data...\n\n")

# Load all relevant auxfiles (tree and surface, all instruments)
tree_auxfiles <- list(
  LGR1 = read_csv("intermediate/auxfiles/tree_auxfile_lgr1_complete.csv", show_col_types = FALSE),
  LGR2 = read_csv("intermediate/auxfiles/tree_auxfile_lgr2_complete.csv", show_col_types = FALSE),
  LGR3 = read_csv("intermediate/auxfiles/tree_auxfile_lgr3_complete.csv", show_col_types = FALSE),
  Picarro = read_csv("intermediate/auxfiles/tree_auxfile_picarro_complete.csv", show_col_types = FALSE)
)

surface_auxfiles <- list(
  LGR1 = read_csv("intermediate/auxfiles/soilwater_auxfile_lgr1_goflux.csv", show_col_types = FALSE),
  LGR2 = read_csv("intermediate/auxfiles/soilwater_auxfile_lgr2_goflux.csv", show_col_types = FALSE),
  LGR3 = read_csv("intermediate/auxfiles/soilwater_auxfile_lgr3_goflux.csv", show_col_types = FALSE),
  Picarro = read_csv("intermediate/auxfiles/soilwater_auxfile_picarro_goflux.csv", show_col_types = FALSE)
)

# Combine all auxfiles into one lookup table
all_auxfiles <- bind_rows(
  bind_rows(tree_auxfiles, .id = "aux_analyzer") %>% mutate(aux_type = "tree"),
  bind_rows(surface_auxfiles, .id = "aux_analyzer") %>% mutate(aux_type = "surface")
)

cat("  Total auxfile entries:", nrow(all_auxfiles), "\n")

# Match failed measurements to auxfile entries
failed_with_aux <- failed %>%
  left_join(
    all_auxfiles %>% select(UniqueID, DATE, TIME, start.time, Area, offset,
                             Vcham, Vtube, Vinst, Vtot, Tcham, Pcham,
                             aux_analyzer, aux_type),
    by = c("flux_id" = "UniqueID")
  )

has_auxfile <- sum(!is.na(failed_with_aux$start.time))
no_auxfile <- sum(is.na(failed_with_aux$start.time))

cat("  Matched to auxfile:", has_auxfile, "\n")
cat("  No auxfile match:", no_auxfile, "\n")

if (no_auxfile > 0) {
  cat("\n  Measurements without auxfile entries (cannot recover):\n")
  failed_with_aux %>%
    filter(is.na(start.time)) %>%
    select(flux_id, plot, date, analyzer_source, measurement_type) %>%
    print(n = Inf)
}

# =============================================================================
# STEP 3: Check which measurements have matching raw analyzer data
# =============================================================================

cat("\nStep 3: Checking raw data availability...\n\n")

# Define raw data paths
raw_data_paths <- list(
  LGR1 = "data/analyzer/LGR_GLA131/LGR1",
  LGR2 = "data/analyzer/LGR_GLA131/LGR2",
  LGR = "data/analyzer/LGR_GLA131/LGR2",  # "LGR" in dataset maps to LGR2 based on the rescue scripts
  LGR3 = "data/analyzer/LGR_GLA131/LGR3",
  Picarro = "data/analyzer/Picarro_G4301/Minimal"
)

# For each failed measurement with an auxfile, check if raw data exists
recoverable <- failed_with_aux %>%
  filter(!is.na(start.time)) %>%
  mutate(
    # Determine which analyzer directory to look in
    analyzer_dir = case_when(
      aux_analyzer == "LGR1" ~ "LGR1",
      aux_analyzer == "LGR2" ~ "LGR2",
      aux_analyzer == "LGR3" ~ "LGR3",
      aux_analyzer == "Picarro" ~ "Picarro",
      TRUE ~ NA_character_
    ),
    # Extract date from auxfile DATE field
    raw_date = as.Date(DATE),
    has_raw_data = NA  # will fill in below
  )

# Check each measurement for raw data
for (i in seq_len(nrow(recoverable))) {
  analyzer <- recoverable$analyzer_dir[i]
  meas_date <- recoverable$raw_date[i]

  if (is.na(analyzer) || is.na(meas_date)) {
    recoverable$has_raw_data[i] <- FALSE
    next
  }

  if (analyzer %in% c("LGR1", "LGR2", "LGR3")) {
    # LGR data: organized as date-named folders
    date_dir <- file.path(raw_data_paths[[analyzer]], format(meas_date, "%Y-%m-%d"))
    recoverable$has_raw_data[i] <- dir.exists(date_dir)
  } else if (analyzer == "Picarro") {
    # Picarro data: organized as YYYY/MM/DD
    date_dir <- file.path(raw_data_paths[["Picarro"]],
                          format(meas_date, "%Y"),
                          format(meas_date, "%m"),
                          format(meas_date, "%d"))
    recoverable$has_raw_data[i] <- dir.exists(date_dir)
  }
}

n_recoverable <- sum(recoverable$has_raw_data, na.rm = TRUE)
n_no_raw <- sum(!recoverable$has_raw_data, na.rm = TRUE)

cat("  Potentially recoverable (raw data exists):", n_recoverable, "\n")
cat("  No raw data found:", n_no_raw, "\n")

if (n_no_raw > 0) {
  cat("\n  Measurements without raw data (unrecoverable):\n")
  recoverable %>%
    filter(!has_raw_data) %>%
    select(flux_id, plot, raw_date, analyzer_dir) %>%
    print(n = Inf)
}

# =============================================================================
# STEP 4: Create recovery table
# =============================================================================

cat("\nStep 4: Creating recovery table...\n\n")

recovery_table <- recoverable %>%
  filter(has_raw_data) %>%
  select(
    flux_id, plot, measurement_type, component,
    analyzer_source, analyzer_dir, raw_date,
    DATE, TIME, start.time,
    Area, offset, Vcham, Vtube, Vinst, Vtot, Tcham, Pcham
  ) %>%
  arrange(analyzer_dir, raw_date, flux_id)

cat("Recovery table:\n")
print(recovery_table %>% select(flux_id, analyzer_dir, raw_date, component), n = Inf)

# Save recovery table for reference
write_csv(recovery_table, "intermediate/rescue/recovery_table_55_failed.csv")
cat("\nSaved: intermediate/rescue/recovery_table_55_failed.csv\n")

# =============================================================================
# STEP 5: Helper functions for data import, diagnostics, and flux recovery
# =============================================================================
#
# --- Import & diagnostics ---
#
# detect_clock_streams():
#   Reads raw LGR .txt file in true write order to detect clock resets
#   (timestamps >30 sec behind the running max). Returns "ahead"/"behind"
#   stream labels. Works whether streams are sequential blocks or interspersed.
#
# import_lgr_tagged():
#   Imports LGR data ONE FILE AT A TIME, tagging each row with source_file
#   and serial_number. Detects gross clock resets via detect_clock_streams()
#   and splits those files into separate temp files before import.
#
# diagnose_overlaps():
#   Summarizes data by stream. Reports overlapping time ranges.
#
# --- Interactive flux recovery ---
#
# click_flux_2d():   *** PRIMARY RECOVERY TOOL ***
#   Interactive 2D bounding-box selection. For each measurement: plots the
#   data, user clicks two corners of a box around the GOOD data (selecting
#   both the time range AND the concentration band), then goFlux calculates
#   the flux immediately. Naturally handles interleaved streams — just draw
#   the box around the stream you want. Replaces obs.win + click.peak2 +
#   goFlux loop in one step.
#
# --- Automated alternatives (if click_flux_2d is too manual) ---
#
# separate_window_streams():
#   Separates two interleaved streams within a SINGLE observation window.
#   Simple state machine: big concentration jump → switch streams.
#
# obs_win_deinterleave():
#   Automated per-window deinterleaving. For each measurement: slices data
#   to window, calls separate_window_streams(), keeps the smoother stream,
#   adds Etime + chamber specs directly (bypasses obs.win). Use this if
#   you want to automate stream selection instead of clicking.
#
# --- Plotting & data management ---
#
# plot_obs_window_by_file():
#   Plots a single observation window colored by stream_id.
#
# plot_all_recovery_windows():
#   Loops over all auxfile entries, saves PNGs to a directory.
#
# remove_streams_from_data():
#   Remove rows by stream_id. Use after reviewing diagnostic plots.
#
# remove_files_from_data():
#   Remove ALL rows from specified source files.

library(ggplot2)

detect_clock_streams <- function(filepath) {
  # Reads a raw LGR .txt file to detect clock resets from TRUE write order.
  #
  # Why read the raw file instead of using import2RData output?
  # import2RData sorts by timestamp, so if a clock reset created overlapping
  # timestamps, the two streams get interleaved and we lose the true write
  # order. By reading the raw file, we see the data as the analyzer wrote it.
  #
  # Uses a "running max" approach: any row whose timestamp is >30 sec behind
  # the cumulative max timestamp is from the reset stream. This works whether
  # the two streams are in sequential blocks OR interspersed row-by-row.
  # Returns "ahead" (rows advancing the time frontier) vs "behind" (rows
  # that fall behind — from the clock reset).

  raw_lines <- readLines(filepath, warn = FALSE)

  # Find data lines: start with a date like M/D/YYYY or MM/DD/YYYY
  date_pattern <- "^\\s*\\d{1,2}/\\d{1,2}/\\d{4}\\s+\\d{1,2}:\\d{2}:\\d{2}"
  is_data <- grepl(date_pattern, raw_lines)

  if (sum(is_data) < 2) {
    return(list(has_reset = FALSE, streams = rep("ahead", sum(is_data)),
                stream_labels = "ahead",
                header_lines = raw_lines[!is_data], data_lines = raw_lines[is_data]))
  }

  # Everything before the first data line is "header"
  first_data <- min(which(is_data))
  header_lines <- raw_lines[seq_len(first_data - 1)]
  data_lines <- raw_lines[is_data]

  # Parse timestamps from each data line (first comma-separated field)
  ts_strings <- trimws(sub(",.*", "", data_lines))
  timestamps <- suppressWarnings(
    as.POSIXct(ts_strings, format = "%m/%d/%Y %H:%M:%OS", tz = "UTC")
  )

  # Drop any lines that failed to parse
  valid <- !is.na(timestamps)
  if (sum(valid) < 2) {
    return(list(has_reset = FALSE, streams = rep("ahead", length(data_lines)),
                stream_labels = "ahead",
                header_lines = header_lines, data_lines = data_lines))
  }

  # Running-max approach: rows that are >30 sec behind the cumulative max
  # are from the clock-reset stream. Works whether the two streams are in
  # sequential blocks or interspersed row-by-row throughout the file.
  ts_num <- as.numeric(timestamps)
  # "ahead" = at or advancing the running max; "behind" = reset stream
  streams <- rep("ahead", length(data_lines))

  valid_idx <- which(valid)
  valid_ts <- ts_num[valid_idx]
  running_max <- cummax(valid_ts)
  is_behind <- valid_ts < (running_max - 30)
  streams[valid_idx[is_behind]] <- "behind"

  has_reset <- any(is_behind)

  return(list(
    has_reset = has_reset,
    streams = streams,
    stream_labels = if (has_reset) c("ahead", "behind") else "ahead",
    header_lines = header_lines,
    data_lines = data_lines,
    timestamps = timestamps
  ))
}

import_lgr_tagged <- function(analyzer_path, needed_dates = NULL) {
  cat("  Importing LGR data from:", analyzer_path, "\n")

  # If specific dates provided, only look in those subdirectories
  if (!is.null(needed_dates)) {
    date_dirs <- file.path(analyzer_path, format(needed_dates, "%Y-%m-%d"))
    date_dirs <- date_dirs[dir.exists(date_dirs)]
    cat("  Targeting", length(date_dirs), "date directories\n")
  } else {
    date_dirs <- list.dirs(analyzer_path, recursive = FALSE)
  }

  if (length(date_dirs) == 0) {
    stop("No date directories found in ", analyzer_path)
  }

  # Extract any remaining zip files
  zip_files <- list.files(date_dirs, recursive = TRUE, pattern = "\\.zip$", full.names = TRUE)
  if (length(zip_files) > 0) {
    cat("  Extracting", length(zip_files), "zip files...\n")
    for (zf in zip_files) {
      tryCatch(
        unzip(zf, exdir = dirname(zf), overwrite = TRUE),
        error = function(e) cat("    Error extracting", basename(zf), ":", e$message, "\n")
      )
    }
  }

  # Find ONLY _f (flux) data files
  all_txt <- list.files(date_dirs, recursive = TRUE, pattern = "_f[0-9]+\\.txt$", full.names = TRUE)
  real_files <- all_txt[file.info(all_txt)$isdir == FALSE]
  real_files <- real_files[!grepl("\\._", real_files)]
  real_files <- real_files[file.size(real_files) > 0]
  cat("  Found", length(real_files), "flux data files (_f type only)\n")

  if (length(real_files) == 0) {
    stop("No _f*.txt data files found")
  }

  # Check serial numbers
  serial_numbers <- sapply(real_files, function(f) {
    line1 <- readLines(f, n = 1, warn = FALSE)
    sn <- regmatches(line1, regexpr("SN:[^ ]+", line1))
    if (length(sn) == 0) NA_character_ else sn
  })
  unique_sns <- unique(serial_numbers[!is.na(serial_numbers)])
  if (length(unique_sns) > 1) {
    cat("  WARNING: Multiple analyzer serial numbers found!\n")
    for (sn in unique_sns) {
      n_files <- sum(serial_numbers == sn, na.rm = TRUE)
      cat("    ", sn, ":", n_files, "files\n")
    }
    cat("  ALL files will be imported and tagged — you decide which to keep.\n")
  } else if (length(unique_sns) == 1) {
    cat("  All files from analyzer:", unique_sns, "\n")
  }

  # Import each file: first detect clock resets from raw file order,
  # then split into segments and import each segment separately via import2RData.
  # This ensures correct segment tagging even when import2RData sorts by time.
  cat("  Importing files (detecting clock resets from raw file order)...\n")
  all_data <- list()

  for (i in seq_along(real_files)) {
    f <- real_files[i]
    fname <- basename(f)
    sn <- serial_numbers[i]

    tryCatch({
      # Detect clock resets from the raw file (true write order, before
      # import2RData sorts by time and interspersed rows get mixed)
      stream_info <- detect_clock_streams(f)

      if (stream_info$has_reset) {
        # Clock reset found — two interspersed streams.
        # Split into separate files and import each independently.
        cat("    ", fname, ": CLOCK RESET — splitting into ahead/behind streams\n")

        for (label in stream_info$stream_labels) {
          stream_mask <- stream_info$streams == label
          stream_lines <- c(stream_info$header_lines, stream_info$data_lines[stream_mask])

          tmp <- tempfile(pattern = paste0("lgr_", label, "_"))
          dir.create(tmp)
          tmp_fname <- sub("\\.txt$", paste0("_", label, ".txt"), fname)
          writeLines(stream_lines, file.path(tmp, tmp_fname))

          stream_data <- import2RData(
            path = tmp,
            instrument = "UGGA",
            date.format = "mdy",
            timezone = "UTC",
            keep_all = FALSE,
            prec = c(0.35, 0.9, 200),
            merge = TRUE
          )
          unlink(tmp, recursive = TRUE)

          stream_data$clock_stream <- label
          stream_data$row_order <- which(stream_mask)
          stream_data$source_file <- fname
          stream_data$serial_number <- ifelse(is.na(sn), "unknown", sn)
          stream_data$stream_id <- paste0(fname, ":", label)

          all_data[[paste0(fname, "_", label)]] <- stream_data

          cat("      ", label, ":", nrow(stream_data), "rows,",
              as.character(min(stream_data$POSIX.time)), "->",
              as.character(max(stream_data$POSIX.time)), "\n")
        }
      } else {
        # No clock reset — import normally
        tmp <- tempfile(pattern = "lgr_single_")
        dir.create(tmp)
        file.copy(f, file.path(tmp, fname))

        one_file <- import2RData(
          path = tmp,
          instrument = "UGGA",
          date.format = "mdy",
          timezone = "UTC",
          keep_all = FALSE,
          prec = c(0.35, 0.9, 200),
          merge = TRUE
        )
        unlink(tmp, recursive = TRUE)

        one_file$clock_stream <- "all"
        one_file$row_order <- seq_len(nrow(one_file))
        one_file$source_file <- fname
        one_file$serial_number <- ifelse(is.na(sn), "unknown", sn)
        one_file$stream_id <- fname

        all_data[[fname]] <- one_file

        cat("    ", fname, ":", nrow(one_file), "rows |",
            as.character(min(one_file$POSIX.time)), "->",
            as.character(max(one_file$POSIX.time)), "|", sn, "\n")
      }
    }, error = function(e) {
      cat("    ", fname, ": IMPORT ERROR -", e$message, "\n")
    })
  }

  if (length(all_data) == 0) {
    stop("No files were successfully imported")
  }

  # Combine all segments/files into one dataframe
  combined <- bind_rows(all_data)
  combined <- combined %>% arrange(POSIX.time)

  n_streams <- length(unique(combined$stream_id))
  n_reset_rows <- sum(combined$clock_stream == "behind")
  cat("\n  Total imported:", nrow(combined), "rows from", length(all_data), "streams\n")
  cat("  Unique streams:", n_streams, "\n")
  if (n_reset_rows > 0) {
    cat("  Rows from clock-reset (behind) streams:", n_reset_rows, "\n")
    cat("  >>> Review diagnostic plots to determine which streams to keep.\n")
  }

  return(combined)
}

diagnose_overlaps <- function(raw_data) {
  cat("\n=== DATA STREAM DIAGNOSTIC ===\n\n")

  # Summarize by stream (file, or file:ahead / file:behind if clock reset)
  stream_summary <- raw_data %>%
    group_by(stream_id, source_file, clock_stream, serial_number) %>%
    summarise(
      n_rows = n(),
      start = min(POSIX.time),
      end = max(POSIX.time),
      .groups = "drop"
    ) %>%
    arrange(start)

  cat("Streams loaded:\n")
  print(stream_summary, n = Inf, width = 140)

  # Report clock resets (files split into ahead/behind streams)
  reset_files <- stream_summary %>%
    group_by(source_file) %>%
    filter(n() > 1) %>%
    ungroup()
  if (nrow(reset_files) > 0) {
    cat("\n*** CLOCK RESETS detected — interspersed streams in these files:\n")
    for (f in unique(reset_files$source_file)) {
      streams <- reset_files %>% filter(source_file == f)
      cat("  ", f, ":\n")
      for (j in seq_len(nrow(streams))) {
        cat("    ", streams$clock_stream[j], ":", streams$n_rows[j], "rows,",
            as.character(streams$start[j]), "->", as.character(streams$end[j]), "\n")
      }
    }
  }

  # Check for overlapping time ranges between streams
  cat("\nOverlap check between streams:\n")
  overlaps_found <- FALSE

  for (i in seq_len(nrow(stream_summary))) {
    for (j in seq(i + 1, nrow(stream_summary))) {
      if (j > nrow(stream_summary)) break
      a <- stream_summary[i, ]
      b <- stream_summary[j, ]
      if (a$start < b$end && b$start < a$end) {
        overlap_start <- max(a$start, b$start)
        overlap_end <- min(a$end, b$end)
        overlap_sec <- as.numeric(difftime(overlap_end, overlap_start, units = "secs"))
        if (overlap_sec > 60) {
          overlaps_found <- TRUE
          cat(sprintf("  OVERLAP (%.0f min): %s <-> %s\n",
                      overlap_sec / 60, a$stream_id, b$stream_id))
        }
      }
    }
  }

  if (!overlaps_found) {
    cat("  No overlapping time ranges found between streams.\n")
  }

  return(stream_summary)
}

separate_window_streams <- function(window_data, gas_col = NULL, threshold = NULL) {
  # Separates two interleaved streams within a SINGLE observation window.
  #
  # Only operates on the small slice of data around one measurement (~10 min).
  # Within this short window, the two streams are clearly distinct, so the
  # simple state machine (big jump → switch) works reliably. Chamber events
  # and convergence that cause problems globally don't affect a single window
  # where the streams are well-separated.
  #
  # Returns the data with stream_id updated to "stream_a" / "stream_b".
  # The smoother stream is reported so you know which to keep.

  # Auto-detect gas column
  if (is.null(gas_col)) {
    col_names <- names(window_data)
    gas_col <- col_names[grep("CO2.*dry|CO2d", col_names, ignore.case = TRUE)][1]
    if (is.na(gas_col)) {
      stop("Cannot find gas column. Available: ", paste(col_names, collapse = ", "))
    }
  }

  conc <- window_data[[gas_col]]
  n <- length(conc)
  if (n < 4) return(window_data)

  diffs <- abs(diff(conc))

  if (is.null(threshold)) {
    threshold <- median(diffs, na.rm = TRUE) * 3
  }

  # Simple state machine — works well within a single short window
  stream_label <- character(n)
  stream_label[1] <- "stream_a"
  for (i in 2:n) {
    if (!is.na(diffs[i - 1]) && diffs[i - 1] > threshold) {
      stream_label[i] <- ifelse(stream_label[i - 1] == "stream_a", "stream_b", "stream_a")
    } else {
      stream_label[i] <- stream_label[i - 1]
    }
  }

  window_data$stream_id <- stream_label

  # Report
  a_n <- sum(stream_label == "stream_a")
  b_n <- sum(stream_label == "stream_b")
  if (b_n > 0) {
    a_smooth <- mean(abs(diff(conc[stream_label == "stream_a"])), na.rm = TRUE)
    b_smooth <- mean(abs(diff(conc[stream_label == "stream_b"])), na.rm = TRUE)
    cat("    Separated:", a_n, "vs", b_n, "rows |",
        "roughness: stream_a =", round(a_smooth, 1), ", stream_b =", round(b_smooth, 1), "\n")
  }

  return(window_data)
}

obs_win_deinterleave <- function(raw_data, auxfile, gas_col = "CO2dry_ppm",
                                  obs_length = 300, shoulder = 300,
                                  threshold = NULL, keep = "smoother") {
  # Creates observation windows from interleaved data, deinterleaving PER WINDOW.
  #
  # For each measurement in the auxfile:
  #   1. Slices the raw data to [start - shoulder, start + obs_length + shoulder]
  #   2. Separates the two interleaved streams within that window
  #   3. Keeps the smoother stream (or lets you choose)
  #   4. Adds Etime and chamber specs (bypassing obs.win, which chokes on
  #      pre-sliced data)
  #
  # This avoids the problem of global separation failing at convergence points
  # or chamber events — each window is short enough that the streams are
  # clearly distinct.
  #
  # Args:
  #   raw_data: imported data with POSIX.time and gas columns (can have
  #             source_file etc — they'll be stripped automatically)
  #   auxfile: dataframe with UniqueID, start.time, Area, offset, etc.
  #   gas_col: gas column name (auto-detected if not in data)
  #   obs_length, shoulder: observation length and shoulder in seconds
  #   threshold: jump threshold for stream separation (auto if NULL)
  #   keep: which stream to keep per window:
  #         "smoother" (default) = auto-pick the stream with lower roughness
  #         "stream_a" or "stream_b" = always keep that one
  #         "ask" = print both and let you choose interactively
  #
  # Returns: named list of data frames (same format as obs.win output),
  #          each with Etime, UniqueID, and chamber spec columns.

  # Auto-detect gas column
  if (is.null(gas_col) || !gas_col %in% names(raw_data)) {
    col_names <- names(raw_data)
    gas_col <- col_names[grep("CO2.*dry|CO2d", col_names, ignore.case = TRUE)][1]
    if (is.na(gas_col)) {
      stop("Cannot find gas column. Available: ", paste(col_names, collapse = ", "))
    }
  }

  # Columns to strip (goFlux doesn't expect them)
  extra_cols <- intersect(names(raw_data),
                          c("source_file", "serial_number", "clock_stream",
                            "row_order", "stream_id"))

  cat("Processing", nrow(auxfile), "windows with per-window deinterleaving...\n")
  cat("  Gas column:", gas_col, "\n")
  cat("  Keep strategy:", keep, "\n\n")

  all_windows <- list()

  for (i in seq_len(nrow(auxfile))) {
    row <- auxfile[i, ]
    uid <- row$UniqueID
    start_t <- as.POSIXct(row$start.time, tz = "UTC")
    obs_offset <- if (!is.null(row$offset) && !is.na(row$offset)) row$offset else 0
    adj_start <- start_t + obs_offset
    window_start <- adj_start - shoulder
    window_end <- adj_start + obs_length + shoulder

    # Slice to this window
    w <- raw_data %>%
      filter(POSIX.time >= window_start, POSIX.time <= window_end)

    if (nrow(w) < 5) {
      cat("  ", uid, ": only", nrow(w), "rows — skipping\n")
      next
    }

    cat("  ", uid, ":", nrow(w), "rows in window\n")

    # Separate streams within this window
    w <- separate_window_streams(w, gas_col = gas_col, threshold = threshold)

    n_streams <- length(unique(w$stream_id))

    if (n_streams > 1) {
      # Pick which stream to keep
      a_data <- w %>% filter(stream_id == "stream_a")
      b_data <- w %>% filter(stream_id == "stream_b")
      a_rough <- mean(abs(diff(a_data[[gas_col]])), na.rm = TRUE)
      b_rough <- mean(abs(diff(b_data[[gas_col]])), na.rm = TRUE)

      if (keep == "smoother") {
        chosen <- ifelse(a_rough <= b_rough, "stream_a", "stream_b")
        cat("    Keeping", chosen, "(roughness:",
            round(ifelse(chosen == "stream_a", a_rough, b_rough), 1), "vs",
            round(ifelse(chosen == "stream_a", b_rough, a_rough), 1), ")\n")
      } else if (keep %in% c("stream_a", "stream_b")) {
        chosen <- keep
      } else {
        # "ask" — show info and let user choose
        cat("    stream_a:", nrow(a_data), "rows, roughness:", round(a_rough, 1), "\n")
        cat("    stream_b:", nrow(b_data), "rows, roughness:", round(b_rough, 1), "\n")
        cat("    Keep which? (a/b): ")
        ans <- readline()
        chosen <- ifelse(grepl("b", ans, ignore.case = TRUE), "stream_b", "stream_a")
      }

      w <- w %>% filter(stream_id == chosen)
    }

    # Strip extra columns
    w_clean <- w %>% select(-any_of(c(extra_cols, "stream_id")))

    # Add columns that obs.win normally provides: elapsed time + chamber specs.
    # (obs.win chokes on pre-sliced data and can produce windows without Etime;
    # building the window manually is more reliable.)
    w_clean$UniqueID <- uid
    w_clean$Etime <- as.numeric(difftime(w_clean$POSIX.time, adj_start, units = "secs"))
    w_clean$Area <- row$Area
    w_clean$offset <- obs_offset
    w_clean$Vcham <- row$Vcham
    w_clean$Vtube <- row$Vtube
    w_clean$Vinst <- row$Vinst
    w_clean$Vtot <- row$Vtot
    w_clean$Tcham <- row$Tcham
    w_clean$Pcham <- row$Pcham

    all_windows[[uid]] <- w_clean
  }

  cat("\nProcessed", length(all_windows), "windows successfully.\n")
  return(all_windows)
}

click_flux_2d <- function(raw_data, auxfile, gas_col = "CO2dry_ppm",
                           obs_length = 300, shoulder = 300,
                           plot.lim = NULL, save.plots = NULL) {
  # Interactive 2D flux measurement: click a box around the good data.
  #
  # Replaces obs.win + click.peak2 + goFlux loop with one interactive step.
  # For each measurement:
  #   1. Plots the full observation window (shoulder to shoulder)
  #   2. User clicks TWO corners of a bounding box around the GOOD data
  #      — this selects both the time range AND the concentration band,
  #        naturally excluding any interleaved second stream
  #   3. goFlux calculates the flux on the selected data
  #
  # Blue vertical lines show the expected measurement start and end.
  # Right-click or press Escape to skip a measurement.
  #
  # Args:
  #   raw_data: imported data (can include tagging columns, they're stripped)
  #   auxfile: dataframe with UniqueID, start.time, Area, offset, Vtot, etc.
  #   gas_col: which gas column to use for flux calculation
  #   obs_length, shoulder: observation length and shoulder in seconds
  #   plot.lim: y-axis limits for plots, e.g. c(200, 1200)
  #   save.plots: directory to save selection plots (NULL = don't save)
  #
  # Returns: data frame of goFlux results (pass to best.flux to pick model)

  # Auto-detect gas column
  if (is.null(gas_col) || !gas_col %in% names(raw_data)) {
    col_names <- names(raw_data)
    gas_col <- col_names[grep("CO2.*dry|CO2d", col_names, ignore.case = TRUE)][1]
    if (is.na(gas_col)) {
      stop("Cannot find gas column. Available: ", paste(col_names, collapse = ", "))
    }
  }

  # Columns to strip (from import_lgr_tagged tagging)
  extra_cols <- intersect(names(raw_data),
                          c("source_file", "serial_number", "clock_stream",
                            "row_order", "stream_id"))

  if (!is.null(save.plots)) {
    dir.create(save.plots, showWarnings = FALSE, recursive = TRUE)
    if (!dir.exists(save.plots)) {
      cat("WARNING: Could not create save directory:", normalizePath(save.plots, mustWork = FALSE), "\n")
      cat("  Plots will not be saved.\n")
      save.plots <- NULL
    }
  }

  # Pre-compute the data time range once (for diagnostics on 0-row windows)
  data_time_range <- range(raw_data$POSIX.time, na.rm = TRUE)

  # Open a native Quartz window — locator() needs this, NOT the RStudio viewer.
  # The RStudio Plots pane doesn't support locator() properly and dev.copy()
  # fails with "QuartzBitmap_Output - unable to open file".
  quartz(title = "click_flux_2d — click box corners around good data",
         width = 10, height = 5)
  interactive_dev <- dev.cur()

  cat("=== Interactive 2D flux selection ===\n")
  cat("A native Quartz window has opened for interactive clicking.\n")
  cat("For each measurement: click BOTTOM-LEFT then TOP-RIGHT of the good data.\n")
  cat("Right-click or Escape to skip a measurement.\n\n")

  all_results <- list()

  for (i in seq_len(nrow(auxfile))) {
    row <- auxfile[i, ]
    uid <- row$UniqueID
    start_t <- as.POSIXct(row$start.time, tz = "UTC")
    obs_offset <- if (!is.null(row$offset) && !is.na(row$offset)) row$offset else 0
    adj_start <- start_t + obs_offset

    # Slice to window
    w <- raw_data %>%
      filter(POSIX.time >= adj_start - shoulder,
             POSIX.time <= adj_start + obs_length + shoulder) %>%
      select(-any_of(extra_cols))

    if (nrow(w) < 5) {
      # Diagnostic: show expected window vs actual data range to debug mismatches
      cat(uid, ": only", nrow(w), "rows — skipping\n")
      cat("  Expected window: ", format(adj_start - shoulder, "%Y-%m-%d %H:%M:%S"),
          " to ", format(adj_start + obs_length + shoulder, "%Y-%m-%d %H:%M:%S"), "\n")
      cat("  Raw data spans:  ", format(data_time_range[1], "%Y-%m-%d %H:%M:%S"),
          " to ", format(data_time_range[2], "%Y-%m-%d %H:%M:%S"), "\n")
      # Show closest data to help diagnose timezone issues
      time_diffs <- abs(as.numeric(difftime(raw_data$POSIX.time, adj_start, units = "hours")))
      closest_idx <- which.min(time_diffs)
      cat("  Closest data point: ", format(raw_data$POSIX.time[closest_idx], "%Y-%m-%d %H:%M:%S"),
          " (", round(time_diffs[closest_idx], 1), " hours away)\n\n")
      next
    }

    # Ensure the interactive Quartz window is still active (reopen if closed)
    tryCatch({
      dev.set(interactive_dev)
    }, error = function(e) {
      quartz(title = "click_flux_2d — click box corners around good data",
             width = 10, height = 5)
      interactive_dev <<- dev.cur()
    })

    # Plot the full window
    conc <- w[[gas_col]]
    ylim <- if (!is.null(plot.lim)) plot.lim else range(conc, na.rm = TRUE) * c(0.98, 1.02)

    plot(w$POSIX.time, conc,
         main = paste0("[", i, "/", nrow(auxfile), "]  ", uid),
         xlab = "Time (UTC)", ylab = gas_col,
         pch = 16, cex = 0.5, ylim = ylim)
    abline(v = adj_start, col = "blue", lwd = 1.5)
    abline(v = adj_start + obs_length, col = "blue", lwd = 1.5, lty = 2)
    legend("topright", c("obs start", "obs end"), col = "blue",
           lty = c(1, 2), lwd = 1.5, cex = 0.8)

    cat("[", i, "/", nrow(auxfile), "]", uid, ":", nrow(w), "pts — click box corners\n")

    pts <- tryCatch(locator(2), error = function(e) NULL)

    if (is.null(pts) || length(pts$x) < 2) {
      cat("  Skipped.\n\n")
      next
    }

    # Filter to bounding box
    x_min <- as.POSIXct(min(pts$x), origin = "1970-01-01", tz = "UTC")
    x_max <- as.POSIXct(max(pts$x), origin = "1970-01-01", tz = "UTC")
    y_min <- min(pts$y)
    y_max <- max(pts$y)

    selected <- w %>%
      filter(POSIX.time >= x_min, POSIX.time <= x_max,
             .data[[gas_col]] >= y_min, .data[[gas_col]] <= y_max)

    # Re-plot with selection highlighted (on interactive device)
    plot(w$POSIX.time, conc,
         main = paste0(uid, " — selected: ", nrow(selected), " pts"),
         xlab = "Time (UTC)", ylab = gas_col,
         pch = 16, cex = 0.5, col = "gray70", ylim = ylim)
    points(selected$POSIX.time, selected[[gas_col]],
           pch = 16, cex = 0.5, col = "dodgerblue")
    rect(x_min, y_min, x_max, y_max, border = "red", lwd = 2)
    abline(v = adj_start, col = "blue", lwd = 1.5)
    abline(v = adj_start + obs_length, col = "blue", lwd = 1.5, lty = 2)

    # Save plot to PNG using a SEPARATE device (avoids dev.copy/QuartzBitmap crash).
    # Uses cairo type to avoid conflicts with the interactive quartz() window.
    # Wrapped in tryCatch so a save failure doesn't crash the loop.
    if (!is.null(save.plots)) {
      tryCatch({
        png_type <- if (capabilities("cairo")) "cairo" else "quartz"
        png(file.path(save.plots, paste0(uid, "_selected.png")),
            width = 1000, height = 500, res = 120, type = png_type)
        # Redraw on the PNG device
        plot(w$POSIX.time, conc,
             main = paste0(uid, " — selected: ", nrow(selected), " pts"),
             xlab = "Time (UTC)", ylab = gas_col,
             pch = 16, cex = 0.5, col = "gray70", ylim = ylim)
        points(selected$POSIX.time, selected[[gas_col]],
               pch = 16, cex = 0.5, col = "dodgerblue")
        rect(x_min, y_min, x_max, y_max, border = "red", lwd = 2)
        abline(v = adj_start, col = "blue", lwd = 1.5)
        abline(v = adj_start + obs_length, col = "blue", lwd = 1.5, lty = 2)
        dev.off()  # closes PNG device; returns to the interactive Quartz window
      }, error = function(e) {
        tryCatch(dev.off(), error = function(e2) NULL)  # clean up orphaned PNG device
        cat("  Warning: could not save plot:", e$message, "\n")
      })
    }

    if (nrow(selected) < 5) {
      cat("  Too few points (", nrow(selected), "). Skipping.\n\n")
      next
    }

    # Add goFlux required columns.
    # flag = 1 marks all rows as "use" (normally added by click.peak2).
    # Chamber spec columns come from the auxfile row.
    selected$UniqueID <- uid
    selected$flag <- 1
    selected$Etime <- as.numeric(difftime(selected$POSIX.time, adj_start, units = "secs"))
    selected$Area <- row$Area
    selected$Vcham <- row$Vcham
    selected$Vtube <- row$Vtube
    selected$Vinst <- row$Vinst
    selected$Vtot <- row$Vtot
    selected$offset <- obs_offset
    selected$Tcham <- row$Tcham
    selected$Pcham <- row$Pcham

    # Calculate flux
    tryCatch({
      flux_result <- goFlux(selected, gas_col)
      all_results[[uid]] <- flux_result
      cat("  Flux calculated OK\n\n")
    }, error = function(e) {
      cat("  goFlux error:", e$message, "\n\n")
    })

    Sys.sleep(0.5)
  }

  # Clean up: close the interactive Quartz window
  tryCatch(dev.off(interactive_dev), error = function(e) NULL)

  if (length(all_results) > 0) {
    combined <- bind_rows(all_results)
    cat("\nSuccessfully processed", length(all_results), "of",
        nrow(auxfile), "measurements.\n")
    return(combined)
  } else {
    cat("\nNo measurements processed.\n")
    return(NULL)
  }
}

plot_obs_window_by_file <- function(raw_data, auxfile_row, gas_col = "CO2dry_ppm",
                                     shoulder = 300, obs_length = 300) {
  # Plot a single observation window with points colored by stream_id.
  # Use this to see whether stream separation worked correctly.

  uid <- auxfile_row$UniqueID
  start_t <- as.POSIXct(auxfile_row$start.time, tz = "UTC")
  window_start <- start_t - shoulder
  window_end <- start_t + obs_length + shoulder

  # Filter data to this window
  window_data <- raw_data %>%
    filter(POSIX.time >= window_start, POSIX.time <= window_end)

  if (nrow(window_data) == 0) {
    cat("No data in window for", uid, "\n")
    return(invisible(NULL))
  }

  # Discover the gas column if needed
  if (!gas_col %in% names(window_data)) {
    col_names <- names(window_data)
    gas_col <- col_names[grep("CO2.*dry|CO2d", col_names, ignore.case = TRUE)][1]
    if (is.na(gas_col)) {
      stop("Cannot find CO2 column. Available: ", paste(col_names, collapse = ", "))
    }
  }

  n_streams <- length(unique(window_data$stream_id))
  cat("Window for", uid, ":", nrow(window_data), "rows from", n_streams, "stream(s)\n")

  p <- ggplot(window_data, aes(x = POSIX.time, y = .data[[gas_col]], color = stream_id)) +
    geom_point(size = 1, alpha = 0.6) +
    geom_vline(xintercept = as.numeric(start_t), color = "blue", linewidth = 0.8) +
    geom_vline(xintercept = as.numeric(start_t + obs_length), color = "blue", linewidth = 0.8) +
    labs(
      title = uid,
      subtitle = paste(n_streams, "stream(s) in window"),
      x = "Time", y = gas_col, color = "Stream (file:segment)"
    ) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 7))

  print(p)
  return(invisible(window_data))
}

plot_all_recovery_windows <- function(raw_data, auxfile_df, gas_col = "CO2dry_ppm",
                                       shoulder = 300, obs_length = 300,
                                       save_dir = NULL) {
  if (!is.null(save_dir)) {
    dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
  }

  for (i in seq_len(nrow(auxfile_df))) {
    row <- auxfile_df[i, ]

    if (!is.null(save_dir)) {
      png(file.path(save_dir, paste0(row$UniqueID, "_by_stream.png")),
          width = 1200, height = 600, res = 120)
    }

    plot_obs_window_by_file(raw_data, row, gas_col, shoulder, obs_length)

    if (!is.null(save_dir)) {
      dev.off()
    }
  }

  if (!is.null(save_dir)) {
    cat("Saved", nrow(auxfile_df), "diagnostic plots to", save_dir, "\n")
  }
}

remove_streams_from_data <- function(raw_data, streams_to_remove) {
  # Remove rows from specified stream_ids (file:segment combinations).
  # streams_to_remove: character vector, e.g. c("micro_2022-10-25_f0000.txt:seg2")
  before <- nrow(raw_data)
  raw_data <- raw_data %>% filter(!stream_id %in% streams_to_remove)
  after <- nrow(raw_data)
  cat("Removed", before - after, "rows from streams:",
      paste(streams_to_remove, collapse = ", "), "\n")
  cat("Remaining:", after, "rows from streams:",
      paste(unique(raw_data$stream_id), collapse = ", "), "\n")
  return(raw_data)
}

remove_files_from_data <- function(raw_data, files_to_remove) {
  # Remove ALL rows from specified source files (all segments).
  before <- nrow(raw_data)
  raw_data <- raw_data %>% filter(!source_file %in% files_to_remove)
  after <- nrow(raw_data)
  cat("Removed", before - after, "rows from files:",
      paste(files_to_remove, collapse = ", "), "\n")
  cat("Remaining:", after, "rows from files:",
      paste(unique(raw_data$source_file), collapse = ", "), "\n")
  return(raw_data)
}

# =============================================================================
# STEP 6: Process by analyzer group (interactive)
# =============================================================================

cat("\n===============================================================================\n")
cat("INTERACTIVE RECOVERY\n")
cat("===============================================================================\n\n")
cat("The following sections process each analyzer group interactively.\n")
cat("Each section uses click_flux_2d(): click a box around the good data\n")
cat("to select the time range AND concentration band in one step.\n")
cat("Uncomment and run each section as needed.\n\n")

# --- Summary of what to process ---
cat("Groups to process:\n")
recovery_table %>%
  count(analyzer_dir, measurement_type) %>%
  print()

cat("\n")
cat("To proceed with recovery, uncomment the relevant section below and run\n")
cat("interactively in RStudio. Each section:\n")
cat("  1. Imports raw data (tagged by source file, detecting clock resets)\n")
cat("  2. Diagnoses overlaps — review and optionally remove bad files\n")
cat("  3. Runs click_flux_2d: plots each measurement, you click a 2D box\n")
cat("     around the good data (selecting time range + concentration band),\n")
cat("     then goFlux calculates the flux immediately\n")
cat("  4. Saves results to intermediate/rescue/\n\n")

# ============================================================================
# SECTION A: LGR2 Trees — 2 measurements (Oct 2022 SRS5 CWD)
# ============================================================================
#
# Uncomment to run:
#
# cat("=== Processing LGR2 Tree Measurements (2 recoverable) ===\n")
#
# lgr2_recovery <- recovery_table %>% filter(analyzer_dir == "LGR2")
# cat("Measurements to recover:\n")
# print(lgr2_recovery %>% select(flux_id, plot, raw_date, component))
#
# # Import raw LGR2 data (tagged by source file, detecting clock resets)
# lgr2_raw <- import_lgr_tagged(
#   "data/analyzer/LGR_GLA131/LGR2",
#   needed_dates = unique(lgr2_recovery$raw_date)
# )
#
# # Diagnose overlaps between files
# lgr2_file_summary <- diagnose_overlaps(lgr2_raw)
#
# # Build auxfile for recovery measurements
# lgr2_aux <- lgr2_recovery %>%
#   transmute(
#     UniqueID = flux_id, DATE, TIME,
#     start.time = as.POSIXct(start.time, tz = "UTC"),
#     Area, offset, Vcham, Vtube, Vinst, Vtot, Tcham, Pcham
#   )
#
# # >>> OPTIONAL: remove entire bad files after reviewing diagnostic output.
# # lgr2_raw <- remove_files_from_data(lgr2_raw, c("filename_to_remove.txt"))
#
# # Interactive: for each measurement, click a 2D box around the good data.
# # This selects the time range AND filters out any interleaved stream.
# results_lgr2 <- click_flux_2d(
#   lgr2_raw, lgr2_aux,
#   gas_col = "CO2dry_ppm",
#   obs_length = 300, shoulder = 300,
#   plot.lim = c(200, 1200),
#   save.plots = "intermediate/rescue/lgr2_recovery_plots"
# )
#
# if (!is.null(results_lgr2)) {
#   best_lgr2 <- best.flux(results_lgr2)
#   write_csv(best_lgr2, "intermediate/rescue/recovered_lgr2_tree_fluxes.csv")
#   cat("Saved", nrow(best_lgr2), "recovered LGR2 fluxes\n")
# }

# ============================================================================
# SECTION B: LGR3 Trees — 20 measurements (Oct 2022 SRS5/SRS6/BL60)
# ============================================================================
#
# Uncomment to run:
#
# cat("=== Processing LGR3 Tree Measurements (20 recoverable) ===\n")
#
# lgr3_tree_recovery <- recovery_table %>%
#   filter(analyzer_dir == "LGR3", measurement_type == "tree")
# cat("Measurements to recover:\n")
# print(lgr3_tree_recovery %>% select(flux_id, plot, raw_date, component))
#
# # Import raw LGR3 data (tagged by source file, detecting clock resets)
# # Includes surface dates too so Section C can reuse lgr3_raw
# lgr3_all_dates <- unique(c(
#   lgr3_tree_recovery$raw_date,
#   (recovery_table %>% filter(analyzer_dir == "LGR3", measurement_type == "surface"))$raw_date
# ))
# lgr3_raw <- import_lgr_tagged(
#   "data/analyzer/LGR_GLA131/LGR3",
#   needed_dates = lgr3_all_dates
# )
#
# # Diagnose overlaps between files
# lgr3_file_summary <- diagnose_overlaps(lgr3_raw)
#
# # Build auxfile
# lgr3_tree_aux <- lgr3_tree_recovery %>%
#   transmute(
#     UniqueID = flux_id, DATE, TIME,
#     start.time = as.POSIXct(start.time, tz = "UTC"),
#     Area, offset, Vcham, Vtube, Vinst, Vtot, Tcham, Pcham
#   )
#
# # >>> OPTIONAL: remove entire bad files after reviewing diagnostic output.
# # lgr3_raw <- remove_files_from_data(lgr3_raw, c("filename_to_remove.txt"))
#
# # Interactive: for each measurement, click a 2D box around the good data.
# # This selects the time range AND filters out any interleaved stream.
# results_lgr3_tree <- click_flux_2d(
#   lgr3_raw, lgr3_tree_aux,
#   gas_col = "CO2dry_ppm",
#   obs_length = 300, shoulder = 300,
#   plot.lim = c(200, 1200),
#   save.plots = "intermediate/rescue/lgr3_tree_recovery_plots"
# )
#
# if (!is.null(results_lgr3_tree)) {
#   best_lgr3_tree <- best.flux(results_lgr3_tree)
#   write_csv(best_lgr3_tree, "intermediate/rescue/recovered_lgr3_tree_fluxes.csv")
#   cat("Saved", nrow(best_lgr3_tree), "recovered LGR3 tree fluxes\n")
# }

# ============================================================================
# SECTION C: LGR3 Surface — BL60 Water measurements
# ============================================================================
#
# INVESTIGATION NOTES:
# The auxfile lists these as 2023-03-22, but all 3 LGR analyzers have
# essentially no data on that date (only brief power-on snippets).
# Diagnostic analysis found the actual data in a NESTED continuation file:
#   LGR3/2023-03-16/micro_2023-03-16_f0001.txt/micro_2023-03-16_f0001.txt
# This file covers 12:31–15:29 UTC on March 16 — the same day as the
# BL60 soil measurements. The auxfile TIME column values (15:08–16:00)
# are already in UTC (the +4h EDT conversion in start.time was incorrect).
# Data cuts off at 15:29, so only measurements 168–171 have enough data.
#
# Uncomment to run:
#
# cat("=== Processing LGR3 BL60 Water Measurements ===\n")
# cat("Date corrected: 2023-03-22 → 2023-03-16\n")
# cat("Data source: LGR3 f0001.txt (nested), 12:31–15:29 UTC\n\n")
#
# # Import the continuation file (f0001) — must copy to temp dir for import2RData
# water_data_file <- "data/analyzer/LGR_GLA131/LGR3/2023-03-16/micro_2023-03-16_f0001.txt/micro_2023-03-16_f0001.txt"
# tmp_dir <- tempfile(pattern = "lgr3_water_")
# dir.create(tmp_dir)
# file.copy(water_data_file, file.path(tmp_dir, "micro_2023-03-16_f0001.txt"))
#
# lgr3_water_raw <- import2RData(
#   path = tmp_dir,
#   instrument = "UGGA",
#   date.format = "mdy",
#   timezone = "UTC",
#   keep_all = FALSE,
#   prec = c(0.35, 0.9, 200),
#   merge = TRUE
# )
# unlink(tmp_dir, recursive = TRUE)
# cat("Imported", nrow(lgr3_water_raw), "rows from LGR3 March 16 f0001\n")
# cat("Time range:", format(range(lgr3_water_raw$POSIX.time), "%H:%M:%S"), "\n\n")
#
# # Build corrected auxfile:
# #   - Date: March 16 (not March 22)
# #   - start.time: use TIME column as UTC (no timezone conversion needed)
# #   - Only include measurements 168–171 (data ends at 15:29, later ones cut off)
# lgr3_water_aux <- read_csv("intermediate/auxfiles/soilwater_auxfile_lgr3.csv",
#                             show_col_types = FALSE) %>%
#   filter(grepl("BL60_Water", UniqueID)) %>%
#   mutate(
#     DATE = as.Date("2023-03-16"),
#     start.time = as.POSIXct(paste("2023-03-16", format(TIME, "%H:%M:%S")), tz = "UTC")
#   ) %>%
#   filter(start.time <= as.POSIXct("2023-03-16 15:24:00", tz = "UTC"))
#
# cat("Recoverable water measurements (data available):\n")
# print(lgr3_water_aux %>% select(UniqueID, start.time))
#
# # Interactive: click a 2D box around the good data for each measurement
# results_lgr3_water <- click_flux_2d(
#   lgr3_water_raw, lgr3_water_aux,
#   gas_col = "CO2dry_ppm",
#   obs_length = 300, shoulder = 300,
#   plot.lim = c(200, 1200),
#   save.plots = NULL
# )
#
# if (!is.null(results_lgr3_water)) {
#   best_lgr3_water <- best.flux(results_lgr3_water)
#   write_csv(best_lgr3_water, "intermediate/rescue/recovered_lgr3_water_fluxes.csv")
#   cat("Saved", nrow(best_lgr3_water), "recovered LGR3 water fluxes\n")
# }

# ============================================================================
# SECTION D: CH4 recovery for the 7 CO2-recovered measurements
# ============================================================================
#
# Sections B and C recovered CO2 fluxes only (click_flux_2d was run with
# gas_col = "CO2dry_ppm"). The LGR records CH4 and CO2 simultaneously,
# so the same raw data files contain CH4. This section processes CH4
# for the same 7 measurements.
#
# D1: 3 BL60 tree stems (Oct 25, 2022) — same data as Section B
# D2: 4 BL60 water (Mar 16, 2023) — same data as Section C
#
# Uncomment to run:

# --- D1: CH4 for 3 BL60 tree stems ---
#
# cat("=== Processing CH4 for 3 recovered BL60 tree stems ===\n")
#
# # Use same LGR3 Oct 25 data as Section B
# # (If lgr3_raw is still in memory from Section B, skip this import)
# lgr3_tree_ch4_dates <- c("2022-10-25")
# lgr3_raw_ch4 <- import_lgr_tagged(
#   "data/analyzer/LGR_GLA131/LGR3",
#   needed_dates = lgr3_tree_ch4_dates
# )
#
# # Build auxfile for just the 3 recovered tree measurements
# tree_ch4_ids <- c("Oct_22_237_BL60_stem", "Oct_22_238_BL60_stem", "Oct_22_239_BL60_stem")
# lgr3_tree_ch4_aux <- recovery_table %>%
#   filter(flux_id %in% tree_ch4_ids) %>%
#   transmute(
#     UniqueID = flux_id, DATE, TIME,
#     start.time = as.POSIXct(start.time, tz = "UTC"),
#     Area, offset, Vcham, Vtube, Vinst, Vtot, Tcham, Pcham
#   )
# cat("Tree CH4 measurements to process:\n")
# print(lgr3_tree_ch4_aux %>% select(UniqueID, start.time))
#
# # Interactive: click a 2D box around the CH4 data for each measurement
# results_tree_ch4 <- click_flux_2d(
#   lgr3_raw_ch4, lgr3_tree_ch4_aux,
#   gas_col = "CH4dry_ppm",
#   obs_length = 300, shoulder = 300,
#   plot.lim = c(1.5, 15),
#   save.plots = "intermediate/rescue/lgr3_tree_ch4_recovery_plots"
# )
#
# if (!is.null(results_tree_ch4)) {
#   best_tree_ch4 <- best.flux(results_tree_ch4)
#   write_csv(best_tree_ch4, "intermediate/rescue/recovered_lgr3_tree_CH4_fluxes.csv")
#   cat("Saved", nrow(best_tree_ch4), "recovered LGR3 tree CH4 fluxes\n")
# }

# --- D2: CH4 for 4 BL60 water ---
#
# cat("=== Processing CH4 for 4 recovered BL60 water measurements ===\n")
# cat("Date: 2023-03-16 (corrected from 2023-03-22)\n\n")
#
# # Import continuation file (same as Section C)
# water_data_file <- "data/analyzer/LGR_GLA131/LGR3/2023-03-16/micro_2023-03-16_f0001.txt/micro_2023-03-16_f0001.txt"
# tmp_dir <- tempfile(pattern = "lgr3_water_ch4_")
# dir.create(tmp_dir)
# file.copy(water_data_file, file.path(tmp_dir, "micro_2023-03-16_f0001.txt"))
#
# lgr3_water_ch4_raw <- import2RData(
#   path = tmp_dir,
#   instrument = "UGGA",
#   date.format = "mdy",
#   timezone = "UTC",
#   keep_all = FALSE,
#   prec = c(0.35, 0.9, 200),
#   merge = TRUE
# )
# unlink(tmp_dir, recursive = TRUE)
# cat("Imported", nrow(lgr3_water_ch4_raw), "rows from LGR3 March 16 f0001\n\n")
#
# # Build corrected auxfile (same date correction as Section C)
# lgr3_water_ch4_aux <- read_csv("intermediate/auxfiles/soilwater_auxfile_lgr3.csv",
#                                 show_col_types = FALSE) %>%
#   filter(grepl("BL60_Water", UniqueID)) %>%
#   mutate(
#     DATE = as.Date("2023-03-16"),
#     start.time = as.POSIXct(paste("2023-03-16", format(TIME, "%H:%M:%S")), tz = "UTC")
#   ) %>%
#   filter(start.time <= as.POSIXct("2023-03-16 15:24:00", tz = "UTC"))
#
# cat("Water CH4 measurements to process:\n")
# print(lgr3_water_ch4_aux %>% select(UniqueID, start.time))
#
# # Interactive: click a 2D box around the CH4 data for each measurement
# results_water_ch4 <- click_flux_2d(
#   lgr3_water_ch4_raw, lgr3_water_ch4_aux,
#   gas_col = "CH4dry_ppm",
#   obs_length = 300, shoulder = 300,
#   plot.lim = c(1.5, 15),
#   save.plots = NULL
# )
#
# if (!is.null(results_water_ch4)) {
#   best_water_ch4 <- best.flux(results_water_ch4)
#   write_csv(best_water_ch4, "intermediate/rescue/recovered_lgr3_water_CH4_fluxes.csv")
#   cat("Saved", nrow(best_water_ch4), "recovered LGR3 water CH4 fluxes\n")
# }

# --- After running D1 and D2, integrate CH4 results: ---
#
# To integrate CH4 results into the assembly pipeline, run:
#
# library(readr)
# library(dplyr)
#
# existing_ch4 <- read_csv("intermediate/rescue/ALL_RESCUED_CH4_BEST_FLUX.csv", show_col_types=FALSE)
#
# new_tree_ch4 <- read_csv("intermediate/rescue/recovered_lgr3_tree_CH4_fluxes.csv", show_col_types=FALSE)
# new_water_ch4 <- read_csv("intermediate/rescue/recovered_lgr3_water_CH4_fluxes.csv", show_col_types=FALSE)
#
# combined_ch4 <- bind_rows(existing_ch4, new_tree_ch4, new_water_ch4)
# cat("CH4 rescue file:", nrow(existing_ch4), "->", nrow(combined_ch4), "rows\n")
#
# file.copy("intermediate/rescue/ALL_RESCUED_CH4_BEST_FLUX.csv",
#           "intermediate/rescue/ALL_RESCUED_CH4_BEST_FLUX_backup.csv", overwrite=TRUE)
# write_csv(combined_ch4, "intermediate/rescue/ALL_RESCUED_CH4_BEST_FLUX.csv")
#
# # Then re-run: source("code/05_integration/assemble_clean_dataset.R")

# ============================================================================
# STEP 7: Summary
# ============================================================================

cat("\n===============================================================================\n")
cat("RECOVERY SUMMARY\n")
cat("===============================================================================\n\n")

cat("Failed measurements:     ", nrow(failed), "\n")
cat("With auxfile entry:      ", has_auxfile, "\n")
cat("With raw data available: ", n_recoverable, "\n")
cat("No raw data (unrecoverable): ", n_no_raw, "\n\n")

cat("Next steps for", n_recoverable, "recoverable measurements:\n")
cat("  1. Open this script in RStudio\n")
cat("  2. Uncomment each SECTION (A=LGR2 trees, B=LGR3 trees, C=LGR3 surface)\n")
cat("  3. Run interactively — click_flux_2d needs a graphics device\n")
cat("  4. Recovered fluxes will be saved to intermediate/rescue/\n")
cat("  5. Re-run assemble_clean_dataset.R to integrate recovered fluxes\n")
cat("     (you will need to add the recovery files as additional inputs)\n\n")

cat("For the", no_auxfile, "measurements WITHOUT auxfile entries:\n")
cat("  These need new auxfiles built from field notes before recovery.\n")
cat("  Check data/field_notes/ for start times and chamber specs.\n")
cat("  Most are Picarro trees (Mar 2022) and LGR trees (Mar 2022/2023).\n\n")

# Check if any recovery files already exist
recovery_files <- list.files("intermediate/rescue/", pattern = "^recovered_", full.names = TRUE)
if (length(recovery_files) > 0) {
  cat("Existing recovery files found:\n")
  for (f in recovery_files) {
    cat("  ", f, "\n")
  }
}

cat("\n=== DONE ===\n")
