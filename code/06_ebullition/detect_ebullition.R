# =============================================================================
# EBULLITION DETECTION IN RAW ANALYZER DATA
# =============================================================================
# Finds all chamber-placement periods in continuous LGR/Picarro data
# (segments of sustained CH4 increase = diffusive flux), then identifies
# abrupt upward jumps (>= 0.1 ppm) as ebullition events within those traces.
#
# Approach:
#   1. Load raw analyzer files for each water-measurement day
#   2. Segment the day into "rising periods" where CH4 is trending upward
#      (candidate chamber placements on water)
#   3. Within each placement, fit a linear model (diffusive baseline)
#   4. Detect point-to-point upward jumps >= JUMP_THRESH as ebullition
#   5. Plot each placement trace: raw CH4, linear fit, jump locations
#   6. Export placement + ebullition summary tables
# =============================================================================

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(zoo)
library(patchwork)   # for combining panels

# ---- CONFIG -----------------------------------------------------------------

# Chamber placement detection — specified in SECONDS, converted to points
# per instrument at runtime (see scale_params())
SMOOTH_SEC       <- 35    # seconds for smoothing window to find rising segments
MIN_RISE_SEC     <- 120   # minimum duration (sec) to call a segment a placement
MIN_RISE_PPM     <- 0.005 # minimum total CH4 increase over the segment (ppm)

# Ebullition detection
JUMP_THRESH      <- 0.10  # ppm; point-to-point upward CH4 jump to flag
TRIM_SEC         <- 20    # seconds to exclude from start and end of each trace
PLOT_CONTEXT_SEC <- 120   # seconds of context to show before/after each trace
MAX_TRACE_SEC    <- 600   # split long placements into chunks of this length (10 min)
WATER_BLOCK_PAD  <- 3600  # seconds of padding around water block for trace inclusion

# Manual exclusions (analyzer artifacts, not real ebullition)
# NOTE: IDs may shift when traces are split; match by site + time window instead
EXCLUDE_WINDOWS <- list(
  list(site = "CP40", date = "2023-03-15", after = "12:19", before = "12:27")  # 1.16 ppm analyzer artifact
)

# Traces to exclude entirely (Picarro noise / analyzer artifacts, not real placements)
EXCLUDE_TRACE_IDS <- c(
  "LGR3_2023-03-15_CP40_P07",      # dry season noise, 54 false jumps
  "Picarro_2022-10-18_FLM30_P09",   # Picarro noise, not a real placement
  "Picarro_2022-10-25_BL60_P07"     # Picarro noise, not a real placement
)

# Confirmed ebullition traces (manually verified). All other detected "jumps"
# are Picarro noise or analyzer artifacts and should be zeroed out.
CONFIRMED_EBULLITION <- c(
  "LGR2_2022-10-23_CP40_P02",      # 11 jumps, wet
  "LGR2_2022-10-23_CP40_P06",      # 5 jumps, wet
  "LGR2_2022-10-23_CP40_P10",      # 10 jumps, wet
  "Picarro_2022-10-18_FLM30_P08",   # 2 jumps, wet
  "Picarro_2022-10-25_BL60_P02",    # 2 jumps, wet
  "Picarro_2022-10-25_BL60_P03"     # 4 jumps, wet
)

# Rate-based filter: trace diffusive rate must be within this factor of the
# site's mean processed water flux rate. E.g., 3.0 = keep traces where
# diffusive rate is between 1/3x and 3x the processed mean.
RATE_FILTER_FACTOR <- 5.0

# Directories
DATA_ROOT   <- "data/analyzer"
OUTPUT_DIR  <- "output/ebullition"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# ---- WATER MEASUREMENT METADATA --------------------------------------------

water_fluxes <- read_csv("output/soil_water_surface_fluxes_ORIGINAL.csv",
                         show_col_types = FALSE) %>%
  filter(surface_type == "water", !is.na(CH4_best.flux)) %>%
  mutate(
    date = as.Date(date),
    start_time = hms::as_hms(start_time),
    end_time   = hms::as_hms(end_time),
    season = ifelse(month(date) == 10, "wet (Oct 2022)",
                    ifelse(year(date) == 2022, "dry (Mar 2022)", "dry (Mar 2023)"))
  )

# Load ALL fluxes to identify non-water measurement windows on the same analyzer
# IMPORTANT: use the ORIGINAL flux dataset (not combined_gas_flux_dataset.csv)
# to avoid circular dependency — the combined dataset is modified downstream
# by integrate_ebullition.R, which would change detection results on re-runs
all_fluxes <- read_csv("output/soil_water_surface_fluxes_ORIGINAL.csv",
                       show_col_types = FALSE) %>%
  filter(!is.na(start_time), !is.na(end_time)) %>%
  mutate(date = as.Date(date))

# Build time blocks per component per analyzer-day
# A "block" spans from first flux start to last flux end for each component
water_analyzer_days <- water_fluxes %>% distinct(date, analyzer_source)

component_blocks <- all_fluxes %>%
  inner_join(water_analyzer_days, by = c("date", "analyzer_source")) %>%
  mutate(
    block_start = as.POSIXct(paste(date, start_time), tz = "UTC"),
    block_end   = as.POSIXct(paste(date, end_time), tz = "UTC")
  ) %>%
  group_by(date, analyzer_source, component) %>%
  summarise(
    block_start = min(block_start, na.rm = TRUE),
    block_end   = max(block_end, na.rm = TRUE),
    n_fluxes    = n(),
    .groups = "drop"
  )

cat("Component blocks on water analyzer-days:\n")
component_blocks %>% arrange(date, analyzer_source, block_start) %>% print(n = 30)

# Non-water blocks for filtering traces
nonwater_blocks <- component_blocks %>% filter(component != "water")

cat("Water measurements to scan:", nrow(water_fluxes), "\n")
cat("Analyzer-day combos:\n")
water_fluxes %>%
  distinct(analyzer_source, date, plot) %>%
  arrange(analyzer_source, date) %>%
  print(n = 50)

# ---- HELPER: PARSE LGR RAW FILE --------------------------------------------

parse_lgr_file <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)

  header_idx <- grep("\\[CH4\\]_ppm|CH4.*ppm", lines)[1]
  if (is.na(header_idx)) {
    warning("Could not find header in ", filepath)
    return(NULL)
  }

  header <- trimws(strsplit(lines[header_idx], ",")[[1]])

  data_lines <- lines[(header_idx + 1):length(lines)]
  data_lines <- data_lines[grepl("^\\d{2}/\\d{2}/\\d{4}", data_lines)]

  if (length(data_lines) == 0) {
    warning("No data lines in ", filepath)
    return(NULL)
  }

  df <- read.csv(text = paste(data_lines, collapse = "\n"),
                 header = FALSE, stringsAsFactors = FALSE)

  n_cols <- min(ncol(df), length(header))
  names(df)[1:n_cols] <- header[1:n_cols]

  time_col <- if ("SysTime" %in% names(df)) "SysTime" else names(df)[1]
  df$datetime <- mdy_hms(trimws(df[[time_col]]))

  ch4_col <- grep("CH4.*d.*ppm|CH4d_ppm|\\[CH4\\]d_ppm", names(df), value = TRUE)[1]
  co2_col <- grep("CO2.*d.*ppm|CO2d_ppm|\\[CO2\\]d_ppm", names(df), value = TRUE)[1]
  h2o_col <- grep("H2O.*ppm|\\[H2O\\]_ppm", names(df), value = TRUE)[1]

  if (is.na(ch4_col)) {
    ch4_col <- grep("\\[CH4\\]_ppm", names(df), value = TRUE)[1]
  }

  result <- tibble(
    datetime = df$datetime,
    CH4_ppm  = as.numeric(trimws(df[[ch4_col]])),
    CO2_ppm  = as.numeric(trimws(df[[co2_col]])),
    H2O_ppm  = if (!is.na(h2o_col)) as.numeric(trimws(df[[h2o_col]])) else NA_real_
  ) %>%
    filter(!is.na(datetime)) %>%
    arrange(datetime)

  return(result)
}

# ---- HELPER: PARSE PICARRO RAW FILE ----------------------------------------

parse_picarro_file <- function(filepath) {
  df <- read.table(filepath, header = TRUE, stringsAsFactors = FALSE)

  result <- tibble(
    datetime = ymd_hms(paste(df$DATE, df$TIME)),
    CH4_ppm  = df$CH4_dry,
    CO2_ppm  = df$CO2_dry,
    H2O_ppm  = if ("H2O" %in% names(df)) df$H2O else NA_real_
  ) %>%
    filter(!is.na(datetime)) %>%
    arrange(datetime)

  return(result)
}

# ---- HELPER: FIND RAW FILES FOR AN ANALYZER-DAY ----------------------------

# ---- HELPER: SCALE PARAMETERS BY LOGGING INTERVAL --------------------------

scale_params <- function(day_data) {
  # Compute median logging interval in seconds
  dt <- median(as.numeric(diff(day_data$datetime), units = "secs"), na.rm = TRUE)
  # Convert time-based config to point counts
  smooth_w <- max(3, round(SMOOTH_SEC / dt))
  # Force odd for rolling median
  if (smooth_w %% 2 == 0) smooth_w <- smooth_w + 1
  # Scale jump threshold: longer intervals accumulate more diffusive change,
  # so raise the threshold proportionally relative to a 5-s baseline
  jump_adj <- JUMP_THRESH * max(1, dt / 5)
  list(smooth_window = smooth_w, jump_thresh = jump_adj, dt = dt)
}

# ---- HELPER: FIND RAW FILES FOR AN ANALYZER-DAY ----------------------------

find_raw_files <- function(analyzer, date_str) {
  if (grepl("LGR", analyzer)) {
    lgr_num <- gsub("LGR", "", analyzer)
    base_dir <- file.path(DATA_ROOT, "LGR_GLA131",
                          paste0("LGR", lgr_num), date_str)
    if (!dir.exists(base_dir)) return(character(0))
    files <- list.files(base_dir, pattern = "_f\\d+\\.txt$",
                        full.names = TRUE, recursive = TRUE)
    return(files)

  } else if (grepl("Picarro", analyzer, ignore.case = TRUE)) {
    parts <- as.Date(date_str)
    base_dir <- file.path(DATA_ROOT, "Picarro_G4301", "Minimal",
                          format(parts, "%Y"),
                          format(parts, "%m"),
                          format(parts, "%d"))
    if (!dir.exists(base_dir)) return(character(0))
    files <- list.files(base_dir, pattern = "\\.dat$", full.names = TRUE)
    return(files)
  }

  return(character(0))
}

# ---- HELPER: FIND RISING SEGMENTS (CHAMBER PLACEMENTS) ---------------------

find_placements <- function(df,
                            smooth_w,
                            min_dur  = MIN_RISE_SEC,
                            min_rise = MIN_RISE_PPM) {
  # Smooth CH4 to suppress instrument noise
  df <- df %>%
    arrange(datetime) %>%
    mutate(
      CH4_smooth = rollapply(CH4_ppm, width = smooth_w, FUN = median,
                             fill = NA, align = "center"),
      # Point-to-point change in smoothed CH4
      dCH4 = c(NA, diff(CH4_smooth)),
      # Time step in seconds
      dt_sec = c(NA, as.numeric(diff(datetime), units = "secs")),
      # Flag: smoothed concentration is rising
      rising = !is.na(dCH4) & dCH4 > 0
    )

  # Allow brief dips within a placement by filling short non-rising gaps.
  # Use ~15 seconds worth of points as tolerance (prevents over-fragmenting
  # long slow fluxes like SE1 while still splitting distinct placements)
  med_dt <- median(df$dt_sec, na.rm = TRUE)
  gap_tol <- max(5, round(15 / med_dt))

  rle_rise <- rle(df$rising)
  for (i in seq_along(rle_rise$lengths)) {
    if (!rle_rise$values[i] && rle_rise$lengths[i] <= gap_tol) {
      # Only fill if flanked by rising on both sides
      if (i > 1 && i < length(rle_rise$values) &&
          rle_rise$values[i - 1] && rle_rise$values[i + 1]) {
        rle_rise$values[i] <- TRUE
      }
    }
  }
  df$rising_filled <- inverse.rle(rle_rise)

  # Assign segment IDs to contiguous rising blocks
  df <- df %>%
    mutate(
      seg_change = rising_filled != lag(rising_filled, default = FALSE),
      seg_id = cumsum(seg_change)
    )

  # Keep only rising segments
  rising_segs <- df %>%
    filter(rising_filled) %>%
    group_by(seg_id) %>%
    summarise(
      start_time   = min(datetime),
      end_time     = max(datetime),
      duration_sec = as.numeric(difftime(max(datetime), min(datetime),
                                         units = "secs")),
      n_points     = n(),
      CH4_start    = first(CH4_ppm),
      CH4_end      = last(CH4_ppm),
      CH4_rise     = last(CH4_ppm) - first(CH4_ppm),
      CH4_max      = max(CH4_ppm),
      CO2_start    = first(CO2_ppm),
      CO2_end      = last(CO2_ppm),
      .groups = "drop"
    ) %>%
    # Filter: must be long enough and show net CH4 increase
    filter(duration_sec >= min_dur, CH4_rise >= min_rise)

  return(rising_segs)
}

# ---- HELPER: ANALYZE A SINGLE PLACEMENT TRACE ------------------------------

analyze_placement <- function(trace_df, jump_thresh = JUMP_THRESH,
                             trim_sec = TRIM_SEC) {
  # trace_df: subset of raw data for one placement period
  # Returns: the trace with jump annotations + summary stats

  trace_df <- trace_df %>%
    arrange(datetime) %>%
    mutate(
      elapsed_sec = as.numeric(difftime(datetime, min(datetime), units = "secs"))
    )

  # Trim first and last TRIM_SEC seconds
  max_elapsed <- max(trace_df$elapsed_sec)
  trace_df <- trace_df %>%
    filter(elapsed_sec >= trim_sec, elapsed_sec <= max_elapsed - trim_sec)

  if (nrow(trace_df) < 3) {
    return(list(
      trace   = trace_df %>% mutate(dCH4 = NA, is_jump = FALSE,
                                     CH4_linear = NA, CH4_residual = NA,
                                     CH4_linear_clean = NA),
      summary = tibble(n_points = 0, duration_sec = 0, CH4_total_rise_ppm = NA,
                       diffusive_rate_ppm_s = NA, diffusive_rate_clean_ppm_s = NA,
                       lm_r2 = NA, n_jumps = 0, total_ebullitive_ppm = NA,
                       max_jump_ppm = NA, mean_jump_ppm = NA,
                       ebullitive_fraction = NA),
      jumps   = tibble()
    ))
  }

  # Recompute elapsed from trimmed start
  trace_df <- trace_df %>%
    mutate(
      elapsed_sec = as.numeric(difftime(datetime, min(datetime), units = "secs")),
      # Point-to-point CH4 change
      dCH4 = c(NA, diff(CH4_ppm)),
      # Flag upward jumps >= threshold
      is_jump = !is.na(dCH4) & dCH4 >= jump_thresh
    )

  # Fit linear model to the full trace (diffusive component)
  lm_fit <- lm(CH4_ppm ~ elapsed_sec, data = trace_df)
  trace_df$CH4_linear <- predict(lm_fit, trace_df)
  trace_df$CH4_residual <- trace_df$CH4_ppm - trace_df$CH4_linear

  # Diffusive flux rate (ppm/sec from linear slope)
  diffusive_rate_ppm_s <- coef(lm_fit)[["elapsed_sec"]]

  # Step-correct: subtract cumulative jump magnitude from all points after
  # each ebullition event, creating a "de-ebulliated" trace that removes
  # the persistent concentration step-up caused by bubbles
  trace_df$cum_jump <- cumsum(ifelse(trace_df$is_jump, trace_df$dCH4, 0))
  trace_df$CH4_deebull <- trace_df$CH4_ppm - trace_df$cum_jump

  # Exclude buffer window around each jump (±15s) — concentration is still
  # equilibrating immediately after a bubble and may show leading edge before
  EBULL_BUFFER_SEC <- 15
  jump_times <- trace_df$elapsed_sec[trace_df$is_jump]
  trace_df$near_jump <- sapply(trace_df$elapsed_sec, function(t) {
    any(abs(t - jump_times) <= EBULL_BUFFER_SEC)
  })
  # If no jumps, near_jump is all FALSE
  if (length(jump_times) == 0) trace_df$near_jump <- FALSE

  # Fit linear model to de-ebulliated trace excluding jump neighborhoods
  deebull_df <- trace_df %>% filter(!near_jump)
  if (nrow(deebull_df) > 2) {
    lm_clean <- lm(CH4_deebull ~ elapsed_sec, data = deebull_df)
    diffusive_rate_clean <- coef(lm_clean)[["elapsed_sec"]]
    trace_df$CH4_linear_clean <- predict(lm_clean, newdata = trace_df)
  } else {
    diffusive_rate_clean <- diffusive_rate_ppm_s
    trace_df$CH4_linear_clean <- trace_df$CH4_linear
  }

  # Ebullitive component: sum of all upward jumps above threshold
  jumps <- trace_df %>% filter(is_jump)
  total_ebullitive_ppm <- sum(jumps$dCH4, na.rm = TRUE)
  total_rise <- trace_df$CH4_ppm[nrow(trace_df)] - trace_df$CH4_ppm[1]

  summary <- tibble(
    n_points            = nrow(trace_df),
    duration_sec        = max(trace_df$elapsed_sec),
    CH4_total_rise_ppm  = total_rise,
    diffusive_rate_ppm_s = diffusive_rate_ppm_s,
    diffusive_rate_clean_ppm_s = diffusive_rate_clean,
    lm_r2               = summary(lm_fit)$r.squared,
    n_jumps             = nrow(jumps),
    total_ebullitive_ppm = total_ebullitive_ppm,
    max_jump_ppm        = if (nrow(jumps) > 0) max(jumps$dCH4) else NA_real_,
    mean_jump_ppm       = if (nrow(jumps) > 0) mean(jumps$dCH4) else NA_real_,
    ebullitive_fraction = if (total_rise > 0) total_ebullitive_ppm / total_rise else NA_real_
  )

  return(list(trace = trace_df, summary = summary, jumps = jumps))
}

# ---- MAIN LOOP: PROCESS EACH ANALYZER-DAY ----------------------------------

analyzer_days <- water_fluxes %>%
  distinct(analyzer_source, date, plot) %>%
  arrange(analyzer_source, date)

all_placements <- list()
all_traces     <- list()
all_raw_data   <- list()

cat("\n=== SCANNING RAW DATA FOR CHAMBER PLACEMENTS & EBULLITION ===\n\n")

for (i in seq_len(nrow(analyzer_days))) {
  analyzer  <- analyzer_days$analyzer_source[i]
  meas_date <- analyzer_days$date[i]
  site      <- analyzer_days$plot[i]
  date_str  <- format(meas_date, "%Y-%m-%d")

  cat(sprintf("[%d/%d] %s | %s | %s\n", i, nrow(analyzer_days),
              analyzer, date_str, site))

  # Find raw files
  raw_files <- find_raw_files(analyzer, date_str)
  if (length(raw_files) == 0) {
    cat("  -> No raw files found, skipping\n")
    next
  }
  cat("  -> Found", length(raw_files), "raw file(s)\n")

  # Parse and combine all files for this day
  parse_fn <- if (grepl("LGR", analyzer)) parse_lgr_file else parse_picarro_file
  day_data <- bind_rows(lapply(raw_files, function(f) {
    tryCatch(parse_fn(f), error = function(e) {
      cat("  -> Error parsing", basename(f), ":", e$message, "\n")
      return(NULL)
    })
  })) %>%
    distinct(datetime, .keep_all = TRUE) %>%
    arrange(datetime)

  # Apply timestamp corrections per analyzer
  # Picarro internal clock was offset by ~7 hours; LGR clocks have small offsets
  # determined by aligning raw data with field note start times
  LGR_OFFSETS <- list(
    list(date = as.Date("2022-10-23"), analyzer = "LGR2", offset_sec = -1091),
    list(date = as.Date("2023-03-11"), analyzer = "LGR2", offset_sec = -28),
    list(date = as.Date("2023-03-12"), analyzer = "LGR3", offset_sec = -24),
    list(date = as.Date("2023-03-15"), analyzer = "LGR3", offset_sec = -13),
    list(date = as.Date("2023-03-16"), analyzer = "LGR3", offset_sec = -24),
    list(date = as.Date("2023-03-17"), analyzer = "LGR2", offset_sec = -28),
    list(date = as.Date("2023-03-18"), analyzer = "LGR1", offset_sec = -14),
    list(date = as.Date("2023-03-22"), analyzer = "LGR3", offset_sec = -24)
  )

  if (grepl("Picarro", analyzer, ignore.case = TRUE) && nrow(day_data) > 0) {
    day_data <- day_data %>% mutate(datetime = datetime - 25220)
    cat("  -> Applied Picarro timestamp correction (-25220s)\n")
  } else {
    # Check for LGR offset
    for (lo in LGR_OFFSETS) {
      if (lo$date == meas_date && grepl(lo$analyzer, analyzer, ignore.case = TRUE)) {
        day_data <- day_data %>% mutate(datetime = datetime + lo$offset_sec)
        cat(sprintf("  -> Applied %s offset: %ds\n", lo$analyzer, lo$offset_sec))
        break
      }
    }
  }

  if (nrow(day_data) == 0) {
    cat("  -> No data parsed, skipping\n")
    next
  }

  # Scale parameters to this instrument's logging rate
  params <- scale_params(day_data)
  cat(sprintf("  -> Parsed %d obs from %s to %s (dt=%.1fs, smooth=%d pts, jump_thresh=%.3f ppm)\n",
              nrow(day_data),
              format(min(day_data$datetime), "%H:%M"),
              format(max(day_data$datetime), "%H:%M"),
              params$dt, params$smooth_window, params$jump_thresh))

  # Store raw data for overview plots
  all_raw_data[[paste(analyzer, date_str, site)]] <- day_data %>%
    mutate(analyzer = analyzer, date = meas_date, site = site)

  # Find chamber placements (rising segments)
  placements <- find_placements(day_data, smooth_w = params$smooth_window)

  if (nrow(placements) == 0) {
    cat("  -> No rising segments found\n")
    next
  }

  cat("  -> Found", nrow(placements), "candidate placement(s)\n")

  # Analyze each placement — split long ones into MAX_TRACE_SEC chunks
  chunk_counter <- 0
  for (p in seq_len(nrow(placements))) {
    pl <- placements[p, ]
    trace_data <- day_data %>%
      filter(datetime >= pl$start_time & datetime <= pl$end_time)

    # Split long traces into chunks
    if (pl$duration_sec > MAX_TRACE_SEC && nrow(trace_data) > 0) {
      chunk_starts <- seq(min(trace_data$datetime),
                          max(trace_data$datetime),
                          by = MAX_TRACE_SEC)
      chunks <- lapply(seq_along(chunk_starts), function(ci) {
        t_start <- chunk_starts[ci]
        t_end   <- if (ci < length(chunk_starts)) chunk_starts[ci + 1] else max(trace_data$datetime)
        trace_data %>% filter(datetime >= t_start & datetime < t_end)
      })
      # Keep only chunks with enough data and duration >= 60s
      chunks <- chunks[sapply(chunks, function(ch) {
        nrow(ch) >= 10 && as.numeric(difftime(max(ch$datetime), min(ch$datetime), units = "secs")) >= 60
      })]
    } else {
      chunks <- list(trace_data)
    }

    for (ci in seq_along(chunks)) {
      chunk_counter <- chunk_counter + 1
      chunk_data <- chunks[[ci]]
      if (nrow(chunk_data) < 5) next

      result <- analyze_placement(chunk_data, jump_thresh = params$jump_thresh)

      placement_id <- sprintf("%s_%s_%s_P%02d", analyzer, date_str, site, chunk_counter)

      chunk_start <- min(chunk_data$datetime)
      chunk_end   <- max(chunk_data$datetime)

      result$summary <- result$summary %>%
        mutate(
          placement_id = placement_id,
          analyzer     = analyzer,
          date         = meas_date,
          site         = site,
          start_time   = chunk_start,
          end_time     = chunk_end,
          jump_thresh_used = params$jump_thresh
        )

      result$trace <- result$trace %>%
        mutate(placement_id = placement_id)

      result$raw_key <- paste(analyzer, date_str, site)

      all_placements[[placement_id]] <- result$summary
      all_traces[[placement_id]]     <- result

      n_j <- result$summary$n_jumps
      dur <- as.numeric(difftime(chunk_end, chunk_start, units = "secs"))
      if (n_j > 0) {
        cat(sprintf("     P%02d: %s-%s (%.0fs) | %.0f jump(s), max=%.3f ppm, ebull frac=%.1f%%\n",
                    chunk_counter,
                    format(chunk_start, "%H:%M:%S"),
                    format(chunk_end, "%H:%M:%S"),
                    dur, n_j,
                    result$summary$max_jump_ppm,
                    result$summary$ebullitive_fraction * 100))
      } else {
        cat(sprintf("     P%02d: %s-%s (%.0fs) | diffusive only (rate=%.4f ppm/s)\n",
                    chunk_counter,
                    format(chunk_start, "%H:%M:%S"),
                    format(chunk_end, "%H:%M:%S"),
                    dur, result$summary$diffusive_rate_ppm_s))
      }
    }
  }
}

# ---- COMPILE RESULTS -------------------------------------------------------

placements_df <- bind_rows(all_placements) %>%
  filter(n_points > 0) %>%
  mutate(
    season = ifelse(month(date) == 10, "wet (Oct 2022)",
                    ifelse(year(date) == 2022, "dry (Mar 2022)", "dry (Mar 2023)"))
  )

# Apply time-window-based exclusions (robust to ID renumbering from splitting)
excluded_mask <- rep(FALSE, nrow(placements_df))
for (ew in EXCLUDE_WINDOWS) {
  match <- placements_df$site == ew$site &
    format(placements_df$date, "%Y-%m-%d") == ew$date &
    format(placements_df$start_time, "%H:%M") >= ew$after &
    format(placements_df$start_time, "%H:%M") <= ew$before
  excluded_mask <- excluded_mask | match
}
placements_df$excluded <- excluded_mask

# Also exclude specific trace IDs (noise/artifacts identified during QC)
placements_df$excluded <- placements_df$excluded |
  (placements_df$placement_id %in% EXCLUDE_TRACE_IDS)

# Flag traces that fall within non-water time blocks.
# For most site-days, soil and water were done in separate sequential blocks.
# Exception: BL60 2023-03-16 has interleaved soil/water — use individual
# flux window overlap there instead of block overlap.
# Days where soil/water are interleaved — skip non-water block filter
# but only after the specified time (before that, use normal block filter)
INTERLEAVED_DAYS <- list(
  list(date = as.Date("2023-03-16"), analyzer = "LGR3", after = "15:00")  # BL60 interleaved from 15:00+
)

nonwater_indiv <- all_fluxes %>%
  filter(component != "water") %>%
  inner_join(water_analyzer_days, by = c("date", "analyzer_source")) %>%
  mutate(
    nw_start = as.POSIXct(paste(date, start_time), tz = "UTC"),
    nw_end   = as.POSIXct(paste(date, end_time), tz = "UTC")
  )

is_interleaved <- function(d, a, time_hm) {
  any(sapply(INTERLEAVED_DAYS, function(x)
    x$date == d && x$analyzer == a && time_hm >= x$after))
}

if (nrow(nonwater_blocks) > 0 || nrow(nonwater_indiv) > 0) {
  in_nonwater <- sapply(seq_len(nrow(placements_df)), function(i) {
    row <- placements_df[i, ]
    midpoint <- row$start_time + (row$end_time - row$start_time) / 2

    mid_hm <- format(midpoint, "%H:%M")
    if (is_interleaved(row$date, row$analyzer, mid_hm)) {
      # Skip non-water filter for interleaved period — rate filter handles it
      return(FALSE)
    } else {
      # Block overlap for sequential days
      nw <- nonwater_blocks %>%
        filter(date == row$date, analyzer_source == row$analyzer)
      if (nrow(nw) == 0) return(FALSE)
      return(any(midpoint >= nw$block_start & midpoint <= nw$block_end, na.rm = TRUE))
    }
  })
  n_blocked <- sum(in_nonwater)
  cat("Traces in non-water periods:", n_blocked, "of", nrow(placements_df), "\n")
  placements_df <- placements_df %>% filter(!in_nonwater)
  cat("Remaining after non-water filter:", nrow(placements_df), "\n")
}

# Water-block proximity filter: keep only traces whose midpoint falls within
# the water measurement block ± WATER_BLOCK_PAD seconds
# Site-specific pad overrides (seconds before, seconds after water block)
SITE_PAD_OVERRIDES <- list(
  # FLM30 2023-03-18: real long flux starts well before water block
  list(date = as.Date("2023-03-18"), analyzer = "LGR1", pad_before = 7200, pad_after = WATER_BLOCK_PAD),
  # CP40 2022-10-23: real water fluxes continue well after water block (data runs to 16:19)
  list(date = as.Date("2022-10-23"), analyzer = "LGR2", pad_before = WATER_BLOCK_PAD, pad_after = 18000)
)

get_pads <- function(d, a) {
  for (ov in SITE_PAD_OVERRIDES) {
    if (ov$date == d && ov$analyzer == a) return(c(ov$pad_before, ov$pad_after))
  }
  return(c(WATER_BLOCK_PAD, WATER_BLOCK_PAD))
}

water_blocks <- component_blocks %>% filter(component == "water")
if (nrow(water_blocks) > 0) {
  near_water <- sapply(seq_len(nrow(placements_df)), function(i) {
    row <- placements_df[i, ]
    wb <- water_blocks %>%
      filter(date == row$date, analyzer_source == row$analyzer)
    if (nrow(wb) == 0) return(TRUE)  # no water block info → keep
    midpoint <- row$start_time + (row$end_time - row$start_time) / 2
    pads <- get_pads(row$date, row$analyzer)
    any(midpoint >= (wb$block_start - pads[1]) &
        midpoint <= (wb$block_end + pads[2]), na.rm = TRUE)
  })
  n_far <- sum(!near_water)
  if (n_far > 0) {
    cat("Traces too far from water block:", n_far, "\n")
    placements_df <- placements_df %>% filter(near_water)
    cat("Remaining near water block:", nrow(placements_df), "\n")
  }
}

# Rate-based filter: keep only traces whose diffusive rate is plausible
# for water fluxes at this site x season. Converts processed flux (nmol/m2/s)
# to expected ppm/s using chamber params, then keeps traces within range.
R_gas <- 8.314; P_atm <- 101.325

site_expected_rate <- water_fluxes %>%
  group_by(site = plot, season) %>%
  summarise(
    mean_flux_nmol = mean(CH4_best.flux, na.rm = TRUE),
    T_K = mean(air_temp, na.rm = TRUE) + 273.15,
    V_L = first(total_system_volume_cm3) / 1000,
    A_m2 = first(surface_area_cm2) / 10000,
    .groups = "drop"
  ) %>%
  mutate(
    expected_ppm_s = mean_flux_nmol / 1000 * R_gas * T_K * A_m2 / (V_L * P_atm)
  )

cat("\nExpected diffusive rates (ppm/s) from processed water fluxes:\n")
site_expected_rate %>% select(site, season, mean_flux_nmol, expected_ppm_s) %>% print(n = 20)

n_before_rate <- nrow(placements_df)
rate_keep <- sapply(seq_len(nrow(placements_df)), function(i) {
  row <- placements_df[i, ]
  er <- site_expected_rate %>%
    filter(site == row$site, season == row$season)
  if (nrow(er) == 0 || is.na(row$diffusive_rate_clean_ppm_s)) return(TRUE)
  rate <- abs(row$diffusive_rate_clean_ppm_s)
  expected <- er$expected_ppm_s[1]
  # Keep if within RATE_FILTER_FACTOR of expected
  rate <= expected * RATE_FILTER_FACTOR & rate >= expected / RATE_FILTER_FACTOR
})
placements_df <- placements_df %>% filter(rate_keep)
n_rate_removed <- n_before_rate - nrow(placements_df)
cat("Traces removed by rate filter (outside", RATE_FILTER_FACTOR, "x of expected):",
    n_rate_removed, "\n")
cat("Remaining after rate filter:", nrow(placements_df), "\n")

if (any(placements_df$excluded)) {
  cat("Excluded", sum(placements_df$excluded), "artifact(s):",
      paste(placements_df$placement_id[placements_df$excluded], collapse = ", "), "\n")
  # Zero out ebullition stats for excluded traces (keep trace for plotting)
  placements_df <- placements_df %>%
    mutate(
      n_jumps = ifelse(excluded, 0, n_jumps),
      total_ebullitive_ppm = ifelse(excluded, 0, total_ebullitive_ppm),
      max_jump_ppm = ifelse(excluded, NA_real_, max_jump_ppm),
      mean_jump_ppm = ifelse(excluded, NA_real_, mean_jump_ppm),
      ebullitive_fraction = ifelse(excluded, 0, ebullitive_fraction)
    )
}

# Zero out ebullition for any trace NOT in the confirmed whitelist
# (Picarro noise, analyzer artifacts, etc. that pass the automated threshold)
false_ebull <- placements_df$n_jumps > 0 &
  !placements_df$placement_id %in% CONFIRMED_EBULLITION &
  !placements_df$excluded
if (any(false_ebull)) {
  cat("Zeroing ebullition for", sum(false_ebull), "false positive(s):",
      paste(placements_df$placement_id[false_ebull], collapse = ", "), "\n")
  placements_df <- placements_df %>%
    mutate(
      n_jumps = ifelse(false_ebull, 0, n_jumps),
      total_ebullitive_ppm = ifelse(false_ebull, 0, total_ebullitive_ppm),
      max_jump_ppm = ifelse(false_ebull, NA_real_, max_jump_ppm),
      mean_jump_ppm = ifelse(false_ebull, NA_real_, mean_jump_ppm),
      ebullitive_fraction = ifelse(false_ebull, 0, ebullitive_fraction)
    )
}

# ---- TAG TRACES AS "PROCESSED" vs "ADDITIONAL" ------------------------------
# Match each trace to processed water fluxes by time overlap (±60s tolerance)
wf_times <- water_fluxes %>%
  filter(!is.na(start_time), !is.na(end_time)) %>%
  mutate(
    flux_start = ymd_hms(paste(date, start_time)),
    flux_end   = ymd_hms(paste(date, end_time))
  ) %>%
  filter(!is.na(flux_start))

OVERLAP_TOL <- 60  # seconds tolerance for matching
placements_df$trace_type <- "additional"
placements_df$matched_flux_id <- NA_character_

for (i in seq_len(nrow(placements_df))) {
  tr_start <- placements_df$start_time[i]
  tr_end   <- placements_df$end_time[i]
  tr_date  <- as.Date(tr_start)
  matches <- wf_times %>%
    filter(as.Date(flux_start) == tr_date,
           flux_start <= tr_end + OVERLAP_TOL,
           flux_end   >= tr_start - OVERLAP_TOL)
  if (nrow(matches) > 0) {
    placements_df$trace_type[i] <- "processed"
    placements_df$matched_flux_id[i] <- matches$flux_id[1]
  }
}

n_proc <- sum(placements_df$trace_type == "processed")
n_add  <- sum(placements_df$trace_type == "additional")

cat("\n=== PLACEMENT SUMMARY ===\n")
cat("Total good traces:", nrow(placements_df), "\n")
cat("  Overlapping processed fluxes:", n_proc, "\n")
cat("  Additional (new):", n_add, "\n")
cat("Traces with ebullition:", sum(placements_df$n_jumps > 0), "\n")
cat("Traces diffusive only:", sum(placements_df$n_jumps == 0), "\n\n")

placements_df %>%
  select(placement_id, trace_type, start_time, duration_sec,
         CH4_total_rise_ppm, n_jumps, total_ebullitive_ppm,
         ebullitive_fraction, diffusive_rate_clean_ppm_s) %>%
  print(n = 100)

write_csv(placements_df, file.path(OUTPUT_DIR, "placements_summary.csv"))
saveRDS(all_traces, file.path(OUTPUT_DIR, "all_traces.rds"))
cat("\nSaved to:", file.path(OUTPUT_DIR, "placements_summary.csv"), "\n")
cat("Saved trace data to:", file.path(OUTPUT_DIR, "all_traces.rds"), "\n")

# (season already added to water_fluxes and placements_df above)

# ---- SITE × SEASON EBULLITION STATISTICS -----------------------------------

cat("\n=== SITE × SEASON EBULLITION SUMMARY ===\n\n")

# Number of logged water fluxes per site × season
n_water_fluxes <- water_fluxes %>%
  group_by(site = plot, season) %>%
  summarise(n_water_fluxes = n(), .groups = "drop")

# Unit conversion factor: ppm/s -> nmol/m2/s
T_K_default <- 298.15
V_L_default <- water_fluxes$total_system_volume_cm3[1] / 1000
A_m2_default <- water_fluxes$surface_area_cm2[1] / 10000
conv_factor <- P_atm * V_L_default / (R_gas * T_K_default * A_m2_default) * 1e3  # ppm/s -> nmol/m2/s

site_summary <- placements_df %>%
  filter(!excluded) %>%
  group_by(site, season, analyzer) %>%
  summarise(
    n_traces       = n(),
    n_processed    = sum(trace_type == "processed"),
    n_additional   = sum(trace_type == "additional"),
    n_with_ebull   = sum(n_jumps > 0),
    pct_with_ebull = round(n_with_ebull / n_traces * 100, 1),
    total_obs_sec  = sum(duration_sec, na.rm = TRUE),
    total_jumps    = sum(n_jumps, na.rm = TRUE),
    jumps_per_hour = total_jumps / (total_obs_sec / 3600),
    mean_diff_rate = mean(diffusive_rate_clean_ppm_s, na.rm = TRUE),
    total_ebull_ppm = sum(total_ebullitive_ppm, na.rm = TRUE),
    total_rise_ppm  = sum(CH4_total_rise_ppm, na.rm = TRUE),
    diffusive_ppm_hr  = mean_diff_rate * 3600,
    ebullitive_ppm_hr = total_ebull_ppm / (total_obs_sec / 3600),
    pct_CH4_ebullitive = round(
      ebullitive_ppm_hr / (diffusive_ppm_hr + ebullitive_ppm_hr) * 100, 1),
    # Separate means for processed vs additional
    mean_diff_proc_ppm_s = mean(diffusive_rate_clean_ppm_s[trace_type == "processed"], na.rm = TRUE),
    mean_diff_add_ppm_s  = mean(diffusive_rate_clean_ppm_s[trace_type == "additional"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(n_water_fluxes, by = c("site", "season")) %>%
  left_join(
    site_expected_rate %>% select(site, season, mean_flux_nmol, expected_ppm_s),
    by = c("site", "season")
  ) %>%
  mutate(
    # Convert rates to nmol/m2/s for comparison
    diff_nmol     = mean_diff_rate * conv_factor,
    diff_proc_nmol = mean_diff_proc_ppm_s * conv_factor,
    diff_add_nmol  = mean_diff_add_ppm_s * conv_factor,
    ebull_nmol    = ebullitive_ppm_hr / 3600 * conv_factor,
    total_nmol    = diff_nmol + ebull_nmol
  ) %>%
  arrange(site, season)

# Print compact summary table
cat("Site × Season Summary:\n\n")
site_summary %>%
  select(site, season, analyzer, n_water_fluxes, n_traces, n_processed,
         n_additional, n_with_ebull, pct_with_ebull, pct_CH4_ebullitive,
         jumps_per_hour) %>%
  print(n = 50, width = Inf)

# Print rate comparison table
cat("\n=== RATE COMPARISON (nmol CH4/m²/s): Processed Fluxes vs Trace Estimates ===\n\n")
cat(sprintf("%-6s %-18s %4s %4s %4s | %8s %8s %8s | %8s | %8s %8s\n",
    "Site", "Season", "Trc", "Prc", "Add",
    "Diffus.", "Ebull.", "Total", "Proc.Fx", "Dif/Prc", "Tot/Prc"))
cat(strrep("-", 110), "\n")
for (i in seq_len(nrow(site_summary))) {
  r <- site_summary[i,]
  dif_ratio <- if(!is.na(r$mean_flux_nmol) & r$mean_flux_nmol != 0) r$diff_nmol / r$mean_flux_nmol else NA
  tot_ratio <- if(!is.na(r$mean_flux_nmol) & r$mean_flux_nmol != 0) r$total_nmol / r$mean_flux_nmol else NA
  cat(sprintf("%-6s %-18s %4d %4d %4d | %8.2f %8.2f %8.2f | %8.2f | %7.2fx %7.2fx\n",
      r$site, r$season, r$n_traces, r$n_processed, r$n_additional,
      r$diff_nmol, r$ebull_nmol, r$total_nmol,
      ifelse(is.na(r$mean_flux_nmol), NA, r$mean_flux_nmol),
      ifelse(is.na(dif_ratio), NA, dif_ratio),
      ifelse(is.na(tot_ratio), NA, tot_ratio)))
}

write_csv(site_summary, file.path(OUTPUT_DIR, "site_season_ebullition.csv"))
cat("\nSaved to:", file.path(OUTPUT_DIR, "site_season_ebullition.csv"), "\n")

# ---- PLOTS: INDIVIDUAL PLACEMENT TRACES ------------------------------------

cat("\n=== GENERATING PLACEMENT TRACE PLOTS ===\n")

trace_plots <- list()

for (pid in names(all_traces)) {
  res   <- all_traces[[pid]]
  tr    <- res$trace
  summ  <- res$summary
  jumps <- res$jumps
  jt    <- summ$jump_thresh_used

  if (nrow(tr) == 0) next

  # Look up trace type from placements_df
  pl_row <- placements_df %>% filter(placement_id == pid)
  trace_type <- if (nrow(pl_row) > 0) pl_row$trace_type[1] else "additional"

  # Color scheme based on trace type
  trace_color <- if (trace_type == "processed") "steelblue" else "darkorange"
  shade_fill  <- if (trace_type == "processed") "lightblue" else "lightyellow"
  type_label  <- if (trace_type == "processed") "Processed" else "Additional"

  # ---- Pull context window from raw data ----
  raw_key <- res$raw_key
  if (!is.null(raw_key) && raw_key %in% names(all_raw_data)) {
    raw_day <- all_raw_data[[raw_key]]
    t_min <- min(tr$datetime) - PLOT_CONTEXT_SEC
    t_max <- max(tr$datetime) + PLOT_CONTEXT_SEC
    ctx <- raw_day %>%
      filter(datetime >= t_min & datetime <= t_max) %>%
      mutate(in_trace = datetime >= min(tr$datetime) & datetime <= max(tr$datetime))
  } else {
    ctx <- tr %>% mutate(in_trace = TRUE)
  }

  # Placement time boundaries for shading
  trace_start <- min(tr$datetime)
  trace_end   <- max(tr$datetime)

  # ---- Panel 1: CH4 trace with linear fit and jumps ----
  p1 <- ggplot() +
    # Context (before/after) in light grey
    geom_line(data = ctx %>% filter(!in_trace),
              aes(x = datetime, y = CH4_ppm), color = "grey75", linewidth = 0.4) +
    # Placement region shading (color-coded by type)
    annotate("rect", xmin = trace_start, xmax = trace_end,
             ymin = -Inf, ymax = Inf, fill = shade_fill, alpha = 0.5) +
    # Analyzed trace
    geom_line(data = tr, aes(x = datetime, y = CH4_ppm),
              color = "grey30", linewidth = 0.5) +
    geom_line(data = tr %>% filter(!is.na(CH4_linear_clean)),
              aes(x = datetime, y = CH4_linear_clean), color = trace_color,
              linewidth = 0.7, linetype = "dashed") +
    theme_bw(base_size = 10) +
    labs(y = "CH4 dry (ppm)")

  if (nrow(jumps) > 0) {
    p1 <- p1 +
      geom_point(data = jumps, aes(x = datetime, y = CH4_ppm),
                 color = "red", size = 2.5, shape = 17) +
      geom_segment(data = jumps,
                   aes(x = datetime, xend = datetime,
                       y = CH4_ppm - dCH4, yend = CH4_ppm),
                   color = "red", linewidth = 0.6,
                   arrow = arrow(length = unit(0.08, "inches")))
  }

  # Build readable title: Site | Season | date time range | type
  pl_season <- ifelse(month(summ$date) == 10, "Wet (Oct 2022)",
                      ifelse(year(summ$date) == 2022, "Dry (Mar 2022)", "Dry (Mar 2023)"))
  title_text <- sprintf("%s | %s | %s %s\u2013%s | [%s]",
                        summ$site, pl_season, format(summ$date, "%Y-%m-%d"),
                        format(trace_start, "%H:%M"), format(trace_end, "%H:%M"),
                        type_label)
  subtitle_text <- sprintf(
    "Diff rate: %.4f ppm/s (R\u00b2=%.3f) | Jumps: %d (thresh=%.3f ppm) | Ebull: %.3f ppm (%.0f%%) | %s",
    summ$diffusive_rate_clean_ppm_s, summ$lm_r2,
    summ$n_jumps, jt, summ$total_ebullitive_ppm,
    ifelse(is.na(summ$ebullitive_fraction), 0, summ$ebullitive_fraction * 100),
    summ$analyzer)

  p1 <- p1 + labs(title = title_text, subtitle = subtitle_text)

  # ---- Panel 2: CO2 trace for context ----
  p2 <- ggplot() +
    geom_line(data = ctx %>% filter(!in_trace),
              aes(x = datetime, y = CO2_ppm), color = "grey75", linewidth = 0.4) +
    annotate("rect", xmin = trace_start, xmax = trace_end,
             ymin = -Inf, ymax = Inf, fill = "lightyellow", alpha = 0.5) +
    geom_line(data = tr, aes(x = datetime, y = CO2_ppm),
              color = "grey30", linewidth = 0.5) +
    theme_bw(base_size = 10) +
    labs(y = "CO2 dry (ppm)")

  if (nrow(jumps) > 0) {
    p2 <- p2 +
      geom_vline(data = jumps, aes(xintercept = datetime),
                 color = "red", linetype = "dotted", alpha = 0.6)
  }

  # ---- Panel 3: Point-to-point CH4 change ----
  p3 <- ggplot(tr %>% filter(!is.na(dCH4)), aes(x = datetime, y = dCH4)) +
    geom_col(aes(fill = is_jump), width = 3, show.legend = FALSE) +
    scale_fill_manual(values = c("FALSE" = "grey60", "TRUE" = "red")) +
    geom_hline(yintercept = jt, color = "red", linetype = "dashed") +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    theme_bw(base_size = 10) +
    labs(x = "Time", y = expression(Delta * "CH4 (ppm)"))

  # Combine panels
  combined <- p1 / p2 / p3 + plot_layout(heights = c(3, 2, 2))
  trace_plots[[pid]] <- combined
}

# Sort trace plots by site then date (via placement summary order)
trace_order <- placements_df %>%
  filter(n_points > 0, !excluded) %>%
  arrange(site, date) %>%
  pull(placement_id)
trace_order <- trace_order[trace_order %in% names(trace_plots)]

# Save all trace plots to PDF
if (length(trace_order) > 0) {
  pdf_path <- file.path(OUTPUT_DIR, "placement_traces.pdf")
  pdf(pdf_path, width = 12, height = 10)
  for (pid in trace_order) {
    print(trace_plots[[pid]])
  }
  dev.off()
  cat("Saved", length(trace_order), "placement trace plots to", pdf_path, "\n")
}

# ---- OVERVIEW PLOT: FULL-DAY RAW DATA WITH FINAL FLUXES MARKED --------------
# Shows the end result: each flux that ends up in the final dataset,
# with vertical lines for start/end, colored by source and ebullition status.
# Uses placements_df (non-excluded) as the source — these match what gets
# integrated into the combined dataset.

cat("\n=== GENERATING FULL-DAY OVERVIEW PLOTS ===\n")

overview_plots <- list()

# Use only non-excluded placements (matches what goes into final dataset)
final_placements <- placements_df %>% filter(!excluded)

for (key in names(all_raw_data)) {
  raw <- all_raw_data[[key]]
  analyzer  <- raw$analyzer[1]
  meas_date <- raw$date[1]
  site      <- raw$site[1]

  # Fluxes for this day
  day_fluxes <- final_placements %>%
    filter(analyzer == !!analyzer, date == !!meas_date, site == !!site) %>%
    mutate(
      flux_class = case_when(
        n_jumps > 0 ~ "ebullitive",
        trace_type == "additional" ~ "additional",
        TRUE ~ "processed"
      ),
      mid_time = start_time + (end_time - start_time) / 2,
      short_id = gsub(".*_P", "P", placement_id)
    )

  # Component blocks for background shading
  day_blocks <- component_blocks %>%
    filter(date == meas_date, analyzer_source == analyzer)

  ov_season <- ifelse(month(meas_date) == 10, "Wet (Oct 2022)",
                      ifelse(year(meas_date) == 2022, "Dry (Mar 2022)", "Dry (Mar 2023)"))

  block_colors <- c(water = "#2196F3", soil = "#8B4513", stem = "#4CAF50",
                    root = "#FF9800", cwd = "#9E9E9E", leaves = "#CDDC39",
                    pneumatophore = "#E91E63")
  flux_colors <- c("processed" = "#4682B4", "additional" = "#4682B4",
                   "ebullitive" = "#D2691E")
  flux_linetypes <- c("processed" = "solid", "additional" = "dashed",
                      "ebullitive" = "solid")

  p <- ggplot(raw, aes(x = datetime, y = CH4_ppm))

  # 1. Component block shading
  if (nrow(day_blocks) > 0) {
    p <- p +
      geom_rect(data = day_blocks,
                aes(xmin = block_start, xmax = block_end,
                    ymin = -Inf, ymax = Inf, fill = component),
                alpha = 0.15, inherit.aes = FALSE) +
      scale_fill_manual(values = block_colors, name = "Component")
  }

  # 2. Raw CH4 trace (grey background)
  p <- p + geom_line(color = "grey70", linewidth = 0.3)

  # 3. Highlight raw data within each flux window
  if (nrow(day_fluxes) > 0) {
    raw_tagged <- raw %>% mutate(flux_class = NA_character_, trace_id = NA_character_)
    for (j in seq_len(nrow(day_fluxes))) {
      fl <- day_fluxes[j, ]
      in_range <- raw_tagged$datetime >= fl$start_time & raw_tagged$datetime <= fl$end_time
      raw_tagged$flux_class[in_range] <- fl$flux_class
      raw_tagged$trace_id[in_range] <- fl$placement_id
    }
    raw_in_flux <- raw_tagged %>% filter(!is.na(flux_class))

    if (nrow(raw_in_flux) > 0) {
      p <- p +
        geom_line(data = raw_in_flux,
                  aes(x = datetime, y = CH4_ppm, color = flux_class, group = trace_id),
                  linewidth = 0.6, inherit.aes = FALSE) +
        scale_color_manual(
          values = flux_colors, name = "Flux type",
          labels = c("processed" = "Diffusive (processed)",
                     "additional" = "Diffusive (additional)",
                     "ebullitive" = "Ebullitive"))
    }

    # Vertical lines at start/end of each flux
    for (j in seq_len(nrow(day_fluxes))) {
      fl <- day_fluxes[j, ]
      lcolor <- flux_colors[fl$flux_class]
      ltype <- flux_linetypes[fl$flux_class]
      p <- p +
        geom_vline(xintercept = as.numeric(fl$start_time),
                   color = lcolor, linetype = ltype, linewidth = 0.4, alpha = 0.6) +
        geom_vline(xintercept = as.numeric(fl$end_time),
                   color = lcolor, linetype = ltype, linewidth = 0.4, alpha = 0.6)
    }

    # Label each flux at bottom
    p <- p +
      annotate("text", x = day_fluxes$mid_time, y = -Inf,
               label = day_fluxes$short_id,
               vjust = -0.3, size = 1.8, color = "grey30", angle = 45)
  }

  # 4. Ebullition jump markers
  day_pids <- day_fluxes$placement_id[day_fluxes$n_jumps > 0]
  day_jumps <- bind_rows(lapply(
    day_pids[day_pids %in% names(all_traces)],
    function(pid) {
      j <- all_traces[[pid]]$jumps
      if (!is.null(j) && nrow(j) > 0) j else NULL
    }
  ))
  if (nrow(day_jumps) > 0) {
    p <- p +
      geom_point(data = day_jumps, aes(x = datetime, y = CH4_ppm),
                 color = "#D2691E", size = 2, shape = 17, inherit.aes = FALSE)
  }

  # Counts
  n_proc  <- sum(day_fluxes$flux_class == "processed")
  n_add   <- sum(day_fluxes$flux_class == "additional")
  n_ebull <- sum(day_fluxes$flux_class == "ebullitive")

  p <- p +
    theme_bw(base_size = 10) +
    labs(
      title = sprintf("%s | %s | %s (%s)", site, ov_season,
                       format(meas_date, "%Y-%m-%d"), analyzer),
      subtitle = sprintf(
        "%d fluxes: %d processed (solid blue) + %d additional (dashed blue) + %d ebullitive (orange)",
        nrow(day_fluxes), n_proc, n_add, n_ebull),
      x = "Time", y = "CH4 dry (ppm)"
    )

  sort_key <- sprintf("%s_%s", site, format(meas_date, "%Y-%m-%d"))
  overview_plots[[sort_key]] <- p
}

if (length(overview_plots) > 0) {
  # Sort by key (site then date)
  ov_order <- sort(names(overview_plots))
  pdf_path <- file.path(OUTPUT_DIR, "fullday_overview.pdf")
  pdf(pdf_path, width = 14, height = 5)
  for (k in ov_order) {
    print(overview_plots[[k]])
  }
  dev.off()
  cat("Saved", length(ov_order), "overview plots to", pdf_path, "\n")
}

# ---- DONE -------------------------------------------------------------------

cat("\n=== DONE ===\n")
cat("Output directory:", OUTPUT_DIR, "\n")
cat("Key outputs:\n")
cat("  - placements_summary.csv    (all detected placements + ebullition stats)\n")
cat("  - site_season_ebullition.csv (site x season summary + hourly ebull fraction)\n")
cat("  - placement_traces.pdf      (per-placement: CH4 trace, linear fit, jumps)\n")
cat("  - fullday_overview.pdf      (full-day CH4 with placements & jumps marked)\n")
cat("\nTunable parameters:\n")
cat("  SMOOTH_SEC     =", SMOOTH_SEC, "s    (smoothing window for placement detection)\n")
cat("  MIN_RISE_SEC   =", MIN_RISE_SEC, "s    (minimum placement duration)\n")
cat("  MIN_RISE_PPM   =", MIN_RISE_PPM, "ppm  (minimum CH4 rise to count)\n")
cat("  JUMP_THRESH    =", JUMP_THRESH, "ppm  (base upward jump threshold; scaled by dt)\n")
cat("  TRIM_SEC       =", TRIM_SEC, "s    (trimmed from each trace end)\n")
cat("  PLOT_CONTEXT   =", PLOT_CONTEXT_SEC, "s    (context shown before/after trace)\n")
