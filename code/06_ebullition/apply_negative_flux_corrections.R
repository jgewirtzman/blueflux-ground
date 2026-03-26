# =============================================================================
# Apply Negative Flux Corrections
# =============================================================================
# 1. Remove 7 artifact measurements from the combined dataset
# 2. Reprocess 14 trimmed traces through goFlux with corrected time windows
# 3. Update combined_gas_flux_dataset.csv
#
# Depends on:
#   - output/ebullition/corrected_time_windows.csv (from interactive picker)
#   - output/combined_gas_flux_dataset.csv
#   - Raw analyzer data + auxfiles for reprocessing
# =============================================================================

library(dplyr)
library(readr)
library(lubridate)
library(goFlux)
library(zoo)

# ---- Config ------------------------------------------------------------------

ARTIFACT_IDS <- c(
  "Oct_22_199_CP40_stem",
  "Oct_22_56_SRS6_stem",
  "Mar_23_119_BL60_stem",
  "Oct_22_33_SRS6_stem",
  "Oct_22_54_SRS6_stem",
  "Mar_23_76_FLM30_stem",
  "Oct_22_51_SRS6_stem"
)

windows <- read_csv("output/ebullition/corrected_time_windows.csv", show_col_types = FALSE)
cat("Artifact IDs to remove:", length(ARTIFACT_IDS), "\n")
cat("Traces to reprocess with new windows:", nrow(windows), "\n\n")

# ---- Load combined dataset ---------------------------------------------------

# Read current combined dataset (after ebullition integration and HA/HB corrections)
df <- read_csv("output/combined_gas_flux_dataset.csv", show_col_types = FALSE)
cat("Input dataset:", nrow(df), "rows\n")

# ---- STEP 1: Remove artifacts -----------------------------------------------

n_before <- nrow(df)
df <- df %>% filter(!flux_id %in% ARTIFACT_IDS)
n_removed <- n_before - nrow(df)
cat("Removed", n_removed, "artifact rows\n")

# ---- STEP 2: Reprocess trimmed traces through goFlux ------------------------

cat("\nReprocessing", nrow(windows), "traces with corrected time windows...\n")

# Load auxfiles
aux_lgr1 <- tryCatch(read_csv("intermediate/auxfiles/tree_auxfile_lgr1.csv", show_col_types = FALSE), error = function(e) NULL)
aux_lgr2 <- read_csv("intermediate/auxfiles/tree_auxfile_lgr2_goflux.csv", show_col_types = FALSE)
aux_lgr3 <- read_csv("intermediate/auxfiles/tree_auxfile_lgr3_goflux.csv", show_col_types = FALSE)
aux_soil2 <- tryCatch(read_csv("intermediate/auxfiles/soilwater_auxfile_lgr2_goflux.csv", show_col_types = FALSE), error = function(e) NULL)
aux_soil3 <- tryCatch(read_csv("intermediate/auxfiles/soilwater_auxfile_lgr3_goflux.csv", show_col_types = FALSE), error = function(e) NULL)
aux_all <- bind_rows(aux_lgr1, aux_lgr2, aux_lgr3, aux_soil2, aux_soil3)

# Load flux results for nb.obs
flux_files <- c(
  "intermediate/results_trees/CH4_flux_lgr1_results.csv",
  "intermediate/results_trees/CH4_flux_lgr2_results.csv",
  "intermediate/results_trees/CH4_flux_lgr3_results.csv",
  "intermediate/results_surface/CH4_flux_lgr2_results_soil.csv",
  "intermediate/results_surface/CH4_flux_lgr3_results_soil.csv"
)
flux_all <- lapply(flux_files, function(f) {
  tryCatch(read_csv(f, show_col_types = FALSE) %>% select(UniqueID, nb.obs),
           error = function(e) NULL)
}) %>% bind_rows()

# Load manual ID files for existing obs data
manual_files <- c(
  "intermediate/results_trees/lgr1_manual_identification_results.csv",
  "intermediate/results_trees/lgr3_manual_identification_results.csv",
  "intermediate/results_trees/lgr3_manual_identification_results_additional.csv",
  "intermediate/results_surface/lgr3_manual_identification_results_soil.csv",
  "intermediate/results_surface/lgr2_manual_identification_results_soil.csv",
  "intermediate/rescue/lgr2_manual_identification_results.csv"
)
existing_obs <- lapply(manual_files, function(f) {
  if (!file.exists(f)) return(NULL)
  read_csv(f, show_col_types = FALSE) %>%
    select(UniqueID, Etime, CH4dry_ppb, flag, Area, Vcham, Vtube, Vinst, Vtot, Tcham, Pcham)
}) %>% bind_rows()

# Raw data parser
parse_lgr <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)
  header_idx <- grep("CH4.*_ppm", lines)[1]
  if (is.na(header_idx)) return(NULL)
  header <- trimws(strsplit(lines[header_idx], ",")[[1]])
  data_lines <- lines[(header_idx + 1):length(lines)]
  data_lines <- data_lines[grepl("^\\d{2}/\\d{2}/\\d{4}", data_lines)]
  if (length(data_lines) == 0) return(NULL)
  df2 <- read.csv(text = paste(data_lines, collapse = "\n"), header = FALSE, stringsAsFactors = FALSE)
  n_cols <- min(ncol(df2), length(header))
  names(df2)[1:n_cols] <- header[1:n_cols]
  time_col <- if ("SysTime" %in% names(df2)) "SysTime" else names(df2)[1]
  ch4_col <- grep("CH4.*d_ppm", names(df2), value = TRUE)[1]
  tibble(datetime = mdy_hms(trimws(df2[[time_col]])),
         CH4_ppm = as.numeric(trimws(df2[[ch4_col]]))) %>%
    filter(!is.na(datetime)) %>% arrange(datetime)
}

raw_cache <- new.env(parent = emptyenv())

get_trace_data <- function(fid, new_start, new_end) {
  # Try existing manual ID data first
  d <- existing_obs %>% filter(UniqueID == fid)
  if (nrow(d) > 0) {
    # Trim to new window
    d_trimmed <- d %>%
      filter(Etime >= new_start, Etime <= new_end) %>%
      mutate(flag = 1)
    return(d_trimmed)
  }

  # Reconstruct from raw
  row <- df %>% filter(flux_id == fid)
  if (nrow(row) == 0) return(NULL)
  aux_row <- aux_all %>% filter(UniqueID == fid)
  if (nrow(aux_row) == 0) return(NULL)
  nb <- flux_all %>% filter(UniqueID == fid) %>% pull(nb.obs)
  nb <- if (length(nb) > 0 && !is.na(nb[1])) nb[1] else 120

  d_date <- as.Date(row$date[1])
  src <- row$source_file[1]
  analyzer_dir <- if (grepl("LGR3", src)) paste0("data/analyzer/LGR_GLA131/LGR3/", d_date)
                  else if (grepl("LGR1", src)) paste0("data/analyzer/LGR_GLA131/LGR1/", d_date)
                  else paste0("data/analyzer/LGR_GLA131/LGR2/", d_date)
  if (!dir.exists(analyzer_dir)) return(NULL)

  cache_key <- analyzer_dir
  if (!exists(cache_key, envir = raw_cache)) {
    files <- list.files(analyzer_dir, pattern = "f\\d+\\.txt$", full.names = TRUE, recursive = TRUE)
    raw <- bind_rows(lapply(files, parse_lgr)) %>%
      distinct(datetime, .keep_all = TRUE) %>% arrange(datetime)
    assign(cache_key, raw, envir = raw_cache)
  }
  raw <- get(cache_key, envir = raw_cache)

  meas_start <- ymd_hms(aux_row$start.time[1])
  obs_len <- nb * 5

  # Build trace with chamber params from auxfile
  trace <- raw %>%
    filter(datetime >= meas_start - 10, datetime <= meas_start + obs_len + 60) %>%
    mutate(
      Etime = as.numeric(difftime(datetime, meas_start, units = "secs")),
      CH4dry_ppb = CH4_ppm * 1000,
      UniqueID = fid,
      Area = aux_row$Area[1],
      Vcham = aux_row$Vcham[1],
      Vtube = aux_row$Vtube[1],
      Vinst = aux_row$Vinst[1],
      Vtot = aux_row$Vtot[1],
      Tcham = aux_row$Tcham[1],
      Pcham = aux_row$Pcham[1]
    )

  # Trim to new window
  trace %>%
    filter(Etime >= new_start, Etime <= new_end) %>%
    mutate(flag = 1)
}

# Process each trace
reprocessed <- list()

for (i in seq_len(nrow(windows))) {
  w <- windows[i, ]
  fid <- w$flux_id
  cat(sprintf("  [%d/%d] %s: Etime %d-%d s ... ",
              i, nrow(windows), fid, w$new_start_etime, w$new_end_etime))

  trace <- get_trace_data(fid, w$new_start_etime, w$new_end_etime)
  if (is.null(trace) || nrow(trace) < 5) {
    cat("insufficient data (", nrow(trace %||% 0), "pts)\n")
    next
  }

  # Get chamber params
  Area <- trace$Area[1]
  Vcham <- trace$Vcham[1]
  Vtot <- trace$Vtot[1]
  Tcham <- trace$Tcham[1]
  Pcham <- trace$Pcham[1]

  if (any(is.na(c(Area, Vcham, Vtot, Tcham, Pcham)))) {
    # Fall back to dataset metadata
    row <- df %>% filter(flux_id == fid)
    if (nrow(row) > 0) {
      Area <- row$surface_area_cm2[1]
      Vcham <- row$chamber_volume_cm3[1]
      Vtot <- row$total_system_volume_cm3[1]
      Tcham <- coalesce(row$air_temp[1], 25)
      Pcham <- 101.325
    }
  }

  ch4_ppb <- trace$CH4dry_ppb
  etime <- trace$Etime - min(trace$Etime)  # reset to 0
  n_obs <- length(ch4_ppb)

  # Simple LM fit
  lm_fit <- lm(ch4_ppb ~ etime)
  lm_slope <- coef(lm_fit)[2]  # ppb/s
  lm_r2 <- summary(lm_fit)$r.squared
  lm_pval <- summary(lm_fit)$coefficients[2, 4]

  # Convert slope to flux: nmol/m2/s
  # slope_ppb_s * 1e-9 * P / (R * T) * Vtot / Area * 1e9
  R_gas <- 8.314
  T_K <- Tcham + 273.15
  # Vtot in cm3 -> m3, Area in cm2 -> m2
  Vtot_m3 <- Vtot / 1e6
  Area_m2 <- Area / 1e4
  P_Pa <- Pcham * 1000  # kPa -> Pa

  lm_flux_nmol <- lm_slope * 1e-9 * P_Pa / (R_gas * T_K) * Vtot_m3 / Area_m2 * 1e9

  # Try HM fit via goFlux if possible
  hm_flux_nmol <- NA
  hm_r2 <- NA
  tryCatch({
    # goFlux expects specific column structure; use manual calculation instead
    # HM: C(t) = Ci + (C0 - Ci) * exp(-k*t), flux = k * (C0 - Ci) * P*V/(R*T*A)
    # Simplified: try nls
    C0_est <- ch4_ppb[1]
    Ct_est <- ch4_ppb[n_obs]
    if (Ct_est > C0_est) {
      # Increasing: standard HM
      nls_fit <- nls(ch4_ppb ~ Ci + (C0 - Ci) * exp(-k * etime),
                     data = data.frame(ch4_ppb = ch4_ppb, etime = etime),
                     start = list(Ci = Ct_est * 1.5, C0 = C0_est, k = -0.005),
                     control = nls.control(maxiter = 200, warnOnly = TRUE))
      hm_C0 <- coef(nls_fit)["C0"]
      hm_Ci <- coef(nls_fit)["Ci"]
      hm_k <- coef(nls_fit)["k"]
      hm_slope_t0 <- -hm_k * (hm_C0 - hm_Ci)  # ppb/s at t=0
      hm_flux_nmol <- hm_slope_t0 * 1e-9 * P_Pa / (R_gas * T_K) * Vtot_m3 / Area_m2 * 1e9
      ss_res <- sum(residuals(nls_fit)^2)
      ss_tot <- sum((ch4_ppb - mean(ch4_ppb))^2)
      hm_r2 <- 1 - ss_res / ss_tot
    }
  }, error = function(e) NULL)

  # Select best model
  if (!is.na(hm_r2) && hm_r2 > lm_r2 && !is.na(hm_flux_nmol)) {
    best_flux <- hm_flux_nmol
    best_model <- "HM"
  } else {
    best_flux <- lm_flux_nmol
    best_model <- "LM"
  }

  cat(sprintf("n=%d, LM=%.3f (r2=%.3f), HM=%s, best=%s %.3f nmol/m2/s\n",
              n_obs, lm_flux_nmol,  lm_r2,
              ifelse(is.na(hm_flux_nmol), "NA", sprintf("%.3f", hm_flux_nmol)),
              best_model, best_flux))

  reprocessed[[fid]] <- tibble(
    flux_id = fid,
    new_CH4_best.flux = best_flux,
    new_CH4_model = best_model,
    new_CH4_LM.flux = lm_flux_nmol,
    new_CH4_LM.r2 = lm_r2,
    new_CH4_LM.p.val = lm_pval,
    new_CH4_HM.flux = hm_flux_nmol,
    new_CH4_HM.r2 = hm_r2,
    n_obs = n_obs,
    new_start = w$new_start_etime,
    new_end = w$new_end_etime
  )
}

reprocessed_df <- bind_rows(reprocessed)
cat("\nReprocessed:", nrow(reprocessed_df), "traces\n")
write_csv(reprocessed_df, "output/ebullition/reprocessed_negative_fluxes.csv")

# ---- STEP 3: Update combined dataset ----------------------------------------

cat("\nUpdating combined dataset...\n")

for (i in seq_len(nrow(reprocessed_df))) {
  r <- reprocessed_df[i, ]
  idx <- which(df$flux_id == r$flux_id)
  if (length(idx) != 1) { cat("  Warning: flux_id", r$flux_id, "not found or duplicated\n"); next }

  old_flux <- df$CH4_best.flux[idx]
  df$CH4_best.flux[idx] <- r$new_CH4_best.flux
  df$CH4_model[idx] <- r$new_CH4_model
  df$CH4_LM.flux[idx] <- r$new_CH4_LM.flux
  df$CH4_LM.r2[idx] <- r$new_CH4_LM.r2
  df$CH4_LM.p.val[idx] <- r$new_CH4_LM.p.val
  if (!is.na(r$new_CH4_HM.flux)) {
    df$CH4_HM.flux[idx] <- r$new_CH4_HM.flux
    df$CH4_HM.r2[idx] <- r$new_CH4_HM.r2
  }

  cat(sprintf("  %s: %.3f -> %.3f (%s)\n", r$flux_id, old_flux, r$new_CH4_best.flux, r$new_CH4_model))
}

# Save
write_csv(df, "output/combined_gas_flux_dataset.csv")
cat("\nSaved updated dataset:", nrow(df), "rows\n")

# ---- Summary -----------------------------------------------------------------

cat("\n=== SUMMARY ===\n")
cat("Artifacts removed:", n_removed, "\n")
cat("Traces reprocessed:", nrow(reprocessed_df), "\n")
cat("  Now positive:", sum(reprocessed_df$new_CH4_best.flux > 0), "\n")
cat("  Still negative:", sum(reprocessed_df$new_CH4_best.flux <= 0), "\n")

# Check remaining negatives
remaining_neg <- df %>% filter(CH4_flux_status == "valid", CH4_best.flux < 0)
cat("\nRemaining negative CH4 fluxes in dataset:", nrow(remaining_neg), "\n")
cat("  (was 69, removed", n_removed, "artifacts, reprocessed", nrow(reprocessed_df), ")\n")
