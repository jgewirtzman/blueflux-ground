# =============================================================================
# Build one goFlux auxfile for every chamber measurement from tracked inputs
# only (handoff work plan, step 2).
#
# Inputs (all tracked):
#   data/field_notes/blueflux compiled tree fluxes.csv (+ _additional.csv)
#   data/field_notes/BlueFlux Dataset_soils_water.csv
#   data/field_notes/dimension_csvs/*.csv          chamber + instrument dimensions
#   data/flux_metadata/*.csv                        curated overrides (03_migrate_...)
#
# Output: output/rebuild/auxfile.csv, one row per measurement, with geometry
#   (Area cm2, Vcham/Vtube/Vinst cm3, Vtot L), Tcham (C) and its source, Pcham,
#   field-log start/end and exclusion flag. The legacy HA/HB, Mar 2022 soil and
#   volume-reconciliation patches are not needed: geometry is right at source.
#
# Geometry rules are ported unchanged from code/02_preprocess/assign_*_vol_area.R
# and air-temperature filling from fill_air_temp.R / fill_soil_air_temp.R; the
# comparison block at the end checks the result against the legacy tables.
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(lubridate); library(purrr); library(stringr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

dims_dir <- "data/field_notes/dimension_csvs"
meta_dir <- "data/flux_metadata"
PCHAM    <- 101.325   # kPa; used only where the tower has no pressure (all of Mar 2022)

rd <- function(f, ...) read_csv(f, show_col_types = FALSE, ...)
parse_dt <- function(d, t) {
  t <- sub("^\\s*(\\d{1,2}:\\d{2})\\s*$", "\\1:00", t)   # "12:57" -> "12:57:00"
  x <- suppressWarnings(mdy_hms(paste(d, t), quiet = TRUE))
  i <- is.na(x); x[i] <- suppressWarnings(dmy_hms(paste(d[i], t[i]), quiet = TRUE))
  i <- is.na(x); x[i] <- suppressWarnings(ymd_hms(paste(d[i], t[i]), quiet = TRUE))
  x
}

# ---- Dimension tables -----------------------------------------------------------
instr <- rd(file.path(dims_dir, "additional_vol.csv")) %>%
  mutate(analyzer = if_else(tolower(instrument) == "lgr_mgga", "LGR", "Picarro"))
iv <- function(a, col) instr[[col]][instr$analyzer == a]
lgr_cell <- iv("LGR", "analyzer_cell"); lgr_tube <- iv("LGR", "tubing")
dr_small <- instr$drierite_small[1];     dr_large <- instr$drierite_large[1]

simp <- rd(file.path(dims_dir, "simplified_volume.csv"))
names(simp) <- gsub("\\s+", "_", gsub("\\(|\\)", "", gsub("\\n|\\r", "_", names(simp))))
# A-D pure chamber volume = injection total (LGR in loop, no Drierite) - tubing - LGR cell
abcd_vol <- simp %>% filter(Chamber_Alt_ID %in% c("A", "B", "C", "D")) %>%
  group_by(chamber_class = Chamber_Alt_ID) %>%
  summarise(vol = mean(Total_Volume_mL - lgr_tube - lgr_cell, na.rm = TRUE), .groups = "drop")
sa <- rd(file.path(dims_dir, "surface_area.csv")) %>%
  transmute(chamber_class = substr(`Chamber ID`, 1, 1), a_inch = as.numeric(`a (inch)`),
            area = as.numeric(`SA cm2`))
leaf <- rd(file.path(dims_dir, "mangrove_leaf_data.csv")) %>%
  transmute(species = recode(Species, "Rhizophora mangle" = "RHMA", "Laguncularia racemosa" = "LARA",
                             "Avicennia germinans" = "AVGE"),
            forest_type = `Forest Type`, leaf_area = `1-sided leaf area for 15 leaves (cm2)`)
swd <- rd(file.path(dims_dir, "soil_water_dims.csv"))

vol_of  <- function(cl) abcd_vol$vol[match(cl, abcd_vol$chamber_class)]
area_of <- function(cl) sa$area[match(cl, sa$chamber_class)]
h_of    <- function(cl) sa$a_inch[match(cl, sa$chamber_class)] * 2.54   # chamber height, cm

drierite <- function(date) case_when(
  year(date) == 2022 & month(date) == 3 ~ dr_small,
  (year(date) == 2022 & month(date) == 10) | (year(date) == 2023 & month(date) == 3) ~ dr_large,
  TRUE ~ NA_real_)

# ---- Curated metadata -------------------------------------------------------------
cham_ovr <- rd(file.path(meta_dir, "chamber_overrides.csv"))
date_fix <- rd(file.path(meta_dir, "date_corrections.csv"))
excluded <- rd(file.path(meta_dir, "excluded_measurements.csv"))
scan_ids <- rd(file.path(meta_dir, "chamber_ids_from_scans.csv"))   # chambers missing from the compiled sheet
an_fix   <- rd(file.path(meta_dir, "analyzer_corrections.csv"))     # sheet says one analyzer, data are on another
parse_date <- function(x) as.Date(coalesce(mdy(x, quiet = TRUE), dmy(x, quiet = TRUE), ymd(x, quiet = TRUE)))

# ---- Tower air temperature and pressure (US-Skr, AmeriFlux BASE) -------------------
# Timestamps are local standard time (EST, UTC-5); field times are local clock
# time (America/New_York, so EDT in most campaigns). Both are compared in UTC.
tower <- read.csv("data/tower/AMF_US-Skr_BASE_HH_2-5.csv", skip = 2, na.strings = "-9999")
tower_t <- as.numeric(as.POSIXct(as.character(tower$TIMESTAMP_START), format = "%Y%m%d%H%M",
                                 tz = "Etc/GMT+5")) + 900          # half-hour midpoint
tower_value <- function(t, var, max_gap_s = 3 * 3600) {
  ok <- is.finite(tower[[var]])
  x <- tower_t[ok]; y <- tower[[var]][ok]
  v <- approx(x, y, xout = t, rule = 1)$y
  nearest <- vapply(t, function(ti) if (is.na(ti)) NA_real_ else min(abs(x - ti)), 1)
  v[is.na(nearest) | nearest > max_gap_s] <- NA_real_
  v
}
local_to_utc <- function(x) as.numeric(force_tz(x, "America/New_York"))

# ---- Field rows ----------------------------------------------------------------------
read_trees <- function(f) rd(f, col_types = cols(.default = col_character()))
trees <- bind_rows(read_trees("data/field_notes/blueflux compiled tree fluxes.csv"),
                   read_trees("data/field_notes/blueflux compiled tree fluxes_additional.csv")) %>%
  filter(!is.na(flux_id))
stopifnot(!anyDuplicated(trees$flux_id))
sw_raw <- rd("data/field_notes/BlueFlux Dataset_soils_water.csv", col_types = cols(.default = col_character()))
names(sw_raw)[1] <- "index"
sw <- sw_raw %>% filter(!is.na(flux_id))
stopifnot(!anyDuplicated(sw$flux_id))

field <- bind_rows(
  trees %>% transmute(flux_id, measurement_type = "tree", component, plot, analyzer = analyzer_id,
                      date_rec = parse_date(date), fieldlog_start = start_time, fieldlog_end = end_time,
                      air_temp_measured = suppressWarnings(as.numeric(air_temp)),
                      chamber_class, species, diameter = as.numeric(diameter)),
  sw %>% transmute(flux_id, measurement_type = "surface", component = tolower(Surface), plot = Plot,
                   analyzer = `Gas Analyzer`, date_rec = parse_date(Date),
                   fieldlog_start = `Flux Start Time`, fieldlog_end = `Flux End Time`,
                   air_temp_measured = NA_real_, recorded_chamber_id = `Chamber ID`)
) %>%
  # date and analyzer corrections come first, so everything below uses them
  left_join(date_fix %>% select(flux_id, date_fixed = date), by = "flux_id") %>%
  left_join(an_fix %>% select(flux_id, analyzer_fixed = analyzer), by = "flux_id") %>%
  mutate(analyzer = coalesce(analyzer_fixed, analyzer)) %>%
  mutate(date = coalesce(as.Date(date_fixed), date_rec),
         dt = parse_dt(format(date, "%m/%d/%Y"), fieldlog_start),
         t_utc = local_to_utc(dt))

# ---- Chamber air temperature ------------------------------------------------------------
# Measured values only, never values filled earlier:
#   1. measured on the sheet;
#   2. mean of same-plot tree-sheet readings within 30 min (same day);
#   3. tower air temperature (TA_1_1_1) at the measurement time;
#   4. nearest same-plot reading on the same day;
#   5. no start time: mean of same-plot readings that day.
pool <- field %>% filter(!is.na(air_temp_measured), !is.na(t_utc)) %>%
  select(pid = flux_id, plot, date, t_utc, at = air_temp_measured)
field$tower_TA_raw <- tower_value(field$t_utc, "TA_1_1_1")
# The tower (SRS6 canopy) reads cooler than chamber-side air at most plots.
# Calibrate it per plot x campaign by the median (field - tower) difference
# over measured readings (n >= 3), else the overall median.
field$campaign <- format(field$date, "%Y-%m")
tower_bias <- field %>% filter(!is.na(air_temp_measured), !is.na(tower_TA_raw)) %>%
  mutate(d = air_temp_measured - tower_TA_raw)
overall_bias <- median(tower_bias$d)
tower_bias <- tower_bias %>% group_by(plot, campaign) %>%
  summarise(n_bias = n(), bias = median(d), .groups = "drop") %>% filter(n_bias >= 3)
field <- field %>% left_join(tower_bias, by = c("plot", "campaign")) %>%
  mutate(tower_bias_basis = if_else(is.na(bias), "overall", "plot x campaign"),
         bias = coalesce(bias, overall_bias),
         tower_TA = tower_TA_raw + bias)
write_csv(tower_bias %>% add_row(plot = "(overall)", campaign = "all", n_bias = nrow(tower_bias), bias = overall_bias),
          "output/rebuild/tower_air_temp_bias.csv")
field$Tcham <- field$air_temp_measured
field$Tcham_source <- if_else(!is.na(field$Tcham), "field sheet", NA_character_)
for (i in which(is.na(field$Tcham))) {
  same <- pool[pool$plot == field$plot[i] & pool$date == field$date[i] & pool$pid != field$flux_id[i], ]
  if (!is.na(field$t_utc[i])) {
    gap <- abs(same$t_utc - field$t_utc[i])
    if (any(gap <= 1800)) {
      field$Tcham[i] <- mean(same$at[gap <= 1800]); field$Tcham_source[i] <- "same-plot tree air <=30 min"
    } else if (!is.na(field$tower_TA[i])) {
      field$Tcham[i] <- field$tower_TA[i]
      field$Tcham_source[i] <- paste0("tower TA_1_1_1 + ", field$tower_bias_basis[i], " bias")
    } else if (nrow(same)) {
      field$Tcham[i] <- same$at[which.min(gap)]; field$Tcham_source[i] <- "same-plot tree air, nearest same day"
    }
  } else if (nrow(same)) {
    field$Tcham[i] <- mean(same$at); field$Tcham_source[i] <- "same-plot daily mean (no start time)"
  } else {
    # no start time and no same-plot readings: tower mean over the span of the
    # same-plot measurements that day
    span <- range(field$t_utc[field$plot == field$plot[i] & field$date == field$date[i]], na.rm = TRUE)
    if (all(is.finite(span))) {
      field$Tcham[i] <- mean(tower_value(seq(span[1], span[2], by = 600), "TA_1_1_1"), na.rm = TRUE) + field$bias[i]
      field$Tcham_source[i] <- "tower TA_1_1_1 + bias, mean over same-plot session (no start time)"
    }
  }
}

# Decision (Jon, 2026-10-01): chamber air temperature is the tower record for
# every measurement; the handheld readings run warm (pocket / sun). The
# handheld-based value above is kept as Tcham_handheld for comparison and is
# used only where the tower has no value.
field$Tcham_handheld <- field$Tcham; field$Tcham_handheld_source <- field$Tcham_source
no_start <- is.na(field$t_utc)
session_tower <- vapply(seq_len(nrow(field)), function(i) {
  if (!no_start[i]) return(NA_real_)
  span <- range(field$t_utc[field$plot == field$plot[i] & field$date == field$date[i]], na.rm = TRUE)
  if (!all(is.finite(span))) return(NA_real_)
  mean(tower_value(seq(span[1], span[2], by = 600), "TA_1_1_1"), na.rm = TRUE)
}, 1)
field <- field %>% mutate(
  Tcham = coalesce(tower_TA_raw, session_tower, Tcham_handheld),
  Tcham_source = case_when(!is.na(tower_TA_raw) ~ "tower TA_1_1_1",
                           !is.na(session_tower) ~ "tower TA_1_1_1, mean over same-plot session (no start time)",
                           TRUE ~ paste0("handheld (no tower value): ", Tcham_handheld_source)))

# ---- Chamber pressure ---------------------------------------------------------------------
field$Pcham <- tower_value(field$t_utc, "PA")
field$Pcham_source <- if_else(is.na(field$Pcham), "default 101.325 kPa (no tower PA)", "tower PA")
field$Pcham[is.na(field$Pcham)] <- PCHAM

# ---- Geometry: trees ------------------------------------------------------------------------
geo_trees <- field %>% filter(measurement_type == "tree") %>%
  mutate(
    Vcham = case_when(
      chamber_class %in% c("A", "B", "C", "D") ~ vol_of(chamber_class),
      chamber_class == "HA" ~ vol_of("A") - pi * (diameter / 2)^2 * h_of("A"),
      chamber_class == "HB" ~ vol_of("B") - pi * (diameter / 2)^2 * h_of("B"),
      chamber_class == "LB" ~ vol_of("D"),
      TRUE ~ NA_real_),
    Area = case_when(
      component %in% c("leaf", "leaves") ~
        leaf$leaf_area[match(paste(species, if_else(plot == "SE1", "Scrub", "Fringe")),
                             paste(leaf$species, leaf$forest_type))],
      chamber_class %in% c("A", "B", "C", "D") ~ area_of(chamber_class),
      chamber_class == "HA" ~ 2 * pi * (diameter / 2) * h_of("A"),
      chamber_class == "HB" ~ 2 * pi * (diameter / 2) * h_of("B"),
      TRUE ~ NA_real_),
    geometry_rule = case_when(
      component %in% c("leaf", "leaves") ~ paste0("leaf area table; chamber ", chamber_class),
      chamber_class %in% c("HA", "HB") ~ paste0(chamber_class, ": ", if_else(chamber_class == "HA", "A", "B"),
                                                 " chamber minus stem cylinder (diameter cm)"),
      TRUE ~ paste0("chamber ", chamber_class)),
    chamber_id = coalesce(chamber_class, scan_ids$chamber_id[match(flux_id, scan_ids$flux_id)]),
    geometry_rule = if_else(is.na(chamber_class) & !is.na(chamber_id),
                            paste0("chamber ", chamber_id, " (from scanned sheet; no dimensions yet)"), geometry_rule))

# ---- Geometry: soil and water ---------------------------------------------------------------
# Floating chamber: the "collar" in soil_water_dims.csv is the foam float
# (2.54 cm), which adds headspace above the water surface.
geo_sw <- field %>% filter(measurement_type == "surface") %>%
  left_join(cham_ovr %>% select(flux_id, chamber_override = chamber_id, collar_offset_override = collar_offset_cm),
            by = "flux_id") %>%
  mutate(chamber_id = coalesce(chamber_override, recorded_chamber_id)) %>%
  left_join(swd %>% select(chamber_id = Chamber, dome_cm3 = Chamber_Volume_cm3,
                           `Chamber+Collar_Volume_L`, Area = Ground_Surface_Area_cm2),
            by = "chamber_id") %>%
  mutate(
    Vcham = if_else(is.na(collar_offset_override), `Chamber+Collar_Volume_L` * 1000,
                    dome_cm3 + Area * collar_offset_override),
    geometry_rule = if_else(is.na(collar_offset_override), paste0("soil_water_dims: ", chamber_id),
                            paste0("override: ", chamber_id, ", collar ", collar_offset_override, " cm")))

# ---- Field-log end times ---------------------------------------------------------------------
# Some sheets carry the wrong hour on the end time (e.g. start 13:23, end
# "12:30"; the saved manual window ends at 13:30). Where the logged closure is
# <= 0 or > MAX_CLOSURE_S, keep the end time's minutes and seconds and take the
# hour that places it within (0, MAX_CLOSURE_S] after the start; if no hour
# does, the end time is dropped.
MAX_CLOSURE_S <- 1800
repair_end <- function(start, end) {
  out <- end
  bad <- !is.na(start) & !is.na(end) &
    (as.numeric(difftime(end, start, units = "secs")) <= 0 |
       as.numeric(difftime(end, start, units = "secs")) > MAX_CLOSURE_S)
  for (i in which(bad)) {
    ms <- minute(end[i]) * 60 + second(end[i])
    cand <- floor_date(start[i], "hour") + ms + c(0, 3600)
    d <- as.numeric(difftime(cand, start[i], units = "secs"))
    ok <- d > 0 & d <= MAX_CLOSURE_S
    out[i] <- if (any(ok)) cand[ok][1] else as.POSIXct(NA, tz = tz(end))
  }
  out
}

# ---- Combine ----------------------------------------------------------------------------------
aux <- bind_rows(geo_trees, geo_sw) %>%
  mutate(
    instrument = if_else(grepl("^LGR", analyzer), "LGR", "Picarro"),
    cell = if_else(instrument == "LGR", lgr_cell, iv("Picarro", "analyzer_cell")),
    tube = if_else(instrument == "LGR", lgr_tube, iv("Picarro", "tubing")),
    filt = drierite(date),
    start.time = format(dt, "%Y-%m-%d %H:%M:%S"),
    end_raw    = parse_dt(format(date, "%m/%d/%Y"), fieldlog_end),
    end_fixed  = repair_end(dt, end_raw),
    end_time_repair = case_when(is.na(end_raw) ~ "no end time on sheet",
                                is.na(end_fixed) ~ "end time inconsistent with start; dropped",
                                end_fixed != end_raw ~ paste0("hour corrected from ", format(end_raw, "%H:%M:%S")),
                                TRUE ~ NA_character_),
    end.time   = format(end_fixed, "%Y-%m-%d %H:%M:%S"),
    obs.length = as.numeric(difftime(end_fixed, dt, units = "secs")),
    Vtube = tube, Vinst = cell + filt,
    Vtot = (Vcham + tube + cell + filt) / 1000,
    offset = 0,
    excluded = flux_id %in% excluded$flux_id
  ) %>%
  transmute(UniqueID = flux_id, measurement_type, component, plot, analyzer, date, start.time, end.time,
            obs.length, chamber_id, geometry_rule, Area, offset, Vcham, Vtube, Vinst, Vtot,
            Tcham, Tcham_source, Tcham_handheld, Tcham_handheld_source, tower_TA_raw, tower_TA_bias = bias, Pcham, Pcham_source, end_time_repair,
            date_corrected = !is.na(date_fixed), analyzer_corrected = !is.na(analyzer_fixed), excluded) %>%
  arrange(date, analyzer, start.time, UniqueID)
stopifnot(!anyDuplicated(aux$UniqueID))
write_csv(aux, "output/rebuild/auxfile.csv")
cat("Wrote output/rebuild/auxfile.csv:", nrow(aux), "measurements\n")
cat("  missing Area:", sum(is.na(aux$Area)), "| missing Vtot:", sum(is.na(aux$Vtot)),
    "| missing Tcham:", sum(is.na(aux$Tcham)), "| missing start:", sum(is.na(aux$start.time)), "\n")

# ---- Verify against the legacy pipeline ------------------------------------------------
# (a) legacy preprocessing tables (before the dataset-level patches)
# (b) legacy final dataset (after HA/HB, Mar 2022 and volume patches)
leg_pre <- bind_rows(
  rd("intermediate/main_trees_complete.csv", col_types = cols(.default = col_character())),
  rd("intermediate/main_trees_complete_additional.csv", col_types = cols(.default = col_character())),
  rd("intermediate/main_soilwater_complete.csv", col_types = cols(.default = col_character()))) %>%
  distinct(flux_id, .keep_all = TRUE) %>%
  transmute(UniqueID = flux_id, pre_Area = as.numeric(surface_area_cm2),
            pre_Vtot_cm3 = as.numeric(total_system_volume_cm3), pre_Tcham = as.numeric(air_temp))
leg_final <- rd("output/rebuild/baseline/output__data_products__combined_gas_flux_dataset.csv") %>%
  filter(data_source != "ebullition_reprocessing") %>%
  transmute(UniqueID = flux_id, final_Area = surface_area_cm2, final_Vtot_cm3 = total_system_volume_cm3,
            final_Tcham = air_temp, final_date = as.Date(date))
rel <- function(a, b) abs(a - b) / pmax(abs(b), 1e-9)
cmp <- aux %>% select(UniqueID, measurement_type, component, plot, analyzer, chamber_id, excluded,
                      Area, Vtot, Tcham, date) %>%
  left_join(leg_pre, by = "UniqueID") %>% left_join(leg_final, by = "UniqueID") %>%
  mutate(Vtot_cm3 = Vtot * 1000,
         d_pre_Area = rel(Area, pre_Area), d_pre_Vtot = rel(Vtot_cm3, pre_Vtot_cm3),
         d_pre_T = abs(Tcham - pre_Tcham),
         d_final_Area = rel(Area, final_Area), d_final_Vtot = rel(Vtot_cm3, final_Vtot_cm3),
         d_final_T = abs(Tcham - final_Tcham),
         date_matches_final = is.na(final_date) | (!is.na(date) & date == final_date))
tol <- 1e-6
flag <- function(x) !is.na(x) & x > tol
cmp <- cmp %>% mutate(
  geometry_mismatch_vs_final = flag(d_final_Area) | flag(d_final_Vtot) | !date_matches_final,
  Tcham_changed = flag(d_final_T) | (is.na(Tcham) != is.na(final_Tcham)))
write_csv(cmp, "output/rebuild/auxfile_vs_legacy.csv")
n_final <- sum(!is.na(cmp$final_Area) | !is.na(cmp$final_Vtot_cm3))
cat("\nComparison with the legacy final dataset (", n_final, "rows with geometry):\n")
cat("  geometry/date mismatches:", sum(cmp$geometry_mismatch_vs_final), "(must be 0)\n")
stopifnot(sum(cmp$geometry_mismatch_vs_final) == 0)
cat("  Tcham changed (intended: tower air temperature for all):", sum(cmp$Tcham_changed), "\n")
print(as.data.frame(cmp %>% filter(Tcham_changed) %>% left_join(aux %>% select(UniqueID, Tcham_source), by = "UniqueID") %>%
  group_by(Tcham_source) %>%
  summarise(n = n(), median_dT = median(Tcham - final_Tcham, na.rm = TRUE),
            max_abs_dT = max(abs(Tcham - final_Tcham), na.rm = TRUE),
            now_filled = sum(!is.na(Tcham) & is.na(final_Tcham)), .groups = "drop")), row.names = FALSE)
cat("\nTcham source:\n"); print(table(aux$Tcham_source, useNA = "ifany"))
cat("\nPcham source:\n"); print(table(aux$Pcham_source, useNA = "ifany"))
cat("Pcham range (kPa):", round(range(aux$Pcham), 3), "\n")
