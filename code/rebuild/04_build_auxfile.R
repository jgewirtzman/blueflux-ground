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
PCHAM    <- 101.325   # kPa; no reliable per-measurement pressure (see README)

rd <- function(f, ...) read_csv(f, show_col_types = FALSE, ...)
parse_dt <- function(d, t) {
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
air_ovr  <- rd(file.path(meta_dir, "air_temperature_overrides.csv"))
cham_ovr <- rd(file.path(meta_dir, "chamber_overrides.csv"))
date_fix <- rd(file.path(meta_dir, "date_corrections.csv"))
excluded <- rd(file.path(meta_dir, "excluded_measurements.csv"))

# ---- Trees -------------------------------------------------------------------------
read_trees <- function(f, sheet) rd(f, col_types = cols(.default = col_character())) %>%
  mutate(sheet = sheet)
trees <- bind_rows(read_trees("data/field_notes/blueflux compiled tree fluxes.csv", "main"),
                   read_trees("data/field_notes/blueflux compiled tree fluxes_additional.csv", "additional")) %>%
  filter(!is.na(flux_id))
stopifnot(!anyDuplicated(trees$flux_id))

# Air temperature: as fill_air_temp.R, within each sheet, in row order: mean of
# same-date values 0 < |dt| <= 30 min, else nearest same-day value, else NA.
# As in the legacy script, values filled earlier in the loop feed later fills.
fill_tree_temp <- function(d) {
  d <- d %>% mutate(dt = parse_dt(date, start_time), day = as.Date(dt), at = as.numeric(air_temp),
                    Tcham_source = if_else(!is.na(at), "field sheet", NA_character_))
  for (i in which(is.na(d$at) & !is.na(d$dt))) {
    gap <- abs(as.numeric(difftime(d$dt, d$dt[i], units = "mins")))
    pool <- which(!is.na(d$at) & d$day == d$day[i] & !is.na(d$dt))
    near <- pool[gap[pool] <= 30 & gap[pool] > 0]
    if (length(near)) { d$at[i] <- mean(d$at[near]); d$Tcham_source[i] <- "tree same-sheet <=30 min" }
    else if (length(pool)) { d$at[i] <- d$at[pool[which.min(gap[pool])]]; d$Tcham_source[i] <- "tree same-sheet same day" }
  }
  d
}
trees <- trees %>% group_split(sheet) %>% map_dfr(fill_tree_temp)

trees_aux <- trees %>%
  mutate(
    date = as.Date(coalesce(mdy(date, quiet = TRUE), dmy(date, quiet = TRUE), ymd(date, quiet = TRUE))),
    analyzer = analyzer_id,
    instrument = if_else(grepl("^LGR", analyzer_id), "LGR", "Picarro"),
    diameter = as.numeric(diameter),     # cm (HA/HB: stem diameter in cm, see README)
    cell = if_else(instrument == "LGR", lgr_cell, iv("Picarro", "analyzer_cell")),
    tube = if_else(instrument == "LGR", lgr_tube, iv("Picarro", "tubing")),
    filt = drierite(date),
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
    measurement_type = "tree", chamber_id = chamber_class,
    fieldlog_start = start_time, fieldlog_end = end_time,
    Tcham = at
  )

# ---- Soil and water ------------------------------------------------------------------
sw_raw <- rd("data/field_notes/BlueFlux Dataset_soils_water.csv", col_types = cols(.default = col_character()))
names(sw_raw)[1] <- "index"
sw <- sw_raw %>% filter(!is.na(flux_id)) %>%
  transmute(flux_id, plot = Plot, component = tolower(Surface), analyzer = `Gas Analyzer`,
            recorded_chamber_id = `Chamber ID`, date_raw = Date,
            fieldlog_start = `Flux Start Time`, fieldlog_end = `Flux End Time`,
            dt = parse_dt(Date, `Flux Start Time`),
            date = as.Date(coalesce(mdy(Date, quiet = TRUE), dmy(Date, quiet = TRUE), ymd(Date, quiet = TRUE))))
stopifnot(!anyDuplicated(sw$flux_id))

# Air temperature from the tree sheets (all sites), as fill_soil_air_temp.R:
# mean within 30 min on the same date, else nearest same-day value, else the
# frozen weather-station value in air_temperature_overrides.csv.
tree_temps <- trees %>% filter(!is.na(as.numeric(air_temp)), !is.na(dt)) %>%
  transmute(dt, day = as.Date(dt), at = as.numeric(air_temp))
sw$Tcham <- NA_real_; sw$Tcham_source <- NA_character_
for (i in which(!is.na(sw$dt))) {
  pool <- which(tree_temps$day == sw$date[i])
  if (!length(pool)) next
  gap <- abs(as.numeric(difftime(tree_temps$dt[pool], sw$dt[i], units = "mins")))
  if (any(gap <= 30)) { sw$Tcham[i] <- mean(tree_temps$at[pool[gap <= 30]]); sw$Tcham_source[i] <- "tree sheets <=30 min" }
  else { sw$Tcham[i] <- tree_temps$at[pool[which.min(gap)]]; sw$Tcham_source[i] <- "tree sheets same day" }
}
ovr <- match(sw$flux_id, air_ovr$flux_id)
use <- is.na(sw$Tcham) & !is.na(ovr)
sw$Tcham[use] <- air_ovr$air_temp_C[ovr[use]]
sw$Tcham_source[use] <- paste0("override: ", air_ovr$legacy_temp_source[ovr[use]])

sw_aux <- sw %>%
  left_join(cham_ovr %>% select(flux_id, chamber_override = chamber_id, collar_offset_override = collar_offset_cm),
            by = "flux_id") %>%
  mutate(
    chamber_id = coalesce(chamber_override, recorded_chamber_id),
    instrument = if_else(grepl("^LGR", analyzer), "LGR", "Picarro"),
    cell = if_else(instrument == "LGR", lgr_cell, iv("Picarro", "analyzer_cell")),
    tube = if_else(instrument == "LGR", lgr_tube, iv("Picarro", "tubing")),
    filt = drierite(date)
  ) %>%
  left_join(swd %>% select(chamber_id = Chamber, dome_cm3 = Chamber_Volume_cm3, Offset_cm,
                           Collar_Volume_cm3, `Chamber+Collar_Volume_L`, Area = Ground_Surface_Area_cm2),
            by = "chamber_id") %>%
  mutate(
    Vcham = if_else(is.na(collar_offset_override), `Chamber+Collar_Volume_L` * 1000,
                    dome_cm3 + Area * collar_offset_override),
    geometry_rule = if_else(is.na(collar_offset_override), paste0("soil_water_dims: ", chamber_id),
                            paste0("override: ", chamber_id, ", collar ", collar_offset_override, " cm")),
    measurement_type = "surface")

# ---- Combine ---------------------------------------------------------------------------
aux <- bind_rows(
  trees_aux %>% select(flux_id, measurement_type, component, plot, analyzer, date, fieldlog_start, fieldlog_end,
                       chamber_id, geometry_rule, Area, Vcham, cell, tube, filt, Tcham, Tcham_source),
  sw_aux %>% select(flux_id, measurement_type, component, plot, analyzer, date, fieldlog_start, fieldlog_end,
                    chamber_id, geometry_rule, Area, Vcham, cell, tube, filt, Tcham, Tcham_source)
) %>%
  left_join(date_fix %>% select(flux_id, date_fixed = date), by = "flux_id") %>%
  mutate(
    date = coalesce(as.Date(date_fixed), date),
    start.time = format(parse_dt(format(date, "%m/%d/%Y"), fieldlog_start), "%Y-%m-%d %H:%M:%S"),
    end.time   = format(parse_dt(format(date, "%m/%d/%Y"), fieldlog_end), "%Y-%m-%d %H:%M:%S"),
    obs.length = as.numeric(difftime(ymd_hms(end.time, quiet = TRUE), ymd_hms(start.time, quiet = TRUE), units = "secs")),
    Vtube = tube, Vinst = cell + filt,
    Vtot = (Vcham + tube + cell + filt) / 1000,
    Pcham = PCHAM, offset = 0,
    excluded = flux_id %in% excluded$flux_id
  ) %>%
  transmute(UniqueID = flux_id, measurement_type, component, plot, analyzer, date, start.time, end.time,
            obs.length, chamber_id, geometry_rule, Area, offset, Vcham, Vtube, Vinst, Vtot,
            Tcham, Tcham_source, Pcham, date_corrected = !is.na(date_fixed), excluded) %>%
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
  mismatch_vs_final = flag(d_final_Area) | flag(d_final_Vtot) | flag(d_final_T) |
    (is.na(Tcham) != is.na(final_Tcham) & !is.na(final_Area)) | !date_matches_final,
  mismatch_vs_pre   = flag(d_pre_Area) | flag(d_pre_Vtot) | flag(d_pre_T))
write_csv(cmp, "output/rebuild/auxfile_vs_legacy.csv")
cat("\nComparison with legacy (tolerance", tol, "):\n")
cat("  rows in legacy final dataset:", sum(!is.na(cmp$final_Area) | !is.na(cmp$final_Vtot_cm3)),
    "| mismatching:", sum(cmp$mismatch_vs_final), "\n")
cat("  rows in legacy preprocessing tables:", sum(!is.na(cmp$pre_Vtot_cm3) | !is.na(cmp$pre_Area)),
    "| mismatching:", sum(cmp$mismatch_vs_pre), "\n")
cat("  not in legacy final dataset:", sum(is.na(cmp$final_Area) & is.na(cmp$final_Vtot_cm3)),
    "(", paste(head(cmp$UniqueID[is.na(cmp$final_Area) & is.na(cmp$final_Vtot_cm3)], 10), collapse = ", "), ")\n")
