# Scripted flux-workflow rebuild

Work plan: `_archive/HANDOFF_flux_workflow_rebuild.md`. Scripts run from the
project root; outputs go to `output/rebuild/`.

| Script | Writes | Notes |
|--------|--------|-------|
| `01_freeze_baseline.R` | `output/rebuild/baseline/` + `MANIFEST.csv` | Copies the legacy dataset and key tables (md5, rows, git commit). Refuses to overwrite unless `FREEZE_OVERWRITE=1`. Frozen at `600d03d`. |
| `02_measurement_inventory.R` | `raw_file_index.csv`, `measurement_inventory.csv`, `inventory_summary.txt` | One row per measurement (834 fitted + 40 ebullition rows = 874; 867 in the final dataset): analyzer and logger serial, field-log times, raw coverage, logging interval, saved manual window and its offset from the field log, current flux source, patches applied. Needs `data/analyzer/` and `intermediate/`. Zipped raw files are extracted to a temp directory only. |
| `03_migrate_curated_metadata.R` | `data/flux_metadata/*.csv` | One-time copy of hand-curated values out of gitignored intermediates and hard-coded script constants (air temps, chamber and date overrides, exclusions, trimmed windows, ebullition lists), with provenance. Refuses to overwrite unless `MIGRATE_OVERWRITE=1`. |
| `04_build_auxfile.R` | `output/rebuild/auxfile.csv`, `auxfile_vs_legacy.csv` | One goFlux auxfile for all 834 measurements from tracked inputs only (field sheets, dimension tables, `data/flux_metadata/`, US-Skr tower file). Area and Vtot match the legacy final dataset exactly for all 805 rows with geometry (the script stops otherwise), including the rows the HA/HB and Mar 2022 patches corrected. |

Logger serials: LGR1 = SN:3K60180500001585, LGR2 = SN:3K60180500001583,
LGR3 = SN:3K60180500001584. Picarro `.dat` files carry no serial.

### Changes from the legacy preprocessing (04_build_auxfile.R)

- Date corrections are applied before anything else (legacy: after the
  temperature fill, so BL60 water 168-171 got temperatures for the wrong day).
- Air temperature is the US-Skr tower `TA_1_1_1` at the measurement time for
  every measurement (Jon, 2026-10-01: handheld readings run ~1.3 C warm). The
  handheld chain (field sheet; same-plot readings within 30 min; calibrated
  tower; nearest same-plot reading) is kept as `Tcham_handheld` and used only
  where the tower has no value. `05_air_temperature_options.R` shows the
  difference: median +0.4% flux (10-90%: -0.9 to +1.4%).
- Pressure from tower `PA` (706 rows, 100.55-101.87 kPa); 101.325 kPa where
  the tower has none (all of Mar 2022, 128 rows).
- Field-log times written "HH:MM" are read as HH:MM:00 (one was misread as a
  2020 timestamp).
- End times with a wrong hour (closure <= 0 or > 30 min) are repaired by
  keeping minutes:seconds and choosing the hour that gives a 0-30 min
  closure (13 rows, all within ~5 min of the saved manual windows); 2 that
  cannot be repaired are dropped. Recorded in `end_time_repair`.

### Step 3: clock offsets and windows

| Script | Writes | Notes |
|--------|--------|-------|
| `lib_raw.R` | - | Raw reader (`read_raw(unit, from, to)`): own logger serial only, duplicates skipped, zips read in a temp dir. LGR `[H2O]_ppm` is negative (~ -760 ppm) and the Picarro H2O (%) has spikes; to be handled before goFlux's H2O correction. |
| `06_clock_offsets.R` | `clock_offsets.csv`, `clock_offsets_closures.csv` | Per-closure CO2-rise detection (`fluxqc::find_rise`, limits scaled to the logging interval) around field start + prior. A diagnostic: `fluxqc::find_clock_offset()` gave flat score curves on most days, and per-closure detection is often pinned at the search edge. |
| `07_windows.R` | `windows.csv`, `windows_disagreements.csv` | Offset per analyzer-day from the saved manual windows (same day, else same analyzer-campaign; LGR within ~+-30 s of the field watch, Picarro +25093 to +25231 s). Window = curated trimmed window, else saved manual window, else field log + offset (276 closures, mostly LGR2 trees). Where saved and scripted windows both exist they are compared; 127 large disagreements listed, saved kept (Jon's decision 3). |

The 14 trimmed windows were picked on analyzer-clock data, so their absolute
times need no offset.

### Step 4: fit

| Script | Writes | Notes |
|--------|--------|-------|
| `08_fit_fluxes.R` | `fit/CO2/`, `fit/CH4/` (`fluxes.csv`, `settings.json`) | `fluxqc::process_fluxes()` per gas on windows from `windows.csv` (padded 5 min for the MAD precision); tower Tcham/Pcham; no H2O correction; legacy `best.flux` criteria; HM only with >= 30 points, else LM; group = analyzer x campaign; MDF = 1.96 sigma_MAD / t x flux.term. QC screens: c0, co2_tracer (off for water/leaves/CWD), convex, min_window, noisy; ambient_start off (windows start after a dead band by design). 770 of 804 closures fitted; 34 have no raw data in the window (29 of them have no legacy flux either; 5 legacy-only: 3 Picarro 2022-10-20, 2 LGR3 2023-03-15 after the last file). |
| `09_compare_fit_vs_legacy.R` | `fit_vs_legacy_CH4.csv` | Saved-window closures: median new/legacy 1.004, 79% within 5%. Scripted windows (legacy windows lost): median 0.985, IQR 0.78-1.27, 18 sign flips. |

`03_migrate_curated_metadata.R` now writes a table only if absent or named in
`MIGRATE_ONLY`; the trimmed-window anchors were corrected to the local-clock
start times of the `*_goflux` auxfiles (the `*_all_instruments` files store
them shifted to UTC, which put 12 windows 4-5 h late).
| `10_audit_saved_traces.R` | `saved_trace_audit.csv` | Checks that each saved window's rows come from the assigned analyzer's raw record. Legacy imports used `import2RData(merge = TRUE)` on a shared `RData/` folder, so some closures were fitted on another analyzer's record: 5 CP40 closures of 2023-03-15 on LGR2 (fixed via `analyzer_corrections.csv` / `saved_window_rejections.csv`), 8 Oct 2022 closures (7 BL60, CP40 stem 200) on interleaved LGR3 + LGR1/LGR2 rows (the refit reads only the own analyzer), and the 3 Picarro waters of 2022-10-20 on LGR tree closures (excluded: no Picarro record exists). |

### Unmeasured water flux

| Script | Writes | Notes |
|--------|--------|-------|
| `11_water_flux_from_pch4.R` | `water_flux_estimates.csv`, `water_k_calibration.csv` | SRS5/SRS6 Oct 2022 have no chamber water flux (Picarro not logging). F = k600 (Sc/600)^-0.5 (Cw - Ceq), Cw from the SRS5 plot GC sample (19 Oct) and aquatic transect station SRS 6 (15 Oct); k600 calibrated on 7 chamber/dissolved pairs (median 1.10, range 0.56-7.07 cm h-1; CP40 Oct 2022 dropped, k600 = 532). SRS5 0.48 (0.25-3.08), SRS6 0.59 (0.30-3.79) nmol m-2 s-1. Read by `08_upscaling/upscale_methane_to_plots.R` for water with no site-level chamber flux. |
