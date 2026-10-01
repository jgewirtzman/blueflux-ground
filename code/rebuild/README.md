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
- Air temperature uses measured values only, from the same plot: field sheet;
  else mean of same-plot tree readings within 30 min; else tower `TA_1_1_1`
  at the measurement time; else nearest same-plot reading that day. Legacy
  fills pooled all sites, reused values filled earlier in the loop, skipped
  readings at the identical time, and fell back to a worldmet download.
  138 rows change; the largest changes replace implausible legacy values
  (e.g. 16.8 C on an October afternoon at SRS6, now 25.4 C from the tower).
- Pressure from tower `PA` (706 rows, 100.55-101.87 kPa); 101.325 kPa where
  the tower has none (all of Mar 2022, 128 rows).
- Field-log times written "HH:MM" are read as HH:MM:00 (one was misread as a
  2020 timestamp).
- End times with a wrong hour (closure <= 0 or > 30 min) are repaired by
  keeping minutes:seconds and choosing the hour that gives a 0-30 min
  closure (13 rows, all within ~5 min of the saved manual windows); 2 that
  cannot be repaired are dropped. Recorded in `end_time_repair`.
