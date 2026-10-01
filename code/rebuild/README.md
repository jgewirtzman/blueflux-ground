# Scripted flux-workflow rebuild

Work plan: `_archive/HANDOFF_flux_workflow_rebuild.md`. Scripts run from the
project root; outputs go to `output/rebuild/`.

| Script | Writes | Notes |
|--------|--------|-------|
| `01_freeze_baseline.R` | `output/rebuild/baseline/` + `MANIFEST.csv` | Copies the legacy dataset and key tables (md5, rows, git commit). Refuses to overwrite unless `FREEZE_OVERWRITE=1`. Frozen at `600d03d`. |
| `02_measurement_inventory.R` | `raw_file_index.csv`, `measurement_inventory.csv`, `inventory_summary.txt` | One row per measurement (834 fitted + 40 ebullition rows = 874; 867 in the final dataset): analyzer and logger serial, field-log times, raw coverage, logging interval, saved manual window and its offset from the field log, current flux source, patches applied. Needs `data/analyzer/` and `intermediate/`. Zipped raw files are extracted to a temp directory only. |
| `03_migrate_curated_metadata.R` | `data/flux_metadata/*.csv` | One-time copy of hand-curated values out of gitignored intermediates and hard-coded script constants (air temps, chamber and date overrides, exclusions, trimmed windows, ebullition lists), with provenance. Refuses to overwrite unless `MIGRATE_OVERWRITE=1`. |
| `04_build_auxfile.R` | `output/rebuild/auxfile.csv`, `auxfile_vs_legacy.csv` | One goFlux auxfile for all 834 measurements from tracked inputs only (field sheets, dimension tables, `data/flux_metadata/`). Geometry and Tcham match the legacy final dataset exactly for all 805 rows that have geometry, including the rows the HA/HB and Mar 2022 patches corrected. |

Logger serials: LGR1 = SN:3K60180500001585, LGR2 = SN:3K60180500001583,
LGR3 = SN:3K60180500001584. Picarro `.dat` files carry no serial.
