# Data hygiene (historical record)

These steps reconciled the raw field records into the clean inputs the analysis runs from. They are
kept as a record of how `data/inputs/` was made; `run_all.R` does not run them, and the pipeline and
the ORNL DAAC deposit start from their outputs.

- `01_metadata/`: index the vendor analyzer files, export them as clean daily CSVs
  (`data/deposit/chamber_fluxes/analyzer_records/`), and build the per-closure metadata (dates,
  analyzers, chamber geometry, temperature and pressure) from the field sheets, scanned data sheets
  and curated corrections (`data/field_notes/`, `data/flux_metadata/`).
- `02_windows/`: align analyzer and field clocks, set each closure's fit window, and write the
  clean inputs: `data/inputs/closures.csv`, `unlogged_placements.csv` and the chamber-dimension
  tables (`03_export_inputs.R`).

To regenerate the inputs (needs `data/field_notes/`, `data/flux_metadata/`, the vendor analyzer
files in `data/analyzer/`, not in the repository, and the US-Skr tower file), run the scripts in order
from the project root, e.g. `Rscript code/hygiene/01_metadata/00_index_raw_files.R`, then
`00b_export_analyzer_csv.R`, `01_build_auxfile.R`, and `02_windows/01_rise_detection.R`,
`02_windows.R`, `03_export_inputs.R`. The last run reproduced `data/inputs/` byte-for-byte.
