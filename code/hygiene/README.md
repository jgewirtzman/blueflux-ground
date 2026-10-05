# Data hygiene (internal stages 01-02)

These steps reconcile the raw field records into the clean inputs the analysis runs from. They are
kept for provenance; the public pipeline and the ORNL DAAC deposit start from their outputs.

- `01_metadata/`: index the vendor analyzer files, export them as clean daily CSVs
  (`data/deposit/chamber_fluxes/analyzer_records/`), and build the per-closure metadata (dates,
  analyzers, chamber geometry, temperature and pressure) from the field sheets, scanned data sheets
  and curated corrections (`data/field_notes/`, `data/flux_metadata/`).
- `02_windows/`: align analyzer and field clocks, set each closure's fit window, and write the
  clean inputs: `data/inputs/closures.csv`, `unlogged_placements.csv` and the chamber-dimension
  tables (`03_export_inputs.R`).

`run_all.R` runs these steps only where `data/field_notes/` and the vendor analyzer files
(`data/analyzer/`, not in the repository) are present; otherwise the run starts from `data/inputs/`.
