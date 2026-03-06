# Step 5: Data Integration

Combine results from all analyzers and measurement types into a single master dataset.

## Scripts (run in order)

1. `rescue_auxfile_merge.R` — Consolidate rescue auxfiles and merge rescued data
2. `stitch_all_files.R` — Combine tree and soil/water results from all analyzers
3. `date_harmonize.R` — Standardize date/time fields across datasets

## Inputs

- `intermediate/results_trees/`, `intermediate/results_surface/` (from Step 3)
- `intermediate/rescue/` (from Step 4)

## Outputs

- `output/combined_gas_flux_dataset.csv` — Master dataset (all measurements)
- `output/combined_gas_flux_dataset_with_month_year.csv` — With temporal binning
