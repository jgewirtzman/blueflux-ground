# Step 5: Data Integration

Combine results from all analyzers and measurement types into a single master dataset.

## Scripts (run in order)

1. `rescue_auxfile_merge.R` — Consolidate rescue auxfiles and merge rescued data
2. `stitch_all_files.R` — Combine tree and soil/water results from all analyzers
3. `date_harmonize.R` — Standardize date/time fields across datasets

## Volume reconciliation

`assemble_clean_dataset.R` compares the total system volume each flux was
fitted with (stored alongside the goFlux results) with the current volume in
`intermediate/main_trees_complete.csv`, `main_trees_complete_additional.csv`
and `main_soilwater_complete.csv` (Step 2), and scales flux, SE and MDF by
`Vtot_current / Vtot_as-processed`. goFlux output is exactly linear in Vtot, so
this equals re-running goFlux on the same manually selected windows. HA/HB
chambers are left to `apply_chamber_corrections.R`.

## Inputs

- `intermediate/results_trees/`, `intermediate/results_surface/` (from Step 3)
- `intermediate/rescue/` (from Step 4)

## Outputs

- `output/data_products/combined_gas_flux_dataset.csv` — Master dataset (all measurements)
- `output/data_products/combined_gas_flux_dataset_with_month_year.csv` — With temporal binning
