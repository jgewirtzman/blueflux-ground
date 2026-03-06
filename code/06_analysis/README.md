# Step 6: Analysis

Summary statistics, publication figures, and clean data product generation.

## Scripts

- `initial_summary_plots.R` — Overview flux distribution visualizations
- `map.R` — Publication-ready site location map
- `count_obs.R` — Observation counts by analyzer and site
- `tree_height_plots.R` — Tree height vs flux relationship plots
- `tree_height_ridges.R` — Ridge plots of tree height distributions
- `summary_table.R` — Flux statistics by site and gas
- `create_data_products.R` — Generate clean subsets for ORNL DAAC submission

## Inputs

- `output/combined_gas_flux_dataset.csv` (from Step 5)

## Outputs

- `output/figures/` — Publication-quality PNG figures
- `output/tree_stem_fluxes.csv`, `output/soil_water_surface_fluxes.csv` — Subset datasets
- `output/flux_statistics_table.csv`
