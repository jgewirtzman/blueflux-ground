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

- `output/data_products/combined_gas_flux_dataset.csv` (from Step 5)

## Outputs

- `output/figures/other/` — Publication-quality PNG figures
- `output/data_products/tree_stem_fluxes.csv`, `output/data_products/soil_water_surface_fluxes.csv` — Subset datasets
- `output/data_products/flux_statistics_table.csv`
