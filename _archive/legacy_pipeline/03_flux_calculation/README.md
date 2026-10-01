# Step 3: Flux Calculation

Calculate CO2 and CH4 fluxes from concentration time series using the goFlux package. Each script processes one analyzer for one measurement type.

## Scripts

**Tree stems:**
- `goflux_lgr1_trees.R`, `goflux_lgr2_trees.R`, `goflux_lgr3_trees.R`
- `goflux_lgr3_trees_additional.R` — Additional LGR3 tree measurements
- `picarro_trees.R`

**Soil/water surfaces:**
- `goflux_lgr1_soil.R`, `goflux_lgr2_soil.R`, `goflux_lgr3_soil.R`
- `picarro_soils.R`

## Inputs

- Raw analyzer data (`data/analyzer/`)
- Auxfiles from Step 2 (`intermediate/auxfiles/`)

## Outputs

- `intermediate/results_trees/` — Per-analyzer flux results, best-flux selections, diagnostic plots
- `intermediate/results_surface/` — Same for soil/water
- `intermediate/plots/` — PDF flux diagnostic plots
