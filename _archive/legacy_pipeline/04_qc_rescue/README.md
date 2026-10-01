# Step 4: Quality Control and Rescue

Manual quality control for measurements that failed automated processing, and rescue workflows for recovering usable fluxes from problematic data.

## Scripts

- `offset_finder_app.R` — Interactive Shiny app for manual peak identification
- `goflux_lgr1_rescue.R`, `goflux_lgr2_rescue.R` — Rescue LGR1/LGR2 failed measurements
- `goflux_lgr3_trees_rescue.R`, `lgr3_rescue2.R` — Rescue LGR3 measurements (two passes)
- `picarro_rescue.R` — Rescue Picarro failed measurements
- `fix_problem_flux.R` — Correct identified problematic flux values

## Inputs

- Results from Step 3 (`intermediate/results_trees/`, `intermediate/results_surface/`)
- Raw analyzer data (for re-processing)

## Outputs

- `intermediate/rescue/` — Rescued flux results
- Updated manual identification CSVs
