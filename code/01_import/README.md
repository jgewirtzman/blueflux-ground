# Step 1: Data Import

Extract and import raw analyzer data from ZIP archives into R objects.

## Scripts

- `lgr_1_import.R` — Import LGR1 analyzer data
- `lgr_2_import.R` — Import LGR2 analyzer data
- `lgr_3_import.R` — Import LGR3 analyzer data

## Inputs

- `data/analyzer/LGR_GLA131/LGR1/`, `LGR2/`, `LGR3/` (zipped TXT files)

## Outputs

- R objects in memory (passed to subsequent scripts)
- Picarro data is imported directly in `code/03_flux_calculation/picarro_*.R`
