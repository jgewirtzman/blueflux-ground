# Step 2: Preprocessing

Prepare field metadata for goFlux: assign chamber dimensions, create auxfiles, and gap-fill temperature data.

## Scripts (run in order)

1. `assign_tree_vol_area.R` — Assign chamber volumes and surface areas to tree measurements
2. `assign_soil_water_vol_area.R` — Same for soil/water measurements
3. `fill_air_temp.R` — Gap-fill missing air temperature (trees)
4. `fill_soil_air_temp.R` — Gap-fill missing soil/air temperature (soil/water)
5. `convert_to_auxfile.R` — Convert tree metadata to goFlux auxfile format
6. `convert_to_auxfile_soil_water.R` — Convert soil/water metadata to goFlux auxfile format
7. `soil_water_prepare_goflux.R` — Final preparation of soil/water data for goFlux

## Inputs

- `data/field_notes/` (field measurement sheets, dimension CSVs)
- `data/environmental/` (weather station data for temp gap-filling)

## Outputs

- `intermediate/main_trees_complete.csv`, `intermediate/main_soilwater_complete.csv`
- `intermediate/auxfiles/*.csv` (goFlux-format input files)
