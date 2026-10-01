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

## System volumes

Instrument volumes (analyzer cell, tubing, Drierite) come from
`data/field_notes/dimension_csvs/additional_vol.csv`; see the README there for
the change log (LGR GLA131 cell = 28 cm3). The A–D tree-chamber volumes are
derived from injection totals measured with the LGR in the loop
(chamber = total - tubing - LGR cell), so LGR totals for those chambers do not
depend on the assumed cell volume, while Picarro totals and all geometric
(soil / floating) chamber totals do.

`assign_tree_vol_area.R` also processes the March 2022 "additional" sheet
(`intermediate/blueflux_trees_filled_additional.csv` ->
`intermediate/main_trees_complete_additional.csv`), and `convert_to_auxfile.R`
writes its auxfiles. `assign_soil_water_vol_area.R` carries forward air
temperatures that were filled by hand in an existing
`intermediate/main_soilwater_complete.csv`.

Fluxes already fitted interactively (Step 3/4) are not re-fitted when a
dimension changes: `code/05_integration/assemble_clean_dataset.R` rescales them
to the volumes in the tables written here (flux is proportional to Vtot).

## Inputs

- `data/field_notes/` (field measurement sheets, dimension CSVs)
- `data/environmental/` (weather station data for temp gap-filling)

## Outputs

- `intermediate/main_trees_complete.csv`, `intermediate/main_soilwater_complete.csv`
- `intermediate/auxfiles/*.csv` (goFlux-format input files)
