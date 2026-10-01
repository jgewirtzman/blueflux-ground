# Curated flux metadata

Hand-curated values that the flux workflow needs and that are not in the field
sheets or dimension tables. Written once by `code/rebuild/03_migrate_curated_metadata.R`
from the legacy sources named in each file's `source`/`reason` column; values
were copied, not edited. Read by `code/rebuild/04_build_auxfile.R` (and, later,
the fitting and ebullition steps). Change a value here, never in an
intermediate file, and say why in the row.

| File | Rows | Content |
|------|-----:|---------|
| `air_temperature_overrides.csv` | 22 | Air temperature for soil/water rows with no same-day tree reading: 20 from a worldmet weather-station download (not reproducible, so frozen) and 2 whose date/time could not be parsed (NA). |
| `chamber_overrides.csv` | 30 | Mar 2022 soil at BL60/FLM30/MI: recorded "Soil 8 in", measured with the 6-inch dome on a 2 cm collar. |
| `date_corrections.csv` | 4 | BL60 water 168-171: recorded 2023-03-22, measured 2023-03-16. |
| `excluded_measurements.csv` | 7 | Stem traces that are analyzer artifacts. |
| `trimmed_windows.csv` | 14 | Fit windows chosen interactively for traces that first gave negative CH4 flux, as absolute analyzer-clock times plus the original Etime and its anchor. |
| `ebullition_confirmed_traces.csv` | 6 | Floating-chamber placements with manually verified bubbles. |
| `ebullition_exclusions.csv` | 4 | Placements (3) and a time window (1) that are noise or artifacts, not ebullition. |

## Notes

- **HA/HB stem chambers.** The field-sheet column is "Tree Diameter" and holds
  centimetres, as for every other tree (e.g. 28-31 at SRS5). HA/HB geometry
  uses that diameter in cm directly; no override table is needed. The legacy
  patch (`apply_chamber_corrections.R`) fixed an earlier step that had
  multiplied these values by 2.54. `Mar_23_13_BL60_root` (HA) has no diameter
  and therefore no geometry.
- **Weather-station temperatures for BL60 water 168-171** were looked up for
  the recorded date (2023-03-22), before the date correction. They are kept
  as they were; a same-day value for 2023-03-16 would be more defensible.
- **Trimmed windows**: 12 of 14 are anchored on the field-log start time with
  no clock offset (the legacy behaviour when no saved manual window existed).
- **Pressure** is 101.325 kPa for all measurements. 40 Mar 2023 soil rows have
  "Pressure start/end" on the field sheet, but in mixed units (values near 30
  and near 1020) and are not used.
- **Mar 2022 soil, model choice**: the legacy correction also forced the linear
  model for these sparse (~10 s) traces. That is a fitting rule, not metadata;
  it belongs to the fitting step.
- **No geometry (no flux, legacy and rebuild alike)**: 21 Mar 2022 trees have no
  chamber class on the compiled sheet (16 Picarro, 5 LGR3), plus the HA root
  above. The scanned datasheets may hold the missing chamber IDs.
