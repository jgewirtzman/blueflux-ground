# Curated flux metadata

Hand-curated values that the flux workflow needs and that are not in the field
sheets or dimension tables. Written once by `code/rebuild/03_migrate_curated_metadata.R`
from the legacy sources named in each file's `source`/`reason` column; values
were copied, not edited. Read by `code/rebuild/04_build_auxfile.R` (and, later,
the fitting and ebullition steps). Change a value here, never in an
intermediate file, and say why in the row.

| File | Rows | Content |
|------|-----:|---------|
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
- **Trimmed windows**: 12 of 14 are anchored on the field-log start time with
  no clock offset (the legacy behaviour when no saved manual window existed).
- **Air temperature and pressure** are not curated here; `04_build_auxfile.R`
  derives them (see `code/rebuild/README.md`). The legacy worldmet
  weather-station temperatures were replaced by the US-Skr tower record.
  Pressure comes from the tower (`PA`) except in Mar 2022, when the tower
  logged none and 101.325 kPa is used. The 40 field-sheet "Pressure start/end"
  values (Mar 2023 soil) are in mixed units and not used.
- **Floating chamber**: the 2.54 cm "collar" in `soil_water_dims.csv` is the
  foam float, which adds 463 cm3 of headspace above the water (confirmed by Jon,
  2026-10-01).
- **Mar 2022 soil, model choice**: the legacy correction also forced the linear
  model for these sparse (~10 s) traces. That is a fitting rule, not metadata;
  it belongs to the fitting step.
- **No geometry (no flux, legacy and rebuild alike)**: 21 Mar 2022 trees have no
  chamber class on the compiled sheet (16 Picarro, 5 LGR3), plus the HA root
  above. The scanned datasheets may hold the missing chamber IDs.
