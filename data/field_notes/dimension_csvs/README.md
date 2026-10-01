# Chamber and instrument dimension tables

CSV exports of the workbooks in `../dimension_calcs/`. The CSVs here are what
the pipeline reads (`code/02_preprocess/`); the workbooks are the historical
source and are not read by any code.

| File | Content |
|------|---------|
| `additional_vol.csv` | Analyzer cell, tubing and Drierite column volumes (cm3) per instrument |
| `simplified_volume.csv` | Injection (dilution) measurements of total system volume for the A–D tree chambers, made with the LGR in the loop |
| `soil_water_dims.csv` | Geometric soil / floating chamber and collar dimensions |
| `surface_area.csv` | Enclosed surface area of the A–D tree chambers |
| `mangrove_leaf_data.csv` | Leaf area per 15 leaves by species and forest type |

## Change log

### `additional_vol.csv`: `lgr_mgga` analyzer cell 70 -> 28 cm3

The LGR used in BlueFlux is the ABB/LGR GLA131-GGA microportable ("MGGA").
The original 70 cm3 was the goFlux example-auxfile value for the larger LGR
UGGA, not this instrument. The lab convention for the GLA131 internal volume
(adopted in ch4-data-filtering and applied across projects) is **28 cm3**.

`additional_vol.csv` now carries 28. `../dimension_calcs/additional_vol.xlsx`
is left unchanged as the historical source and still shows 70. The Picarro
cell (35), tubing (29) and Drierite (849 / 277) values are unchanged.

Effect on total system volume (the quantity flux scales with):

- **A–D (and HA/HB, LB) tree chambers measured with an LGR: unchanged.** Their
  total was measured by injection with the LGR in the loop, so the cell is
  inside the measured total; only the split between "chamber" (+42 cm3) and
  "analyzer cell" (-42 cm3) moves.
- **A–D tree chambers measured with the Picarro: +42 cm3.** The pure chamber
  volume is derived as injection total - tubing - LGR cell, so it grows by
  42 cm3 and is then combined with the Picarro cell.
- **Soil and floating chambers (geometric volume) measured with an LGR:
  -42 cm3.** The cell is added to a geometric chamber volume.
- **Soil and floating chambers measured with the Picarro: unchanged.**
