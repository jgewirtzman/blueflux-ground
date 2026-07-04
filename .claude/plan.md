# Plan: Soil Profile Figures + Publication Map

## Task 1: Soil Profile / Porewater Chemistry Figures

### Available Data
- **`data/environmental/salinity/Blueflux Salinity.xlsx`** ("Terrestrial Data (Jon)" sheet)
  - 72 rows for 6 sites: BL60, CP40, FLM30, SE1, SRS5, SRS6
  - Variables: Salinity (PSU), pH, HDO (dissolved oxygen), Specific Conductivity
  - Sample types: Pore Water (depths 15, 40, 100 cm) and Surface Water (0 cm)
  - Seasons: October (wet) and March (dry)

- **`data/environmental/porewater_gas/GC Run_Dec_2023_Peterman_Gewirtzman (1).xlsx`**
  - Dissolved CH4 and CO2 concentrations in ppm (headspace GC)
  - Porewater + surface water for: BL60 (at 15cm, 40cm, 100cm), CP40, FLM30, MI, SE1, SRS5, SRS6

### Figures to Create (new script: `code/06_analysis/publication_figures_soilprofile.R`)

**Figure S1: Porewater salinity by site & season**
- Barplot or jitter+boxplot of porewater salinity (PSU) by site, faceted or colored by season
- Side-by-side porewater vs surface water comparison
- Sites ordered by disturbance gradient

**Figure S2: Porewater chemistry panel (pH, conductivity, dissolved oxygen)**
- Multi-panel (3 rows): pH, specific conductivity, HDO
- Sites on x-axis, grouped by disturbance type
- Points + error bars or boxplots, colored by sample type (pore vs surface)

**Figure S3: Porewater depth profiles at BL60**
- BL60 has data at 15, 40, and 100 cm depth
- Depth on y-axis (inverted), salinity/gas on x-axis
- Profile plots showing depth gradient

**Figure S4: Dissolved gas in porewater vs surface water**
- CH4 and CO2 concentrations (ppm) from GC data
- Paired porewater vs surface water by site
- Log or asinh scale given huge range (2-30,000 ppm CH4; 100-97,000 ppm CO2)

---

## Task 2: Publication-Quality Map

### Approach
Rewrite `code/06_analysis/map.R` using available packages: `sf`, `rnaturalearth`, `rnaturalearthdata`, `ggspatial`, `maps`, `mapdata`, `ggrepel`, `patchwork`

EVERSpatDat can't be installed (download fails), so we'll use high-res coastline from `mapdata` + Natural Earth data.

### Map Design
**Main panel**: Study area zoomed in on south Florida
- Sites as points, **colored by disturbance type** (healthy/regenerating/ghost/scrub)
- Clear labels with leader lines
- Scale bar + north arrow (via `ggspatial::annotation_scale()` + `annotation_north_arrow()`)
- Grayscale land, light blue water
- Lat/lon grid lines

**Inset**: Full Florida showing study area bounding box
- Small panel in corner using patchwork or `annotation_custom`

### Output
- `output/figures/pub_study_site_map.pdf/.png`

---

## Implementation Steps

1. Create `code/06_analysis/publication_figures_soilprofile.R`
   - Read and clean salinity Excel (terrestrial sheet)
   - Parse and clean GC porewater gas data (filter to study site samples)
   - Generate Figures S1-S4 with `theme_pub()` styling from the existing script
   - Save as PDF + PNG

2. Rewrite `code/06_analysis/map.R`
   - Add disturbance coloring, scale bar, north arrow, Florida inset
   - Match publication style
   - Save to `output/figures/pub_study_site_map.{pdf,png}`
