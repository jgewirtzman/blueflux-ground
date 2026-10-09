# BlueFlux Ground

Greenhouse gas (CO2 and CH4) flux measurements from mangrove and coastal wetland ecosystems across south Florida, collected as part of the NASA Carbon Monitoring System BlueFlux field campaign (March 2022 - March 2023).

## Directory Structure

```
blueflux-ground/
├── run_all.R                  # Ordered pipeline: raw analyzer files -> datasets -> budgets -> figures
├── data/                      # Inputs (read-only)
│   ├── analyzer/              # Raw LGR / Picarro files (gitignored, ~280 MB)
│   ├── tower/                 # AmeriFlux US-Skr (large CSV gitignored)
│   ├── field_notes/           # Field sheets, chamber dimension tables, scanned datasheets
│   ├── flux_metadata/         # Curated corrections, exclusions, saved windows (README)
│   ├── environmental/         # Salinity, porewater gas, dissolved gas, aquatic transect
│   ├── carafe_topdown/        # Airborne two-end-member fluxes (closure)
│   ├── porewater/  tls/  photos/  gis/
│
├── code/                      # One folder per pipeline stage (see Workflow)
│   ├── 00_lib/                # Shared helpers (raw analyzer reader, goFlux-fork installer)
│   ├── 01_metadata/ ... 08_figures/
│   └── qa/                    # Legacy comparisons, audits, one-time migration (not in the main run)
│
├── output/
│   ├── flux/                  # Stage outputs: 00_raw, 01_metadata, 02_windows, 03_fit, 04_ebullition
│   ├── data_products/         # Compiled datasets + data dictionary (stage 05)
│   ├── (legacy ebullition outputs and orphan tables: _archive/superseded_output/)
│   ├── upscaling/  gpp/       # Stage 07
│   ├── figures/main, SI/      # Curated display items (written by stage 08)
│   ├── figures/presentation/  # Deck figures written alongside
│   ├── qa/                    # Frozen legacy baseline and comparison tables
│   └── logs/                  # Per-step logs (gitignored)
│
├── manuscript/                # Manuscript text (not read by code, except manuscript_results.txt output)
└── _archive/                  # Superseded code (legacy_pipeline/) and outputs (superseded_output/)
```

Scripts anchor to the project root with `here::here()`. The raw analyzer files and the AmeriFlux
US-Skr half-hourly file are gitignored; obtain them from ORNL DAAC / AmeriFlux to run stages 01-03 on a
fresh clone. Everything from stage 05 on runs from tracked files.

## Workflow

```bash
Rscript run_all.R                     # all stages, in order
Rscript run_all.R --from 05_dataset   # from a stage (or a step, e.g. 07_upscaling/02)
Rscript run_all.R --only 08_figures   # one stage or step
Rscript run_all.R --list              # list the steps
Rscript run_all.R --qa                # also run the legacy comparisons in code/qa/
```

Each step runs in its own R process; logs go to `output/logs/`.

| Stage | Scripts | What it does | Main outputs |
|-------|---------|--------------|--------------|
| 01 metadata | `00_index_raw_files.R`, `01_build_auxfile.R` | Index raw files (serial, interval, span); field sheets + dimension tables + `data/flux_metadata/` corrections -> one goFlux auxfile (geometry, tower air temperature and pressure) | `output/flux/00_raw/raw_file_index.csv`, `output/flux/01_metadata/auxfile.csv` |
| 02 windows | `01_rise_detection.R`, `02_windows.R` | Clock offset per analyzer-day; fit window per closure (curated trimmed > saved manual > field log + offset) | `output/flux/02_windows/windows.csv` |
| 03 fit | `01_fit_fluxes.R`, `02_water_flux_from_dissolved.R` | goFlux fork release 0.5.0.9001 per gas (MAD precision, 1.96 sigma / t MDF, QC screens, HM >= 30 points); water flux from dissolved CH4 where no chamber flux exists | `output/flux/03_fit/{CH4,CO2}/fluxes.csv`, `water_flux_estimates.csv` |
| 04 ebullition | `01_placements.R`, `02_partition.R` | Floating-chamber placements from the raw CH4 record (chamber on to lift; one flux per placement, plus the curated unlogged placements); diffusive CH4 from the first 10 min of placements > 12 min (else the stage-02 window), de-ebulliated (goAquaFlux, goFlux fork); ebullitive CH4 over the whole placement; CO2 on the diffusive window | `output/flux/04_ebullition/placements.csv`, `partition.csv` |
| 05 dataset | `01_compile_datasets.R`, `02_data_products.R`, `03_data_dictionary.R` | Compiled datasets, written once; cleaning, QC, exclusions and the analysis rule as columns | `output/data_products/flux_measurements_all.csv`, `combined_gas_flux_dataset.csv` (analysis set), `data_dictionary.csv` |
| 06 analysis | `01_summary_table.R`, `02_manuscript_results.R` | Bootstrap statistics; manuscript numbers | `flux_statistics_table.csv`, `manuscript/text/manuscript_results.txt` |
| 07 upscaling | `01_tower_gpp.R` ... `08_supplementary_analyses.R` | Tower GPP; CH4 / CO2 plot budgets (chambers x TLS); net forcing; Monte Carlo; carbon budget; supplementary analyses | `output/upscaling/`, `output/gpp/` |
| 08 figures | display-item scripts, `collect_figures.R` | Main-text and SI figures; copied into `figures/main` and `figures/SI` | `output/figures/` |

### Data hygiene (stage 05)

Nothing is deleted; every decision is a column of `flux_measurements_all.csv`:

1. **Field metadata cleaning** (stage 01): date and analyzer corrections, end-time repairs, chamber overrides (`data/flux_metadata/`).
2. **Fit-level QC** (stage 03): `*_MDF_emp`, `*_det_class_emp`, `*_qc_*`, `*_hm_min_obs_rule`.
3. **Exclusions**: `excluded`, `exclusion_reason` (curated list, no chamber geometry, no closure time, no raw data).
4. **Analysis rule**: `use_in_analysis`, `analysis_note`: not excluded and has a flux. Below-MDF fluxes keep their measured value; QC flags are carried, not applied.

`combined_gas_flux_dataset.csv` is the `use_in_analysis` subset with legacy-compatible column names.
Legacy fluxes are kept side by side (`legacy_CH4_best.flux`, `legacy_CO2_best.flux`).

## Display Items

Figure scripts are in `code/08_figures/`; `collect_figures.R` copies each display item into
`output/figures/main/` and `output/figures/SI/` (the mapping lives in that script).

| Figure | Script | Content |
|--------|--------|---------|
| Fig 1 | `publication_map_composite.R` | Disturbance gradient + multi-scale framework (**assembled interactively; not in run_all**) |
| Fig 2, S4 | `fig2_component_boot.R` | Component CH4/CO2 fluxes; per plot x campaign |
| Fig 3 | `fig3_stem_height.R` | Stem height x species x status |
| Fig 4 | `plot_budget_figs.R` | Bottom-up budgets (TLS x chambers) |
| Fig 5, S12-S13 | `fig6_porewater_pca.R` | Porewater PCA; TA-DIC; excess TA vs SO4 deficit |
| Fig 6 | `plot_closure.R` | Independent closure + net radiative forcing |
| Figs 7-10 | `plot_carbon_budget.R`, `plot_budget_multisource.R`, `plot_budget_flow.R`, `plot_budget_waterfall.R` | Carbon budget figures |
| Fig S1-S3 | `figS1_ebullition.R`, `figS2_pneumatophore.R`, `figS3_chamber_photos.R` | Ebullition; pneumatophore density; chamber designs |
| Fig S5 | `plot_extrap_clean.R` | Stem height extrapolation |
| Fig S6, S7, S9 | `code/07_upscaling/02_upscale_methane.R` | Extrapolation sensitivity; tide/stem scenarios; Monte Carlo decomposition |
| Fig S8 | `plot_SA_height_fixedY.R` | TLS surface area by height |
| Fig S10 | `plot_us_skr_gpp.R` | Tower GPP diurnal cycle |
| Fig S11, S14 | `site_characterization_figures.R` | Porewater depth profiles; salinity vs dissolved CH4 |

## Instruments

- 3x ABB/LGR GLA131 microportable greenhouse gas analyzer (LGR1-3; analyzer cell 28 cm3) -- CO2 and CH4 at ~1 Hz (Mar 2022 LGR3: ~10 s)
- 1x Picarro G4301 -- CO2 and CH4 at ~0.2 Hz (5 s)

## Study Sites

| Code | Site | Disturbance Class |
|------|------|-------------------|
| FLM30 | Flamingo | Ghost forest |
| CP40 | Christian Point | Ghost forest |
| BL60 | Bear Lake | Regenerating mangrove |
| SRS5 | Gunboat Island | Healthy mangrove |
| SRS6 | Lower Shark | Healthy mangrove |
| SE1 | SE-1 / US-EvM | Scrub mangrove ecotone |
| MI | Marco Island | Ghost forest |
| RB10 | Rookery Bay | Healthy mangrove |

## Dependencies

```r
install.packages(c("here", "dplyr", "tidyr", "readr", "readxl", "lubridate", "stringr", "purrr",
                   "ggplot2", "patchwork", "cowplot", "ggh4x", "ggridges", "ggdist", "ggbeeswarm",
                   "ggrepel", "ggpubr", "scales", "forcats", "lme4", "lmerTest", "emmeans",
                   "boot", "MASS", "data.table", "magick", "openxlsx", "jsonlite", "sf", "rnaturalearth"))
install.packages(c("callr", "remotes"))
# Flux calculation (stage 03): goFlux (Rheault et al. 2024) version 0.5.0.9001 with additions
# (Gewirtzman 2026, doi:10.5281/zenodo.23254791), installed automatically on first use into the
# project library .Rlib/goflux-0.5.0.9001 from jgewirtzman/goFlux@v0.5.0.9001 (code/00_lib/goflux_release.R).
```

Stage 04 also needs `goAquaFlux(diffusion.window = "deebulliated")`, which exists
only on the goFlux fork branch `feat/aqua-diffusive-deebulliated` (commit
2ed7224, not on any remote). Its package source is vendored and pinned in
`vendor/goFlux_0.4.0_aqua-deebulliated_2ed7224.tar.gz`; stage 04 installs it on
first use into the project library `.Rlib/` (gitignored) and runs it in a
separate R process, so the goFlux used by stage 03 is untouched
(`code/00_lib/goflux_fork.R`).

Requires R >= 4.3.

## Citation

Poulter, B., Adams-Metayer, F. M., Amaral, C., Barenblitt, A., Campbell, A., Charles, S. P., ... & Zhang, Z. (2023). Multi-scale observations of mangrove blue carbon ecosystem fluxes: The NASA Carbon Monitoring System BlueFlux field campaign. *Environmental Research Letters*, 18(7), 075009. https://doi.org/10.1088/1748-9326/acdae6

## License

MIT License -- see [LICENSE](LICENSE).

## Contact

Jon Gewirtzman (jonathan.gewirtzman@yale.edu)
