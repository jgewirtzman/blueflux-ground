# ORNL DAAC deposit (draft)

Two datasets, rebuilt by `Rscript run_all.R` (code/09_archive). Nothing here has been submitted.

| Folder | Dataset | Built by |
|---|---|---|
| `chamber_fluxes/` | BlueFlux ground chamber CH4 and CO2 fluxes, 2022-2023: one row per measurement with its inputs (times, analyzer-clock fit windows, area, volume, temperature, pressure) and results; sites; chamber geometry; dictionary; guide. `analyzer_records/` (92 daily CSVs, 75 MB; gitignored) is the analyzer granule from which every flux can be recomputed. | `build_ornl_daac_package.R`; `analyzer_records/` by `code/hygiene/01_metadata/00b_export_analyzer_csv.R` |
| `porewater_biogeochemistry/` | Porewater and plot surface-water dissolved CH4/CO2 (per vial), field salinity/pH/DO (2022-2023) and October 2025 depth profiles (sonde, sulfide, iron, anions, N, alkalinity, DOC, d13C-CH4) | `build_porewater_package.R` |

Not included, because they are archived elsewhere and cited: the BlueFlux aquatic survey (Vaughn and Raymond, ORNL DAAC 2333), TLS scans (ORNL DAAC 2311), CARAFE airborne fluxes, AmeriFlux US-Skr, FCE LTER water levels. Metagenomes go to NCBI SRA.

The pipeline itself starts from `data/inputs/` (clean, corrected closure table, unlogged placements and chamber dimensions) plus `chamber_fluxes/analyzer_records/`.

## Open questions

Chamber fluxes: see `chamber_fluxes/OPEN_QUESTIONS.md` (species codes Cyprus / Mahogany / Slash Pine; habitat labels for the non-mangrove comparison sites).

Porewater:
1. Sulfide units: mg L-1 per the analyst's data sheet (M. Zhang, Everglade_Porewater_2025.xlsx); confirm the dilution used for the methylene-blue reading.
2. Specific conductance (Oct 2025) mixes units on the sheet (BL60 surface 6.257, porewater ~5000 at 35 PSU), so it is left out; salinity is reported.
3. Oct 2025 surface-water samples at SRS5/SRS6 may also appear in a future version of DAAC 2333: check with D. Vaughn.
4. GC protocol (instrument, detectors, standards) for the 2022-2023 vials; the SI has a CHECK for it too.
