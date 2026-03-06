# BlueFlux Ground: Methods Summary

## Overview

Greenhouse gas (CO2 and CH4) flux measurements from mangrove and coastal wetland ecosystems across south Florida, collected as part of the NASA Carbon Monitoring System BlueFlux field campaign.

## Measurement Period

March 2022 - March 2023

## Study Sites

Eight sites across the Florida Everglades and Ten Thousand Islands region, spanning a gradient of mangrove ecosystem types from ghost forests to tall-stature mangroves. See `site_metadata.csv` for coordinates and descriptions.

## Instruments

- 3x Los Gatos Research Ultraportable Greenhouse Gas Analyzers (UGGA, model GLA131)
- 1x Picarro G4301 Gas Concentration Analyzer

## Measurement Types

**Tree stem fluxes**: Closed-chamber measurements using custom collars affixed to mangrove tree stems. Chambers (various sizes) were sealed to collars and connected to gas analyzers via tubing. Measurement duration: 2-5 minutes per chamber closure.

**Soil/water surface fluxes**: Floating chambers on water surfaces and cylinder chambers inserted into sediment. Same gas analyzers and measurement protocol.

## Flux Calculation

Fluxes were calculated using the goFlux R package (Rheault et al.), which fits both linear (LM) and Hutchinson-Mosier (HM) nonlinear models to concentration time series. The best-estimate flux for each measurement was selected based on automated quality criteria including R-squared thresholds, MAE/RMSE limits, and AICc comparisons. Measurements that failed automated processing were manually reviewed and rescued where possible.

## Data Products

- `combined_gas_flux_dataset.csv`: Streamlined master dataset with best-estimate fluxes for all measurements
- `tree_stem_fluxes.csv`: Tree stem measurements only
- `soil_water_surface_fluxes.csv`: Soil and water surface measurements only
- `data_dictionary.csv`: Column definitions, units, and methods
- `site_metadata.csv`: Site descriptions, coordinates, and ecosystem classifications

## Citation

Poulter, B., Adams-Metayer, F. M., Amaral, C., et al. (2023). Multi-scale observations of mangrove blue carbon ecosystem fluxes: The NASA Carbon Monitoring System BlueFlux field campaign. Environmental Research Letters, 18(7), 075009. https://doi.org/10.1088/1748-9326/acdae6
