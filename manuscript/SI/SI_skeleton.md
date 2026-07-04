# Supplementary Information — skeleton
_For the Nature-format manuscript. Sources in brackets; [PLACEHOLDER] = needs collaborator input._

## Supplementary Methods (full Online Methods)
- **S.M1 Study system and disturbance gradient** — [manuscript_methods_text 2.1]
- **S.M2 Seasonal campaigns and multi-scale framework** — [2.2]
- **S.M3 Component flux measurements** (chamber designs, 7 components) — [3.1; Fig. S3]
- **S.M4 Flux calculation** (goFlux LM/HM, AICc, MDF, QC) — [3.2]
- **S.M5 Ebullition partitioning** (step-correction) — [3.2; Fig. S1]
- **S.M6 Terrestrial laser scanning** (acquisition, registration, segmentation, surface-area extraction, allometry) — **[PLACEHOLDER — Powell/Stovall]**
- **S.M7 Bottom-up scaling and budget construction** — CH4 (rate x TLS surface area, tide handling); CO2 NEE = Reco - GPP, with leaf-respiration term (Rd25, Heskel T-correction, Beer's-law LAI) and the NEE = NEE_tower + (Reco_bu - Reco_tower) reconciliation — [3.4; GPP_literature_outputs]
- **S.M8 Eddy-covariance tower (SRS-6 / US-Skr)** — **[PLACEHOLDER — instruments, processing, u* filtering]**
- **S.M9 Airborne eddy covariance (CARAFE)** — CWT fluxes, footprints, two-end-member disaggregation; regenerating class not separable — **[PLACEHOLDER — Delaria/JGR]**
- **S.M10 Tower GPP partitioning and leaf-respiration synthesis** — nighttime NEE-T model, PAR light-response; literature Rd/LAI [GPP_literature_outputs provenance]
- **S.M11 Porewater geochemistry** (sampling, field colorimetry, headspace CH4/CO2 + d13C, ions, TAlk/DIC) — [3.7]
- **S.M12 Net radiative forcing and landscape scaling** (GWP100/20; per-area differential x Irma dieback area, Lagomasino 2021) — [3.8] **[need dieback-area value]**
- **S.M13 Statistical analysis** (asinh transform, bootstrap, mixed models, PCA, Monte Carlo) — [3.9]

## Supplementary Text (robustness / interpretation)
- **S.T1** Stem height-extrapolation scenarios (exp-zero-asymptote vs zero-above-max vs linear); <2% effect on totals.
- **S.T2** Tide / inundation scenarios and 50/50 weighting.
- **S.T3** Coarse-woody-debris surface-area sensitivity.
- **S.T4** Monte Carlo uncertainty decomposition (which terms dominate the budget/forcing CI).
- **S.T5** CO2 closure caveats: airborne midday->daily conversion; chamber-midday respiration; lateral DIC export (bottom-up Reco may exceed tower Reco legitimately).
- **S.T6** Regenerating class: component rates measured, but no TLS/closure -> excluded from budgets/forcing.
- **S.T7 Carbonate-buffer drawdown at ghost sites (TA-DIC).** Elevated TA:DIC slopes at ghost sites (>1) indicate ongoing CaCO3 dissolution, consistent with episodic acid generation via sulfide/pyrite reoxidation once the protective vegetation cover is lost. Over time this represents a non-renewable drawdown of the sediment's carbonate buffering reservoir, in contrast to the biogenic alkalinity generation (1:1 TA:DIC, from sulfate reduction) sustained in healthy and regenerating sites. [Figs. S12-S13.]

## Supplementary Figures (mostly built)
| # | Content | Source |
|---|---|---|
| S1 | Ebullition partitioning + traces | pub_SI_ebullition_partition |
| S2 | Soil flux vs pneumatophore density | pub_SI_pneumatophore_density |
| S3 | Chamber designs | pub_SI_chamber_photos |
| S4 | Full per-plot x campaign flux distributions (CH4+CO2) | pub_component_by_plot_campaign_combined_condensed_boot |
| S5 | Stem height x species x status emmeans (CH4+CO2) | pub_stem_height_composite_combined |
| S6 | Stem height-extrapolation scenarios + sensitivity | stem_extrap_method, height_extrap_sensitivity, pub_extrap_dumbbell |
| S7 | Tide/inundation scenario comparison | scenario_comparison_tidal |
| S8 | Surface area by segment class and height | SA_by_segment_height_fixedY |
| S9 | Budget decomposition (rate x area -> integrated -> %) | 11c-11f |
| S10 | Monte Carlo uncertainty decomposition | pub_uncertainty_decomp |
| S11 | Tower GPP diurnal/seasonal + LUE context | US-Skr GPP plots |
| S12 | Porewater depth profiles (all variables) | pub_porewater_depth_profiles |
| S13 | TAlk-DIC, excess-TA / SO4-deficit | pub_SI_ta_vs_dic, pub_SI_TA_vs_SO4_deficit |
| S14 | Salinity vs dissolved CH4 by site | pub_SI_salinity_vs_ch4_bysite |
| S15 | CARAFE footprint / land-cover disaggregation | **[from Delaria 2024]** |
| S16 | Closure residual analysis | **[new]** |

## Supplementary Tables
- **S1** Site metadata (coordinates, class, salinity, canopy, n). [site_metadata.csv]
- **S2** Component CH4/CO2 flux rates by class and season, with 95% CIs and n. [from results]
- **S3** Stem mixed-model results (species, height, status; EMMs, contrasts).
- **S4** Airborne end-member fluxes used (per-flight, from Delaria 2024 Tables S2/S3). [delaria_endmembers_campaign.csv]
- **S5** Stand-level budgets and net forcing by class, with Monte Carlo CIs. [net_forcing_by_class, mc_*]
- **S6** Literature leaf Rd25 and LAI values used in the CO2 budget. [GPP_literature_outputs/value_catalog.csv]

## Data & code availability
- Chamber flux dataset (combined_gas_flux_dataset.csv); analysis workflow [repo URL]; airborne fluxes (Delaria 2024 / ORNL DAAC); tower data (AmeriFlux US-Skr).

---
### Outstanding to complete SI
1. TLS methods text (S.M6) — Powell/Stovall.
2. Tower + CARAFE methods (S.M8, S.M9) — Delaria/JGR reference.
3. Irma ghost-forest dieback area (S.M12) for the landscape forcing panel — Lagomasino 2021 value.
4. Build Fig S16 (closure residual) and source Fig S15 (CARAFE footprint).
