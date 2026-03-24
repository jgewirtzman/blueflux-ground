# Methods

## 2.1 Study sites

We measured greenhouse gas fluxes at eight mangrove sites across a hurricane disturbance gradient in south Florida, USA. Sites spanned Everglades National Park and adjacent coastal areas, representing three disturbance categories: healthy mangrove (SRS5, SRS6, RB10), regenerating mangrove (BL60), and ghost forest (CP40, FLM30, MI), plus one scrub mangrove site at the freshwater-saltwater ecotone (SE1). The five core sites (BL60, CP40, FLM30, SRS5, SRS6) were sampled during both wet and dry seasons; MI, RB10, and SE1 were sampled during a single campaign only.

Healthy sites at Shark River Slough (SRS5, SRS6) are dominated by tall *Rhizophora mangle* L. (red mangrove) with a closed canopy and intact root structures. The regenerating site (BL60, Bear Lake) was heavily damaged by Hurricane Irma (September 2017) and has since developed a mixed canopy of *R. mangle* and *Avicennia germinans* (L.) L. (black mangrove) with abundant standing dead wood. Ghost forest sites (CP40, Christian Point; FLM30, Flamingo) experienced catastrophic canopy loss following Irma and remain dominated by standing dead *A. germinans* trunks with sparse regeneration by *Conocarpus erectus* L. (buttonwood) and *Laguncularia racemosa* (L.) C.F. Gaertn. (white mangrove). Water depth and inundation patterns varied by site and season: ghost and regenerating sites were nearly completely inundated during the wet season, while healthy sites maintained exposed soil year-round.

## 2.2 Measurement campaigns

Measurements were conducted during two seasonal campaigns: a wet-season campaign in October 2022 and a dry-season campaign in March 2023. A total of 867 flux measurements were collected from seven ecosystem components: tree stems (n = 486), soil surfaces (n = 118), water surfaces (n = 92), roots and pneumatophores (n = 65), coarse woody debris (n = 27), and leaves (n = 19).

## 2.3 Gas analyzers

We used four closed-path infrared gas analyzers in parallel across sites: three Los Gatos Research Ultra-Portable Greenhouse Gas Analyzers (UGGA, model GLA131; designated LGR1, LGR2, LGR3) and one Picarro G4301 analyzer. Both instrument types measured CO2 and CH4 dry-mole fractions simultaneously at approximately 1 Hz (LGR) or 0.17 Hz (Picarro). Manufacturer-specified precision was 0.9 ppb for CH4 and 0.35 ppm for CO2 on the LGR units, and 1.0 ppb for CH4 and 0.2 ppm for CO2 on the Picarro.

## 2.4 Chamber designs and deployment

We used custom-built closed dynamic chambers connected to the analyzers via closed-loop tubing with inline desiccant (Drierite). Four elliptical stem chamber sizes (A through D) accommodated stem diameters from approximately 3 cm to greater than 20 cm, with surface areas ranging from 40 to 462 cm2. Chambers were sealed against stem surfaces using closed-cell foam gaskets and secured with adjustable straps. Multiple heights were measured on individual trees, typically at 0 (sediment surface), 50, 100, and 150 cm above ground.

Soil fluxes were measured using circular chambers (14.3 or 19.4 cm diameter) placed over PVC collars inserted approximately 5 cm into the sediment. Water-surface fluxes were measured using floating circular chambers (19.4 cm diameter, 324.3 cm2 footprint) resting on the water surface. The same chamber bodies were used for coarse woody debris (dead wood) and root/pneumatophore measurements, with chamber size selected to match the substrate diameter. Leaf measurements used the smallest chamber (A-series, 40.5 cm2) to enclose photosynthetically active foliage.

Total system volumes, including chamber headspace, collar volume, tubing, desiccant column, and analyzer cell volume, were calculated for each chamber-analyzer combination and ranged from 1.2 L (A-series stem chamber with LGR) to 4.3 L (floating water chamber with LGR). Each flux measurement consisted of a 5-minute closed-chamber deployment. Analyzers operated continuously throughout measurement days; the chamber was flushed with ambient air between deployments.

## 2.5 Flux calculation

Gas fluxes were calculated using the goFlux R package (Qepanna/goFlux). For each measurement, concentration time series were extracted from the raw analyzer data using measurement start times and chamber-specific metadata compiled in auxiliary files. Both a linear model (LM) and the Hutchinson-Mosier nonlinear model (HM) were fitted to the concentration increase over the measurement period. The best-fit model was selected using the corrected Akaike Information Criterion (AICc). Flux rates (nmol m-2 s-1 for CH4; umol m-2 s-1 for CO2) were computed from the initial slope of the selected model, corrected for chamber volume, surface area, air temperature, and atmospheric pressure.

Minimum detectable flux (MDF) thresholds were calculated dynamically for each measurement based on chamber volume, analyzer precision, and measurement duration. Measurements were flagged if their flux fell below the MDF or if diagnostic criteria (intercept deviation, model fit statistics, or g-factor for the HM model) indicated unreliable fits. Of 867 measurements, 855 produced valid CH4 fluxes and 855 produced valid CO2 fluxes.

Air temperature was recorded in the field where available and gap-filled using seasonal defaults (28 C for October, 25 C for March) where direct measurements were missing. Atmospheric pressure was set to 101.325 kPa for all measurements.

## 2.6 Quality control and measurement rescue

All flux measurements were visually inspected using diagnostic trace plots generated during processing. Borderline measurements---those near the MDF threshold, with ambiguous model selection, or with visible disturbance artifacts (e.g., chamber repositioning, water ingress)---were reviewed using an interactive Shiny application that allowed manual adjustment of measurement start and end times. Measurements with clear artifacts (e.g., erratic concentration spikes from chamber handling) were flagged and excluded (7 measurements removed as artifacts). An additional 14 measurements with negative CH4 fluxes caused by chamber removal artifacts or analyzer drift were reprocessed with manually selected time windows to isolate the valid portion of the concentration trace, recovering 11 as positive fluxes.

## 2.7 Ebullition detection and partitioning

To quantify the contribution of ebullition to water-surface CH4 fluxes, we developed an algorithm to detect bubble events in the raw analyzer time series during chamber deployments. The analyzer operated continuously while chambers were placed, moved, and retrieved across each measurement day. We identified chamber-on-water periods as sustained intervals of increasing CH4 concentration (rolling 35-second median filter applied to reduce noise; minimum duration 120 seconds; minimum total CH4 rise of 0.005 ppm). The first and last 20 seconds of each detected placement were excluded to avoid artifacts from chamber deployment and removal.

Within each placement, we identified ebullition events as point-to-point upward concentration jumps of 0.10 ppm or greater, which far exceed the expected inter-sample diffusive accumulation (typically 0.001-0.01 ppm per sample interval). Detected placements were filtered to retain only those within or near documented water-measurement time windows and whose diffusive accumulation rate was within a factor of 5 of the mean processed water flux rate for that site and season. Long placements (>10 minutes) were split into consecutive segments. Manual exclusions were applied for known analyzer artifacts identified during visual inspection of full-day concentration time series.

Each retained trace was reprocessed through the goFlux pipeline after removing the identified ebullition jump points, yielding a diffusive-only flux estimate via LM or HM model fitting. The ebullitive CH4 component was calculated as the sum of all jump magnitudes converted to a flux rate using the chamber volume, surface area, and trace duration. Total water-surface CH4 flux was the sum of the diffusive and ebullitive components. A total of 75 traces (492 minutes of deployment) were analyzed, of which 48 overlapped previously processed flux measurement windows and 27 were additional deployments not captured in the original processing. Traces were tagged accordingly to avoid double-counting.

## 2.8 Dataset integration

Per-analyzer flux results from stems, soil, water, roots, CWD, and leaves were merged into a unified dataset. Ebullition-reprocessed water fluxes replaced the original pipeline values for traces that overlapped previously processed measurements; additional traces were appended as new observations. Environmental metadata (water depth, air temperature, soil temperature) and tree-level attributes (species, diameter, height, alive/dead status) were joined from field data sheets. The final dataset comprised 867 flux observations across all components, sites, and seasons.

## 2.9 Statistical analysis

All CH4 and CO2 flux values were transformed using the inverse hyperbolic sine function (asinh) prior to linear modeling. The asinh transformation accommodates both positive and negative values and approximates the natural logarithm for large values while remaining linear near zero, making it suitable for flux data spanning several orders of magnitude with occasional negative values.

Component-level summary statistics (mean, median, standard error) and 95% confidence intervals were computed using nonparametric bootstrap resampling (5000 iterations, percentile method). Summary statistics were stratified by component, site, season, and disturbance class.

Species and height effects on stem CH4 and CO2 flux were assessed using linear mixed-effects models (lmer, lmerTest package) with site as a random intercept. Three model formulations were evaluated: (1) species + height (continuous) + season, to test overall species and height effects; (2) species x height category (0-50, 50-100, 100-150 cm) + season, to test species-specific height gradients; and (3) species-status combinations (separating alive and dead *A. germinans* and *R. mangle*, with *C. erectus* and *L. racemosa* pooled by species) + height category + season, to test the effect of tree mortality. These models were restricted to the three sites with species-level identification (BL60, SRS5, SRS6; 4 species with n >= 5 retained). Estimated marginal means (emmeans package) were computed on the asinh scale and back-transformed to flux units (nmol m-2 s-1) via the sinh function for reporting. Pairwise contrasts used Tukey adjustment for multiple comparisons. Type III F-tests were used for fixed-effect significance.

## 2.10 Porewater geochemistry

Porewater was collected at four sites (SRS5, SRS6, BL60, CP40) at five depths (surface, 0, 15, 45, 90 cm below sediment surface) using push-point samplers during the dry-season campaign. Samples were analyzed for dissolved CH4 and CO2 concentrations (uM), dissolved oxygen (mg L-1), oxidation-reduction potential (ORP, mV), pH, sulfide (mg L-1), iron (mg L-1), sulfate (mg L-1), chloride (mg L-1), nitrate (mg L-1), phosphate (mg L-1), dissolved organic carbon (DOC), alkalinity, salinity (PSU), and stable carbon isotope ratios of dissolved CH4 and CO2 (d13C, per mille vs VPDB). Dissolved oxygen values were corrected for a systematic sensor offset of -1.8 mg L-1 identified during post-hoc calibration, with a minimum floor applied at 0 mg L-1 (3 of 40 measurements adjusted).

Multivariate structure in porewater chemistry was assessed using principal components analysis (PCA) on 11 variables (salinity, dissolved CH4, SO4, d13C-CH4, ORP, dissolved O2, sulfide, iron, DOC, alkalinity, and dissolved CO2) across all site-depth combinations. Variables were centered and scaled prior to analysis. Site-level 95% confidence ellipses were computed assuming multivariate normality.

## 2.11 CO2-equivalent forcing

Component-level CH4 fluxes were converted to CO2 equivalents using 100-year and 20-year global warming potentials (GWP100 = 27.9, GWP20 = 81.2; IPCC AR6). Annual CH4 emission rates (g CO2eq m-2 yr-1) were computed by multiplying mean flux rates by the molar mass of CH4 (16.04 g mol-1) and the appropriate GWP, then scaling to annual values assuming constant emission rates. Chamber-based CO2 respiration fluxes (soil + stems + roots + water) were summed by disturbance class for comparison. These CO2 fluxes represent respiratory losses only and do not include canopy photosynthetic uptake, which would be required to compute net ecosystem CO2 exchange.

## 2.12 Software and reproducibility

All analyses were conducted in R (version 4.3+). Flux calculations used the goFlux package. Mixed-effects models were fitted with lme4 and lmerTest; estimated marginal means were computed with emmeans. Figures were produced with ggplot2, with additional packages ggridges, ggrepel, ggbeeswarm, and ggpubr. Bootstrap confidence intervals were computed using base R sampling functions. The complete analytical workflow, from raw data import through figure generation, is documented in sequentially numbered scripts (code/01_import/ through code/07_analysis/) and is available at [repository URL].
