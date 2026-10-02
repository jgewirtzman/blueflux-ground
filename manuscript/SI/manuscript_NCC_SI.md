# Supplementary Information

**Hurricane-induced mortality switches mangroves from carbon sinks to methane-amplified sources**

Gewirtzman et al.

_Placeholders marked [PLACEHOLDER — …] require collaborator input or an outstanding value; all other text is drafted. Section IDs (S.M#, S.T#, Fig. S#, Table S#) are referenced from the main-text Methods._

---

## Supplementary Methods

These sections give the full detail underlying the condensed main-text Methods. Where the main text summarizes a procedure, the corresponding S.M section is the complete, reproducible description.

### S.M1 Study system and disturbance gradient

This study was conducted across mangrove forests of the southwest Florida coast within and adjacent to Everglades National Park. Sites were selected to represent three condition classes along a hurricane-disturbance gradient: ghost forest, early-regenerating forest and mature intact forest. The ghost-forest sites (FLM30, CP40) experienced extensive tree mortality and defoliation following Hurricane Irma in September 2017, whose storm surge caused widespread mangrove drowning and dieback across the region. Five years post-storm, these sites retained standing dead trunks and root structures but showed no canopy recovery or significant regeneration. The early-regenerating site (BL60) was also damaged by Irma but exhibited active recruitment of seedlings and saplings at the time of sampling, with a developing but low, discontinuous canopy. The mature intact sites (SRS5, SRS6) are tall riverine mangrove forests along the Shark River Slough that sustained comparatively less structural damage from Irma, though the broader landscape has been shaped by a history of major hurricanes including Andrew (1992) and Wilma (2005), which reduced canopy heights below pre-storm levels in some areas.

The dominant species across all sites are *Rhizophora mangle*, *Avicennia germinans*, *Laguncularia racemosa* and *Conocarpus erectus*. Species composition and relative abundance vary with geomorphic position, tidal connectivity and disturbance history. The region has a subtropical climate with a wet season (May–October) and dry season (November–April); average annual rainfall is 1,000–1,700 mm, ~70% during the wet season. Tidal range, freshwater inputs from the Shark River Slough, and residence time vary across sites, producing gradients in salinity and hydroperiod that influence both vegetation structure and biogeochemistry. Additional single-campaign measurements at Marco Island (MI), Rookery Bay (RB10) and the SE-1 scrub-mangrove ecotone site provided broader spatial context across hydrological and salinity settings.

### S.M2 Seasonal campaigns and multi-scale framework

Repeat measurements were made during two seasonal campaigns: October 2022 (tail of the wet season, inundated soils, elevated water levels) and March 2023 (dry season, lower water levels, reduced freshwater inputs). Both campaigns included ground chamber fluxes, TLS and environmental sampling at the five core sites (BL60, CP40, FLM30, SRS5, SRS6). CARAFE airborne eddy-covariance flights were part of the broader NASA BlueFlux campaign, which comprised four deployments (April 2022, October 2022, February 2023, April 2023), each of six to eight flights over ~25 flight hours; the October 2022 and February/March 2023 deployments overlapped the ground campaigns. Hurricane Ian made landfall north of the study region on 28 September 2022, and elevated water levels were observed at the eddy-covariance tower in the following weeks. For the overall campaign design, see ref. [BlueFlux ERL, Poulter et al. 2023].

The study integrates four nested scales. At the finest, static chambers isolate flux rates from individual components (stems at multiple heights, prop roots, pneumatophores, sediments, surface water). These flux densities are multiplied by component-specific TLS surface areas to produce stand-level estimates. At the ecosystem scale, an eddy-covariance tower at SRS-6 provides continuous half-hourly net exchange of CO2 and CH4. At the landscape scale, the CARAFE airborne platform measures spatially resolved fluxes across the full gradient. Bottom-up budgets from chambers and TLS are compared with the top-down tower and airborne constraints to assess closure and identify unmeasured pathways.

### S.M3 Component flux measurements (chamber designs)

CH4 and CO2 fluxes were measured with closed dynamic chambers connected to portable cavity ring-down analyzers (Los Gatos Research Ultra-Portable Greenhouse Gas Analyzer GLA131; Picarro G4301) recording dry-mole fractions at 1 Hz (LGR) or 0.17 Hz (Picarro). Three LGR analyzers and one Picarro operated in parallel across sites. Manufacturer precision was 0.9 ppb (CH4) and 0.35 ppm (CO2) on the LGRs, and 1.0 ppb and 0.2 ppm on the Picarro.

Chamber designs were adapted to component geometry (Fig. S3). Four elliptical stem-chamber sizes (A–D) accommodated stem diameters from ~3 cm to >20 cm, with enclosed areas 40–462 cm2. Stem chambers were sealed to the bark with modelling clay at 0, 50 and 100 cm above the sediment (and 150 cm where diameter permitted) to test for vertical gradients diagnostic of soil-origin gas transport. Prop-root chambers enclosed sections of individual *R. mangle* aerial roots, sealed with clay. Leaf fluxes used a transparent chamber (a modified container with a rubber gasket and flat transparent top angled toward the sun) enclosing a branch cluster passing through a clay-sealed notch; it was not temperature- or VPD-controlled and was used to determine flux direction and approximate magnitude, with incubations terminated while CO2 uptake remained approximately linear. Two soil designs were used: open-bottom acrylic cylinders (23.5 cm diameter, inserted 30–91 cm and capped with a foam-gasketed lid) in the wet season, and smaller PVC collars (14.3 or 19.4 cm, inserted ~5 cm ≥1 h before sampling, capped with a domed chamber) in the dry season; both sealed with minimal downward pressure to avoid degassing or inducing ebullition. Soil measurements included pneumatophores within the footprint, with density recorded per collar. Surface-water fluxes used floating chambers (19.4 cm diameter, 324.3 cm2 footprint). All chambers used closed-loop tubing with inline desiccant (Drierite) and a ventilation port sealed before measurement, and were CO2 leak-tested before each measurement. System volumes were computed per chamber–analyzer combination (headspace + collar + tubing + desiccant + cell): 0.5–3.1 L (stems), 0.5–2.0 L (roots), 2.1–39.6 L (soil), 4.3 L (floating water).

Measurements covered all four species, including standing dead trunks and snags at ghost sites (≥5 stems per species per site; 10 soil collars per plot). Median incubations were 180 s (stems, soil, roots, CWD, leaves) and 300 s (water); full range 60–1,800 s. Meteorological and ancillary variables were recorded at each measurement — air, stem, soil and water temperature, water depth, barometric pressure and relative humidity — together with (for stems) DBH, perimeter at each height, species and alive/dead status. In total 867 fluxes were collected across six components: stems (n = 486), soil including pneumatophores (n = 118), water (n = 92), roots (n = 65), coarse woody debris (n = 27) and leaves (n = 19).

### S.M4 Flux calculation

Fluxes were computed with the goFlux R package. Both a linear (LM) and the Hutchinson–Mosier nonlinear (HM) model were fitted to each headspace time series; HM accounts for nonlinear saturation of concentration build-up in closed chambers, yielding higher initial estimates when headspace approaches equilibrium with the source. The best model was selected by corrected Akaike Information Criterion (AICc). Rates (nmol m−2 s−1 CH4; µmol m−2 s−1 CO2) were computed from the initial slope of the selected model, corrected for chamber volume, surface area, air temperature and barometric pressure. The ideal-gas conversion used each measurement's field-recorded air temperature; missing air-temperature values were gap-filled hierarchically — the mean of records within 30 min of the same date, otherwise the nearest record on the same day — and pressure defaulted to 101.325 kPa where a field reading was unavailable. Minimum-detectable-flux (MDF) thresholds were computed dynamically per measurement from volume, analyzer precision and duration. Sub-detection fluxes were retained at their measured values. All traces were visually inspected; seven measurements with clear artefacts were excluded, and 14 traces that initially returned negative CH4 fluxes were refitted over manually trimmed time windows using the same flux conversion. Measurements that failed automated goFlux processing were manually reviewed and recovered where a valid initial slope could be identified, and stem-chamber system volumes were corrected post hoc for two chamber classes with a headspace-geometry offset.

### S.M5 Ebullition partitioning

Water-surface CH4 fluxes were partitioned into diffusive and ebullitive components. Within each deployment, ebullition events were identified as abrupt upward concentration jumps exceeding 0.10 ppm between consecutive readings, far above the expected inter-sample diffusive accumulation (0.001–0.01 ppm). To isolate the diffusive signal, the cumulative magnitude of all preceding jumps was subtracted from post-bubble values (step-correction), with a 15 s buffer excluded on either side of each jump. The diffusive flux was fitted to the step-corrected series (LM/HM selection as above). The ebullitive component was the total magnitude of detected jumps converted to a flux over the deployment duration (Fig. S1).

### S.M6 Terrestrial laser scanning [PLACEHOLDER — Powell/Stovall to confirm and complete]

Three-dimensional forest structure was measured with a RIEGL VZ-400i terrestrial laser scanning system paired with GNSS positioning (Trimble R8 rover, R6 base). Four panorama scans per plot were acquired at 0.03° resolution; point clouds were registered via an iterative-closest-point algorithm in RiSCAN PRO and georeferenced to WGS 84 / UTM 17N. Clouds were segmented into component classes (stems, prop roots, pneumatophores, ground surface); surface area was extracted by component and height interval, and allometric relationships linking TLS-derived areas to DBH and species were used for plot-level scaling. [PLACEHOLDER: comparison of TLS-derived surface areas with traditional allometric estimates; sensitivity of bottom-up budgets to the surface-area method; final acquisition/segmentation parameters and QC to be confirmed by collaborators.]

### S.M7 Bottom-up scaling and budget construction

**Methane.** Stand-level CH4 budgets were constructed by multiplying component flux densities (nmol CH4 m−2 surface s−1) by TLS-derived surface area per unit ground area (m2 component m−2 ground) for each class (soil, surface water, prop roots, stems, coarse woody debris), summed over components. Species-specific stem rates were weighted by relative surface-area contribution. Stem flux was integrated over height from the measured 0/50/100(/150) cm rates; the primary budget extrapolated above the highest measurement with an exponential (log-linear, strictly positive) decay, bounded by a zero-above-maximum alternative (S.T1). Inundation controls which surfaces emit (S.T2), and the emitting-surface fraction was set from measured chamber water depth. Non-tidal ghost sites (CP40, FLM30) were inundated in both analysed campaigns (mean water depth 8.7 and 6.6 cm in the dry season, 10.8 and 16.1 cm in the wet), so their emitting surface was treated as 100% water in both; exposed-soil emission at these sites was measured only in the dropped March 2022 campaign, when they were dry. Tidal intact sites (SRS5, SRS6) were represented as high-tide (flooded floor) and low-tide (exposed floor) states weighted by the measured fraction of time the floor was flooded in each campaign month (FCE LTER water level; Table S9). At high tide, prop-root surface below the mean flooded depth was excluded (7–13 % of root surface). Uncertainties were propagated by Monte Carlo (S.M13; S.T4).

**Net ecosystem CO2 exchange.** Bottom-up NEE = Reco − GPP (atmospheric sign; positive = source). Reco was built by scaling chamber CO2 efflux densities with the same TLS surface areas (soil, water, prop roots, woody stems plus branch surface) and adding a literature-based canopy foliar-respiration term (S.M10). GPP was not constructed bottom-up: it was imported from the co-located SRS-6 (US-Skr) tower, where GPP is partitioned from observed NEE with a nighttime NEE–temperature respiration model (GPP = Reco,tower − NEEtower). For ghost plots the defoliated canopy was assumed to have negligible GPP, so NEE ≈ Reco,bottom-up. Because imported tower GPP embeds the tower's own NEE and partitioned respiration, intact-forest bottom-up NEE reduces algebraically to NEE = NEEtower + (Reco,bottom-up − Reco,tower); it is therefore a reconciliation of bottom-up against tower-partitioned respiration rather than an independent net-flux estimate, and is treated as such. Tower-partitioned Reco extrapolates nighttime respiration across daylight without light inhibition, whereas the bottom-up foliar term applies daytime inhibition, so the two respiration estimates are not identical in construction (S.T5). Independent top-down evaluation of both FCO2 and FCH4 is provided by CARAFE (S.M9).

### S.M8 Eddy-covariance tower (SRS-6 / US-Skr) [PLACEHOLDER — instruments/processing to confirm]

Continuous half-hourly fluxes of CO2 and CH4 were measured at 27 m on a 30 m tower; instruments include a Gill sonic anemometer, an open-path CO2/H2O analyzer (LI-7500) and a CH4 analyzer (LI-7700) sampling at 20 Hz. CH4 measurements began in 2018. Standard eddy-covariance processing with friction-velocity (u*) filtering was applied. The tower is part of the Florida Coastal Everglades LTER and AmeriFlux (US-Skr) networks. [PLACEHOLDER: final instrument models, tower height, u* threshold, gap-filling and partitioning settings to be confirmed by tower PIs.]

### S.M9 Airborne eddy covariance (CARAFE) [PLACEHOLDER — Delaria/JGR to confirm]

Flights at ~90 m altitude aboard a Beechcraft King Air A90 carried a Picarro G2311-f (10 Hz CO2/CH4/H2O for eddy covariance), a Picarro G2401m (0.5 Hz, calibrated to NOAA/WMO standards) and an Aventech AIMMS-20 probe (20 Hz 3D winds, temperature, pressure, position). Fluxes were computed by continuous wavelet transform. Flux legs were segments >15 km with roll <5° and altitude within ±10 m. Median 1-km detection limits were 5.8 nmol m−2 s−1 (CH4) and 0.9 µmol m−2 s−1 (CO2). Two-dimensional footprints followed Kljun et al. (2015) using HRRR 3-km boundary-layer heights. Fluxes were disaggregated by land-cover class via multilinear regression (Hutjes et al. 2010; Hannun et al. 2020), with the ghost-forest class from Lagomasino et al. (2021). Because the regenerating class could not be spatially separated from the airborne footprints, closure used a two-end-member (intact, ghost) disaggregation following Delaria et al. (2024); attempts to add a regenerating end-member produced physically inverted values and were not pursued. The March 2023 airborne value is the mean of the February and April 2023 deployments (variances combined). For full instrument and processing detail, see ref. [CARAFE JGR, Delaria et al. 2024].

### S.M10 Tower GPP partitioning and leaf-respiration synthesis

Tower GPP was obtained by partitioning observed half-hourly NEE. Ecosystem respiration was fit as a log-linear (exponential) function of tower air temperature to nighttime records (incoming shortwave ≤ 10 W m−2), extrapolated across daylight, and subtracted from observed NEE to give daytime GPP (GPP = Reco − NEE, not clipped, so that the noise in NEE averages out; GPP = 0 at night); a rectangular-hyperbola light response of GPP to shortwave radiation was then fit, and uncertainty propagated by bootstrapping the nighttime and daytime records. Over the two campaign windows this gives a 24-h mean GPP for the intact class of ≈7.8 µmol m−2 s−1 (Oct 2022 7.1; Mar 2023 8.5), the value used in the CO2 budget. The bottom-up CO2 budget requires a canopy foliar-respiration term that the TLS cannot supply (no foliage surface area); we built it from a literature synthesis specific to these species and site (full provenance, value IDs and corrections in manuscript/literature/; Table S6). Key values: species leaf dark respiration at 25 °C, Rd25 = 1.62 ± 1.32 µmol m−2 s−1 for *R. mangle* (Barr 2009, at-site, LI-6400/Farquhar) and Rd25 = 1.28–1.54 µmol m−2 s−1, Q10 = 2.39 for *A. germinans* (Sturchio 2022); a species-weighted central Rd25 of 1.55 (range 1.28–1.62). Leaf-area index, the dominant scaling lever, used the SRS-6 ground value L = 2.8 (Barr, unpublished, in Troxler et al. 2015) as central, with a range from 2.3 (ground optical) to 5.55 (MODIS at US-Skr; Reed et al. 2025, from 24-day maxima and therefore high). Canopy LAI recovered within about a year of Wilma and Irma (Reed et al. 2025), so no post-hurricane reduction was applied for 2022–23. Because self-shading reduces respiratory capacity through the canopy, the leaf term used an effective LAI, LAIeff = (1 − e−kL)/k with extinction coefficient k = 0.5 (L = 2.8 → LAIeff ≈ 1.51). The short-term temperature response followed Heskel et al. (2016): f(T) = exp[0.1012(T − 25) − 0.0005(T2 − 252)], driven by tower air temperature, with a 30% daytime light-inhibition factor applied to the daytime fraction (global mean of Kok-method measurements, Atkin et al. 2014; 20–50 % in the Monte Carlo; no mangrove-specific measurements exist). Canopy foliar respiration (µmol m−2 ground s−1) = Rd25 × f(T)day × LAIeff. Supporting context: foliar respiration is ~1/3 of Reco (proxy), below-canopy chamber respiration is 45–65% of Reco at this site (Troxler 2015), and midday GPP for tall riverine Shark River can reach ~37 µmol m−2 s−1. Provenance caveat: OCR unit errors in the Barr 2009/2010 PDFs (µmol printed as mmol) were corrected to µmol. Closure with tower Reco was not forced (S.T5).

### S.M11 Porewater geochemistry

Porewater was collected at four sites (SRS5, SRS6, BL60, CP40) at five depths (surface, 0, 15, 45 and 90 cm below the sediment surface) with MHE PushPoint samplers during the dry-season campaign. Temperature, pH, electrical conductivity, dissolved oxygen and oxidation–reduction potential were measured in the field (Hanna HI98494). Sulfide and ferrous iron were determined colorimetrically in the field immediately after sampling (Hach DR900; sulfide methylene-blue method, reagents 1816/1817; iron FerroVer, reagent 2105769). Dissolved CH4 and CO2 concentrations and stable carbon isotopes (δ13C-CH4, δ13C-CO2) were measured by headspace equilibration in syringes on a Picarro G2201-i cavity ring-down spectrometer with SAM autosampler and ultra-zero-air carrier at Yale University. Dissolved organic carbon (Shimadzu TOC), major ions (sulfate, chloride, nitrate, phosphate; Metrohm ion chromatograph) and total alkalinity (titration) were measured at the Yale Analytical and Stable Isotope Center (YASIC).

### S.M12 Net radiative forcing and landscape scaling

Component CH4 fluxes were converted to CO2 equivalents using 100- and 20-year global warming potentials (GWP100 = 27.9, GWP20 = 81.2; IPCC AR6) and combined with net CO2 exchange to give net forcing (g CO2-eq m−2 yr−1) for each class. The intact CO2 NEE was anchored to the SRS-6 tower; ghost and regenerating CO2 exchange were evaluated against CARAFE disaggregated fluxes. The disturbance-induced switch is the intact-to-ghost differential. A regional estimate multiplies the per-area differential by the area of Caribbean mangrove converted to ghost forest by recent hurricanes (Fig. 6e,f). [PLACEHOLDER — Caribbean ghost-forest extent value/source and, for the Everglades-specific figure, the Irma dieback area from Lagomasino et al. (2021), to be finalized.]

### S.M13 Statistical analysis

Fluxes were transformed with the inverse hyperbolic sine (asinh) before modelling; asinh accommodates positive and negative values, approximates the natural logarithm for large values and is linear near zero. Component summaries and 95% CIs used nonparametric bootstrap resampling (5,000 iterations, percentile method) stratified by component, site, season and class. Stem species and height effects used linear mixed-effects models (lme4/lmerTest) with site as a random intercept; three formulations were evaluated — (1) species + continuous height + season; (2) species × height category (0–50, 50–100, 100–150 cm) + season; (3) species–status combinations (alive/dead *A. germinans* and *R. mangle* separate; *C. erectus* and *L. racemosa* pooled) + height category + season — restricted to sites with species identification (BL60, SRS5, SRS6; species with n ≥ 5). Estimated marginal means were computed on the asinh scale, back-transformed via sinh, and compared with Tukey-adjusted contrasts and Type III F-tests. Porewater structure used principal-components analysis on 11 centred, scaled variables (salinity, dissolved CH4, sulfate, δ13C-CH4, ORP, dissolved O2, sulfide, iron, DOC, alkalinity, dissolved CO2) across all site–depth combinations. Budget and forcing uncertainty was propagated by Monte Carlo (5,000 draws) combining bootstrapped chamber flux densities, the leaf term (Rd25 ~ U[1.28, 1.62]; LAI lognormal with median 2.8 and 5.55 at the upper 97.5 %; daytime inhibition triangular on 0.2–0.5 with mode 0.3), the downed-wood area (lognormal over the Krauss range), tower GPP uncertainty and the CH4 budget total. Analyses were run in R (≥4.3).

### S.M14 Literature-derived values and how they enter the budget

Wherever a budget term could not be measured in this study, we used a published value. Table S9 lists every such value, with its source, how it was obtained or converted, how it is used, and what alternatives were considered. Measured terms (chamber fluxes, TLS areas, tower NEE, dissolved gas) are not listed. Values are given in the units used in the code (`code/07_upscaling/`). Literature terms for lateral export, burial and biomass are in `06_carbon_budget.R` section 1, and the full reading list is `manuscript/carbon_budget_literature.md`.

**Table S9. Literature-derived values.**

| Term | Value used (range) | Source | How obtained / converted | Use | Alternatives considered |
|---|---|---|---|---|---|
| Leaf dark respiration at 25 °C, Rd25 | 1.55 (1.28–1.62) µmol m⁻² leaf s⁻¹ | Barr et al. 2009 (*R. mangle*, at site); Sturchio et al. 2022 (*A. germinans*) | Species-weighted central value (S.M10) | Canopy leaf respiration, healthy class | Leaf chambers here were transparent (net exchange), so they cannot give Rd |
| Leaf area index | 2.8 (2.3–5.55) | SRS-6 ground LAI 2.80 ± 1.38 (Barr, unpublished, in Troxler et al. 2015); ground optical 2.3; MODIS at US-Skr 5.55 (Reed et al. 2025) | Effective LAI with Beer's law, k = 0.5 | Canopy leaf respiration | MODIS uses 24-day maxima and reads high. LAI recovered within ~1 yr of Wilma and Irma (Reed et al. 2025), so no hurricane reduction for 2022–23 |
| Leaf temperature response | f(T) = exp[0.1012(T−25) − 0.0005(T²−25²)] | Heskel et al. 2016 | Driven by tower air temperature; 30 % daytime light inhibition (Atkin et al. 2014; 20–50 % in the Monte Carlo) | Canopy leaf respiration, 24 h | — |
| Downed coarse woody debris volume | 67 (13–181) m³ ha⁻¹ | Krauss et al. 2005 (line-intersect surveys, South Florida mangroves, 9–10 yr after Hurricane Andrew) | Lateral surface = 4V/d with d = 10 cm (as Troxler et al. 2015 did at SRS-6), i.e. 0.27 (0.05–0.72) m² of wood per m² of ground. Exchanges with the air only above the water (S.T3). | CWD CO2 and CH4, all classes | Placeholder of 10 m² per plot (superseded); eyewall value 132 m³ ha⁻¹ as sensitivity. No ghost-specific inventory exists, so the same distribution is used. |
| Woody litterfall (context only) | 68–95 (2001–04); 47–56 (2022–23) g dry m⁻² yr⁻¹ | FCE LTER, Castañeda-Moya et al., knb-lter-fce.1195.12 (SRS-4/5/6, monthly baskets, 2001–2023) | Annual sums of the Wood fraction | Supports using the Krauss volume (S.T3) | — |
| Component CO2 effluxes at SRS-6 (context) | soil 1.27; soil + pneumatophores 3.17; prop roots 1.94; CWD 2.34 µmol m⁻² s⁻¹; scaled CWD respiration 1.6 t C ha⁻¹ yr⁻¹; below-canopy 715 g C m⁻² yr⁻¹ | Troxler et al. 2015 | As published | Comparison with our component rates and below-canopy respiration | — |
| Temperature sensitivity of chamber respiration (day → 24 h) | Q10 = 1.15 [1.13–1.18] (central: tower within-month); 2 (literature) and 4.3 (our stem chambers) as sensitivity | Tower night-time NEE (US-Skr, 2004–2023; SW_IN < 10 W m⁻², u* > 0.2 m s⁻¹, n = 46,641), log(NEE) ~ T with a year × month fixed effect (`code/07_upscaling/01_tower_gpp.R`); our stem CO2 vs temperature | Factor = mean over 24 h of Q10^(T/10) ÷ mean over measurement times | Stem, root, soil and CWD CO2 (not water, not leaf) | The within-month slope matches what the correction spans (day–night and day-to-day swings). Across seasons the tower gives 1.8, which also carries phenology, water level and salinity. Stem chambers give 4.3, likely inflated because daytime stem efflux also follows sap flow. See S.T5. |
| CH4 solubility | Bunsen coefficient (T, S) | Yamamoto et al. 1976 | — | Water CH4 flux from dissolved CH4 | — |
| CO2 solubility | K0 (T, S) | Weiss 1974 | — | Water CO2 flux from dissolved CO2 | — |
| Schmidt numbers (CH4, CO2) | Freshwater polynomials | Wanninkhof 2014 | k = k600 (Sc/600)^−0.5 | Water fluxes from dissolved gas | k600 itself is calibrated on our chamber/dissolved pairs (median 1.10 cm h⁻¹, range 0.56–7.07) |
| Fraction of time the intact forest floor is flooded (tide-state weights, SRS5/SRS6) | SRS5: 1.00 (Oct 2022), 0.69 (Mar 2023); SRS6: 0.98, 0.72. Microtopography range (level > +5 cm vs > −5 cm): SRS5 0.74–1.00, 0.29–0.73; SRS6 0.83–1.00, 0.53–0.80 | FCE LTER hourly water level above the soil surface at SRS5 and SRS6 (Castañeda-Moya et al., knb-lter-fce.1168.15) | Logger level corrected by the median difference to our own water-depth readings where we recorded standing water (+0.5 cm SRS5, n = 69; +1.1 cm SRS6, n = 51); fraction of hours above 0 in the campaign month (`code/07_upscaling/01b_flood_fraction.R`) | Weights of the high-tide (water surface; no soil, CWD exposure 0) and low-tide (exposed soil and CWD) states in the CH4 and CO2 budgets | Replaces an equal 50/50 split. Long-term (2010 onward) flooded fraction: SRS5 0.62, SRS6 0.46. Our soil collars sat on raised microsites (~0 cm while the logger read 4–6 cm), hence the range. |
| Atmospheric mixing ratios | CH4 1.95 ppm; CO2 417 µatm | Global/regional means for 2022–23 | Equilibrium concentrations | Water fluxes from dissolved gas | — |
| Lateral DIC export | 622 (311–1244) g C m⁻² yr⁻¹ | Reithmaier et al. 2020 (Eulerian, normalized to the 15.9 km² tidally inundated mangrove area) | Area basis checked against our per-ground basis | NECB, healthy | Zhao et al. 2021 (145) and Ho et al. 2017 (~86, lower bound) as the low scenario |
| Lateral DOC export | 171 (88–346) | Reithmaier et al. 2020 | As above | NECB, healthy | Romigh et al. 2006 (56); Ho et al. 2017 (8–10) |
| Lateral POC export | 144 (71–205) | Zhao et al. 2021 (litter POC, SRS-4/5/6) | — | NECB, healthy | — |
| Lateral aqueous CH4 | 0.35 (0.22–0.48) | Yau et al. 2024 (non-FCE analog) | — | NECB, healthy | — |
| Soil C burial | 123 (69–157) | Zhao et al. 2021; Breithaupt et al. | — | Storage check, healthy | — |
| Biomass change (wood increment) | 200 (65–500) | Castañeda-Moya et al. 2013 (SRS-6 repeat census); Chen & Twilley 1999 (high end) | Carbon fraction 0.45 | Storage check, healthy | Coarse roots would add ~30–50 % |
| Ghost-class lateral, burial, biomass | none | — | No ghost-specific values exist | Not included | Healthy values are not transferred to ghost stands |
| GWP of CH4 | 27.9 (100 yr); 81.2 (20 yr) | IPCC AR6 | — | Net radiative forcing | — |
| Earlier tower budget (context) | NEE −1,170 ± 127; GPP ≈ 2,270; ER ≈ 1,100 g C m⁻² yr⁻¹ (2004) | Barr et al. 2010 (same tower) | As published | Comparison only | — |
| Airborne end-members | per-flight CH4 and CO2 fluxes | Delaria et al. 2024 (CARAFE) | Matched to our campaigns: Oct 2022, plus a Mar 2023 analog = mean of Feb and Apr 2023 | Top-down comparison | Daytime flights (S.T5) |

---

## Supplementary Text

### S.T1 Stem height-extrapolation scenarios

Stem CH4 declines with height, so a stand budget requires integrating flux above the highest measured height (100 or 150 cm). The primary model fit an exponential (log-linear) decay of stem CH4 with height per site × campaign to the positive stem observations [log(flux) ~ height], guaranteeing strictly positive extrapolated values, and integrated it over the TLS stem surface-area profile by height bin. We compared this to a zero-above-maximum bound. Class-mean totals changed by ~3% or less between scenarios (ghost 32.0 vs 33.1; intact 5.89 vs 5.93 mg CH4 m−2 d−1), because the stem term is a small fraction of the whole-ecosystem budget. A linear extrapolation was rejected because it can produce negative fluxes above the measured range. (Fig. S6.)

### S.T2 Tide and inundation scenarios

Inundation determines which surfaces emit. Tidal sites (SRS5, SRS6) were represented by high-tide (fully flooded, water-surface emission) and low-tide (exposed soil emission) states, weighted by the measured fraction of time the floor was flooded in each campaign month (FCE LTER water level above the soil surface; Table S9): 1.00 and 0.98 in October 2022 and 0.69 and 0.72 in March 2023 at SRS5 and SRS6. The earlier equal 50/50 split overstated exposed soil and downed wood and understated the water surface; the measured fractions make healthy-class NEE about 270 g C m⁻² yr⁻¹ more negative than the equal split. Floor microtopography is carried as a range (Table S9). Ghost sites (CP40, FLM30) had positive measured water depth in both analysed campaigns (dry-season means 6.6–8.7 cm; lower than the wet season but not drained), so they were treated as inundated in both and emitted through the water surface.

As a robustness check we recomputed the annual ghost budget substituting the exposed-soil pathway (the ghost soil CH4 rate measured when sites were dry, pooled from FLM30 and MI, ≈17.5 mg CH4 m−2 d−1) for the flooded water-surface pathway. The ghost annual budget was ≈13.0 g CH4 m−2 yr−1 (flooded, water) versus ≈7.7 (dry, exposed soil), and ≈13.7 for a wet-flooded / dry-exposed seasonal mix. Treating ghost surfaces as exposed soil would therefore lower the ghost methane budget by up to ~40%, but the ghost class remains a net radiative source under every inundation assumption because its CO2 balance is already positive. The sink-to-source result is therefore insensitive to the inundation assumption. Because ghost emission is overwhelmingly a directly measured surface flux scaled by inundated area, structural uncertainty is concentrated in the intact class (Fig. S7).

### S.T3 Coarse woody debris

**What TLS covers.** TLS represents standing trees, both live and dead, as trunk and branch segments. Standing dead trees therefore enter the budget through the stem term; our stem chambers include dead stems. Downed wood is not modelled by the TLS products, and no plot inventory of downed wood exists for these sites.

**Area.** Following Troxler et al. (2015), who measured CWD efflux at SRS-6, we took downed-wood volume from Krauss et al. (2005). Krauss et al. made line-intersect surveys of South Florida mangroves 9–10 years after Hurricane Andrew and reported 67 m³ ha⁻¹ on average across sites, ranging from 13 to 181. That range reflected forest height, distance from the storm track and wind speed, and the eyewall region held 132. About half of that volume was fine woody debris (< 7.5 cm). We converted volume to lateral surface with a 10 cm piece diameter (SA = 4V/d), as Troxler et al. did. This gives 0.27 (0.05–0.72) m² of wood surface per m² of ground. The fine-debris fraction means this conversion probably underestimates the surface.

**Context from the FCE LTER litterfall record.** Woody litterfall at SRS-4/5/6 (knb-lter-fce.1195.12; monthly baskets, 2001–2023) was:

- 68–95 g dry m⁻² yr⁻¹ in 2001–04, around the time of the Krauss survey;
- 149–389 in 2005 (Hurricane Wilma);
- 264–276 in 2017 (Hurricane Irma);
- 34–46 in 2018–21;
- 47–56 in 2022–23, during our campaigns.

Litter baskets catch fine woody material, not felled stems, so this record indexes inputs, not the standing pool. Our campaigns came about 5 years after Irma, whereas the Krauss survey came 9–10 years after Andrew. With less time for decay since the last major storm, the present downed-wood pool is plausibly at least comparable to the Krauss mean. We therefore use 67 m³ ha⁻¹ as the central value and 13–181 as the range (Monte Carlo: lognormal with these as approximate 95 % bounds). The eyewall value (132) is reported as a sensitivity. For the ghost stands, which have no downed-wood inventory, the same distribution is used, and we flag this as a key data gap.

**Inundation.** Downed wood exchanges gas with the atmosphere only above the water:

- **Tidal (healthy) sites:** CWD contributes at low tide only, as soil does.
- **Always-flooded (ghost) sites:** CWD contributes through the arc of a lying log of the median measured CWD diameter (11 cm) that sits above the waterline, at the site × campaign mean water depth (field records). The exposed fraction is acos((h−r)/r)/π.

At the ghost sites the water was 7–23 cm deep, so most downed wood is submerged there. Respiration from submerged wood goes to the water column and is partly captured by the water-surface chambers.

**Effect.** At full exposure the Krauss central value corresponds to about 0.5 µmol CO2 m⁻² s⁻¹ (ground) of healthy-class respiration, the same order as Troxler et al.'s 1.6 t C ha⁻¹ yr⁻¹ at SRS-6 (~0.42 µmol m⁻² s⁻¹). Because the intact forest floor was flooded most of the time in the analysed campaigns (S.T2; ~1.0 in Oct 2022, ~0.7 in Mar 2023), the time-weighted contribution is about 0.1 µmol m⁻² s⁻¹ in the healthy class and about 0.16 in the ghost class. CH4 from CWD remains < 1 % of the methane budget in all classes. With the tower Q10, the full Krauss range (13–181 m³ ha⁻¹) moves healthy NEE between about −1,240 and −1,140 g C m⁻² yr⁻¹ (−1,250 with no downed wood). Across all Q10 options it spans −1,360 to −1,120, against −1,290 from the tower. Ghost NEE stays a source, about +330 to +525 (`output/qa/budget_scenarios.csv`, which crosses the CWD volume with the Q10 options; CH4: `output/upscaling/cwd_sensitivity.csv`).

### S.T4 Monte Carlo uncertainty decomposition

The Monte Carlo propagation (S.M13) identifies which terms dominate budget and forcing intervals. Stem flux was propagated by drawing the intercept and height-decay coefficient of each site × campaign log-linear fit from their joint sampling distribution; draws implying an increase with height were held constant with height, consistent with the decay model. For the methane budget, intact-class uncertainty is dominated by the exposed-soil fraction and TLS root surface area; ghost-class uncertainty is dominated by water-surface flux variability. For net forcing, the leaf-respiration term (Rd25 and LAI) and tower GPP dominate the intact interval, whereas the ghost interval is set by the CH4 budget. The two-state forcing intervals do not overlap (intact −6,133 to −3,521; ghost +1,474 to +2,045 g CO2-eq m−2 yr−1, GWP100). (Fig. S10.)

### S.T5 CO2 closure caveats

Two factors mean bottom-up respiration may legitimately exceed tower Reco and that airborne–chamber CO2 comparison requires care: (i) airborne midday fluxes were converted to a daily basis, and chamber respiration was measured near midday (chamber stem, root, soil and CWD CO2 were scaled to 24-h temperature with the tower's within-month Q10 of 1.15; Table S9); (ii) tower-partitioned Reco extrapolates nighttime respiration across daylight without light inhibition. Lateral tidal export of dissolved inorganic carbon is not one of these factors: it removes respired carbon that neither the tower nor the chambers see, because both measure only CO2 released to the air. Closure was therefore assessed for consistency within uncertainty rather than forced.

### S.T6 Regenerating class and an approximate upscaled estimate

Component flux rates were measured at the regenerating site (BL60), but no TLS-based surface area or airborne closure was available for a regenerating end-member (S.M9). The regenerating class is therefore reported at the component scale and excluded from the closure-validated budgets and net forcing.

BL60 had the highest component areal CH4 rates of any class — soil 55 nmol m−2 s−1 (95% CI 27–89), water 35 (15–55), stem 15 (4–33) and root 5.7 (2.7–8.1). To place these on a stand basis without TLS, we made a first-order upscaled estimate: BL60 stem and root surface-area-per-ground-area ratios were set to the mean of the ghost and healthy classes (i.e. structure exactly intermediate along the disturbance gradient), and soil/water fractions to BL60's measured inundated fraction (~33% of chambers had standing water). This gives a regenerating CH4 budget of ≈29 g CH4 m−2 yr−1 — exceeding both the ghost (~13) and intact (~2) classes, driven by BL60's very high exposed-soil and water rates. This estimate is illustrative only: it assumes intermediate structure, rests on a single dry-biased sampling, and is not part of the closed budgets or forcing. It nonetheless indicates that early regeneration need not be a low-emission state and may be the peak of the disturbance methane response (supp_regen_budget.csv).

### S.T7 Carbonate-buffer drawdown at ghost sites (TA–DIC)

Elevated total-alkalinity-to-DIC slopes at ghost sites (>1) indicate ongoing CaCO3 dissolution, consistent with episodic acid generation via sulfide/pyrite reoxidation once the protective vegetation cover is lost. Over time this represents a non-renewable drawdown of the sediment's carbonate buffering reservoir, in contrast to the biogenic alkalinity generation (≈1:1 TA:DIC, from sulfate reduction) sustained at healthy and regenerating sites. (Figs. S12–S13.)

### S.T8 Context sites (component areal rates)

Beyond the five core sites, single dry-season visits to additional sites place the core results in a wider geomorphic and salinity context (component areal rates in supp_context_site_areal_rates.csv; all comparisons below are dry-season, like-for-like). (i) **Marco Island (MI), a Ten Thousand Islands ghost site**, emitted far less than the core Everglades ghost sites at comparable (dry) season: soil 2.3 nmol m−2 s−1 (95% CI 1.9–2.7) and stem 0.5 (0.2–0.9), versus core ghost soil up to ~34 and stem 12–13. Ghost-forest methane is therefore strongly setting-dependent, and the high core-Everglades values should not be assumed to transfer to all hurricane-killed mangrove. (ii) **Rookery Bay (RB10), a healthy site**, had a soil rate (1.2 nmol m−2 s−1, 0.5–2.0) comparable to the core intact class (0.6–13), supporting the representativeness of the core intact sites. (iii) **The SE-1 scrub-mangrove ecotone** showed moderate aerial-root (3.2 nmol m−2 s−1) and low stem (0.16) and water (1.0) CH4, with net leaf CO2 uptake (−1.38 µmol m−2 s−1). Leaf chambers at SE-1 and BL60 (leaf CO2 −1.38 and −1.23 µmol m−2 s−1; leaf CH4 ≈ 0.01–0.03) corroborate the direction and magnitude of the leaf CO2 term (−1.3) used in the budget and confirm leaves are not a meaningful CH4 pathway. The literature Rd25 values underlying the modelled canopy respiration derive from *R. mangle* at Shark River (Barr 2009) and *A. germinans* (Sturchio 2022); see S.M10 and Table S6.

---

## Supplementary Figures

| # | Content | Source |
|---|---|---|
| S1 | Ebullition partitioning and example traces | pub_SI_ebullition_partition |
| S2 | Soil flux vs pneumatophore density | pub_SI_pneumatophore_density |
| S3 | Chamber designs (stem, root, soil, water, leaf) | pub_SI_chamber_photos |
| S4 | Full per-plot × campaign flux distributions (CH4 + CO2) | pub_component_by_plot_campaign_combined_condensed_boot |
| S5 | Stem height × species × status estimated marginal means (CH4 + CO2) | pub_stem_height_composite_combined |
| S6 | Stem height-extrapolation scenarios and sensitivity | stem_extrap_method; height_extrap_sensitivity; pub_extrap_dumbbell |
| S7 | Tide/inundation scenario comparison | scenario_comparison_tidal |
| S8 | TLS surface area by segment class and height (fixed y-scales) | SA_by_segment_height_fixedY |
| S9 | Budget decomposition (rate × area → integrated → %) | 11c–11f |
| S10 | Monte Carlo uncertainty decomposition | pub_uncertainty_decomp |
| S11 | Tower GPP diurnal/seasonal and light-use-efficiency context | US-Skr GPP plots |
| S12 | Porewater depth profiles (all variables) | pub_porewater_depth_profiles |
| S13 | Total alkalinity–DIC and excess-TA / SO4-deficit | pub_SI_ta_vs_dic; pub_SI_TA_vs_SO4_deficit |
| S14 | Salinity vs dissolved CH4 by site | pub_SI_salinity_vs_ch4_bysite |
| S15 | CARAFE footprint / land-cover disaggregation | [PLACEHOLDER — from Delaria et al. 2024] |
| S16 | Closure residual analysis | [PLACEHOLDER — to build] |

_[If microbial/metagenomic data are added, they may appear as an added Fig. 5 panel or as a new supplementary figure; see the Results and Discussion placeholders.]_

---

## Supplementary Tables

- **S1** Site metadata (coordinates, class, salinity, canopy, n). [site_metadata.csv]
- **S2** Component CH4/CO2 flux rates by class and season, with 95% CIs and n.
- **S3** Stem mixed-model results (species, height, status; EMMs, contrasts).
- **S4** Airborne end-member fluxes used (per-flight; Delaria et al. 2024, Tables S2/S3). [delaria_endmembers_campaign.csv]
- **S5** Stand-level budgets and net forcing by class, with Monte Carlo 95% CIs. [net_forcing_by_class; mc_*]
- **S6** Literature leaf Rd25 and LAI values used in the CO2 budget. [manuscript/literature/value_catalog.csv]
- **S7** Context-site component areal CH4/CO2 rates (MI, RB10, SE1) with 95% CIs and n, alongside core-site rates. [supp_context_site_areal_rates.csv]
- **S9** Literature-derived values: source, conversion, use and alternatives considered (S.M14).
- **S8** Ghost inundation sensitivity (flooded vs exposed-soil vs mixed) and the regenerating upscaled estimate. [supp_ghost_inundation_sensitivity.csv; supp_regen_budget.csv]

---

## Data and code availability

Chamber flux dataset (combined_gas_flux_dataset.csv) and analysis workflow at [repository URL]; airborne fluxes from Delaria et al. (2024) / ORNL DAAC; tower data from AmeriFlux US-Skr.

## Supplementary References

[To compile: Reed et al. 2025 (GCB, doi:10.1111/gcb.70124); Atkin et al. 2014 (New Phytol., doi:10.1111/nph.12686); Zhu et al. 2024 (GRL, doi:10.1029/2023GL107235); Castañeda-Moya et al. FCE LTER water levels (knb-lter-fce.1168.15); Poulter et al. 2023 (BlueFlux ERL); Krauss et al. 2005; Castañeda-Moya et al. FCE LTER litterfall (knb-lter-fce.1195.12); Castañeda-Moya et al. 2013; Chen & Twilley 1999; Reithmaier et al. 2020; Zhao et al. 2021; Ho et al. 2017; Romigh et al. 2006; Breithaupt et al.; Yau et al. 2024; Yamamoto et al. 1976; Weiss 1974; Wanninkhof 2014; Delaria et al. 2024 (CARAFE JGR); Lagomasino et al. 2021; Kljun et al. 2015; Hutjes et al. 2010; Hannun et al. 2020; Heskel et al. 2016; Barr et al. 2009, 2010; Sturchio 2022; Troxler et al. 2015; IPCC AR6.]

---

### Outstanding to complete SI
1. TLS methods (S.M6) — Powell/Stovall.
2. Tower (S.M8) and CARAFE (S.M9) instrument/processing detail — tower PIs / Delaria.
3. Caribbean ghost-forest extent value and source, and Irma dieback area (S.M12) for Fig. 6e,f.
4. Build Fig. S16 (closure residual) and source Fig. S15 (CARAFE footprint).
5. Compile Supplementary References with DOIs.
6. If sequencing data land: microbial results (main-text Results placeholder) and metagenome discussion, plus any added figure/table.
