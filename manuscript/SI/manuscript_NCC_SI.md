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

Chamber designs were adapted to component geometry (Fig. S3). Four elliptical stem-chamber sizes (A–D) accommodated stem diameters from ~3 cm to >20 cm, with enclosed areas 40–462 cm2. Stem chambers were sealed to the bark with modelling clay at 0, 50 and 100 cm above the sediment (and 150 cm where diameter permitted) to test for vertical gradients diagnostic of soil-origin gas transport. Prop-root chambers enclosed sections of individual *R. mangle* aerial roots, sealed with clay. Leaf fluxes used a transparent chamber (a modified container with a rubber gasket and flat transparent top angled toward the sun) enclosing a branch cluster passing through a clay-sealed notch; it was not temperature- or VPD-controlled and was used to determine flux direction and approximate magnitude, with incubations terminated while CO2 uptake remained approximately linear. Two soil designs were used: open-bottom acrylic cylinders (23.5 cm diameter, inserted 30–91 cm and capped with a foam-gasketed lid) in the wet season, and smaller PVC collars (14.3 or 19.4 cm, inserted ~5 cm ≥1 h before sampling, capped with a domed chamber) in the dry season; both sealed with minimal downward pressure to avoid degassing or inducing ebullition. Soil measurements included pneumatophores within the footprint, with density recorded per collar. Surface-water fluxes used floating chambers (19.4 cm diameter, 324.3 cm2 footprint). All chambers used closed-loop tubing with inline desiccant (Drierite) and a ventilation port sealed before measurement, and were CO2 leak-tested before each measurement. System volumes were computed per chamber–analyzer combination (headspace + collar + tubing + desiccant + cell): 0.5–3.1 L (stems), 0.5–2.0 L (roots), 2.1–39.7 L (soil), 4.3 L (floating water).

Measurements covered all four species, including standing dead trunks and snags at ghost sites (≥5 stems per species per site; 10 soil collars per plot). Median incubations were 180 s (stems, soil, roots, CWD, leaves) and 300 s (water); full range 60–1,800 s. Meteorological and ancillary variables were recorded at each measurement — air, stem, soil and water temperature, water depth, barometric pressure and relative humidity — together with (for stems) DBH, perimeter at each height, species and alive/dead status. In total 867 fluxes were collected across six components: stems (n = 486), soil including pneumatophores (n = 118), water (n = 92), roots (n = 65), coarse woody debris (n = 27) and leaves (n = 19).

### S.M4 Flux calculation

Fluxes were computed with the goFlux R package. Both a linear (LM) and the Hutchinson–Mosier nonlinear (HM) model were fitted to each headspace time series; HM accounts for nonlinear saturation of concentration build-up in closed chambers, yielding higher initial estimates when headspace approaches equilibrium with the source. The best model was selected by corrected Akaike Information Criterion (AICc). Rates (nmol m−2 s−1 CH4; µmol m−2 s−1 CO2) were computed from the initial slope of the selected model, corrected for chamber volume, surface area, air temperature and barometric pressure. The ideal-gas conversion used each measurement's field-recorded air temperature; missing air-temperature values were gap-filled hierarchically — the mean of records within 30 min of the same date, otherwise the nearest record on the same day — and pressure defaulted to 101.325 kPa where a field reading was unavailable. Minimum-detectable-flux (MDF) thresholds were computed dynamically per measurement from volume, analyzer precision and duration. Sub-detection fluxes were retained at their measured values. All traces were visually inspected; seven measurements with clear artefacts were excluded. Measurements that failed automated goFlux processing were manually reviewed and recovered where a valid initial slope could be identified, and stem-chamber system volumes were corrected post hoc for two chamber classes with a headspace-geometry offset.

### S.M5 Ebullition partitioning

Water-surface CH4 fluxes were partitioned into diffusive and ebullitive components. Within each deployment, ebullition events were identified as abrupt upward concentration jumps exceeding 0.10 ppm between consecutive readings, far above the expected inter-sample diffusive accumulation (0.001–0.01 ppm). To isolate the diffusive signal, the cumulative magnitude of all preceding jumps was subtracted from post-bubble values (step-correction), with a 15 s buffer excluded on either side of each jump. The diffusive flux was fitted to the step-corrected series (LM/HM selection as above). The ebullitive component was the total magnitude of detected jumps converted to a flux over the deployment duration (Fig. S1).

### S.M6 Terrestrial laser scanning [PLACEHOLDER — Powell/Stovall to confirm and complete]

Three-dimensional forest structure was measured with a RIEGL VZ-400i terrestrial laser scanning system paired with GNSS positioning (Trimble R8 rover, R6 base). Four panorama scans per plot were acquired at 0.03° resolution; point clouds were registered via an iterative-closest-point algorithm in RiSCAN PRO and georeferenced to WGS 84 / UTM 17N. Clouds were segmented into component classes (stems, prop roots, pneumatophores, ground surface); surface area was extracted by component and height interval, and allometric relationships linking TLS-derived areas to DBH and species were used for plot-level scaling. [PLACEHOLDER: comparison of TLS-derived surface areas with traditional allometric estimates; sensitivity of bottom-up budgets to the surface-area method; final acquisition/segmentation parameters and QC to be confirmed by collaborators.]

### S.M7 Bottom-up scaling and budget construction

**Methane.** Stand-level CH4 budgets were constructed by multiplying component flux densities (nmol CH4 m−2 surface s−1) by TLS-derived surface area per unit ground area (m2 component m−2 ground) for each class (soil, surface water, prop roots, stems, coarse woody debris), summed over components. Species-specific stem rates were weighted by relative surface-area contribution. Stem flux was integrated over height from the measured 0/50/100(/150) cm rates; the primary budget extrapolated above the highest measurement with an exponential (log-linear, strictly positive) decay, bounded by a zero-above-maximum alternative (S.T1). Inundation controls which surfaces emit (S.T2), and the emitting-surface fraction was set from measured chamber water depth. Non-tidal ghost sites (CP40, FLM30) were inundated in both analysed campaigns (mean water depth 8.7 and 6.6 cm in the dry season, 10.8 and 16.1 cm in the wet), so their emitting surface was treated as 100% water in both; exposed-soil emission at these sites was measured only in the dropped March 2022 campaign, when they were dry. Tidal intact sites (SRS5, SRS6) were represented as high-tide (100% water) and low-tide (100% exposed soil) endpoints averaged 50/50. Uncertainties were propagated by Monte Carlo (S.M13; S.T4).

**Net ecosystem CO2 exchange.** Bottom-up NEE = Reco − GPP (atmospheric sign; positive = source). Reco was built by scaling chamber CO2 efflux densities with the same TLS surface areas (soil, water, prop roots, woody stems plus branch surface) and adding a literature-based canopy foliar-respiration term (S.M10). GPP was not constructed bottom-up: it was imported from the co-located SRS-6 (US-Skr) tower, where GPP is partitioned from observed NEE with a nighttime NEE–temperature respiration model (GPP = Reco,tower − NEEtower). For ghost plots the defoliated canopy was assumed to have negligible GPP, so NEE ≈ Reco,bottom-up. Because imported tower GPP embeds the tower's own NEE and partitioned respiration, intact-forest bottom-up NEE reduces algebraically to NEE = NEEtower + (Reco,bottom-up − Reco,tower); it is therefore a reconciliation of bottom-up against tower-partitioned respiration rather than an independent net-flux estimate, and is treated as such. Tower-partitioned Reco extrapolates nighttime respiration across daylight without light inhibition, whereas the bottom-up foliar term applies daytime inhibition, so the two respiration estimates are not identical in construction (S.T5). Independent top-down evaluation of both FCO2 and FCH4 is provided by CARAFE (S.M9).

### S.M8 Eddy-covariance tower (SRS-6 / US-Skr) [PLACEHOLDER — instruments/processing to confirm]

Continuous half-hourly fluxes of CO2 and CH4 were measured at 27 m on a 30 m tower; instruments include a Gill sonic anemometer, an open-path CO2/H2O analyzer (LI-7500) and a CH4 analyzer (LI-7700) sampling at 20 Hz. CH4 measurements began in 2018. Standard eddy-covariance processing with friction-velocity (u*) filtering was applied. The tower is part of the Florida Coastal Everglades LTER and AmeriFlux (US-Skr) networks. [PLACEHOLDER: final instrument models, tower height, u* threshold, gap-filling and partitioning settings to be confirmed by tower PIs.]

### S.M9 Airborne eddy covariance (CARAFE) [PLACEHOLDER — Delaria/JGR to confirm]

Flights at ~90 m altitude aboard a Beechcraft King Air A90 carried a Picarro G2311-f (10 Hz CO2/CH4/H2O for eddy covariance), a Picarro G2401m (0.5 Hz, calibrated to NOAA/WMO standards) and an Aventech AIMMS-20 probe (20 Hz 3D winds, temperature, pressure, position). Fluxes were computed by continuous wavelet transform. Flux legs were segments >15 km with roll <5° and altitude within ±10 m. Median 1-km detection limits were 5.8 nmol m−2 s−1 (CH4) and 0.9 µmol m−2 s−1 (CO2). Two-dimensional footprints followed Kljun et al. (2015) using HRRR 3-km boundary-layer heights. Fluxes were disaggregated by land-cover class via multilinear regression (Hutjes et al. 2010; Hannun et al. 2020), with the ghost-forest class from Lagomasino et al. (2021). Because the regenerating class could not be spatially separated from the airborne footprints, closure used a two-end-member (intact, ghost) disaggregation following Delaria et al. (2024); attempts to add a regenerating end-member produced physically inverted values and were not pursued. The March 2023 airborne value is the mean of the February and April 2023 deployments (variances combined). For full instrument and processing detail, see ref. [CARAFE JGR, Delaria et al. 2024].

### S.M10 Tower GPP partitioning and leaf-respiration synthesis

Tower GPP was obtained by partitioning observed half-hourly NEE. Ecosystem respiration was fit as a log-linear (exponential) function of tower air temperature to nighttime records (incoming shortwave ≤ 10 W m−2), extrapolated across daylight, and subtracted from observed NEE to give daytime GPP (GPP = Reco − NEE, floored at zero); a rectangular-hyperbola light response of GPP to shortwave radiation was then fit, and uncertainty propagated by bootstrapping the nighttime and daytime records. Over the two campaign windows this gives a mean daytime GPP for the intact class of ≈8.7 µmol m−2 s−1 (Oct 2022 8.2; Mar 2023 9.2), the value used in the CO2 budget. The bottom-up CO2 budget requires a canopy foliar-respiration term that the TLS cannot supply (no foliage surface area); we built it from a literature synthesis specific to these species and site (full provenance, value IDs and corrections in manuscript/literature/; Table S6). Key values: species leaf dark respiration at 25 °C, Rd25 = 1.62 ± 1.32 µmol m−2 s−1 for *R. mangle* (Barr 2009, at-site, LI-6400/Farquhar) and Rd25 = 1.28–1.54 µmol m−2 s−1, Q10 = 2.39 for *A. germinans* (Sturchio 2022); a species-weighted central Rd25 of 1.55 (range 1.28–1.62). Leaf-area index was carried as a range from 2.3 (ground optical, hurricane-suppressed) to 5.55 (MODIS, recovered), the dominant scaling lever; the central budget used L = 2.3. Because self-shading reduces respiratory capacity through the canopy, the leaf term used an effective LAI, LAIeff = (1 − e−kL)/k with extinction coefficient k = 0.5 (L = 2.3 → LAIeff ≈ 1.37). The short-term temperature response followed Heskel et al. (2016): f(T) = exp[0.1012(T − 25) − 0.0005(T2 − 252)], driven by tower air temperature, with a 30% daytime light-inhibition factor applied to the daytime fraction. Canopy foliar respiration (µmol m−2 ground s−1) = Rd25 × f(T)day × LAIeff. Supporting context: foliar respiration is ~1/3 of Reco (proxy), below-canopy chamber respiration is 45–65% of Reco at this site (Troxler 2015), and midday GPP for tall riverine Shark River can reach ~37 µmol m−2 s−1. Two provenance caveats: OCR unit errors in the Barr 2009/2010 PDFs (µmol printed as mmol) were corrected to µmol; and lateral tidal DIC export means tower Reco likely under-counts belowground respiration, so a bottom-up chamber+leaf sum may legitimately exceed tower Reco (closure was not forced; S.T5).

### S.M11 Porewater geochemistry

Porewater was collected at four sites (SRS5, SRS6, BL60, CP40) at five depths (surface, 0, 15, 45 and 90 cm below the sediment surface) with MHE PushPoint samplers during the dry-season campaign. Temperature, pH, electrical conductivity, dissolved oxygen and oxidation–reduction potential were measured in the field (Hanna HI98494). Sulfide and ferrous iron were determined colorimetrically in the field immediately after sampling (Hach DR900; sulfide methylene-blue method, reagents 1816/1817; iron FerroVer, reagent 2105769). Dissolved CH4 and CO2 concentrations and stable carbon isotopes (δ13C-CH4, δ13C-CO2) were measured by headspace equilibration in syringes on a Picarro G2201-i cavity ring-down spectrometer with SAM autosampler and ultra-zero-air carrier at Yale University. Dissolved organic carbon (Shimadzu TOC), major ions (sulfate, chloride, nitrate, phosphate; Metrohm ion chromatograph) and total alkalinity (titration) were measured at the Yale Analytical and Stable Isotope Center (YASIC).

### S.M12 Net radiative forcing and landscape scaling

Component CH4 fluxes were converted to CO2 equivalents using 100- and 20-year global warming potentials (GWP100 = 27.9, GWP20 = 81.2; IPCC AR6) and combined with net CO2 exchange to give net forcing (g CO2-eq m−2 yr−1) for each class. The intact CO2 NEE was anchored to the SRS-6 tower; ghost and regenerating CO2 exchange were evaluated against CARAFE disaggregated fluxes. The disturbance-induced switch is the intact-to-ghost differential. A regional estimate multiplies the per-area differential by the area of Caribbean mangrove converted to ghost forest by recent hurricanes (Fig. 6e,f). [PLACEHOLDER — Caribbean ghost-forest extent value/source and, for the Everglades-specific figure, the Irma dieback area from Lagomasino et al. (2021), to be finalized.]

### S.M13 Statistical analysis

Fluxes were transformed with the inverse hyperbolic sine (asinh) before modelling; asinh accommodates positive and negative values, approximates the natural logarithm for large values and is linear near zero. Component summaries and 95% CIs used nonparametric bootstrap resampling (5,000 iterations, percentile method) stratified by component, site, season and class. Stem species and height effects used linear mixed-effects models (lme4/lmerTest) with site as a random intercept; three formulations were evaluated — (1) species + continuous height + season; (2) species × height category (0–50, 50–100, 100–150 cm) + season; (3) species–status combinations (alive/dead *A. germinans* and *R. mangle* separate; *C. erectus* and *L. racemosa* pooled) + height category + season — restricted to sites with species identification (BL60, SRS5, SRS6; species with n ≥ 5). Estimated marginal means were computed on the asinh scale, back-transformed via sinh, and compared with Tukey-adjusted contrasts and Type III F-tests. Porewater structure used principal-components analysis on 11 centred, scaled variables (salinity, dissolved CH4, sulfate, δ13C-CH4, ORP, dissolved O2, sulfide, iron, DOC, alkalinity, dissolved CO2) across all site–depth combinations. Budget and forcing uncertainty was propagated by Monte Carlo (5,000 draws) combining bootstrapped chamber flux densities, the leaf term (Rd25 ~ U[1.28, 1.62]; LAI ~ N[2.3, 0.3] truncated at 1.5), tower GPP uncertainty and the CH4 budget total. Analyses were run in R (≥4.3).

---

## Supplementary Text

### S.T1 Stem height-extrapolation scenarios

Stem CH4 declines with height, so a stand budget requires integrating flux above the highest measured height (100 or 150 cm). The primary model fit an exponential (log-linear) decay of stem CH4 with height per site × campaign to the positive stem observations [log(flux) ~ height], guaranteeing strictly positive extrapolated values, and integrated it over the TLS stem surface-area profile by height bin. We compared this to a zero-above-maximum bound. Plot totals changed by <2% between scenarios (ghost 36.0 vs 36.8; intact 1.42 vs 1.45 mg CH4 m−2 d−1), because the stem term is a small fraction of the whole-ecosystem budget. A linear extrapolation was rejected because it can produce negative fluxes above the measured range. (Fig. S6.)

### S.T2 Tide and inundation scenarios

Inundation determines which surfaces emit. Tidal intact sites (SRS5, SRS6) were represented by high-tide (fully flooded, water-surface emission) and low-tide (exposed soil emission) states averaged 50/50. The ghost inundation state varied interannually: the dropped March 2022 dry campaign found ghost sites exposed (soil emitting), whereas the two campaigns used in the budget (October 2022 and March 2023) had positive measured water depth (6.6–16.1 cm), with the water surface emitting.

The airborne top-down (CARAFE) adjudicates which chamber-scale state is representative of the landscape (supp_carafe_inundation_adjudication.csv). Bottom-up ghost areal flux under the flooded (water) assumption matches the airborne ghost end-member at every campaign: wet-season October 2022 bottom-up 46 vs airborne 51 ± 27, and dry-season March 2023 bottom-up 11 vs airborne ~4.5 nmol m−2 s−1. Under an exposed-soil assumption, however, the dry-2022 bottom-up (65 nmol m−2 s−1, from the high exposed-soil chamber flux of ~62) overshoots the airborne dry-2022 ghost value (5 ± 4) roughly 13-fold. The elevated exposed-soil chamber fluxes are therefore localized hotspots that do not scale to the landscape; the flooded/water treatment used in the budget is top-down validated, and dry-season ghost emission is low in both years regardless of the exposed/flooded microstate. Treating dry-season ghost sites as exposed soil would have overestimated their emission and broken closure with the airborne flux.

For intact sites, bottom-up matches the airborne mangrove-forest end-member in the dry season (Mar 2023 ~1.6 vs airborne 2.0/−2.5) but falls below the wet-season airborne value (Oct 2022 6.9 vs 29 ± 16 nmol m−2 s−1) even under generous soil-exposure fractions (the intact soil term cannot close the gap because intact soil rates are low). This points to a genuine unmeasured wet-season intact term — ebullition, tidal creek and water-surface emission, or airborne footprint mismatch — rather than an inundation-fraction error. Because ghost emission is overwhelmingly a directly measured surface flux scaled by inundated area, structural uncertainty in the closed budgets is concentrated in the intact class (Fig. S7).

### S.T3 Coarse-woody-debris surface-area sensitivity

CWD surface area was extracted from TLS where resolvable and supplemented with field measurements. CWD contributed <1% of the methane budget in all classes; plausible variation in CWD surface area does not affect budget totals or the sink-to-source conclusion. [PLACEHOLDER — final CWD area treatment with TLS collaborators.]

### S.T4 Monte Carlo uncertainty decomposition

The Monte Carlo propagation (S.M13) identifies which terms dominate budget and forcing intervals. For the methane budget, intact-class uncertainty is dominated by the exposed-soil fraction and TLS root surface area; ghost-class uncertainty is dominated by water-surface flux variability. For net forcing, the leaf-respiration term (Rd25 and LAI) and tower GPP dominate the intact interval, whereas the ghost interval is set by the CH4 budget. The two-state forcing intervals do not overlap (intact −6,128 to −3,505; ghost +1,480 to +2,054 g CO2-eq m−2 yr−1, GWP100). (Fig. S10.)

### S.T5 CO2 closure caveats

Three factors mean bottom-up respiration may legitimately exceed tower Reco and that airborne–chamber CO2 comparison requires care: (i) airborne midday fluxes were converted to a daily basis, and chamber respiration was measured near midday; (ii) tower-partitioned Reco extrapolates nighttime respiration across daylight without light inhibition; and (iii) lateral tidal export of dissolved inorganic carbon removes respired carbon that the tower does not see. Closure was therefore assessed for consistency within uncertainty rather than forced.

### S.T6 Regenerating class and an approximate upscaled estimate

Component flux rates were measured at the regenerating site (BL60), but no TLS-based surface area or airborne closure was available for a regenerating end-member (S.M9). The regenerating class is therefore reported at the component scale and excluded from the closure-validated budgets and net forcing.

BL60 had the highest component areal CH4 rates of any class — soil 78 nmol m−2 s−1 (95% CI 38–127), water 35 (16–56), stem 15 (4–32) and root 5.7 (2.6–8.2). To place these on a stand basis without TLS, we made a first-order upscaled estimate: BL60 stem and root surface-area-per-ground-area ratios were set to the mean of the ghost and healthy classes (i.e. structure exactly intermediate along the disturbance gradient), and soil/water fractions to BL60's measured inundated fraction (~33% of chambers had standing water). This gives a regenerating CH4 budget of ≈37 g CH4 m−2 yr−1 — exceeding both the ghost (~13) and intact (~2) classes, driven by BL60's very high exposed-soil and water rates. This estimate is illustrative only: it assumes intermediate structure, rests on a single dry-biased sampling, and is not part of the closed budgets or forcing. It nonetheless indicates that early regeneration need not be a low-emission state and may be the peak of the disturbance methane response (supp_regen_budget.csv).

### S.T7 Carbonate-buffer drawdown at ghost sites (TA–DIC)

Elevated total-alkalinity-to-DIC slopes at ghost sites (>1) indicate ongoing CaCO3 dissolution, consistent with episodic acid generation via sulfide/pyrite reoxidation once the protective vegetation cover is lost. Over time this represents a non-renewable drawdown of the sediment's carbonate buffering reservoir, in contrast to the biogenic alkalinity generation (≈1:1 TA:DIC, from sulfate reduction) sustained at healthy and regenerating sites. (Figs. S12–S13.)

### S.T8 Context sites (component areal rates)

Beyond the five core sites, single dry-season visits to additional sites place the core results in a wider geomorphic and salinity context (component areal rates in supp_context_site_areal_rates.csv; all comparisons below are dry-season, like-for-like). (i) **Marco Island (MI), a Ten Thousand Islands ghost site**, emitted far less than the core Everglades ghost sites at comparable (dry) season: soil 2.8 nmol m−2 s−1 (95% CI 2.1–3.5) and stem 0.5 (0.2–0.9), versus core ghost soil up to ~63 and stem 11–13. Ghost-forest methane is therefore strongly setting-dependent, and the high core-Everglades values should not be assumed to transfer to all hurricane-killed mangrove. (ii) **Rookery Bay (RB10), a healthy site**, had a soil rate (1.2 nmol m−2 s−1, 0.6–2.0) comparable to the core intact class (0.6–13), supporting the representativeness of the core intact sites. (iii) **The SE-1 scrub-mangrove ecotone** showed moderate aerial-root (3.2 nmol m−2 s−1) and low stem (0.16) and water (1.0) CH4, with net leaf CO2 uptake (−1.37 µmol m−2 s−1). Leaf chambers at SE-1 and BL60 (leaf CO2 −1.37 and −1.22 µmol m−2 s−1; leaf CH4 ≈ 0.01–0.03) corroborate the direction and magnitude of the leaf CO2 term (−1.3) used in the budget and confirm leaves are not a meaningful CH4 pathway. The literature Rd25 values underlying the modelled canopy respiration derive from *R. mangle* at Shark River (Barr 2009) and *A. germinans* (Sturchio 2022); see S.M10 and Table S6.

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
- **S8** Ghost inundation: airborne (CARAFE) adjudication of bottom-up flux under flooded vs exposed-soil assumptions by campaign, annual sensitivity, and the regenerating upscaled estimate. [supp_carafe_inundation_adjudication.csv; supp_ghost_inundation_sensitivity.csv; supp_regen_budget.csv]

---

## Data and code availability

Chamber flux dataset (combined_gas_flux_dataset.csv) and analysis workflow at [repository URL]; airborne fluxes from Delaria et al. (2024) / ORNL DAAC; tower data from AmeriFlux US-Skr.

## Supplementary References

[To compile: Poulter et al. 2023 (BlueFlux ERL); Delaria et al. 2024 (CARAFE JGR); Lagomasino et al. 2021; Kljun et al. 2015; Hutjes et al. 2010; Hannun et al. 2020; Heskel et al. 2016; Barr et al. 2009, 2010; Sturchio 2022; Troxler et al. 2015; IPCC AR6.]

---

### Outstanding to complete SI
1. TLS methods (S.M6) and CWD area (S.T3) — Powell/Stovall.
2. Tower (S.M8) and CARAFE (S.M9) instrument/processing detail — tower PIs / Delaria.
3. Caribbean ghost-forest extent value and source, and Irma dieback area (S.M12) for Fig. 6e,f.
4. Build Fig. S16 (closure residual) and source Fig. S15 (CARAFE footprint).
5. Compile Supplementary References with DOIs.
6. If sequencing data land: microbial results (main-text Results placeholder) and metagenome discussion, plus any added figure/table.
