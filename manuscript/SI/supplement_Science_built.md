# Supplementary Materials for

## Hurricane-induced mortality switches mangroves from carbon sink to methane source

Jonathan Gewirtzman et al.

Corresponding author: jonathan.gewirtzman@yale.edu

**This PDF file includes:** Materials and Methods; Supplementary Text S1 to S11; Figs. S1 to S17; Tables S1 to S15; References.

**Other Supplementary Materials for this manuscript include:** Data S1 (chamber flux dataset); Data S2 (external datasets used, with citations).

_[Draft. Section structure follows the analysis workflow (run_all.R); numbers are read from the workflow outputs. Placeholders in brackets. Figures marked † are earlier renders to be restyled to the main-text palette.]_

---

# Materials and Methods

## M1. Study sites and disturbance gradient

We worked in mangrove forests of the southwest Florida coast within and adjacent to Everglades National Park (Fig. 1; table S1). Five core sites span three classes along a hurricane-disturbance gradient. The ghost-forest sites (FLM30, CP40) lost their canopy when Hurricane Irma (September 2017) drove storm surge and prolonged ponding across the region (Lagomasino et al. 2021); five years later they retained standing dead trunks and root structures with no canopy recovery and no significant regeneration. The regenerating site (BL60) was also damaged by Irma but supported active recruitment of seedlings and saplings beneath a low, discontinuous canopy. The intact sites (SRS5, SRS6) are tall riverine forests on the Shark River that sustained comparatively little structural damage from Irma, although the wider landscape has been shaped by earlier storms including Andrew (1992) and Wilma (2005). The dominant species are *Rhizophora mangle*, *Avicennia germinans*, *Laguncularia racemosa* and *Conocarpus erectus*, whose composition varies with geomorphic position, tidal connectivity and disturbance history. The climate is subtropical, with a wet season (May–October) receiving ~70% of the 1,000–1,700 mm annual rainfall. Tidal range, freshwater input from Shark River Slough and residence time vary among sites, producing gradients in salinity and hydroperiod. Single-campaign measurements at Marco Island (MI, ghost forest in the Ten Thousand Islands), Rookery Bay (RB10, intact) and the SE-1 scrub-mangrove ecotone (near the US-EvM tower) provided context across settings. Site coordinates are field GPS positions of the plots (porewater stations, within the laser-scan extent), except SRS5 and SRS6, which use the FCE LTER site coordinates.

## M2. Campaigns and measurement design

Repeat measurements were made in October 2022 (late wet season; inundated soils, high water) and March 2023 (dry season; lower water and reduced freshwater inflow) at the five core sites, each campaign combining chamber fluxes, laser scanning and environmental sampling. A March 2022 pilot campaign at BL60, FLM30, SRS6, MI and RB10 contributed chamber fluxes that enter the component rates and two sensitivity analyses but not the stand budgets. Hurricane Ian made landfall north of the study region on 28 September 2022, and water levels were elevated in the following weeks. Airborne eddy-covariance flights (CARAFE) were part of the wider NASA BlueFlux campaign (Poulter et al. 2023), with deployments in April 2022, October 2022, February 2023, April 2023 and July 2024; the October 2022 deployment coincided with our first campaign, and the February and April 2023 deployments bracket our second. The design nests four scales: chambers isolate flux per unit surface of each component; laser-scanned surface areas convert those rates to stand budgets; the US-Skr eddy-covariance tower at SRS6 constrains intact-forest CO2 exchange; and airborne fluxes constrain intact and ghost forest at landscape scale.

## M3. Chamber flux measurements

CH4 and CO2 were measured with closed dynamic chambers connected in a closed loop to portable analyzers: three ABB/LGR GLA131 microportable greenhouse-gas analyzers (off-axis integrated-cavity output spectroscopy) and one Picarro G4301 (cavity ring-down). The LGR analyzers logged dry mole fractions at 1 Hz (0.1 Hz for one analyzer in March 2022); the Picarro updates CH4 and CO2 on alternate logged rows (~3.5–4.6 s per row), and only rows on which a gas had updated were used for that gas. Chambers were matched to each surface (fig. S1). Four elliptical stem-chamber sizes (enclosed areas 40–462 cm2) were sealed to bark with modelling clay at 0, 50 and 100 cm above the sediment, and at 150 cm where stem diameter allowed; two wrap-around chambers enclosed small stems, with area computed as the cylinder surface from the stem diameter. Prop-root chambers enclosed sections of individual *R. mangle* aerial roots. A transparent leaf chamber enclosed a cluster of 15 leaves (one-sided area 134–428 cm2 by species and stand); it was not temperature-controlled and was used to establish the direction and approximate magnitude of leaf exchange. Soil fluxes used open-bottom acrylic cylinders (23.5 cm diameter) inserted into the sediment and capped with a gasketed lid in the wet season, and PVC collars (14.3 or 19.4 cm) inserted ≥1 h before sampling and capped with a domed chamber in the dry season; both sealed with minimal downward pressure to avoid degassing the sediment. Soil chambers included pneumatophores, whose number per collar was recorded. Water-surface fluxes used floating chambers (324 cm2 footprint) on a closed-cell foam float. All chambers used inline desiccant and a ventilation port sealed before measurement and were leak-tested with the CO2 signal before each closure. Total system volume (chamber, collar, tubing, desiccant and analyzer cell) was computed for each chamber–analyzer combination: 0.8–3.1 L for stems, 0.8–2.0 L for roots, 1.5–39.6 L for soil and 4.3 L for floating chambers. Measurements covered all four species, including standing dead trunks at the ghost sites (≥5 stems per species per site; 10 soil positions per plot). Air, stem, soil and water temperature, water depth at the chamber, stem diameter and perimeter at each height, species and live/dead status were recorded for each closure.

## M4. Analyzer–field time alignment and fit windows

Each closure was located in the analyzer record from its field start and end times plus an analyzer clock offset estimated per analyzer and day. The offset was the median offset of the fit windows identified on traces that day where at least three were available, otherwise the median for that analyzer and campaign. An independent rise-detection algorithm (a sustained rise of ≥6 ppm CO2, or ≥20 ppb CH4, lasting ≥60 s) was run on every closure as a check; days on which it disagreed with the adopted offset by >60 s were inspected. The fit window for each closure was identified on the concentration trace (start after chamber closure, end before chamber removal); where no window had been identified, the window ran from the aligned field start plus a short dead band (median 0–0.75 s) to the aligned field end (or start plus the median window length for that analyzer and campaign). Windows that spanned a chamber lift or placement step were clipped 10 s inside the step identified from the CO2 trace, and no two closures on one analyzer could share more than 30 s of record. Median window lengths were 180–236 s by window source.

## M5. Flux calculation and detection limits

Fluxes were computed with goFlux (Rheault et al. 2024), fitting a linear model and the nonlinear Hutchinson–Mosier model (Hutchinson & Mosier 1981; Pedersen et al. 2010) to each closure and selecting the reported flux with the goFlux best-flux criteria (mean absolute error, root-mean-square error, AICc, standard error, g-factor ≤2, κ, minimum detectable flux, number of observations and p < 0.05). The nonlinear model was considered only with ≥30 observations in the window; otherwise the linear flux was used. Fluxes (nmol CH4 or µmol CO2 per m2 of enclosed surface per s) were computed from the initial slope with the ideal-gas law using the total system volume, enclosed area, and air temperature and barometric pressure from the US-Skr tower interpolated to the closure start (101.325 kPa in March 2022, when the tower recorded no pressure). Fluxes were not corrected for water vapour because the analyzers report dry mole fractions. The minimum detectable flux (MDF) of each closure was 1.96 σ / t × the flux conversion term, with t the closure duration and σ the analyzer precision estimated empirically for each analyzer and campaign as the median absolute deviation of within-window first differences (each closure centred on its own median, then pooled, ÷√2). Fluxes below the MDF were retained at their measured values (170 of 762 CH4 fluxes; 8 of 761 CO2 fluxes), so that component means are unbiased. Five screens were recorded for every closure as flags but not used to exclude data: initial concentration >1.5 times the analyzer–campaign median; a CH4 closure without a detectable CO2 rise (not evaluated for water, leaves and downed wood); a significantly accelerating (convex) trace; a window <60 s; and closure noise >1.5 times the group precision.

## M6. Selection criteria and exclusions

All closures entered the analysis set except those meeting one of the criteria in table S2: no usable analyzer record within the closure (no data in the window, no record for that analyzer and day, or no trace distinct from an adjacent closure); a pilot chamber design without a validated enclosed area; no chamber geometry; an analyzer artefact (spurious concentration steps, such as those coinciding with water-vapour channel switching at high CH4); a chamber placement or seal artefact (headspace starting above ambient after placement, giving spurious uptake); no closure time; or a second closure within one floating-chamber placement (one flux per placement; M7). Records duplicating another measurement were removed and are not counted as independent closures. Of 839 records, 77 were excluded, leaving 762 analysed fluxes (table S3). No flux was removed for being small, negative or below detection. Summary statistics of component rates (table S4; Fig. 2) use the eight named plots and omit two stem closures with CO2 below −10 µmol m−2 s−1 (n = 751).

## M7. Floating chambers and ebullition

Water-surface fluxes were computed once per floating-chamber placement, from placement to lift, however many closures were logged within it. Placements were identified in the CH4 record: a lift as a drop in the 5-point running median of >max(20 ppb, half the excess over background) within 30 s, or a gap in the record >60 s; the start as the last point at background before the fit window. This gave 47 placements (median 6.5 min, range 1.6–79 min). Ebullition was separated with goAquaFlux (a goFlux extension; de-ebulliated diffusive window). Bubbles were detected as steps in the standardised CH4 series where the rolling variance of first differences exceeded the larger of its 70th percentile and median + 4 MAD (requiring a max/median ratio ≥3), merging episodes within 10 s and keeping steps ≥5 ppb; step size was estimated by local regression with an exponential re-equilibration term. The diffusive flux was fitted to the de-ebulliated series over the first 10 min of placements longer than 12 min and over the fit window otherwise; the ebullitive flux was the summed step size over the placement, converted to a flux over the incubation time; the total was their sum. The Picarro's ~5 s CH4 updates cannot resolve bubble steps, so for Picarro placements the total was a two-point flux from the placement start to the end of the diffusive window (15 s means), and ebullition was the excess of total over diffusive (floored at zero). Bubbles occurred in 11 of 47 placements and supplied 14% of water-surface CH4 overall (fig. S4).

## M8. Water-surface flux from dissolved gases

Where no floating-chamber flux existed for a plot and campaign (SRS5 and SRS6, October 2022), water-surface fluxes were estimated as F = k600 (Sc/600)−0.5 (Cw − Ceq), with CH4 solubility from Yamamoto et al. (1976), CO2 solubility from Weiss (1974), Schmidt numbers from Wanninkhof (2014), and atmospheric CH4 1.95 ppm and CO2 417 µatm. Dissolved concentrations came from our surface-water samples at the plot (SRS5) and a BlueFlux aquatic-transect station (SRS6). The gas-transfer velocity k600 was calibrated on the plot × campaign pairs that had both chamber water flux and dissolved CH4 (median of 7 retained pairs, 1.10 cm h−1; range 0.56–7.07, used as the uncertainty range; one pair with k600 >50 cm h−1 was rejected).

## M9. Heights and the water line

Chamber heights are expressed above the sediment surface; where the water surface was the reference, the water depth was added. For *R. mangle* trees whose heights were referenced to the root crown, the crown height above the sediment (site median of the lowest trunk chamber on *R. mangle* trees in March 2023) was added. For analyses of flux with height, heights were then referenced to the water surface where standing water was present (height above sediment minus water depth) and to the sediment surface otherwise, because gas leaving the bark below the water exchanges with the water rather than the air.

## M10. Terrestrial laser scanning and surface area

Plot structure was measured with a RIEGL VZ-400i scanner (1,550 nm; 0.03° angular resolution) from multiple positions per plot _[CHECK: number of positions]_ with Trimble R8/R6 GNSS (Powell et al., in prep.; point clouds in data S2). Scans were registered by iterative closest point and multi-station adjustment in RiSCAN PRO and georeferenced to WGS 84 / UTM 17N. Point clouds were segmented into wood and leaf and woody points into trunk, branch and prop root, and surface area was summarised by segment class in 0.5 m height bins per plot _[PLACEHOLDER — segmentation, quantitative structure models and QC from Powell et al.]_. Stem surface was trunk plus branch; root surface was prop root. Ground area was plot area minus the basal area of trunks and roots. Plot areas were 1,767–2,191 m2 (table S7). The scanner's ground datum is the lowest visible surface; because near-infrared light does not penetrate water, the datum is the water surface where water stood at scan time. Water depths were therefore referenced to the depth at scan time (SRS5 0.7 cm and SRS6 6.8 cm from the FCE LTER logger at the scan hours on 21 and 15/18 October 2022; CP40 10 cm and FLM30 2 cm from our March 2023 depth readings, scans on 10 and 12 March 2023). This correction changes stand CH4 by <2% and net forcing by <1%. The regenerating site was not scanned.

## M11. Tidal inundation and floor microtopography

At the tidal intact sites the floor floods and drains with the tide. Hourly water level relative to the soil surface at SRS5 and SRS6 came from FCE LTER loggers (data S2). Our water-depth readings at chamber positions (n = 129 at SRS5, 69 at SRS6) agreed with the loggers where water was present (median differences +0.5 and +1.1 cm) but varied by up to 27 cm within 30 min because the floor is uneven. We treated each reading as a sample of floor height relative to the logger datum (exact where water was present, left-censored where it was not) and fitted a normal floor-height distribution per site by censored maximum likelihood (mean 4.6 and 1.3 cm above the logger datum; s.d. 11.1 and 9.9 cm). The share of the floor under water each hour is Φ((h + µ)/σ) for logger level h; its mean over each campaign month (table S13) weights a high-tide state (flooded floor: water-surface flux, no soil or downed-wood flux) and a low-tide state (exposed floor). Alternatives (an all-or-nothing switch, the 2010–2023 hydrology, an equal split, and the floor mean ±1.96 SE) are compared in table S10. The ghost sites are not tidal: they were fully flooded in October 2022 (all depth readings 2–31 cm), and in March 2023 the share of our readings at stem, root and downed-wood positions without standing water (6 of 31 at CP40; 9 of 48 at FLM30) was budgeted as exposed soil.

## M12. Stand CH4 budgets

Stand budgets were built for the scanned sites (SRS5, SRS6, CP40, FLM30) in October 2022 and March 2023 as the sum over components of flux per unit surface × surface per unit ground. Component rates were bootstrap means (5,000 resamples) per site, campaign and component (table S7 lists rates and their sources). Where a component was not measured at a site and campaign, the rate was taken from the nearest equivalent: water from dissolved gas (M8); CP40 prop roots from FLM30 in the same campaign; FLM30 prop roots in March 2023 from FLM30 in October 2022; FLM30 downed wood from CP40 in the same campaign; and exposed ghost-forest soil from FLM30 soil chambers in March 2022, when that site had no standing water (CH4 30.8 nmol m−2 s−1, CO2 2.6 µmol m−2 s−1). Stems were pooled across species. Stem CH4 was modelled per site × campaign as log(CH4) = a + b·h on positive stem fluxes, with h the height above the water surface (M9); slopes were −0.4 to −1.4 m−1 at the intact sites and −3.9 to −4.5 m−1 at the ghost sites. For each 0.5 m bin [z0, z1] of laser-scanned stem surface, only the part above the water [max(z0, w), z1] emits; the fitted profile exp(a + b(z − w)) was integrated exactly over that part and averaged over the water depths w of the campaign (every hour of the campaign month at the tidal sites; our recorded depths at the ghost sites). Prop roots were scaled by their above-water surface in the same way, without a height profile. Downed wood was not resolved by scanning; its surface area was 4V/d with volume V = 67 (13–181) m3 ha−1 from South Florida line-intersect surveys (Krauss et al. 2005) and piece diameter d = 10 cm, as at SRS6 by Troxler et al. (2015), giving 0.27 m2 per m2 of ground; it emits only above the water (all of it at low tide at the tidal sites; at the ghost sites, the arc of a log of median measured diameter, 11.4 cm, above the mean water depth). Site × campaign totals were converted to mg CH4 m−2 d−1, weighted by tide state at the tidal sites, and averaged over sites and campaigns within each class to give annual totals (g CH4 m−2 yr−1).

## M13. Stand CO2 budgets

Ecosystem respiration was built from the same components and areas, with chamber CO2 efflux of stems (no height profile), prop roots, soil, water and downed wood, plus canopy leaf respiration. Chamber respiration of stems, roots, soil and downed wood was scaled from measurement-time to 24-h temperature with the within-month Q10 of night-time respiration at the US-Skr tower (1.15; M14), giving factors of 0.96–1.02. Canopy leaf respiration in intact forest was Rd25 × mean[f(T) × (1 − 0.3 × day)] × LAIeff, with leaf dark respiration at 25 °C Rd25 = 1.55 (1.28–1.62) µmol m−2 leaf s−1 from *R. mangle* at SRS6 (Barr et al. 2009) and *A. germinans* (Sturchio et al. 2022), the temperature response of Heskel et al. (2016), f(T) = exp[0.1012(T − 25) − 0.0005(T2 − 252)], driven by tower air temperature, 30% daytime light inhibition (Atkin et al. 2014; daytime when shortwave >5 W m−2), and an effective leaf area LAIeff = (1 − e−0.5L)/0.5 with L = 2.8 (2.3–5.55) (Troxler et al. 2015; Reed et al. 2025), giving 1.7–2.0 µmol m−2 s−1. Leaf respiration was zero in ghost forest. Net ecosystem exchange was respiration minus gross primary production (GPP), with GPP the tower campaign mean in intact forest (M14) and zero in ghost forest (defoliated). Because the imported GPP is partitioned from the tower's own net exchange, intact bottom-up net exchange equals tower net exchange plus the difference between bottom-up and tower-partitioned respiration; it reconciles the two respiration estimates rather than providing an independent net flux (text S5). Airborne fluxes provide the independent check (M15).

## M14. Eddy-covariance tower

Half-hourly CO2 and CH4 fluxes at SRS6 came from the AmeriFlux US-Skr BASE product (data S2) _[CHECK with tower PIs: instruments, measurement height, processing and u* threshold]_. Net exchange was the PI-provided NEE, or FC + SC where NEE was missing. GPP was partitioned per season-year (March–May 2022, September–November 2022, March–May 2023): night-time respiration (shortwave ≤10 W m−2) was fitted as log(NEE) = a + b·T and extrapolated to daytime; daytime GPP = respiration − NEE was fitted with a rectangular-hyperbola light response; NEE was gap-filled with the models; and GPP = respiration − gap-filled NEE in daytime (unclipped) and zero at night. Uncertainty came from 200 bootstrap resamples. Campaign-month means were GPP 7.1 (October 2022) and 8.5 (March 2023) µmol m−2 s−1, and NEE −2.4 and −4.4 (table S8; fig. S15). The within-month Q10 of night-time respiration was estimated from 2004–2023 records (n = 46,641 half-hours; shortwave <10 W m−2, 0 < NEE < 30, u* >0.2 m s−1) as exp(10b) from log(NEE) ~ T with a year × month fixed effect: 1.15 (1.13–1.18).

## M15. Airborne eddy covariance

The CARAFE payload (Wolfe et al. 2018) flew on a Beechcraft King Air A90 at ~90 m above sea level with a Picarro G2311-f (10 Hz CO2, CH4 and H2O), a Picarro G2401m calibrated to NOAA/WMO standards, and an Aventech AIMMS-20 probe (20 Hz winds, temperature, pressure, position and attitude) (Delaria et al. 2024). Fluxes were computed by continuous wavelet transform along straight, level legs (>15 km; roll <5°; altitude within ±10 m); median detection limits at 1 km were 5.8 nmol m−2 s−1 for CH4 and 0.9 µmol m−2 s−1 for CO2. Two-dimensional footprints followed Kljun et al. (2015) with HRRR boundary-layer heights, and fluxes were disaggregated to land-cover classes by multilinear regression on footprint composition (Hutjes et al. 2010; Hannun et al. 2020), with ghost forest delineated following Lagomasino et al. (2021). Because regenerating forest was not separable in the footprints, the comparison used two end-members (intact and ghost), and the March 2023 value is the mean of the February and April 2023 deployments (variances combined; table S6). Midday CO2 was converted to daily values for comparison with the daily chamber budgets. _[PLACEHOLDER — July 2024 deployment, same disaggregation; uncertainty definition (E. Delaria).]_

## M16. Porewater geochemistry

Porewater was sampled in three rounds. In October 2022 and March 2023, porewater (mostly ~40 cm; also 0, 15 and 100 cm at some sites) and surface water were collected at the flux sites for dissolved CH4 (headspace equilibration and gas chromatography) and salinity _[CHECK: sampler and GC protocol]_. In October 2025, depth profiles (surface water and 0, 15, 45 and 90 cm) were collected at SRS5, SRS6, BL60 and CP40 with MHE PushPoint samplers. Temperature, pH, conductivity, dissolved oxygen and redox potential were measured in the field (Hanna HI98494; dissolved oxygen corrected by −1.67 mg L−1 for a sensor offset); total dissolved sulfide (methylene blue) and total dissolved iron (FerroVer) colorimetrically in the field (Hach DR900). Dissolved CH4 and CO2 and their δ13C were measured by headspace equilibration on a Picarro G2201-i with SAM autosampler at Yale; dissolved organic carbon (Shimadzu TOC), major anions (Metrohm ion chromatograph) and total alkalinity (titration) at the Yale Analytical and Stable Isotope Center; and dissolved inorganic nitrogen at Yale _[CHECK: method, detection limit and dilution]_, with values at or below zero treated as below detection and set to zero. Intact-site ammonium was compared with FCE LTER porewater monitoring at SRS5 and SRS6 (data S2; table S11). The principal-components analysis used October 2025 profiles (0–90 cm): numeric variables with ≤20% missing values, excluding coordinates, depth, conductivity and total dissolved solids (redundant with salinity), temperature, percent oxygen saturation, bromide and fluoride, dissolved CO2 and standard deviations; complete cases (16 of 20); centred and scaled. The salinity–CH4 relationship used site × round × depth means from all three rounds (Pearson correlation of salinity with log(1 + CH4) within each class).

## M17. Sediment metagenomes

_[PLACEHOLDER — Peccia laboratory: sediment sampling (sites, depths, dates, preservation); DNA extraction (kit, input by mass); quantification; library preparation and shotgun sequencing (platform, depth); taxonomic profiling and functional annotation (tools, databases); marker genes (mcrA; methylamine methyltransferases mttB, mtbB, mtmB; pmoA, mmoX); normalisation by sediment mass, bulk density and organic carbon.]_

## M18. Radiative forcing and carbon-balance framings

CH4 was converted to CO2 equivalents with AR6 global warming potentials (Forster et al. 2021): GWP20 = 81.2 (headline; Wood et al. 2023) and GWP100 = 27.9. GWP* (Allen et al. 2018; Smith et al. 2021) converts a change in emission rate into warming-equivalent CO2, E*(t) = GWP100 × [4.53 E(t) − 4.25 E(t − 20)]; we applied it only to the intact-to-ghost change, with the intact rate as the pre-conversion baseline, valid for the first 20 years after conversion. For a steady source GWP* gives 0.28 × GWP100 × E, which scores the intact baseline as nearly warming-neutral; we do not use it to characterise intact forest. Net forcing combined CH4 with net CO2 exchange; the switch is the ghost-minus-intact difference. For intact forest we also computed forcing on the net ecosystem carbon balance (text S10), adding literature lateral export in three coherent scenarios, each from one method: low (Lagrangian tracer releases; dissolved ~90 g C m−2 yr−1; Ho et al. 2017), central (Shark River synthesis; DIC 145, DOC 56; Zhao et al. 2021) and high (Eulerian; DIC 622, DOC 171; Reithmaier et al. 2020), each with litter POC 145 and dissolved CH4 0.35 g C m−2 yr−1. Exported alkalinity (TA/DIC 0.68–0.76) was treated either as a durable ocean store (the atmosphere-relevant case) or as returned to the atmosphere. Ghost forest has no lateral-export measurements and is reported on vertical exchange only (text S11).

## M19. Regional scaling

The regional estimate multiplies the per-area switch by the area of 2017 hurricane dieback from the Landsat analysis of Taillie et al. (2020): ΔNDVI < −0.2 within the Global Mangrove Watch v1 baseline, with persistent damage defined as no NDVI recovery over the seven months after the season. The polygons of 2017 damage with little recovery, attributed to country, were provided by D. Lagomasino _[CHECK: relation of this layer (173 km2) to the paper's 790 km2 of persistent damage]_. Polygons were split into patches and their area computed in an Albers equal-area projection (standard parallels 10° and 30° N); patches without a country took the nearest territory, and territories were attributed to Hurricane Irma or Maria by track. Induced CH4 was area × the ghost-minus-intact CH4 difference (10.4 g CH4 m−2 yr−1), with a range from the class Monte Carlo intervals. Because the layer captures 60 of the 108 km2 of post-Irma dieback mapped in Florida (Lagomasino et al. 2021), a Florida-adjusted bound is also reported (table S14). For the map (Fig. 5C), patches were aggregated to 0.25° cells. Mangrove extent in Fig. 1A is Global Mangrove Watch v3.0 for 2016 (Bunting et al. 2022), aggregated to ~100 m for display.

## M20. Statistics and uncertainty

Fluxes were transformed with the inverse hyperbolic sine (asinh), which handles negative values, approximates the logarithm for large values and is linear near zero. Component means and 95% intervals were percentile bootstrap estimates (5,000 resamples), reported for groups of four or more measurements. Woody (stem and prop-root) CH4 by height (Fig. 2D) was modelled with a Gaussian location–scale generalised additive model (mgcv, gaulss, REML) of asinh(CH4) on 425 closures from 148 trees reconstructed from the order of measurements: location = class-specific smooth of log(h + 5) (k = 4) + surface (stem or prop root) + status (live or dead) + season + flooded + random effects of tree and site × campaign; scale = class. This height form was selected by AIC over linear, logarithmic and untransformed-smooth alternatives (table S5). Profiles are shown for live, flooded stems in the wet season, excluding random effects, back-transformed with sinh. Adding species changed little (table S5). The same structure was fitted to woody CO2. This model is descriptive; stand budgets use the per site × campaign height fits (M12). Linear mixed-effects models of stem flux on species, height category, season and live/dead status with site as a random intercept (lme4/lmerTest; Type III tests; Tukey contrasts of estimated marginal means) are reported in fig. S6.

Budget uncertainty was propagated by Monte Carlo (5,000 draws). For CH4, component rates were drawn from normal distributions with standard errors from their bootstrap intervals; laser-scanned areas from lognormal distributions (coefficients of variation: ground 3%, root 20%, stem 12%); downed-wood area from a lognormal spanning the Krauss range; the stem height-fit parameters from their joint sampling distribution (slope capped at zero, intercept at the largest observed stem flux); and intact water-surface CH4 multiplied by a log-triangular tidal-phase factor (0.6–2.0, mode 1). For CO2, component respiration was drawn as normal with relative standard error from the chamber data (capped at 1; 0.5 for n ≤2), with Rd25 ~ U(1.28, 1.62), LAI lognormal (median 2.8, 97.5th percentile 5.55), light inhibition triangular (0.2, 0.3, 0.5), downed-wood area as above and GPP normal with its bootstrap standard deviation. Class intervals are the means of the campaign quantiles. Analyses used R (≥4.3); the complete workflow, from raw analyzer files to figures, is run by run_all.R in the code repository (data S1).

## M21. Literature-derived values and selection criteria

Where a budget term could not be measured, we used a published value chosen by three criteria, in order: measured at our sites or in the same forest type in the Florida Coastal Everglades; measured with a method comparable to ours; and, among several, a central or synthesis value with the others as a range. Each scenario for lateral export was taken from one method rather than combining term-wise extremes. Table S9 lists every literature-derived value, its source, how it was converted and used, and the alternatives considered; table S15 lists the leaf-respiration values. Measured terms (chamber fluxes, scanned areas, tower and airborne fluxes, dissolved gases) are not listed.

## M22. Time-of-day and tidal-phase adjustments

Chambers were run in daytime (~09:00–16:30) and water surfaces near slack or low water. Respiration CO2 was scaled to 24 h (M13); no diel correction was applied to CH4 in the central budget. Because daytime-only sampling underestimates annual mangrove CH4 by ~23% in multi-year eddy covariance (Zhu et al. 2024), a ×1.30 factor is reported as a sensitivity (table S10). Dissolved CH4 in mangrove creeks peaks near low water (2–7 times high-tide values; Bouillon et al. 2007; Reithmaier et al. 2020), so our slack/low-water samples lie near the tidal maximum, whereas flood- and ebb-onset pulses (Lin et al. 2024; Yong et al. 2024) are unquantified; we therefore applied no tidal-phase correction but carried a 0.6–2.0 range on intact water-surface CH4 in the Monte Carlo. No equivalent factor was applied to CO2, which largely leaves the forest as lateral dissolved inorganic carbon (M18).

---

# Supplementary Text

## Text S1. Stem height extrapolation

Stem CH4 declines with height, so stand budgets must extrapolate above the highest chamber (100 or 150 cm). The central budget extrapolates the per site × campaign exponential fit, which stays positive and approaches zero. In a sensitivity analysis on ground-referenced height bins, six forms were compared: zero above 1.5 m; exponential; linear (clamped at zero); linear clamped to the observed range; exponential with a free asymptote; and constant above 1.5 m (fig. S7). Because stems are <1% of both stand budgets, class totals changed little between the exponential and zero-above forms (ghost 32.6 vs 32.6; intact 2.30 vs 2.28 mg CH4 m−2 d−1 at high tide). The constant-above form, which assumes the flux at 1.5 m continues to the top of the canopy, is the only one that changes intact totals appreciably and is not supported by the decline we measured.

## Text S2. Tide and inundation

At the tidal intact sites, high- and low-tide states were weighted by the flooded share of the floor in each campaign month (M11): 0.62 and 0.74 in October 2022 and 0.37 and 0.55 in March 2023 at SRS5 and SRS6 (table S13). For the March 2023 ghost floor without standing water, four sources for the exposed-soil flux were tested, each with its limitation: FLM30 soil chambers in March 2022 (same site and season, different year; central); Marco Island ghost soils (ghost forest, different setting); the two pooled; and exposed hurricane-killed soils at BL60 in March 2023 (same campaign, regenerating class), with two exposed shares (no standing water, central; ≤2 cm). Every option raises the ghost source relative to treating the floor as fully flooded, mainly through soil CO2: ghost net forcing is +2,401 g CO2-eq m−2 yr−1 at GWP20 when fully flooded, +2,775 in the central case, and +2,460 to +4,362 across the options; ghost CH4 ranges 9.9–13.7 g m−2 yr−1 (table S10). An airborne check by campaign (table S6) favours the flooded representation in October 2022 and March 2023.

## Text S3. Downed wood

Laser scanning represents standing live and dead trees as trunk and branch segments, so standing dead trees enter through the stem term (our stem chambers include dead stems), but downed wood is not modelled. We used the South Florida mangrove survey of Krauss et al. (2005), 9–10 years after Hurricane Andrew: 67 m3 ha−1 on average (13–181 across sites; 132 in the eyewall region). Converted with a 10 cm piece diameter (as at SRS6 by Troxler et al. 2015), this gives 0.27 (0.05–0.72) m2 of wood per m2 of ground; because about half the volume was fine debris, the conversion probably underestimates surface. Woody litterfall at SRS4–6 (FCE LTER; data S2) was 68–95 g dry m−2 yr−1 in 2001–04, 149–389 in 2005 (Wilma), 264–276 in 2017 (Irma), 34–46 in 2018–21 and 47–56 in 2022–23. Our campaigns came ~5 years after Irma, against 9–10 years after Andrew for the survey, so the present pool is plausibly at least comparable to the survey mean. Downed wood exchanges with the air only above the water, so most ghost-forest downed wood (water 7–23 cm deep) was submerged. Downed wood is ~2% of intact and <1% of ghost CH4. Across the full volume range, intact net exchange spans about −1,150 to −770 g C m−2 yr−1 and the ghost class remains a source (table S10).

## Text S4. Monte Carlo uncertainty

For the CH4 budget, intact-class uncertainty is dominated by the exposed-soil fraction and the scanned root area, and ghost-class uncertainty by water-surface flux variability. For net forcing, canopy leaf respiration (Rd25 and LAI) and tower GPP dominate the intact interval and the CH4 budget dominates the ghost interval (fig. S10). The intervals of the two states do not overlap: intact −5,233 to −1,939 and ghost +1,922 to +3,978 g CO2-eq m−2 yr−1 at GWP20 (−5,294 to −2,035 and +1,491 to +3,263 at GWP100). Structural choices outside the Monte Carlo are compared one at a time in text S9.

## Text S5. CO2 closure caveats

Bottom-up respiration may legitimately exceed tower-partitioned respiration, and chamber–airborne CO2 comparisons need care, for two reasons: airborne midday fluxes were converted to daily values while chamber respiration was measured in daytime and scaled to 24 h with the tower's within-month Q10; and tower-partitioned respiration extrapolates night-time respiration across daylight without light inhibition, whereas the bottom-up leaf term includes it. Lateral export of dissolved inorganic carbon removes respired carbon that neither the tower nor the chambers see, because both measure only exchange with the air. Closure was therefore judged as consistency within uncertainty rather than forced. Within-month Q10 (1.15) was preferred to the across-season tower value (1.8), which also carries phenology, water level and salinity, and to our stem-chamber value (4.3; 2.5–7.4), which is likely inflated because daytime stem efflux also follows sap flow.

## Text S6. Regenerating stand

Component rates were measured at BL60, but no scanned surface area or airborne end-member exists for regenerating forest, so it is reported at the component scale and excluded from the closed budgets. BL60 had the highest component CH4 rates of any class (soil 56, 95% CI 27–91; water 57, 33–89; stem 14, 3–34; root 5.7, 2.7–8.1 nmol m−2 s−1). Assigning BL60 stem and root areas midway between the ghost and intact classes and its measured inundated share (0.33 of depth readings with standing water) gives ~33 g CH4 m−2 yr−1 (32.7 exposed to 33.3 flooded), above both ghost (~12) and intact (~1.5) forest. This first-order estimate assumes intermediate structure and rests on pooled campaigns; it is not part of the closed budgets or forcing, but indicates that early regeneration need not be a low-emission state.

## Text S7. Porewater alkalinity and sulfate

Porewater alkalinity (October 2025, 0–90 cm) was 11–33 mM (site medians), five to fifteen times seawater: 11–12 mM at the intact sites, 16 mM at CP40 and 33 mM at BL60. Alkalinity at this level indicates that sulfate reduction was active at every site. At CP40, sulfate remained near 32 mM, the highest of any site, alongside the highest dissolved CH4: methane accumulated alongside sulfate reduction rather than after sulfate was exhausted, consistent with methanogenesis from substrates sulfate reducers do not compete for. Alkalinity–DIC slopes are not interpreted, because DIC was calculated from probe pH and alkalinity rather than measured, and each site has four depths (fig. S12).

## Text S8. Context sites

Single dry-season visits place the core results in a wider context (table S12). The Marco Island ghost site emitted far less than the core ghost sites (soil 2.6, 95% CI 2.1–3.1; stem 0.5, 0.2–0.9 nmol m−2 s−1, against core ghost soil up to ~31 and stem 11–14), so ghost-forest methane is setting-dependent. Rookery Bay soil (1.0, 0.5–1.7) fell within the core intact range (0.6–12). The SE-1 scrub ecotone showed moderate prop-root (3.2) and low stem (0.09) and water (1.6; one placement) CH4, with net leaf CO2 uptake (−2.27 µmol m−2 s−1). Leaf chambers at SE-1 and BL60 (CO2 −2.27 and −1.25; CH4 0.01–0.03) corroborate the sign and magnitude of the leaf CO2 term and show that leaves are not a meaningful CH4 pathway.

## Text S9. Sensitivity to analytical choices

The central case combines the choices best supported by our data and the literature: flooding weighted by floor area over the campaign months, the within-month tower Q10, the Krauss downed-wood volume, LAI 2.8, and no tidal-phase or diel correction to CH4. Monte Carlo intervals propagate measurement and parameter uncertainty within that case; structural alternatives are compared one at a time in table S10. Three results hold under every choice: at GWP20 the ghost class is a net source (+2,400 to +4,360 g CO2-eq m−2 yr−1; +1,830 to +3,630 at GWP100), the intact class is a net sink on vertical exchange (−2,690 to −4,310), and intact CH4 emission is clearly non-zero (0.9–2.1 g CH4 m−2 yr−1). The switch spans ~5,600–8,000 g CO2-eq m−2 yr−1 at GWP20 (~5,100–7,300 at GWP100), and ~5,100–5,500 under the plausible carbon-balance framings. The largest levers on the intact budget are canopy leaf respiration (LAI), the downed-wood volume and the flooding representation, which also decides the leading intact CH4 pathway: soils (55%) and roots (28%) in the central case; roots (44%) under the all-or-nothing switch.

## Text S10. Net forcing under carbon-balance framings

On vertical exchange the intact class is a sink of −3,630 g CO2-eq m−2 yr−1 at GWP20 (−3,710 at GWP100), and its CH4 offsets 3% (GWP20) to 1% (GWP100) of the CO2 sink. Under the central lateral scenario (346 g C m−2 yr−1) the intact forest retains ~680 g C m−2 yr−1. With exported alkalinity retained in the ocean and the rest returned to the air, the intact sink is −2,710 g CO2-eq m−2 yr−1 at GWP20 (−2,960 to −1,710 across lateral scenarios), CH4 offsets 5% of it, and the switch is ~5,500 (~5,000 at GWP100). If all exported carbon returns to the air, the intact sink is −2,330 (−2,730 to −150) and the switch ~5,100. On storage alone (burial 123 plus wood increment 131 g C m−2 yr−1; Castañeda-Moya et al. 2013) the intact class is −810 and the switch ~3,600, a lower bound because carbon respired downstream or retained as ocean DOC counts as returned. As an independent check, lateral export should roughly equal net exchange minus wood increment minus burial (≈770 g C m−2 yr−1), which falls between the central and high scenarios (fig. S14). The intact sink stays negative in every coherent scenario and approaches neutrality only with the highest export and complete atmospheric return.

## Text S11. Ghost-forest carbon losses not measured

Our ghost-forest forcing is on-site vertical exchange five to six years after Irma and omits two terms that would add to the source. First, ghost-forest lateral export is unmeasured; after dieback elsewhere, losses shift from lateral outwelling toward atmospheric CO2 (soil CO2 +189%, DIC outwelling −50%; Sippo et al. 2020), and Shark River DIC and DOC fluxes fell by 45% and 27% for up to two years after Irma (Stegehuis et al. 2026), so ghost export is probably smaller than intact but not zero. Second, root death can trigger peat collapse: Honduran mangroves lost ~11 mm yr−1 of elevation after Hurricane Mitch (Cahoon et al. 2003), an Everglades site converted to mudflat by the 1935 hurricane lost ~75 cm (Osland et al. 2020), and topsoil carbon fell by ~2.7 kg C m−2 at a Naples Bay site affected by Irma (Griffiths & Mitsch 2021). Such a pulse would enlarge the switch in its first years; its magnitude at our sites is unknown, so it is discussed but not included.

---

# Supplementary Figures

![](<output/figures/other/pub_SI_chamber_photos.png>){width=6.5in}

**Fig. S1.** Chamber designs: stem chambers at several heights, prop-root chamber, soil collar and cylinder, floating water chamber, leaf chamber and downed-wood chamber. †


![](<output/figures/other/sampling_design.png>){width=6.5in}

![](<output/figures/other/water_positions_by_campaign.png>){width=6.5in}

**Fig. S2.** Sampling design relative to tide and standing water. (Top) Chamber measurements by site and campaign against water level. (Bottom) Floating-chamber positions and water depth by campaign. †


![](<output/figures/other/pub_component_by_plot_campaign_combined_condensed_boot.png>){width=6.5in}

**Fig. S3.** CH4 and CO2 flux by component for every plot and campaign, including context sites (bootstrap means and 95% CIs; individual measurements as points; inverse-hyperbolic-sine axes). †


![](<output/figures/other/pub_SI_ebullition_partition.png>){width=6.5in}

**Fig. S4.** Ebullition. (A) Diffusive and ebullitive water-surface CH4 by site and season. (B) Example placements showing detected bubble steps, the de-ebulliated series and the diffusive fit (M7). †


![](<output/figures/other/pub_SI_pneumatophore_density.png>){width=6.5in}

**Fig. S5.** Soil CH4 and CO2 flux against pneumatophore density per collar, by site (dry season). †


![](<output/figures/other/pub_stem_height_composite_combined.png>){width=6.5in}

**Fig. S6.** Stem detail. Stem CH4 and CO2 by height category, species and live/dead status at sites with species identification; estimated marginal means (95% CI) from linear mixed-effects models (M20). †


![](<output/figures/other/stem_extrap_clean.png>){width=6.5in}

![](<output/figures/other/height_extrap_sensitivity_total.png>){width=6.5in}

**Fig. S7.** Stem height extrapolation (text S1). (Top) Fitted exponential stem CH4 profiles by site and campaign against the measured heights. (Bottom) Stand CH4 under six extrapolation forms. †


![](<output/figures/other/SA_by_segment_height_fixedY.png>){width=6.5in}

**Fig. S8.** Laser-scanned surface area per unit ground area by segment class (trunk, branch, prop root) and 0.5 m height bin for the four scanned plots. †


![](<output/figures/other/scenario_comparison.png>){width=6.5in}

**Fig. S9.** Stand CH4 under tide and inundation representations (text S2; table S10). †


![](<output/figures/other/pub_uncertainty_decomp.png>){width=6.5in}

**Fig. S10.** Monte Carlo uncertainty: contribution of each input to the variance of stand CH4 and net forcing (text S4). †


![](<output/figures/other/ed_porewater_rounds.png>){width=6.5in}

**Fig. S11.** Porewater dissolved CH4 (A) and salinity (B) by site, sampling round (October 2022, March 2023, October 2025) and depth, including FLM30.


![](<output/figures/other/pub_SI_ta_vs_dic.png>){width=6.5in}

![](<output/figures/other/pub_SI_ta_vs_salinity.png>){width=6.5in}

**Fig. S12.** Porewater carbonate chemistry (October 2025): total alkalinity against calculated DIC and against salinity, with conservative-mixing references (text S7). †


![](<output/figures/other/pub_SI_salinity_vs_ch4_bysite.png>){width=6.5in}

**Fig. S13.** Porewater salinity against dissolved CH4 by site, all sampling rounds. †


![](<output/figures/presentation/budget_flow_healthy.png>){width=6.5in}

**Fig. S14.** Carbon flows in intact forest (g C m−2 yr−1): measured vertical exchange (GPP, respiration, CH4), literature lateral export and storage (burial, wood increment), and the closure residual (M18; text S10). †


![](<output/gpp/plots/US-Skr_GPP_mean_diurnal_cycle.png>){width=6.5in}

**Fig. S15.** US-Skr tower: mean diurnal cycle of partitioned GPP during the campaign months (M14). †


![](<output/figures/other/site_closure_comparison.png>){width=6.5in}

**Fig. S16.** Per-site closure: bottom-up stand CH4 and CO2 by site and campaign beside the airborne class values. †




**Fig. S17.** _[PLACEHOLDER — sediment metagenome detail: taxonomy by site and depth, marker-gene abundance per gram sediment and per gram organic carbon, DNA yield (Peccia laboratory).]_


---

# Supplementary Tables

**Table S1. Study sites.**

| Site | Name | Class | Latitude | Longitude | Dominant species |
|---|---|---|---|---|---|
| FLM30 | Flamingo | ghost (core) | 25.15989 | -80.91197 | Avicennia germinans, Conocarpus erectus |
| CP40 | Christian Point | ghost (core) | 25.14932 | -80.91102 | Avicennia germinans |
| BL60 | Bear Lake | regenerating (core) | 25.15783 | -80.92320 | Rhizophora mangle, Avicennia germinans |
| SE1 | SE-1 / US-EvM | scrub (context) | 25.35310 | -80.38071 | Rhizophora mangle, Avicennia germinans |
| MI | Marco Island | ghost (context) | 25.93033 | -81.67251 | Avicennia germinans |
| RB10 | Rookery Bay | intact (context) | 25.86289 | -81.56119 | Rhizophora mangle |
| SRS5 | Gunboat Island / SRS5 | intact (core) | 25.37702 | -81.03235 | Rhizophora mangle |
| SRS6 | Lower Shark / SRS6 | intact (core) | 25.36463 | -81.07795 | Rhizophora mangle |

**Table S2. Measurement exclusions by criterion** (M6). Duplicate records are not independent closures; no flux was removed for being small, negative or below detection.

| Criterion | n | By component |
|---|---|---|
| No usable analyzer record for the closure | 26 | cwd 2, root 6, stem 15, water 3 |
| Duplicate record of another measurement (not an independent closure) | 16 | stem 6, water 10 |
| Pilot chamber design without validated enclosed area | 15 | cwd 2, pneumatophore 6, root 1, stem 6 |
| Analyzer artefact (spurious concentration behaviour from the instrument) | 8 | stem 7, water 1 |
| No chamber geometry | 7 | root 4, stem 3 |
| Chamber placement or seal artefact | 2 | cwd 1, stem 1 |
| No closure time | 2 | soil 1, water 1 |
| Second closure within one floating-chamber placement | 1 | water 1 |
| Total excluded | 77 |  |
| Analysed | 762 |  |

**Table S3. Analysed fluxes by site, campaign and component.**

| Site | Campaign | stem | root | soil | water | cwd | leaves | Total |
|---|---|---|---|---|---|---|---|---|
| BL60 | Mar 2022 | 17 | 0 | 10 | 0 | 0 | 0 | 27 |
| BL60 | Mar 2023 | 37 | 5 | 12 | 0 | 0 | 14 | 68 |
| BL60 | Oct 2022 | 36 | 0 | 1 | 5 | 2 | 0 | 44 |
| CP40 | Mar 2023 | 32 | 0 | 0 | 11 | 5 | 0 | 48 |
| CP40 | Oct 2022 | 46 | 0 | 0 | 6 | 5 | 0 | 57 |
| Cypress Boardwalk | Oct 2022 | 3 | 0 | 0 | 0 | 0 | 0 | 3 |
| FLM30 | Mar 2022 | 33 | 0 | 10 | 0 | 0 | 0 | 43 |
| FLM30 | Mar 2023 | 49 | 0 | 0 | 12 | 0 | 0 | 61 |
| FLM30 | Oct 2022 | 41 | 4 | 0 | 4 | 0 | 0 | 49 |
| Long Pine Key | Oct 2022 | 3 | 0 | 0 | 0 | 0 | 0 | 3 |
| MI | Mar 2022 | 0 | 0 | 10 | 0 | 0 | 0 | 10 |
| MI | Mar 2023 | 9 | 0 | 11 | 0 | 0 | 0 | 20 |
| Mahogany Hammock | Oct 2022 | 3 | 0 | 0 | 0 | 0 | 0 | 3 |
| RB10 | Mar 2022 | 0 | 0 | 10 | 0 | 0 | 0 | 10 |
| SE1 | Mar 2023 | 5 | 6 | 0 | 1 | 0 | 5 | 17 |
| SRS5 | Mar 2023 | 48 | 15 | 16 | 5 | 5 | 0 | 89 |
| SRS5 | Oct 2022 | 36 | 19 | 8 | 0 | 1 | 0 | 64 |
| SRS6 | Mar 2022 | 14 | 1 | 0 | 0 | 0 | 0 | 15 |
| SRS6 | Mar 2023 | 41 | 6 | 10 | 3 | 5 | 0 | 65 |
| SRS6 | Oct 2022 | 34 | 9 | 20 | 0 | 3 | 0 | 66 |

**Table S4. Component flux rates by class and season** (bootstrap mean and 95% CI; CH4 nmol m−2 s−1, CO2 µmol m−2 s−1 per m2 of enclosed surface; intervals for n ≥ 3).

| Component | Class | Season | n | CH4 | CO2 |
|---|---|---|---|---|---|
| cwd | ghost | dry | 5 | -0.10 (-0.74 to 0.31) | 0.51 (0.39 to 0.64) |
| cwd | ghost | wet | 5 | 4.78 (0.30 to 12.26) | 15.37 (4.45 to 33.43) |
| cwd | intact | dry | 10 | 0.27 (0.12 to 0.44) | 2.84 (1.88 to 3.94) |
| cwd | intact | wet | 4 | 0.81 (0.67 to 0.94) | 4.96 (2.82 to 6.65) |
| cwd | regenerating | wet | 2 | 0.20 | 7.60 |
| leaves | regenerating | dry | 14 | 0.02 (0.00 to 0.04) | -1.25 (-2.28 to -0.37) |
| leaves | scrub | dry | 5 | 0.03 (0.02 to 0.05) | -2.27 (-3.32 to -1.23) |
| root | ghost | wet | 4 | 0.61 (0.14 to 1.45) | 2.05 (0.34 to 3.69) |
| root | intact | dry | 22 | 1.05 (0.39 to 2.14) | 2.26 (1.23 to 3.60) |
| root | intact | wet | 28 | 5.48 (0.61 to 14.61) | 2.17 (1.18 to 3.23) |
| root | regenerating | dry | 5 | 5.73 (2.78 to 8.29) | 2.42 (1.54 to 3.09) |
| root | scrub | dry | 6 | 3.23 (1.27 to 5.23) | 0.54 (0.06 to 1.02) |
| soil | ghost | dry | 31 | 11.67 (4.91 to 21.09) | 1.49 (0.94 to 2.02) |
| soil | intact | dry | 36 | 1.42 (0.84 to 2.13) | 1.29 (1.03 to 1.62) |
| soil | intact | wet | 28 | 11.74 (5.67 to 21.37) | 1.97 (1.01 to 3.03) |
| soil | regenerating | dry | 22 | 57.79 (27.58 to 93.44) | 8.70 (6.77 to 10.75) |
| soil | regenerating | wet | 1 | 8.15 | 2.12 |
| stem | ghost | dry | 123 | 7.42 (4.62 to 10.74) | 1.84 (1.42 to 2.37) |
| stem | ghost | wet | 87 | 17.98 (8.70 to 30.81) | 2.70 (2.21 to 3.27) |
| stem | intact | dry | 103 | 0.24 (0.16 to 0.34) | 1.85 (1.57 to 2.15) |
| stem | intact | wet | 69 | 0.36 (0.17 to 0.62) | 1.54 (1.25 to 1.86) |
| stem | regenerating | dry | 54 | 1.14 (0.40 to 2.14) | 3.74 (3.16 to 4.36) |
| stem | regenerating | wet | 35 | 34.69 (7.30 to 80.66) | 7.33 (5.37 to 9.71) |
| stem | scrub | dry | 5 | 0.09 (-0.02 to 0.29) | 0.44 (0.30 to 0.69) |
| water | ghost | dry | 23 | 6.33 (5.33 to 7.43) | 0.38 (0.29 to 0.47) |
| water | ghost | wet | 10 | 38.76 (22.10 to 63.80) | 0.54 (0.43 to 0.66) |
| water | intact | dry | 8 | 0.86 (0.51 to 1.20) | 1.37 (1.15 to 1.58) |
| water | regenerating | wet | 5 | 56.85 (33.34 to 88.21) | 2.06 (1.70 to 2.48) |
| water | scrub | dry | 1 | 1.61 | 1.80 |

**Table S5. Woody CH4 height model** (M20). (A) Height forms compared by AIC. (B) Parametric terms of the selected model (asinh scale; terms ending '.1' are on the scale part). (C) Species added to the selected model (reference *R. mangle*).

(A)

| Height form | AIC | edf | ΔAIC |
|---|---|---|---|
| smooth_log | 977.6 | 19.6 | 0.0 |
| log | 989.1 | 16.7 | 11.5 |
| smooth | 990.3 | 22.1 | 12.7 |
| linear | 1 021 | 17.3 | 43.0 |

(B)

| Term | Estimate | SE | p |
|---|---|---|---|
| (Intercept) | 0.327 | 0.108 | 0.002 |
| classregenerating | 0.733 | 0.182 | 0.000 |
| classghost | 1.042 | 0.160 | 0.000 |
| surfaceprop root | 0.139 | 0.095 | 0.144 |
| statusdead | 0.031 | 0.075 | 0.679 |
| seasondry | -0.081 | 0.127 | 0.523 |
| floodedyes | -0.005 | 0.066 | 0.937 |
| (Intercept).1 | -0.996 | 0.054 | 0.000 |
| classregenerating.1 | 1.046 | 0.098 | 0.000 |
| classghost.1 | 1.196 | 0.078 | 0.000 |

(C)

| term | Estimate | Std. Error | z value | Pr(>|z|) |
|---|---|---|---|---|
| spAVGE | -0.113 | 0.078 | -1.443 | 0.149 |
| spCOER | -0.557 | 0.257 | -2.170 | 0.030 |
| spLARA | -0.149 | 0.093 | -1.610 | 0.107 |
| spunidentified | -2.219 | 0.711 | -3.121 | 0.002 |

**Table S6. Airborne end-member fluxes** (two-class disaggregation, Delaria et al. 2024) and the airborne check of the ghost-forest inundation representation (text S2). _[July 2024 pending; confirm uncertainty definition.]_

| Gas | Deployment | Class | Flux | SE | Units | Source |
|---|---|---|---|---|---|---|
| CH4 | Apr 2022 | mangrove forest | 3.8 | 8.0 | nmol m-2 s-1 | Table S3 flight mean |
| CH4 | Oct 2022 | mangrove forest | 29.0 | 16.0 | nmol m-2 s-1 | paper text (deployment mean) |
| CH4 | Feb 2023 | mangrove forest | 2.0 | 7.0 | nmol m-2 s-1 | Table S3 flight mean |
| CH4 | Apr 2023 | mangrove forest | -2.5 | 10.0 | nmol m-2 s-1 | Table S3 flight mean |
| CH4 | Apr 2022 | ghost forest | 5.0 | 4.0 | nmol m-2 s-1 | Table S3 |
| CH4 | Oct 2022 | ghost forest | 51.0 | 27.0 | nmol m-2 s-1 | paper text (deployment mean) |
| CH4 | Feb 2023 | ghost forest | 7.0 | 7.0 | nmol m-2 s-1 | Table S3 |
| CH4 | Apr 2023 | ghost forest | 2.0 | 13.0 | nmol m-2 s-1 | Table S3 |
| CO2 | Apr 2022 | mangrove forest | -10.8 | 5.0 | umol m-2 s-1 | Table S2 flight mean |
| CO2 | Oct 2022 | mangrove forest | -11.5 | 5.0 | umol m-2 s-1 | Table S2 flight mean |
| CO2 | Feb 2023 | mangrove forest | -11.3 | 4.0 | umol m-2 s-1 | Table S2 flight mean |
| CO2 | Apr 2023 | mangrove forest | -11.7 | 4.0 | umol m-2 s-1 | Table S2 flight mean |
| CO2 | Apr 2022 | ghost forest | 0.2 | 0.5 | umol m-2 s-1 | Table S2 |
| CO2 | Oct 2022 | ghost forest | -1.0 | 2.0 | umol m-2 s-1 | Table S2 |
| CO2 | Feb 2023 | ghost forest | -2.5 | 2.0 | umol m-2 s-1 | Table S2 |
| CO2 | Apr 2023 | ghost forest | 1.1 | 0.8 | umol m-2 s-1 | paper text (deployment mean) |

| campaign | bottomup_flooded | bottomup_exposed | carafe_ghost |
|---|---|---|---|
| Mar 2022 |  | 33.5 | 5.0 |
| Oct 2022 | 41.4 |  | 51.0 |
| Mar 2023 | 9.0 |  | 4.5 |

**Table S7. Stand budgets by site and campaign.** (A) CH4 by component, tide-weighted (mg CH4 m−2 ground d−1). (B) CO2 by component (µmol m−2 ground s−1; NEE = respiration − GPP). (C) Component CH4 rates used and their source (nmol m−2 s−1; 'gap' marks a rate taken from another site or campaign, M12).

(A)

| Site | Campaign | Class | Stem | Root | Soil | Water | Downed wood | Total |
|---|---|---|---|---|---|---|---|---|
| CP40 | Oct 2022 | ghost | 0.31 | 0.03 | 0.00 | 70.14 | 0.26 | 70.73 |
| CP40 | Mar 2023 | ghost | 0.16 | 0.02 | 8.24 | 5.92 | -0.01 | 14.33 |
| FLM30 | Oct 2022 | ghost | 0.15 | 0.01 | 0.00 | 28.81 | 0.00 | 28.98 |
| FLM30 | Mar 2023 | ghost | 0.18 | 0.02 | 7.98 | 8.18 | -0.02 | 16.35 |
| SRS5 | Oct 2022 | intact | 0.03 | 0.22 | 0.65 | 0.41 | 0.10 | 1.42 |
| SRS5 | Mar 2023 | intact | 0.04 | 0.41 | 0.29 | 0.29 | 0.04 | 1.07 |
| SRS6 | Oct 2022 | intact | 0.05 | 3.81 | 5.78 | 0.60 | 0.08 | 10.32 |
| SRS6 | Mar 2023 | intact | 0.05 | 0.18 | 2.22 | 1.02 | 0.06 | 3.53 |

(B)

| Site | Campaign | Stem | Root | Soil | Water | Downed wood | Leaf | Respiration | GPP | NEE |
|---|---|---|---|---|---|---|---|---|---|---|
| CP40 | Mar 2023 | 0.17 | 0.06 | 0.48 | 0.24 | 0.04 | 0.00 | 0.99 | 0.00 | 0.99 |
| CP40 | Oct 2022 | 0.66 | 0.06 | 0.00 | 0.67 | 0.59 | 0.00 | 1.98 | 0.00 | 1.98 |
| FLM30 | Mar 2023 | 0.53 | 0.04 | 0.46 | 0.37 | 0.06 | 0.00 | 1.47 | 0.00 | 1.47 |
| FLM30 | Oct 2022 | 0.38 | 0.04 | 0.00 | 0.35 | 0.00 | 0.00 | 0.77 | 0.00 | 0.77 |
| SRS5 | Mar 2023 | 1.46 | 0.63 | 0.73 | 0.46 | 0.28 | 1.74 | 5.30 | 8.51 | -3.21 |
| SRS5 | Oct 2022 | 0.90 | 0.36 | 0.47 | 0.27 | 0.51 | 2.00 | 4.52 | 7.12 | -2.60 |
| SRS6 | Mar 2023 | 1.60 | 0.17 | 0.67 | 0.86 | 0.47 | 1.74 | 5.51 | 8.51 | -3.00 |
| SRS6 | Oct 2022 | 1.17 | 0.62 | 0.60 | 0.38 | 0.34 | 2.00 | 5.11 | 7.12 | -2.01 |

(C)

| Site | Campaign | Component | CH4 rate (95% CI) | n | Source |
|---|---|---|---|---|---|
| CP40 | Oct 2022 | root | 0.60 (0.14 to 1.45) | 4 | gap: FLM30 root |
| CP40 | Oct 2022 | soil | 30.78 (12.70 to 55.16) | 10 | ghost floor without standing water: FLM30_2022 soil |
| CP40 | Oct 2022 | water | 50.71 (26.10 to 85.52) | 6 | site |
| CP40 | Oct 2022 | cwd | 4.77 (0.30 to 12.26) | 5 | site |
| FLM30 | Oct 2022 | root | 0.60 (0.14 to 1.45) | 4 | site |
| FLM30 | Oct 2022 | soil | 30.78 (12.70 to 55.16) | 10 | ghost floor without standing water: FLM30_2022 soil |
| FLM30 | Oct 2022 | water | 20.82 (13.39 to 32.73) | 4 | site |
| FLM30 | Oct 2022 | cwd | 4.77 (0.30 to 12.26) | 5 | gap: CP40 cwd |
| SRS5 | Oct 2022 | root | 0.70 (0.32 to 1.15) | 19 | site |
| SRS5 | Oct 2022 | soil | 1.23 (0.87 to 1.53) | 8 | site |
| SRS5 | Oct 2022 | water | 0.48 (0.24 to 3.08) | 0 | dissolved CH4 x k (code/03_fit/02_water_flux_from_dissolved.R) |
| SRS5 | Oct 2022 | cwd | 0.70 | 1 | site |
| SRS6 | Oct 2022 | root | 15.58 (0.87 to 43.48) | 9 | site |
| SRS6 | Oct 2022 | soil | 15.90 (7.97 to 28.34) | 20 | site |
| SRS6 | Oct 2022 | water | 0.59 (0.30 to 3.79) | 0 | dissolved CH4 x k (code/03_fit/02_water_flux_from_dissolved.R) |
| SRS6 | Oct 2022 | cwd | 0.85 (0.64 to 0.97) | 3 | site |
| CP40 | Mar 2023 | root | 0.60 (0.14 to 1.45) | 4 | gap: FLM30 root Oct 2022 |
| CP40 | Mar 2023 | soil | 30.78 (12.70 to 55.16) | 10 | ghost floor without standing water: FLM30_2022 soil |
| CP40 | Mar 2023 | water | 5.31 (4.11 to 6.58) | 11 | site |
| CP40 | Mar 2023 | cwd | -0.11 (-0.74 to 0.29) | 5 | site |
| FLM30 | Mar 2023 | root | 0.60 (0.14 to 1.45) | 4 | gap: FLM30 root Oct 2022 |
| FLM30 | Mar 2023 | soil | 30.78 (12.70 to 55.16) | 10 | ghost floor without standing water: FLM30_2022 soil |
| FLM30 | Mar 2023 | water | 7.28 (5.90 to 8.84) | 12 | site |
| FLM30 | Mar 2023 | cwd | -0.11 (-0.74 to 0.29) | 5 | gap: CP40 cwd |
| SRS5 | Mar 2023 | root | 1.25 (0.40 to 2.72) | 15 | site |
| SRS5 | Mar 2023 | soil | 0.33 (0.12 to 0.59) | 16 | site |
| SRS5 | Mar 2023 | water | 0.57 (0.24 to 0.96) | 5 | site |
| SRS5 | Mar 2023 | cwd | 0.19 (0.09 to 0.29) | 5 | site |
| SRS6 | Mar 2023 | root | 0.71 (0.09 to 1.57) | 6 | site |
| SRS6 | Mar 2023 | soil | 3.56 (2.25 to 5.41) | 10 | site |
| SRS6 | Mar 2023 | water | 1.35 (1.13 to 1.47) | 3 | site |
| SRS6 | Mar 2023 | cwd | 0.34 (0.07 to 0.65) | 5 | site |

**Table S8. Annual budgets and net forcing by class** (g m−2 yr−1; forcing in g CO2-eq m−2 yr−1; Monte Carlo 95% intervals).

| Class | CH4 | Net CO2 | Net GWP20 | 95% interval | Net GWP100 | 95% interval | Net GWP* | CH4 share (GWP20) |
|---|---|---|---|---|---|---|---|---|
| intact | 1.49 | -3 755 | -3 634 | -5 233 to -1 939 | -3 713 | -5 294 to -2 035 | -3 743 | 3.1% |
| ghost | 11.90 | 1 809 | 2 775 | 1 922 to 3 978 | 2 141 | 1 491 to 3 263 | 3 136 | 34.8% |

**Table S9. Literature-derived values.**

| Term | Value used (range) | Source | How obtained / converted | Use | Alternatives considered |
|---|---|---|---|---|---|
| Leaf dark respiration at 25 °C, Rd25 | 1.55 (1.28–1.62) µmol m⁻² leaf s⁻¹ | Barr et al. 2009 (*R. mangle*, at site); Sturchio et al. 2022 (*A. germinans*) | Species-weighted central value (M13) | Canopy leaf respiration, healthy class | Leaf chambers here were transparent (net exchange), so they cannot give Rd |
| Leaf area index | 2.8 (2.3–5.55) | SRS-6 ground LAI 2.80 ± 1.38 (Barr, unpublished, in Troxler et al. 2015); ground optical 2.3; MODIS at US-Skr 5.55 (Reed et al. 2025) | Effective LAI with Beer's law, k = 0.5 | Canopy leaf respiration | MODIS uses 24-day maxima and reads high. LAI recovered within ~1 yr of Wilma and Irma (Reed et al. 2025), so no hurricane reduction for 2022–23 |
| Leaf temperature response | f(T) = exp[0.1012(T−25) − 0.0005(T²−25²)] | Heskel et al. 2016 | Driven by tower air temperature; 30 % daytime light inhibition (Atkin et al. 2014; 20–50 % in the Monte Carlo) | Canopy leaf respiration, 24 h | — |
| Downed coarse woody debris volume | 67 (13–181) m³ ha⁻¹ | Krauss et al. 2005 (line-intersect surveys, South Florida mangroves, 9–10 yr after Hurricane Andrew) | Lateral surface = 4V/d with d = 10 cm (as Troxler et al. 2015 did at SRS-6), i.e. 0.27 (0.05–0.72) m² of wood per m² of ground. Exchanges with the air only above the water (text S3). | CWD CO2 and CH4, all classes | Placeholder of 10 m² per plot (superseded); eyewall value 132 m³ ha⁻¹ as sensitivity. No ghost-specific inventory exists, so the same distribution is used. |
| Woody litterfall (context only) | 68–95 (2001–04); 47–56 (2022–23) g dry m⁻² yr⁻¹ | FCE LTER, Castañeda-Moya et al., knb-lter-fce.1195.12 (SRS-4/5/6, monthly baskets, 2001–2023) | Annual sums of the Wood fraction | Supports using the Krauss volume (text S3) | — |
| Component CO2 effluxes at SRS-6 (context) | soil 1.27; soil + pneumatophores 3.17; prop roots 1.94; CWD 2.34 µmol m⁻² s⁻¹; scaled CWD respiration 1.6 t C ha⁻¹ yr⁻¹; below-canopy 715 g C m⁻² yr⁻¹ | Troxler et al. 2015 | As published | Comparison with our component rates and below-canopy respiration | — |
| Temperature sensitivity of chamber respiration (day → 24 h) | Q10 = 1.15 [1.13–1.18] (central: tower within-month); 2 (literature) and 4.3 (our stem chambers) as sensitivity | Tower night-time NEE (US-Skr, 2004–2023; SW_IN < 10 W m⁻², u* > 0.2 m s⁻¹, n = 46,641), log(NEE) ~ T with a year × month fixed effect (`code/07_upscaling/01_tower_gpp.R`); our stem CO2 vs temperature | Factor = mean over 24 h of Q10^(T/10) ÷ mean over measurement times | Stem, root, soil and CWD CO2 (not water, not leaf) | The within-month slope matches what the correction spans (day–night and day-to-day swings). Across seasons the tower gives 1.8, which also carries phenology, water level and salinity. Stem chambers give 4.3, likely inflated because daytime stem efflux also follows sap flow. See S.T5. |
| CH4 solubility | Bunsen coefficient (T, S) | Yamamoto et al. 1976 | — | Water CH4 flux from dissolved CH4 | — |
| CO2 solubility | K0 (T, S) | Weiss 1974 | — | Water CO2 flux from dissolved CO2 | — |
| Schmidt numbers (CH4, CO2) | Freshwater polynomials | Wanninkhof 2014 | k = k600 (Sc/600)^−0.5 | Water fluxes from dissolved gas | k600 itself is calibrated on our chamber/dissolved pairs (median 1.10 cm h⁻¹, range 0.56–7.07) |
| Share of the intact plot floor under water (tide-state weights, SRS5/SRS6) | SRS5 0.62 (Oct 2022), 0.37 (Mar 2023); SRS6 0.74, 0.55. Floor-mean ± 1.96 SE: SRS5 0.54–0.68, 0.31–0.44; SRS6 0.67–0.80, 0.48–0.61 | FCE LTER hourly water level above the soil surface at SRS5 and SRS6 (Castañeda-Moya et al., knb-lter-fce.1168.15), with our own chamber water-depth readings | Floor height relative to the logger fitted by censored maximum likelihood to our depth readings (standing water = exact, none = censored); hourly flooded share Φ((h + μ)/σ), averaged over the campaign month (`code/07_upscaling/01b_flood_fraction.R`; M11) | Weights of the high-tide (water surface; no soil or downed-wood flux) and low-tide (exposed floor) states in the CH4 and CO2 budgets | All-or-nothing switch (0.98–1.00 Oct, ~0.7 Mar); 2010–2023 hydrology (SRS5 0.38, SRS6 0.43); equal 50/50 split (Table S10) |
| Tidal-phase factor, intact water-surface CH4 | 1.0 (0.6–2.0, log-triangular, Monte Carlo) | Bouillon et al. 2007; Reithmaier et al. 2020 (Shark River); Lin et al. 2024; Yong et al. 2024 | Dissolved CH4 peaks near low water (2–7× high tide), so slack/low-water samples sit near the tidal maximum; flood/ebb-onset pulses unquantified (M22) | Intact water-surface CH4 | No CO2 multiplier: flushed CO2 leaves as lateral DIC |
| Day → 24 h CH4 | ×1.30 (sensitivity only) | Zhu et al. 2024 (four years of mangrove eddy covariance) | Daytime-only sampling underestimates annual CH4 by 23.3 % | All CH4 (sensitivity, Table S10) | Night/day ratio 1.69 (×1.35) |
| Atmospheric mixing ratios | CH4 1.95 ppm; CO2 417 µatm | Global/regional means for 2022–23 | Equilibrium concentrations | Water fluxes from dissolved gas | — |
| Lateral dissolved export (DIC + DOC), three scenarios | Central 201 (DIC 145 [61–229] + DOC 56); low ~90; high 793 (DIC 622 + DOC 171) g C m⁻² yr⁻¹ | Central: Zhao et al. 2021 Shark River synthesis (DIC from Ho 2017, Reithmaier 2020, Volta 2020) with Romigh et al. 2006 SRS-6 flume DOC. Low: Lagrangian SF6/³He tracer releases (Ho et al. 2017, 83–107, a stated minimum; Volta et al. 2020; Reithmaier et al. 2020 Lagrangian). High: Reithmaier et al. 2020 Eulerian (one 29-h deployment, November 2018; area normalisation bracketed ×0.5–×2 by the authors) | Each scenario from one coherent method, not a sum of term-wise extremes (M18) | NECB, healthy | Bergamaschi et al. 2012 DOC 180 (upper; mostly upstream DOC per Ho 2017); budget closure (NEE − wood − burial ≈ 770) lies between central and high |
| Exported alkalinity (subset of DIC) | Central 104; low 62; high 425 g C m⁻² yr⁻¹ | TA/DIC 0.72 (central), 0.76 (Lagrangian), 0.68 (Eulerian; Reithmaier et al. 2020) applied to each scenario's DIC | Durable ocean bicarbonate | Atmosphere-relevant NECB framing (M18) | Treated as returning to the air in the upper-bound framing; a small carbonate-derived part (6–7 % of DIC; Volta et al. 2020) is not ecosystem carbon |
| Lateral POC export | 145 (84–205) | Zhao et al. 2021 (litter POC 2001–2018) | Mean of our two intact sites, SRS-5 (84) and SRS-6 (205); same in all scenarios | NECB, healthy | Storm years at SRS-6: 448–548 |
| Ghost floor without standing water (March 2023) | Exposed share 0.19 (CP40 6/31, FLM30 9/48 readings); exposed-soil flux CH4 30.8 (13–56) nmol m⁻² s⁻¹, CO2 2.6 (2.2–3.0) µmol m⁻² s⁻¹ | Our depth readings at stem, root and downed-wood positions; FLM30 soil chambers, March 2022 (no standing water then) | Share of readings with no standing water; bootstrap mean of the FLM30 2022 soil chambers | Ghost CH4 and CO2 budgets, March 2023 | Marco Island ghost soils, pooled FLM30 + MI, BL60 dieback soils; ≤ 2 cm counted as exposed (text S2; Table S10) |
| Lateral aqueous CH4 | 0.35 (0.22–0.48) | Yau et al. 2024 (non-FCE analog) | — | NECB, healthy | — |
| Soil C burial | 123 (69–157) | Zhao et al. 2021; Breithaupt et al. | — | Storage check, healthy | — |
| Biomass change (wood increment) | 131 (65–197) | Castañeda-Moya et al. 2013 (repeat census) | Mean of SRS-5 (65) and SRS-6 (197); carbon fraction 0.45 | Storage check, healthy | Coarse roots would add ~30–50 %; Chen & Twilley 1999 higher (~480–540) |
| Ghost-class lateral, burial, biomass | none | — | No ghost-specific values exist | Not included | Healthy values are not transferred to ghost stands |
| GWP of CH4 | 27.9 (100 yr); 81.2 (20 yr) | IPCC AR6 | — | Net radiative forcing | — |
| Earlier tower budget (context) | NEE −1,170 ± 127; GPP ≈ 2,270; ER ≈ 1,100 g C m⁻² yr⁻¹ (2004) | Barr et al. 2010 (same tower) | As published | Comparison only | — |
| Airborne end-members | per-flight CH4 and CO2 fluxes | Delaria et al. 2024 (CARAFE) | Matched to our campaigns: Oct 2022, plus a Mar 2023 analog = mean of Feb and Apr 2023 | Top-down comparison | Daytime flights (text S5) |

**Table S10. One-at-a-time sensitivity of the budgets and net forcing to analytical choices** (g CO2-eq m⁻² yr⁻¹; GWP20 first, GWP100 in brackets). Each row changes one choice from the central case. Intact NEE in g C m⁻² yr⁻¹ (for the carbon-balance framings, the carbon retained, as net CO2 exchange). Switch = ghost − intact. Source: `output/qa/sensitivity_summary.csv`.

| Choice | Setting | Intact CH4 (g m⁻² yr⁻¹) | Intact NEE (g C) | Intact forcing, GWP20 [GWP100] | Ghost forcing, GWP20 [GWP100] | Switch, GWP20 [GWP100] |
|---|---|---|---|---|---|---|
| **Central** | — | 1.49 | −1,025 | −3,634 [−3,713] | 2,775 [2,141] | 6,409 [5,854] |
| Q10 (day → 24 h, chamber CO2) | none (Q10 = 1) | 1.49 | −1,002 | −3,550 [−3,629] | 3,000 [2,173] | 6,550 [5,803] |
| Q10 (day → 24 h, chamber CO2) | literature (Q10 = 2) | 1.49 | −1,097 | −3,899 [−3,978] | 2,863 [2,036] | 6,762 [6,015] |
| Q10 (day → 24 h, chamber CO2) | our stem chambers (Q10 = 4.33) | 1.49 | −1,167 | −4,154 [−4,234] | 2,758 [1,932] | 6,913 [6,165] |
| Downed CWD volume | none | 1.49 | −1,176 | −4,186 [−4,266] | 2,535 [1,901] | 6,721 [6,167] |
| Downed CWD volume | Krauss low (13 m³ ha⁻¹) | 1.49 | −1,147 | −4,079 [−4,159] | 2,581 [1,947] | 6,661 [6,106] |
| Downed CWD volume | Krauss eyewall (132 m³ ha⁻¹) | 1.49 | −879 | −3,097 [−3,177] | 3,008 [2,374] | 6,105 [5,551] |
| Downed CWD volume | Krauss high (181 m³ ha⁻¹) | 1.49 | −768 | −2,693 [−2,772] | 3,184 [2,549] | 5,876 [5,322] |
| Flooding representation (intact) | equal 50/50 split (SRS5 0.5/0.5; SRS6 0.5/0.5) | 1.98 | −934 | −3,261 [−3,367] | 2,775 [2,141] | 6,036 [5,508] |
| Flooding representation (intact) | all-or-nothing switch, campaign months (SRS5 1/0.69; SRS6 0.98/0.72) | 0.92 | −1,198 | −4,314 [−4,363] | 2,775 [2,141] | 7,089 [6,504] |
| Flooding representation (intact) | all-or-nothing switch, 2010–2023 (SRS5 0.63/0.63; SRS6 0.52/0.52) | 1.93 | −972 | −3,403 [−3,506] | 2,775 [2,141] | 6,178 [5,647] |
| Flooding representation (intact) | area-weighted, floor mean −1.96 SE (SRS5 0.54/0.31; SRS6 0.67/0.48) | 1.66 | −982 | −3,463 [−3,551] | 2,775 [2,141] | 6,238 [5,692] |
| Flooding representation (intact) | area-weighted, floor mean +1.96 SE (SRS5 0.68/0.44; SRS6 0.8/0.61) | 1.34 | −1,065 | −3,793 [−3,864] | 2,775 [2,141] | 6,568 [6,005] |
| Flooding representation (intact) | area-weighted, 2010–2023 (SRS5 0.38/0.38; SRS6 0.43/0.43) | 2.15 | −881 | −3,054 [−3,169] | 2,775 [2,141] | 5,829 [5,310] |
| Tidal phase, intact water CH4 | x 0.6 | 1.41 | −1,025 | −3,641 [−3,715] | 2,775 [2,141] | 6,415 [5,856] |
| Tidal phase, intact water CH4 | x 2.0 | 1.70 | −1,025 | −3,616 [−3,707] | 2,775 [2,141] | 6,391 [5,848] |
| CH4 day → 24 h | x 1.30 (all CH4) | 1.94 | −1,025 | −3,597 [−3,700] | 3,068 [2,242] | 6,665 [5,942] |
| Ghost floor without standing water (Mar 2023) | Marco Island ghost soil, Mar 2022-23 | 1.49 | −1,025 | −3,634 [−3,713] | 2,460 [1,899] | 6,094 [5,612] |
| Ghost floor without standing water (Mar 2023) | Pooled ghost soil (FLM30 2022 + MI) | 1.49 | −1,025 | −3,634 [−3,713] | 2,562 [1,977] | 6,195 [5,690] |
| Ghost floor without standing water (Mar 2023) | BL60 dieback soil, Mar 2023 | 1.49 | −1,025 | −3,634 [−3,713] | 3,356 [2,699] | 6,990 [6,412] |
| Ghost floor without standing water (Mar 2023) | Floor fully inundated | 1.49 | −1,025 | −3,634 [−3,713] | 2,401 [1,830] | 6,035 [5,543] |
| Ghost floor without standing water (Mar 2023) | FLM30 soil, Mar 2022 (same site); <= 2 cm counted as exposed | 1.49 | −1,025 | −3,634 [−3,713] | 3,138 [2,457] | 6,772 [6,170] |
| Ghost floor without standing water (Mar 2023) | Marco Island ghost soil, Mar 2022-23; <= 2 cm counted as exposed | 1.49 | −1,025 | −3,634 [−3,713] | 2,476 [1,946] | 6,110 [5,659] |
| Ghost floor without standing water (Mar 2023) | Pooled ghost soil (FLM30 2022 + MI); <= 2 cm counted as exposed | 1.49 | −1,025 | −3,634 [−3,713] | 2,689 [2,111] | 6,323 [5,824] |
| Ghost floor without standing water (Mar 2023) | BL60 dieback soil, Mar 2023; <= 2 cm counted as exposed | 1.49 | −1,025 | −3,634 [−3,713] | 4,362 [3,633] | 7,996 [7,346] |
| Leaf respiration | Rd25 1.28, LAI 2.3 | 1.49 | −1,203 | −4,287 [−4,366] | 2,775 [2,141] | 7,062 [6,507] |
| Leaf respiration | Rd25 1.62, LAI 5.55 | 1.49 | −811 | −2,851 [−2,930] | 2,775 [2,141] | 5,626 [5,071] |
| Carbon-balance framing | NECB, exported alkalinity retained | 1.96 | −783 | −2,709 [−2,814] | 2,775 [2,141] | 5,484 [4,954] |
| Carbon-balance framing | NECB, all export returned to the air | 1.96 | −679 | −2,328 [−2,432] | 2,775 [2,141] | 5,103 [4,573] |
| Carbon-balance framing | storage only (burial + wood) | 1.49 | −254 | −810 [−889] | 2,775 [2,141] | 3,585 [3,030] |
| Monte Carlo 95 % interval | all propagated terms | – | – | −5233 to −1939 [−5294 to −2035] | 1922 to 3978 [1491 to 3263] | – |

**Table S11. Porewater inorganic nitrogen: this study against FCE LTER monitoring at SRS5 and SRS6** (µmol L−1).

| site | period | n | NH4_median | NH4_q90 | NH4_max | NO3_median |
|---|---|---|---|---|---|---|
| SRS5 | FCE 2000-2024 | 375.00 | 3.59 | 10.30 | 32.30 | 0.23 |
| SRS6 | FCE 2000-2024 | 382.00 | 5.11 | 13.90 | 830.00 | 0.12 |
| SRS5 | FCE 2022-2024 | 48.00 | 1.53 | 6.35 | 9.28 | 0.41 |
| SRS6 | FCE 2022-2024 | 47.00 | 2.83 | 11.50 | 28.90 | 0.36 |
| SRS5 | FCE 2017 (post-Irma) | 16.00 | 3.74 | 12.30 | 17.50 | 0.56 |
| SRS6 | FCE 2017 (post-Irma) | 16.00 | 56.10 | 440.00 | 830.00 | 0.23 |
| SRS5 | FCE 2018 | 8.00 | 10.20 | 14.60 | 18.20 | 1.40 |
| SRS6 | FCE 2018 | 8.00 | 17.10 | 68.70 | 108.00 | 1.63 |
| BL60 | This study | 5.00 | 7.08 | 24.60 | 26.90 | 0.00 |
| CP40 | This study | 5.00 | 200.00 | 298.00 | 316.00 | 0.00 |
| SRS5 | This study | 5.00 | 0.00 | 0.00 | 0.00 | 0.00 |
| SRS6 | This study | 5.00 | 0.00 | 0.00 | 0.00 | 0.00 |

**Table S12. Component CH4 rates at all sites, campaigns pooled** (nmol m−2 s−1 per m2 of surface; 95% CI; context sites MI, RB10, SE1 alongside the core sites; text S8).

| Site | Component | CH4 (95% CI) | n |
|---|---|---|---|
| BL60 | cwd | 0.20 (0.17 to 0.23) | 2 |
| BL60 | leaves | 0.02 (0.00 to 0.04) | 14 |
| BL60 | root | 5.74 (2.50 to 8.33) | 5 |
| BL60 | soil | 55.38 (26.69 to 90.92) | 23 |
| BL60 | stem | 14.20 (3.12 to 33.19) | 90 |
| BL60 | water | 57.13 (33.34 to 88.92) | 5 |
| CP40 | cwd | 2.34 (-0.07 to 6.32) | 10 |
| CP40 | stem | 14.11 (7.43 to 23.02) | 78 |
| CP40 | water | 21.22 (8.80 to 38.01) | 17 |
| FLM30 | root | 0.61 (0.14 to 1.45) | 4 |
| FLM30 | soil | 30.70 (12.59 to 55.67) | 10 |
| FLM30 | stem | 11.17 (5.81 to 18.81) | 123 |
| FLM30 | water | 10.62 (7.33 to 15.10) | 16 |
| MI | soil | 2.57 (2.11 to 3.05) | 21 |
| MI | stem | 0.49 (0.19 to 0.90) | 9 |
| RB10 | soil | 1.04 (0.48 to 1.66) | 10 |
| SE1 | leaves | 0.03 (0.02 to 0.05) | 5 |
| SE1 | root | 3.24 (1.33 to 5.19) | 6 |
| SE1 | stem | 0.09 (-0.02 to 0.29) | 5 |
| SE1 | water | 1.61 | 1 |
| SRS5 | cwd | 0.28 (0.12 to 0.46) | 6 |
| SRS5 | root | 0.93 (0.44 to 1.65) | 34 |
| SRS5 | soil | 0.63 (0.38 to 0.90) | 24 |
| SRS5 | stem | 0.11 (0.09 to 0.13) | 84 |
| SRS5 | water | 0.57 (0.24 to 0.96) | 5 |
| SRS6 | cwd | 0.53 (0.27 to 0.78) | 8 |
| SRS6 | root | 9.05 (0.69 to 24.94) | 16 |
| SRS6 | soil | 11.82 (6.14 to 20.75) | 30 |
| SRS6 | stem | 0.44 (0.27 to 0.66) | 89 |
| SRS6 | water | 1.35 (1.13 to 1.47) | 3 |

**Table S13. Flooded share of the intact forest floor** (M11): campaign-month mean of the hourly flooded share (±1.96 SE of the floor mean), the all-or-nothing switch and the long-term value, with the fitted floor-height distribution (cm relative to the logger datum).

| Site | Campaign | Flooded share | Switch | 2010–2023 | Floor µ (SE) | Floor σ | n readings (wet) |
|---|---|---|---|---|---|---|---|
| SRS5 | Mar 2022 | 0.19 (0.16 to 0.24) | 0.37 | 0.38 | -4.6 (1.2) | 11.1 | 129 (69) |
| SRS5 | Mar 2023 | 0.37 (0.31 to 0.44) | 0.69 | 0.38 | -4.6 (1.2) | 11.1 | 129 (69) |
| SRS5 | Oct 2022 | 0.62 (0.54 to 0.68) | 1.00 | 0.38 | -4.6 (1.2) | 11.1 | 129 (69) |
| SRS6 | Mar 2022 | 0.42 (0.35 to 0.49) | 0.55 | 0.43 | -1.3 (1.3) | 9.9 | 69 (51) |
| SRS6 | Mar 2023 | 0.55 (0.48 to 0.61) | 0.72 | 0.43 | -1.3 (1.3) | 9.9 | 69 (51) |
| SRS6 | Oct 2022 | 0.74 (0.67 to 0.80) | 0.98 | 0.43 | -1.3 (1.3) | 9.9 | 69 (51) |

**Table S14. Regional scaling of the switch over 2017 hurricane dieback** (M19). (A) By territory. (B) By storm, with the Florida bound.

(A)

| Territory | Area (km2) | Switch GWP20 (Tg CO2-eq yr−1) | GWP100 | GWP* | Induced CH4 (Gg yr−1) |
|---|---|---|---|---|---|
| Cuba | 104.03 | 0.667 (0.402 to 0.958) | 0.609 | 0.716 | 1.083 (0.334 to 1.838) |
| United States | 60.09 | 0.385 (0.232 to 0.554) | 0.352 | 0.413 | 0.625 (0.193 to 1.061) |
| Puerto Rico | 5.42 | 0.035 (0.021 to 0.050) | 0.032 | 0.037 | 0.056 (0.017 to 0.096) |
| Venezuela | 2.51 | 0.016 (0.010 to 0.023) | 0.015 | 0.017 | 0.026 (0.008 to 0.044) |
| Antigua and Barbuda | 0.33 | 0.002 (0.001 to 0.003) | 0.002 | 0.002 | 0.003 (0.001 to 0.006) |
| Turks and Caicos Islands | 0.31 | 0.002 (0.001 to 0.003) | 0.002 | 0.002 | 0.003 (0.001 to 0.006) |
| British Virgin Islands | 0.30 | 0.002 (0.001 to 0.003) | 0.002 | 0.002 | 0.003 (0.001 to 0.005) |
| United States Virgin Islands | 0.17 | 0.001 (0.001 to 0.002) | 0.001 | 0.001 | 0.002 (0.001 to 0.003) |
| Mexico | 0.10 | 0.001 (0.000 to 0.001) | 0.001 | 0.001 | 0.001 (0.000 to 0.002) |
| Nicaragua | 0.06 | 0.000 (0.000 to 0.001) | 0.000 | 0.000 | 0.001 (0.000 to 0.001) |
| Sint Maarten | 0.03 | 0.000 (0.000 to 0.000) | 0.000 | 0.000 | 0.000 (0.000 to 0.001) |
| Bahamas, The | 0.02 | 0.000 (0.000 to 0.000) | 0.000 | 0.000 | 0.000 (0.000 to 0.000) |
| Guadeloupe | 0.01 | 0.000 (0.000 to 0.000) | 0.000 | 0.000 | 0.000 (0.000 to 0.000) |
| Trinidad and Tobago | 0.01 | 0.000 (0.000 to 0.000) | 0.000 | 0.000 | 0.000 (0.000 to 0.000) |
| Belize | 0.01 | 0.000 (0.000 to 0.000) | 0.000 | 0.000 | 0.000 (0.000 to 0.000) |
| Saint Martin | 0.01 | 0.000 (0.000 to 0.000) | 0.000 | 0.000 | 0.000 (0.000 to 0.000) |
| Martinique | 0.00 | 0.000 (0.000 to 0.000) | 0.000 | 0.000 | 0.000 (0.000 to 0.000) |
| Dominican Republic | 0.00 | 0.000 (0.000 to 0.000) | 0.000 | 0.000 | 0.000 (0.000 to 0.000) |
| Anguilla | 0.00 | 0.000 (0.000 to 0.000) | 0.000 | 0.000 | 0.000 (0.000 to 0.000) |
| Grenada | 0.00 | 0.000 (0.000 to 0.000) | 0.000 | 0.000 | 0.000 (0.000 to 0.000) |
| Honduras | 0.00 | 0.000 (0.000 to 0.000) | 0.000 | 0.000 | 0.000 (0.000 to 0.000) |
| Total | 173.43 | 1.111 (0.670 to 1.598) | 1.015 | 1.193 | 1.805 (0.556 to 3.063) |

(B)

| Storm | Area (km2) | Switch GWP20 (Tg) | GWP100 (Tg) | Induced CH4 (Gg) |
|---|---|---|---|---|
| Irma | 165.1 | 1.06 (0.64 to 1.52) | 0.97 | 1.72 (0.53 to 2.92) |
| Maria | 5.6 | 0.04 (0.02 to 0.05) | 0.03 | 0.06 (0.02 to 0.10) |
| other | 2.7 | 0.02 (0.01 to 0.02) | 0.02 | 0.03 (0.01 to 0.05) |
| Irma, Florida dieback per Lagomasino et al. 2021 | 212.6 | 1.36 (0.82 to 1.96) | 1.24 | 2.21 (0.68 to 3.76) |

**Table S15. Literature values for canopy leaf respiration and leaf area** (M13; full provenance in the repository).

| ID | Quantity | Value | Units | Species | Site | Method | Source |
|---|---|---|---|---|---|---|---|
| LR-01 | leaf dark respiration | 1.62 ± 1.32 | umol CO2 m-2 leaf s-1 | Rhizophora mangle | Shark River SRS-6 | LI-6400 cuvette; Farquhar A-PAR/A-Ci fit intercept | Barr et al. 2009 |
| LR-02 | leaf dark respiration R_area25 | 1.28-1.54 | umol CO2 m-2 leaf s-1 | Avicennia germinans | N Florida fertilization plots | night cuvette; R-T curve; std to 25C | Sturchio et al. 2022 |
| LR-02b | leaf dark respiration R_mass25 | 5.75-8.73 | nmol CO2 g-1 leaf s-1 | Avicennia germinans | N Florida fertilization plots | night cuvette; R-T curve; std to 25C | Sturchio et al. 2022 |
| LR-03 | leaf dark respiration R_area25 | 0.86-1.71 | umol CO2 m-2 leaf s-1 | Avicennia germinans | GTMNERR marsh-mangrove ecotone NE Florida | night cuvette; Arrhenius/exp R-T fits | Sturchio et al. 2021 |
| LR-04 | Q10 of leaf R | ~2.0 | dimensionless | Rhizophora mangle + Avicennia germinans | FL and Belize provenances | short-term R-T curves; b,c polynomial | Chieppa et al. 2023 |
| LR-05 | Rd25 PFT mean (PROXY) | 0.43 | umol CO2 m-2 leaf s-1 | tropical evergreen broadleaf | pantropical (GlobResp) | leaf cuvette database | Atkin et al. 2015 |
| LR-X | leaf Rd (SUPERSEDED proxy) | 0.17-0.41 | umol CO2 m-2 leaf s-1 | Rhizophora stylosa / Avicennia marina | New Caledonia | night cuvette | Jacotot et al. 2018 |
| LAI-01 | leaf area index (ground optical) | 2.29 ± 0.18 / 2.80 ± 1.38 | m2 m-2 | mixed riverine | Shark River SRS-6 | canopy analyzer / hemispherical | Barr et al. 2010 / Troxler et al. 2015 |
| LAI-02 | leaf area index (MODIS) | 5.55 | m2 m-2 | tall riverine | Shark River SRS-6 | MODIS 500m 8-day | Charkowicz et al. 2025 |
| LAI-03 | leaf area index (riverine corroboration) | 4.66 | m2 m-2 | Rhizophora/Laguncularia | Agua Brava Mexican Pacific | LAI-2000 | Kovacs et al. 2005 |
| FR-01 | foliage % of Reco (PROXY) | 37 (range 18-40) | percent | tropical wet forest | La Selva Costa Rica | canopy chamber transects | Cavaleri et al. 2008 |
| FR-02 | leaf % of Reco (PROXY) | 33 | percent | terra firme rainforest | Manaus Amazon | chamber leaf/wood/soil | Chambers et al. 2004 |
| FR-03 | foliage % of net daytime fixed C (MANGROVE) | 22 | percent | Rhizophora apiculata | SE Asia | carbon-allocation budget | Clough 1997 in Alongi 2014 |
| AH-01 | autotrophic:heterotrophic of Reco | 74:26 | percent | global mangrove | global | mass-balance synthesis | Alongi 2014 |
| AH-02 | below-canopy share of Reco | 45-65 | percent | mixed riverine | Shark River SRS-6 | component chambers + EC | Troxler et al. 2015 |
| AH-03 | ER:GPP (MANGROVE) | 0.65 | ratio | global mangrove | global | EC NEE-partition synthesis | Adame et al. 2024 |
| GN-01 | max daytime NEE | -20 to -25 | umol CO2 m-2 ground s-1 | mixed riverine | Shark River SRS-6 | eddy covariance | Barr et al. 2010 |
| GN-02 | annual NEP (=-NEE) | 1170 ± 127 | g C m-2 yr-1 | mixed riverine | Shark River SRS-6 | eddy covariance | Barr et al. 2010 |
| GN-03 | daytime Rd / mean ecosystem Re | 2.81 ± 2.41 / 1.62 ± 1.38 | umol CO2 m-2 ground s-1 | mixed riverine | Shark River SRS-6 | eddy covariance | Barr et al. 2010 (Re via Troxler 2015) |
| GN-04 | leaf Amax | ~18 | umol CO2 m-2 leaf s-1 | Rhizophora mangle | Shark River SRS-6 | LI-6400 cuvette | Barr et al. 2009 |
| GN-05 | midday GPP plausibility | 25-40 (your 37 high-plausible) | umol CO2 m-2 ground s-1 | mixed riverine | Shark River SRS-6 | derived: GPP=-NEE+Reco | derived from Barr 2010 |
| REF-CM13 | Shark River total NPP | 17.0 ± 1.1 | Mg ha-1 yr-1 | mixed riverine | Shark River | biometric | Castaneda-Moya et al. 2013 |

---

# Data S1 and S2

**Data S1.** Chamber flux dataset (762 analysed fluxes with geometry, fit statistics, detection limits, quality flags and ancillary variables) and data dictionary: combined_gas_flux_dataset.csv and data_dictionary.csv _[repository URL; ORNL DAAC archive in preparation]_.

**Data S2. External datasets used.**

| Dataset | Use |
|---|---|
| citation | use in this study`. |
| Barr, J. G. & Fuentes, J. D. et al. AmeriFlux BASE US-Skr Shark River Slough (Tower SRS-6) Everglades, version 2-5. AmeriFlux AMP (dataset). [CHECK: DOI and creator list] | Tower NEE, GPP partitioning, air temperature and pressure for chamber fluxes, Q10 |
| Castañeda-Moya, E. et al. Water level at the FCE LTER mangrove sites SRS5 and SRS6. Environmental Data Initiative, knb-lter-fce.1168.15 (dataset). [CHECK: title and DOI] | Hourly water level for the flooded share of the intact floor and the stem exposure model |
| Castañeda-Moya, E. et al. Monitoring of nutrient and sulfide concentrations in porewaters of mangrove forests from the Shark River Slough and Taylor Slough, Everglades National Park (FCE LTER), Florida, USA, December 2000–ongoing. Environmental Data Initiative, knb-lter-fce.1171.16 (2025). https://doi.org/10.6073/pasta/cd38181a01297bc25a086d71635a1a26 | Long-term porewater NH4 and NO3 at SRS5 and SRS6 (table S11) |
| Castañeda-Moya, E. et al. Litterfall production of mangrove forests along Shark River Slough and Taylor Slough, Everglades National Park (FCE LTER). Environmental Data Initiative, knb-lter-fce.1195.12 (dataset). [CHECK: title and DOI] | Woody litterfall context for downed wood (text S3) |
| Xiong, L., Lagomasino, D. & Poulter, B. BlueFlux: Terrestrial lidar scans of mangrove forests, Everglades, FL, USA, 2022–2023. ORNL DAAC (2024). https://doi.org/10.3334/ORNLDAAC/2311 | Point clouds for surface area by component and height |
| Delaria, E. R. et al. [CARAFE airborne flux data for BlueFlux]. ORNL DAAC (dataset). [CHECK: DAAC dataset ID] | Airborne CH4 and CO2 fluxes by deployment |
| Doughty, C. L. et al. BlueFlux: Modeled daily CO2 and CH4 wetland fluxes, southern Florida, 2000–2024. ORNL DAAC (2025). https://doi.org/10.3334/ORNLDAAC/2404 | Regional context |
| Bunting, P. et al. Global Mangrove Watch v3.0 (1996–2020). Zenodo (2022). https://doi.org/10.5281/zenodo.6894273 | 2016 mangrove extent (Fig. 1A) |
| Taillie, P. J. et al. 2017 hurricane-season mangrove damage with little recovery (short-term loss), country-attributed polygons; provided by D. Lagomasino (2026). [CHECK: public archive] | 2017 hurricane dieback area (Figs. 1A, 5C; table S14) |
| National Hurricane Center. Atlantic hurricane database (HURDAT2), 1851–2025. https://www.nhc.noaa.gov/data/hurdat/ | Tracks of Irma and Maria (Fig. 5C) |
| Natural Earth. Admin 0 countries, 1:50m. https://www.naturalearthdata.com | Country attribution and base maps |
| [BlueFlux aquatic transect dissolved CO2 and CH4, Shark River, October 2022; CHECK citation] | Dissolved gas at SRS6 for the October 2022 water-surface flux (M8) |

---

# References

- [allen2018 — to add]
- [atkin2014 — to add]
- [barr2009 — to add]
- [bouillon2007 — to add]
- Bunting, P. et al. Global Mangrove Extent Change 1996–2020: Global Mangrove Watch Version 3.0. Remote Sens. 14, 3657 (2022). https://doi.org/10.3390/rs14153657
- Cahoon, D. R. et al. Mass tree mortality leads to mangrove peat collapse at Bay Islands, Honduras after Hurricane Mitch. J. Ecol. 91, 1093–1105 (2003). https://doi.org/10.1046/j.1365-2745.2003.00841.x
- Castañeda-Moya, E., Twilley, R. R. & Rivera-Monroy, V. H. Allocation of biomass and net primary productivity of mangrove forests along environmental gradients in the Florida Coastal Everglades, USA. For. Ecol. Manage. 307, 226–241 (2013). https://doi.org/10.1016/j.foreco.2013.07.011
- Delaria, E. R. et al. Assessment of landscape-scale fluxes of carbon dioxide and methane in subtropical coastal wetlands of South Florida. J. Geophys. Res. Biogeosci. 129, e2024JG008165 (2024). https://doi.org/10.1029/2024JG008165
- Forster, P. et al. The Earth's energy budget, climate feedbacks, and climate sensitivity. In Climate Change 2021: The Physical Science Basis. Contribution of Working Group I to the Sixth Assessment Report of the IPCC, Ch. 7, 923–1054 (Cambridge Univ. Press, 2021).
- [griffiths2021 — to add]
- Hannun, R. A. et al. Spatial heterogeneity in CO2, CH4, and energy fluxes: insights from airborne eddy covariance measurements over the Mid-Atlantic region. Environ. Res. Lett. 15, 035008 (2020). https://doi.org/10.1088/1748-9326/ab7391
- Heskel, M. A. et al. Convergence in the temperature response of leaf respiration across biomes and plant functional types. Proc. Natl Acad. Sci. USA 113, 3832–3837 (2016). https://doi.org/10.1073/pnas.1520282113
- Ho, D. T. et al. Dissolved carbon biogeochemistry and export in mangrove-dominated rivers of the Florida Everglades. Biogeosciences 14, 2543–2559 (2017). https://doi.org/10.5194/bg-14-2543-2017
- Hutchinson, G. L. & Mosier, A. R. Improved soil cover method for field measurement of nitrous oxide fluxes. Soil Sci. Soc. Am. J. 45, 311–316 (1981). https://doi.org/10.2136/sssaj1981.03615995004500020017x
- Hutjes, R. W. A. et al. Dis-aggregation of airborne flux measurements using footprint analysis. Agric. For. Meteorol. 150, 966–983 (2010). https://doi.org/10.1016/j.agrformet.2010.03.004
- Kljun, N., Calanca, P., Rotach, M. W. & Schmid, H. P. A simple two-dimensional parameterisation for Flux Footprint Prediction (FFP). Geosci. Model Dev. 8, 3695–3713 (2015). https://doi.org/10.5194/gmd-8-3695-2015
- Krauss, K. W. et al. Woody debris in the mangrove forests of South Florida. Biotropica 37, 9–15 (2005). https://doi.org/10.1111/j.1744-7429.2005.03058.x
- Lagomasino, D. et al. Storm surge and ponding explain mangrove dieback in southwest Florida following Hurricane Irma. Nat. Commun. 12, 4003 (2021). https://doi.org/10.1038/s41467-021-24253-y
- [lin2024 — to add]
- [osland2020 — to add]
- Pedersen, A. R., Petersen, S. O. & Schelde, K. A comprehensive approach to soil-atmosphere trace-gas flux estimation with static chambers. Eur. J. Soil Sci. 61, 888–902 (2010). https://doi.org/10.1111/j.1365-2389.2010.01291.x
- Poulter, B. et al. Multi-scale observations of mangrove blue carbon ecosystem fluxes: the NASA Carbon Monitoring System BlueFlux field campaign. Environ. Res. Lett. 18, 075009 (2023). https://doi.org/10.1088/1748-9326/acdae6
- Powell, E. B. et al. [Terrestrial laser scanning of Everglades mangrove structure and surface area across a hurricane-disturbance gradient — methods paper in preparation; details to complete]
- [reed2025 — to add]
- [reithmaier2020 — to add]
- Rheault, K., Christiansen, J. R. & Larsen, K. S. goFlux: a user-friendly way to calculate GHG fluxes yourself, regardless of user experience. J. Open Source Softw. 9, 6393 (2024). https://doi.org/10.21105/joss.06393
- Sippo, J. Z. et al. Coastal carbon cycle changes following mangrove loss. Limnol. Oceanogr. 65, 2642–2656 (2020). https://doi.org/10.1002/lno.11476
- Smith, M. A., Cain, M. & Allen, M. R. Further improvement of warming-equivalent emissions calculation. npj Clim. Atmos. Sci. 4, 19 (2021). https://doi.org/10.1038/s41612-021-00169-8
- [stegehuis2026 — to add]
- [sturchio2022 — to add]
- Taillie, P. J. et al. Widespread mangrove damage resulting from the 2017 Atlantic mega hurricane season. Environ. Res. Lett. 15, 064010 (2020). https://doi.org/10.1088/1748-9326/ab82cf
- Troxler, T. G. et al. Component-specific dynamics of riverine mangrove CO2 efflux in the Florida coastal Everglades. Agric. For. Meteorol. 213, 273–282 (2015). https://doi.org/10.1016/j.agrformet.2014.12.012
- [wanninkhof2014 — to add]
- [weiss1974 — to add]
- Wolfe, G. M. et al. The NASA Carbon Airborne Flux Experiment (CARAFE): instrumentation and methodology. Atmos. Meas. Tech. 11, 1757–1776 (2018). https://doi.org/10.5194/amt-11-1757-2018
- [wood2023 — to add]
- [yamamoto1976 — to add]
- [yong2024 — to add]
- Zhao, X. et al. Tropical cyclones cumulatively control regional carbon fluxes in Everglades mangrove wetlands (Florida, USA). Sci. Rep. 11, 13927 (2021). https://doi.org/10.1038/s41598-021-92899-1
- [zhu2024 — to add]
