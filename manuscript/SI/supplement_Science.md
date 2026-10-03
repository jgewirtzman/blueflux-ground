# Supplementary Materials for

## Hurricane-induced mortality switches mangroves from carbon sink to methane source

Jonathan Gewirtzman et al.

Corresponding author: jonathan.gewirtzman@yale.edu

**This PDF file includes:** Materials and Methods; Supplementary Text S1 to S12; Figs. S1 to S17; Tables S1 to S15; References.

**Other Supplementary Materials for this manuscript include:** Data S1 (chamber flux dataset); Data S2 (external datasets used, with citations).

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

Water-surface fluxes were computed once per floating-chamber placement, from placement to lift, however many closures were logged within it. Placements were identified in the CH4 record: a lift as a drop in the 5-point running median of >max(20 ppb, half the excess over background) within 30 s, or a gap in the record >60 s; the start as the last point at background before the fit window. This gave 47 placements (median 6.5 min, range 1.6–79 min). Ebullition was separated with goAquaFlux (a goFlux extension; de-ebulliated diffusive window). Bubbles were detected as steps in the standardised CH4 series where the rolling variance of first differences exceeded the larger of its 70th percentile and median + 4 MAD (requiring a max/median ratio ≥3), merging episodes within 10 s and keeping steps ≥5 ppb; step size was estimated by local regression with an exponential re-equilibration term. The diffusive flux was fitted to the de-ebulliated series over the first 10 min of placements longer than 12 min and over the fit window otherwise; the ebullitive flux was the summed step size over the placement, converted to a flux over the incubation time; the total was their sum. The Picarro's ~5 s CH4 updates cannot resolve bubble steps, so for Picarro placements the total was a two-point flux from the placement start to the end of the diffusive window (15 s means), and ebullition was the excess of total over diffusive (floored at zero). Bubbles occurred in 11 of 47 placements and supplied 14% of water-surface CH4 overall (fig. S4). The detector misses gradual releases and steps in the last seconds of a placement, so for LGR placements the excess of a single fit over the whole placement above diffusive plus detected ebullitive flux was taken as an upper bound on undetected ebullition; with it, the ebullitive share rises to at most 17% overall and to 2–12% (from 0–2%) in the dry-season ghost water (fig. S4).

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

Ecosystem respiration was built from the same components and areas, with chamber CO2 efflux of stems (no height profile), prop roots, soil, water and downed wood, plus canopy leaf respiration. Chamber respiration of stems, roots, soil and downed wood was scaled from measurement-time to 24-h temperature with the within-month Q10 of night-time respiration at the US-Skr tower (1.15; M14), giving factors of 0.96–1.02. Canopy leaf respiration in intact forest was Rd25 × mean[f(T) × (1 − 0.3 × day)] × LAIeff, with leaf dark respiration at 25 °C Rd25 = 1.55 (1.28–1.62) µmol m−2 leaf s−1 from *R. mangle* at SRS6 (Barr et al. 2009) and *A. germinans* (Sturchio et al. 2022), the temperature response of Heskel et al. (2016), f(T) = exp[0.1012(T − 25) − 0.0005(T2 − 252)], driven by tower air temperature, 30% daytime light inhibition (Atkin et al. 2014; Huntingford et al. 2017; daytime when shortwave >5 W m−2) _[CHECK: source of the 30% value]_, and an effective leaf area LAIeff = (1 − e−0.5L)/0.5 with L = 2.8 (2.3–5.55) (Troxler et al. 2015; Reed et al. 2025), giving 1.7–2.0 µmol m−2 s−1. Leaf respiration was zero in ghost forest. Net ecosystem exchange was respiration minus gross primary production (GPP), with GPP the tower campaign mean in intact forest (M14) and zero in ghost forest (defoliated). Because the imported GPP is partitioned from the tower's own net exchange, intact bottom-up net exchange equals tower net exchange plus the difference between bottom-up and tower-partitioned respiration; it reconciles the two respiration estimates rather than providing an independent net flux (text S5). Airborne fluxes provide the independent check (M15).

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

The regional estimate multiplies the per-area switch by the area of 2017 hurricane dieback from the Landsat analysis of Taillie et al. (2020): ΔNDVI < −0.2 within the Global Mangrove Watch v1 baseline, with persistent damage defined as no NDVI recovery over the seven months after the season. The polygons of 2017 damage with little recovery, attributed to country, were provided by D. Lagomasino _[CHECK: relation of this layer (173 km2) to the paper's 790 km2 of persistent damage]_. Polygons were split into patches and their area computed in an Albers equal-area projection (standard parallels 10° and 30° N); patches without a country took the nearest territory, and territories were attributed to Hurricane Irma or Maria by track. Induced CH4 was area × the ghost-minus-intact CH4 difference (10.4 g CH4 m−2 yr−1), with a range from the class Monte Carlo intervals. Because the layer captures 60 of the 108 km2 of post-Irma dieback mapped in Florida (Lagomasino et al. 2021), a Florida-adjusted bound is also reported (table S14). For the map (Fig. 5D), patches were aggregated to 0.25° cells. Mangrove extent in Fig. 1A is Global Mangrove Watch v3.0 for 2016 (Bunting et al. 2022), aggregated to ~100 m for display.

## M20. Statistics and uncertainty

Fluxes were transformed with the inverse hyperbolic sine (asinh), which handles negative values, approximates the logarithm for large values and is linear near zero. Component means and 95% intervals were percentile bootstrap estimates (5,000 resamples), reported for groups of four or more measurements. Woody (stem and prop-root) CH4 by height (Fig. 2D) was modelled with a Gaussian location–scale generalised additive model (mgcv, gaulss, REML) of asinh(CH4) on 425 closures from 148 trees reconstructed from the order of measurements: location = class-specific smooth of log(h + 5) (k = 4) + surface (stem or prop root) + status (live or dead) + season + flooded + random effects of tree and site × campaign; scale = class. This height form was selected by AIC over linear, logarithmic and untransformed-smooth alternatives (table S5). Profiles are shown for live, flooded stems in the wet season, excluding random effects, back-transformed with sinh. Adding species changed little (table S5). The same structure was fitted to woody CO2. This model is descriptive; stand budgets use the per site × campaign height fits (M12). Linear mixed-effects models of stem flux on species, height category, season and live/dead status with site as a random intercept (lme4/lmerTest; Type III tests; Tukey contrasts of estimated marginal means) were also fitted; the estimated marginal means by species and status are shown in fig. S6, C and D.

Budget uncertainty was propagated by Monte Carlo (5,000 draws). For CH4, component rates were drawn from normal distributions with standard errors from their bootstrap intervals; laser-scanned areas from lognormal distributions (coefficients of variation: ground 3%, root 20%, stem 12%); downed-wood area from a lognormal spanning the Krauss range; the stem height-fit parameters from their joint sampling distribution (slope capped at zero, intercept at the largest observed stem flux); and intact water-surface CH4 multiplied by a log-triangular tidal-phase factor (0.6–2.0, mode 1). For CO2, component respiration was drawn as normal with relative standard error from the chamber data (capped at 1; 0.5 for n ≤2), with Rd25 ~ U(1.28, 1.62), LAI lognormal (median 2.8, 97.5th percentile 5.55), light inhibition triangular (0.2, 0.3, 0.5), downed-wood area as above and GPP normal with its bootstrap standard deviation. Class intervals are the means of the campaign quantiles. Analyses used R (≥4.3); the complete workflow, from raw analyzer files to figures, is available in the code repository (data S1).

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

For the CH4 budget, intact-class uncertainty is dominated by the exposed-soil fraction and the scanned root area, and ghost-class uncertainty by water-surface flux variability (component standard errors in fig. S10). For net forcing, canopy leaf respiration (Rd25 and LAI) and tower GPP dominate the intact interval and the CH4 budget dominates the ghost interval. The intervals of the two states do not overlap: intact −5,233 to −1,939 and ghost +1,922 to +3,978 g CO2-eq m−2 yr−1 at GWP20 (−5,294 to −2,035 and +1,491 to +3,263 at GWP100). Structural choices outside the Monte Carlo are compared one at a time in text S9.

## Text S5. CO2 closure caveats

Bottom-up respiration may legitimately exceed tower-partitioned respiration, and chamber–airborne CO2 comparisons need care, for two reasons: airborne midday fluxes were converted to daily values while chamber respiration was measured in daytime and scaled to 24 h with the tower's within-month Q10; and tower-partitioned respiration extrapolates night-time respiration across daylight without light inhibition, whereas the bottom-up leaf term includes it. Lateral export of dissolved inorganic carbon removes respired carbon that neither the tower nor the chambers see, because both measure only exchange with the air. Closure was therefore judged as consistency within uncertainty rather than forced. Within-month Q10 (1.15) was preferred to the across-season tower value (1.8), which also carries phenology, water level and salinity, and to our stem-chamber value (4.3; 2.5–7.4), which is likely inflated because daytime stem efflux also follows sap flow.

## Text S6. Regenerating stand

Component rates were measured at BL60, but no scanned surface area or airborne end-member exists for regenerating forest, so it is reported at the component scale and excluded from the closed budgets. BL60 had the highest component CH4 rates of any class (soil 56, 95% CI 27–91; water 57, 33–89; stem 14, 3–34; root 5.7, 2.7–8.1 nmol m−2 s−1). Assigning BL60 stem and root areas midway between the ghost and intact classes and its measured inundated share (0.33 of depth readings with standing water) gives ~33 g CH4 m−2 yr−1 (32.7 exposed to 33.3 flooded), above both ghost (~12) and intact (~1.5) forest. This first-order estimate assumes intermediate structure and rests on pooled campaigns; it is not part of the closed budgets or forcing, but indicates that early regeneration need not be a low-emission state.

## Text S7. Porewater alkalinity and sulfate

Porewater alkalinity (October 2025, 0–90 cm) was 11–33 mM (site medians), five to fifteen times seawater: 11–12 mM at the intact sites, 16 mM at CP40 and 33 mM at BL60. Alkalinity at this level indicates that sulfate reduction was active at every site. At CP40, sulfate remained near 32 mM, the highest of any site, alongside the highest dissolved CH4: methane accumulated alongside sulfate reduction rather than after sulfate was exhausted, consistent with methanogenesis from substrates sulfate reducers do not compete for. Alkalinity–DIC slopes are not interpreted, because DIC was calculated from probe pH and alkalinity rather than measured, and each site has four depths (fig. S12).

## Text S8. Context sites

Single dry-season visits place the core results in a wider context (table S12); they are not comparable to the core-site annual budgets, which pool a wet and a dry campaign and are dominated in ghost forest by ponded water. The Marco Island ghost site lies in the Fruit Farm Creek die-off (Rookery Bay National Estuarine Research Reserve). A road built in the 1940s restricted tidal exchange there; the resulting prolonged flooding killed the trees over decades, and Hurricane Irma added further damage. A restoration project began in October 2021 and was completed in February 2023. Tidal exchange had not reached the die-off area at either of our visits (March 2022 and March 2023), so these measurements describe an old die-off before restoration. At both visits there was little or no standing water (0–1 cm at chambers), and porewater salinity was as high as at the core ghost sites (60 PSU at 40 cm). Compared within the same season, Marco Island soil CH4 (median 1.8–2.4 nmol m−2 s−1) was an order of magnitude below exposed ghost soil at FLM30 (median 17), while stem CH4 medians were similar (0.4 vs 0.6–0.7); the high stand emission at the core ghost sites comes mainly from ponded water, which Marco Island lacked during the dry season. Dry-season drawdown, decades since mortality with less labile carbon, and a thin organic layer over shell and carbonate sand (Radabaugh et al. 2020) are plausible explanations. Low CH4 from dead stands a few years after mortality has also been reported in Brazil (Pacheco et al. 2024). Ghost-forest CH4 therefore likely depends on hydrology and time since mortality, and the core-site values should not be assumed to transfer to all dead mangrove. Rookery Bay soil (1.0, 95% CI 0.5–1.7 nmol m−2 s−1) fell within the core intact range (0.6–12). The SE-1 scrub ecotone showed moderate prop-root (3.2) and low stem (0.09) and water (1.6; one placement) CH4, with net leaf CO2 uptake (−2.27 µmol m−2 s−1). Leaf chambers at SE-1 and BL60 (CO2 −2.27 and −1.25; CH4 0.01–0.03) corroborate the sign and magnitude of the leaf CO2 term and show that leaves are not a meaningful CH4 pathway.

## Text S12. Comparison with other studies of mangrove die-off

To our knowledge no chamber study has measured CH4 from hurricane-killed mangroves. The closest comparisons are climate-driven dieback in the Gulf of Carpentaria, where dead stems emitted eight times more CH4 than live ones (2.9 vs 0.43 nmol m−2 s−1 on average; 12.4 vs 1.1 at stem bases) and dead stems supplied ~26% of ecosystem CH4 (Jeffrey et al. 2019), close to our water-line stem contrast (12.5 vs 0.9) though stems were <1% of our stand budgets because bark area near the water line is small; the same dieback raised soil CO2 efflux by ~189% and halved DIC outwelling (Sippo et al. 2020); a freeze dieback in south Texas raised creek CH4 by 45% (Yu et al. 2023); and soils of dead *Avicennia* forest in the Tampamachoco lagoon, Veracruz, Mexico, impounded by a power-plant embankment in 1998 and partly reopened in 2011, emitted up to 0.93 mg CH4 m−2 h−1 (~16 nmol m−2 s−1) in the rainy season (Romero-Uribe et al. 2022), similar to our ghost-forest soil and water rates. Soil CO2 from cleared Belizean peat mangroves declined from ~7.6 to ~2.2 µmol m−2 s−1 over 20 years (Lovelock et al. 2011), consistent with a decaying pulse of labile carbon after mortality. Standing dead trees in coastal ghost forests elsewhere also add measurably to ecosystem greenhouse-gas emissions (Martinez & Ardón 2021).

**Regional benchmarks.** For intact forest, Caribbean and Gulf rates span near zero to tens of nmol m−2 s−1: near-zero soil emission in southwest Florida (Cabezas et al. 2018); 1.6 nmol m−2 s−1 in a protected Puerto Rico reserve and ~39 at an urban site (Martin et al. 2020) _[CHECK: means]_; 3–59 across zones in southwest Puerto Rico (Sotomayor et al. 1994) _[CHECK: values]_; and 1.7 nmol m−2 s−1 per unit stem area from healthy stems in Yucatán (Salas-Rabaza et al. 2023), comparable to our intact water-line stems (0.9). Our intact forest lies at the low end of this range. For dead or degraded forest, the impounded Veracruz stand (~16; Romero-Uribe et al. 2022) is close to our ghost rates; a freeze dieback in Texas raised creek CH4 by 45% (Yu et al. 2023); and dead stands in Brazil emitted little about three years after mortality (Pacheco et al. 2024). We found no comparable measurements from Cuba, where most of the mapped 2017 dieback lies.

## Text S9. Sensitivity to analytical choices

The central case combines the choices best supported by our data and the literature: flooding weighted by floor area over the campaign months, the within-month tower Q10, the Krauss downed-wood volume, LAI 2.8, and no tidal-phase or diel correction to CH4. Monte Carlo intervals propagate measurement and parameter uncertainty within that case; structural alternatives are compared one at a time in table S10. Three results hold under every choice: at GWP20 the ghost class is a net source (+2,400 to +4,360 g CO2-eq m−2 yr−1; +1,830 to +3,630 at GWP100), the intact class is a net sink on vertical exchange (−2,690 to −4,310), and intact CH4 emission is clearly non-zero (0.9–2.1 g CH4 m−2 yr−1). The switch spans ~5,600–8,000 g CO2-eq m−2 yr−1 at GWP20 (~5,100–7,300 at GWP100), and ~5,100–5,500 under the plausible carbon-balance framings. The largest levers on the intact budget are canopy leaf respiration (LAI), the downed-wood volume and the flooding representation, which also decides the leading intact CH4 pathway: soils (55%) and roots (28%) in the central case; roots (44%) under the all-or-nothing switch.

## Text S10. Net forcing under carbon-balance framings

On vertical exchange the intact class is a sink of −3,630 g CO2-eq m−2 yr−1 at GWP20 (−3,710 at GWP100), and its CH4 offsets 3% (GWP20) to 1% (GWP100) of the CO2 sink. Under the central lateral scenario (346 g C m−2 yr−1) the intact forest retains ~680 g C m−2 yr−1. With exported alkalinity retained in the ocean and the rest returned to the air, the intact sink is −2,710 g CO2-eq m−2 yr−1 at GWP20 (−2,960 to −1,710 across lateral scenarios), CH4 offsets 5% of it, and the switch is ~5,500 (~5,000 at GWP100). If all exported carbon returns to the air, the intact sink is −2,330 (−2,730 to −150) and the switch ~5,100. On storage alone (burial 123 plus wood increment 131 g C m−2 yr−1; Castañeda-Moya et al. 2013) the intact class is −810 and the switch ~3,600, a lower bound because carbon respired downstream or retained as ocean DOC counts as returned. As an independent check, lateral export should roughly equal net exchange minus wood increment minus burial (≈770 g C m−2 yr−1), which falls between the central and high scenarios (fig. S14). The intact sink stays negative in every coherent scenario and approaches neutrality only with the highest export and complete atmospheric return.

## Text S11. Ghost-forest carbon losses not measured

Our ghost-forest forcing is on-site vertical exchange five to six years after Irma and omits two terms that would add to the source. First, ghost-forest lateral export is unmeasured; after dieback elsewhere, losses shift from lateral outwelling toward atmospheric CO2 (soil CO2 +189%, DIC outwelling −50%; Sippo et al. 2020), and Shark River DIC and DOC fluxes fell by 45% and 27% for up to two years after Irma (Stegehuis et al. 2026), so ghost export is probably smaller than intact but not zero. Second, root death can trigger peat collapse: Honduran mangroves lost ~11 mm yr−1 of elevation after Hurricane Mitch (Cahoon et al. 2003), an Everglades site converted to mudflat by the 1935 hurricane lost ~75 cm (Osland et al. 2020), and topsoil carbon fell by ~2.7 kg C m−2 at a Naples Bay site affected by Irma (Griffiths & Mitsch 2021). Such a pulse would enlarge the switch in its first years; its magnitude at our sites is unknown, so it is discussed but not included.

---

# Supplementary Figures

{{FIG S1}}

{{FIG S2}}

{{FIG S3}}

{{FIG S4}}

{{FIG S5}}

{{FIG S6}}

{{FIG S7}}

{{FIG S8}}

{{FIG S9}}

{{FIG S10}}

{{FIG S11}}

{{FIG S12}}

{{FIG S13}}

{{FIG S14}}

{{FIG S15}}

{{FIG S16}}

{{FIG S17}}

---

# Supplementary Tables

{{TABLE S1}}

{{TABLE S2}}

{{TABLE S3}}

{{TABLE S4}}

{{TABLE S5}}

{{TABLE S6}}

{{TABLE S7}}

{{TABLE S8}}

{{TABLE S9}}

{{TABLE S10}}

{{TABLE S11}}

{{TABLE S12}}

{{TABLE S13}}

{{TABLE S14}}

{{TABLE S15}}

---

# Data S1 and S2

**Data S1.** Chamber flux dataset (762 analysed fluxes with geometry, fit statistics, detection limits, quality flags and ancillary variables) and data dictionary: combined_gas_flux_dataset.csv and data_dictionary.csv _[repository URL; ORNL DAAC archive in preparation]_.

**Data S2. External datasets used.**

{{DATA S2}}

---

# References

{{REFS}}
