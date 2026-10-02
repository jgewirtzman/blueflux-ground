# Draft emails — October 2026

Drafts only; nothing has been sent. Fill the bracketed fields before sending.

---

## 1. Erin Delaria (cc Addie Colwell, Glenn Wolfe) — CARAFE

**Subject:** CARAFE two-class fluxes for the chamber paper (all five deployments)

Hi Erin,

Thanks again for the July run, the footprint files and the script. Here's the plan for the airborne data in the chamber paper, and what I'd need.

**How we'd use CARAFE.** Deployment by deployment, the chambers and CARAFE agree within uncertainty. In October 2022 ghost is 36 (chambers) vs 51 ± 27 nmol m⁻² s⁻¹ (CARAFE); in March 2023 (mean of Feb and Apr) ghost is 11 vs 5 ± 10 and intact is 1.7 vs −0.3 ± 9. Net CO2 agrees too. The one exception is October intact: 4 vs 29 ± 16. Our chambers missed the summer, and July 2024 is where CARAFE fills that gap. So I'd like CARAFE to carry the landscape budget for intact vs ghost across the season, with the chambers doing the process attribution (stems, roots, water, soil, downed wood) and the mechanism. The airborne data would appear as:
1. **A results panel:** CO2 and CH4 for intact vs ghost mangrove at each of the five deployments (Apr 2022, Oct 2022, Feb 2023, Apr 2023, Jul 2024). This would be the first presentation of July 2024 for this comparison.
2. **The closure comparison:** chambers × laser-scanning areas against CARAFE for October 2022 and March 2023.
3. **A seasonal landscape budget** for intact vs ghost built from the deployments, if you think that's defensible.

**The ask.** Could you run the two-class disaggregation (mangrove forest vs ghost forest; regenerating left out, since it isn't separable) with flights merged per deployment, as before, for all five deployments? For each deployment and class, CO2 and CH4, I'd need:
- flux (µmol / nmol m⁻² s⁻¹) with its uncertainty, and whether that's ±1 SE, 1σ or 2σ;
- the number of footprints (or km of flight) informing each class;
- if easy, homogeneous-footprint means (≥80 % one class) as a cross-check.

A small CSV or Excel table is perfect; I'll make the figures here.

**Two questions on going from deployments to an annual number:**
- How would you recommend converting midday fluxes to daily values? We currently use your Fig. S24a factor for CO2. Is there an equivalent for CH4, or is CH4 roughly flat over the day?
- Does the BlueFlux 500 m gridded product (the one Cheryl's paper uses: ~0.12 Mg CH4 ha⁻¹ yr⁻¹ for stable mangrove) give annual values for the ghost-forest class? If so, could we use intact vs ghost annual values from it, in addition to the deployment means?

**On July:** you mentioned the ghost-forest values looked odd, with more coastal, saltwater-influenced coverage, and in the three-class run mangrove and ghost CH4 were nearly identical (~46 and 45 nmol). If you have a quick view on whether that biases the two-class ghost estimate, a sentence would help for the text.

**For the methods (and publication, if it suits you):** the script version and class map; the footprint model; the flight days per deployment; and the QC filters (altitude < 150 m, the 14:00–20:00 UTC window, quality flags). I'll cite DAAC 2327 for the flux files. If the footprint layers and script could go into a repository with the paper, that would be ideal, but "available on request" is fine.

I'm hoping to circulate a full draft in [month], so having these by [date] would be wonderful.

Best,
Jon

---

## 2. David Lagomasino — regional map

**Subject:** Re: Just a reminder about sending the map!

Hi David,

Thanks so much for the map; it's exactly what I needed. I'm using it as a first-order regional scaling: the area of mangrove that showed little recovery after the 2017 storms, multiplied by our per-area change in net forcing from intact to ghost forest. That comes to about 1.1 Tg CO2-eq per year on a 20-year GWP (0.7–1.6), or 1.0 on a 100-year GWP. In the discussion it'll be framed as a scaling estimate, noting that the regional product and our plot measurements differ in resolution and method.

A few quick questions so I describe it correctly:
1. **Area:** recomputed in an equal-area projection, I get 173 km² in total (Cuba 104, US 60, Puerto Rico 5.4, Venezuela 2.5). The `area` attribute sums to 206 km², which I think is planar Mercator. Does that sound right?
2. **The `median` attribute** (values 1–6): what does it represent?
3. **Description and citation:** how should I describe and cite the product? Something like "CIFOR short-term mangrove loss after the 2017 hurricane season, on the Global Mangrove Watch v1 baseline", plus its resolution and how "little recovery" was defined (time window, threshold).
4. **Optional:** if you have total mangrove area for the same region handy (e.g. from GMW), I'd like to give the affected fraction. No worries if not.

Thanks again!
Jon

---

## 3. Yale analytical lab — porewater inorganic N

**Subject:** BlueFlux porewater inorganic N — quick questions

Hi [name],

Thank you for running the inorganic N on our Everglades porewater samples; these are really useful. Before they go in a paper, I'd like to check a few things:

1. **Dilution:** were any of our 20 samples (SRS5, SRS6, CP40, BL60) diluted before analysis? Some are saline (up to ~55 PSU), and we ran the same porewater 1:10 for IC. If they were diluted, are the reported mg N/L values already corrected for it? (For context, our CP40 values of 200–320 µM NH4 are plausible as reported. Our intact-site values match the long-term FCE LTER record at the same sites, a few µM.)
2. **Negative values:** all the SRS5/SRS6 values, NO3 and NH4, are negative (about −0.2 to −0.5 mg N/L), while the near-zero CP40/BL60 values sit around 0. Were the SRS samples run in a separate batch, or with a different blank or calibration? Should we treat them as below detection?
3. **Methods:** what were the method, instrument, detection limits and any matrix (salinity) corrections, so I can describe them?
4. **Sample label:** one CP40 sample is labelled "CP40 C". I'm assuming it's the 0 cm sample; can you confirm?

Thanks so much!
Jon

---

## 4. Lizzy Powell and Lin Xiong — TLS

**Subject:** TLS surface areas for the BlueFlux chamber paper — a few checks

Hi Lizzy and Lin,

I'm finalising the chamber-scaling paper, which uses your TLS surface areas (the April "finalised" set: inverted-root totals of 419.0 m² at SRS5 and 412.7 m² at SRS6, plus the trunk and branch areas by 0.5 m height bin). A few things to check so we stay consistent with your paper:

1. **Final numbers.** Are the April values final? Table 1 of the current TLS draft lists root areas of 264.8 and 240.6 m², which match neither version I have. If the final root areas drop that much (about 40 %), our intact CH4 falls about 10 %. Please send the final per-bin values when they're ready, and I'll rerun.
2. **Plot area at SRS5:** 0.18 ha or 0.1767 ha?
3. **Which scans?** The ORNL DAAC file names suggest SRS5 was scanned 21 Oct 2022, SRS6 15 and 18 Oct 2022, CP40 10 Mar 2023 and FLM30 12 Mar 2023. Are those the scans behind the surface areas? Are the file-name times local clock time?
4. **Ground under water.** The draft says CP and FLM were scanned under flooded conditions, and the ground is the lowest visible points after the low-artifact filter. Since the scanner is near-infrared, I'm treating the "ground" in flooded patches as the water surface at scan time. I correct our waterline model for that using water depth at scan time: from the FCE logger at SRS5/SRS6 (about 1 cm and 7 cm then), and our own depth readings at CP40/FLM30 (about 10 cm and 2 cm). Does that match your understanding? If the RayCloudTools terrain was instead interpolated to the sediment, I'll drop the correction. It's a small effect either way.
5. **Roots at the ghost sites.** The draft attributes CP/FLM root estimates to misclassified basal stems, since those stands are mostly *A. germinans*. But our four root chambers at FLM30 were on dead *Rhizophora mangle* prop roots, so some real prop roots are present there. You may want to soften that sentence. Ghost root area is tiny either way, so it doesn't change our results.

Thanks so much. Happy to share our scaling code or numbers if useful for your paper.

Best,
Jon

---

## 5. Cheryl Doughty — EF paper

**Subject:** EF paper — one citation fix, and aligning our CH4 numbers

Hi Cheryl,

I read the latest EF draft. Thanks for sharing it. Two small things:

1. **GWP citation.** The GWP of 28 is cited to the IPCC impacts report (WGII). The GWP values are in the physical-science report, AR6 WGI Ch. 7 (Forster et al. 2021, Table 7.15). There, the 100-year GWP for non-fossil CH4 is 27.0 (29.8 for fossil); 28 is the AR5 value without carbon-cycle feedbacks.
2. **Aligning our numbers.** Your stable-mangrove CH4 (0.12 Mg ha⁻¹ yr⁻¹, ~12 g m⁻² yr⁻¹) comes from the BlueFlux 500 m product, so it's an aircraft-scale annual value. It's consistent with the CARAFE deployment means (~15 nmol m⁻² s⁻¹, more in the wet season). Our chamber stand-level estimate for intact forest is lower (~1.5 g m⁻² yr⁻¹), mainly because our two campaigns missed the summer peak. In our paper, CARAFE will carry the seasonal landscape budget and the chambers the process attribution. So the two papers should tell a consistent story, and I'll cite yours for the regional context. If you'd like to compare notes on wording, happy to.

Best,
Jon

---

## 6. Jordan Peccia, William Chen, Irvan Luhung — metagenomes (status check)

**Subject:** BlueFlux sediment metagenomes — status and author list

Hi Jordan, William and Irvan,

The ghost-forest paper draft is coming together. I've included William, Jordan and Irvan on the author list (Jordan and Irvan's places to confirm), and set up a placeholder for the metagenome results: either a Fig. 5 panel or an SI figure, laid out by site and depth. It covers methanogen and methane-oxidiser families, marker genes per gram of sediment (mcrA, mttB, pmoA/mmoX), DNA yield and organic carbon, and genes against porewater CH4 and NH4.

Two things that may interest you: porewater NH4 at CP40 is 200–320 µM, versus a few µM at the intact sites, and it tracks dissolved CH4 closely. If methylamine or osmolyte breakdown is part of the story, the methylotrophic (Methanosarcinaceae) signal could connect directly.

Could you let me know:
1. a realistic timeline for normalised results (per gram sediment and per g organic C; Mingyu is running the organic carbon);
2. a methods paragraph (extraction kit, input, sequencing platform and depth, profiling and annotation tools);
3. whether you'd prefer the results as a main-text panel or in the SI.

Thanks!
Jon
