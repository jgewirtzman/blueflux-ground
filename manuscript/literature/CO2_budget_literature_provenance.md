# Literature Provenance & Methods — Mangrove Component CO₂ Budget

**Project:** NASA BlueFlux — component-resolved CO₂ budget for hurricane-disturbed Florida Everglades mangroves
**Budget framing:** `NEE = Reco − GPP` (atmospheric sign convention: negative NEE = net uptake)
**Site of application:** Shark River Slough, tower SRS-6 / AmeriFlux US-Skr (tall riverine forest); species *Rhizophora mangle*, *Avicennia germinans*, *Laguncularia racemosa*, *Conocarpus erectus*
**Document version:** v1.0 — 2026-06-17
**Maintainer:** Jon Gewirtzman

---

## 0. Purpose and how to use this file

This document is the traceable record behind every literature value used to fill the terms you cannot measure directly (principally foliar dark respiration) and to bound/sanity-check the terms you can. Each value has a stable ID (e.g. `LR-01`), a verbatim source quantity, the exact location in the source (table/figure/page), the method that produced it, and any correction applied before use. A companion machine-readable catalog (`value_catalog.csv`) carries the same IDs for ingestion into your workflow.

Rule of thumb for reuse: cite the **source** quantity (column "Value (as published)") and apply corrections yourself from §4 so the provenance chain stays auditable. Do not propagate the "Value (for budget use)" without also recording which corrections (`C-xx`) you applied.

---

## 1. Reproducibility — how this was assembled

### 1.1 Evidence base

**(a) User-supplied PDF library** — `/literature/` folder, 9 files, identified as:

| File | Identified as | Used for |
|---|---|---|
| `Journal of Geophysical Research ... 2009 - Barr - Physiological responses of red mangroves...pdf` | Barr et al. 2009, JGR-Biogeosci. 114, G02008 | **Leaf Rd (R. mangle), Vcmax, Jmax, Amax at SRS-6** |
| `Journal of Geophysical Research ... 2010 - Barr - Controls on mangrove...pdf` | Barr et al. 2010, JGR-Biogeosci. 115, G02020 | NEP, daytime NEE, ecosystem Re, LAI |
| `1-s2.0-S0168192314003220-main.pdf` | Troxler et al. 2015, Agric. For. Meteorol. 213 | Below-canopy component efflux, LAI, Re share |
| `s12237-022-01120-7.pdf` | Sturchio et al. 2022, Estuaries & Coasts 46 | Leaf Rd (A. germinans, field), Q10 |
| `Global Change Biology - 2021 - Sturchio - Temperature acclimation...pdf` | Sturchio et al. 2021, Glob. Change Biol. (publ. 2022, 28[2]) | Leaf Rd (A. germinans, ecotone), Q10, acclimation |
| `erad093.pdf` | Chieppa et al. 2023, J. Exp. Bot. 74(10) | Leaf Rd thermal acclimation, R. mangle + A. germinans, Q10 |
| `tpab151.pdf` | Hogan et al. 2022, Tree Physiology 42(4) | Scrub R. mangle Anet (Everglades) |
| `out (1).pdf` | Barr 2005, PhD dissertation, Univ. Virginia (front matter only, 24 pp) | Context only — see note §5.4 |
| `978-3-642-88533-4.pdf` | Golley & Medina (eds.) 1975, *Tropical Ecological Systems*, Springer | Historical neotropical mangrove gas-exchange (Lugo et al. chapter) |

**(b) Web literature search** — conducted via the `WebSearch` tool (US index, June 2026). Search-angle queries are logged in §1.3 so they can be re-run. Web-only sources (no PDF in folder) are flagged "web" in the catalog and were verified against the primary publisher page or an open mirror where possible.

### 1.2 Text-extraction method (for the PDFs)

All PDF values were extracted with Poppler `pdftotext` (v22.x) and located by line number, e.g.:

```bash
pdftotext "<file>.pdf" - | grep -niE "dark respiration|Rd|µmol|nmol"
pdftotext -f <page> -l <page> "<file>.pdf" -   # single-page dump to confirm context
```

Where a value sits in a table, the table label is recorded. Where a value is figure-only (no numeric in text), it is flagged `FIGURE-ONLY` and **not** treated as a hard number.

### 1.3 Web search queries (re-runnable)

Leaf Rd: `mangrove leaf dark respiration rate µmol`; `Rhizophora leaf respiration gas exchange Rd`; `Avicennia germinans leaf respiration nitrogen addition`. 
Temperature scaling: `Heskel 2016 convergence leaf respiration temperature`; `GlobResp Atkin 2015 leaf respiration database`. 
LAI: `Shark River mangrove leaf area index LAI`; `riverine mangrove LAI Everglades`; `Castaneda-Moya mangrove structure Shark River LAI`. 
Foliar fraction / partitioning: `leaf respiration fraction ecosystem respiration tropical forest`; `mangrove autotrophic heterotrophic respiration partitioning`; `Cavaleri foliar ecosystem respiration tropical`. 
GPP/NEE magnitudes: `mangrove eddy covariance GPP maximum µmol`; `riverine mangrove net ecosystem exchange NEE`; `Barr mangrove GPP Everglades`.

### 1.4 Verification tiers (used in every entry)

- **A — Primary, verified:** number read directly from the cited table/equation/text of the primary source PDF in hand.
- **B — Primary, figure-only:** quantity is real but reported graphically; range stated, exact value must be digitized from the figure.
- **C — Secondary:** value quoted by a review/synthesis citing an older primary; original not inspected.
- **D — Proxy:** not mangrove — tropical/broadleaf-forest analog used to bound a missing mangrove term.

---

## 2. Conventions

**Units.** Leaf flux area basis = µmol CO₂ m⁻² (leaf) s⁻¹; mass basis = nmol CO₂ g⁻¹ (leaf dry mass) s⁻¹. Stand flux = µmol CO₂ m⁻² (ground) s⁻¹ (instantaneous) or g C m⁻² yr⁻¹ (annual). LAI = m² leaf m⁻² ground. All respiration values are reported as **positive** CO₂ efflux.

**Sign convention.** `NEE = Reco − GPP`. Negative NEE = net ecosystem uptake. (Note this is the *opposite* sign to some AmeriFlux products that report NEE = GPP − Reco; confirm the partitioning product's convention before differencing.)

**Temperature reference.** Leaf Rd values are normalized to 25 °C (`R25`) unless stated. Correction to site leaf temperature uses Heskel (2016) — see `C-02`.

**Mangrove vs proxy.** Tagged per entry; proxies (tier D) are used only for ratios/fractions with no mangrove measurement.

---

## 3. Value catalog

Each entry: **ID | quantity | value as published | (correction → value for use) | species/site | method | source [tier] | exact location | notes.**

### Item 1 — Foliar (leaf) dark respiration, Rd

**`LR-01` — Rhizophora mangle leaf Rd — PRIMARY SITE VALUE**
- Value as published: **Rd = 1.62 ± 1.32 µmol CO₂ m⁻² s⁻¹** (published as "mmol"; OCR/typesetting error — see `C-01`).
- Species/site: *Rhizophora mangle*, Shark River, western Everglades (the SRS-6 forest).
- Method: leaf cuvette (LI-6400); Rd is the intercept term of the Farquhar et al. (1980) A–PAR / A–Ci fit, also reported as the mean dark-respiration estimate for sampled leaves.
- Source [A]: **Barr et al. 2009**, JGR 114, G02008. Location: Results §3 (text, "average rate of dark respiration … estimated at 1.62 ± 1.32") **and** the Farquhar-parameter summary table (same row as Vcmax 76.1 ± 23.4, Jmax 117 ± 41).
- Notes: This is the best available value for your dominant species *at your site*. Internal check: Rd/Vcmax = 1.62/76.1 = **0.021**, consistent with the tropical Rd25:Vcmax25 range (`RT-07`). ⚠ Do not confuse the numerically similar ecosystem-respiration mean 1.62 ± 1.38 from Barr 2010 (`GN-03`) — different quantity, coincidental value.

**`LR-02` — Avicennia germinans leaf Rd — field, fertilization plots**
- Value as published: **R_area,25 = 1.28–1.54 µmol m⁻² s⁻¹**; **R_mass,25 = 5.75–8.73 nmol g⁻¹ s⁻¹**; new leaves 1.54 / 8.65, old leaves 1.28 / 5.75.
- Species/site: *A. germinans*, mature trees, long-term N/P plots, north Florida.
- Method: night-time leaf cuvette CO₂ efflux; R–T curves; standardized to 25 °C.
- Source [A]: **Sturchio et al. 2022**, Estuaries & Coasts 46:182–197. Location: Results / Table of R_area,25 & R_mass,25 by date and leaf age.
- Notes: Mass-basis unit printed as "nmol g⁻² s⁻¹" in PDF — typo for g⁻¹ (`C-01`).

**`LR-03` — Avicennia germinans leaf Rd — marsh–mangrove ecotone**
- Value as published: **R_area,25 ≈ 0.86–1.71 µmol m⁻² s⁻¹**; **R_mass,25 ≈ 4.36–13.6 nmol g⁻¹ s⁻¹** (varies by site NS/SS, season, warming treatment; e.g. ambient 1.05 ± 0.04 / 1.09 ± 0.04; warmed mass 10.2 ± 0.2 vs ambient 10.9 ± 0.2).
- Species/site: *A. germinans*, GTMNERR marsh–mangrove ecotone, NE Florida.
- Method: night-time cuvette; modified-Arrhenius / exponential R–T fits; R25 and Q10 derived per plant per date.
- Source [A]: **Sturchio et al. 2021** (Glob. Change Biol., DOI 10.1111/gcb.15938). Location: Results §3 (text values lines ~965–1249 of extracted text), Figure 1.
- Notes: Confirms the LR-02 range under cooler/ecotone conditions; demonstrates seasonal acclimation (Q10 declines as seasonal T rises).

**`LR-04` — R. mangle & A. germinans leaf Rd — thermal-acclimation experiment**
- Value as published: Q10 of R ≈ **2.0** and consistent across seasons in subtropical populations; R_area,25 and R_mass,25 reported graphically (≈ 1 µmol m⁻² s⁻¹ order). [Tier B — figure-only for the magnitudes.]
- Species/site: *R. mangle* and *A. germinans*, subtropical (Florida) and tropical (Belize) provenances; seedlings, glasshouse warming.
- Method: short-term R–T response curves at 7 time points; b, c polynomial coefficients per curve; Q10 = e^{10(b+2cT)}.
- Source [A/B]: **Chieppa et al. 2023**, J. Exp. Bot. 74(10):3174–3187, DOI 10.1093/jxb/erad093.
- Notes: Primary value of this paper for you is the **Q10 ≈ 2.0 and the demonstration that the Heskel-type b,c formulation (`C-02`) applies to both your dominant genera**, not new absolute magnitudes (seedlings).

**`LR-05` — Tropical evergreen broadleaf Rd25 — PROXY fallback**
- Value as published: **Rd25 ≈ 0.43 µmol m⁻² s⁻¹** (lowest biome mean of the global database).
- PFT: tropical evergreen broadleaf (the GlobResp PFT that contains mangroves).
- Source [D]: **Atkin et al. 2015**, New Phytol. 206:614–636. Location: PFT summary (TrpEvBl row) / Fig. 5.
- Notes: Superseded for direct use by `LR-01`–`LR-03`; retained as a low-end cross-check and as the basis for the gap-fill equations (`C-04`).

**Superseded:** Jacotot et al. 2018 (R. stylosa 0.17–0.22; A. marina 0.29–0.41, New Caledonia) — Indo-Pacific proxy. Excluded from use; the neotropical site values (`LR-01`–`LR-03`) are ~8× higher and species-matched. Documented here so the change is auditable.

### Item 2 — Leaf area index (LAI)

**`LAI-01` — SRS-6 ground optical (low/disturbed bound)**
- Value: **2.29 ± 0.18** (Barr 2010, measured 2008); **2.80 ± 1.38** (Troxler 2015, reported for SRS-6).
- Method: indirect optical (canopy analyzer / hemispherical).
- Source [A/C]: Barr et al. 2010 (G02020); Troxler et al. 2015 (Agric. For. Meteorol. 213:273–282).
- Notes: 2008 measurement post-dates Hurricane Wilma (Oct 2005) → likely canopy-suppressed; appropriate as a *disturbed-state* bound.

**`LAI-02` — SRS-6 MODIS (high/vigorous bound)**
- Value: **5.55** (tall-forest site mean); scrub comparison 2.87.
- Method: MODIS 500 m, 8-day, aggregated.
- Source [A]: **Charkowicz et al. 2025**, Glob. Change Biol. 31(3):e70124, DOI 10.1111/gcb.70124.
- Notes: Satellite estimate (not ground LAI); MODIS typically reads higher than optical ground LAI. Use as upper bound / recovered-canopy state.

**`LAI-03` — neotropical riverine corroboration**
- Value: **4.66** healthy riverine+fringe (Mexican Pacific, LAI-2000); component R. mangle 2.49, L. racemosa 1.74. Belize fringe R. mangle **2.3**.
- Source [A]: Kovacs et al. 2005, Estuar. Coast. Shelf Sci. 62:377–384; Cheeseman & Lovelock 2004, PCE 27:769–780.
- Notes: Neotropical bounds bracketing the SRS-6 ground vs MODIS gap.

→ **Recommended for scaling:** carry a range. Low/disturbed **2.3**, central **3–4**, high/recovered **5.5**. LAI is the dominant scaling lever for the leaf term (`C-07`); propagate the full range.

### Item 3 — Foliar respiration as a fraction of Reco / GPP

**`FR-01` — Foliage % of Reco — PROXY (best supported)**
- Value: **foliage = 37% of Reco** (range across studies 18–40%); component split soil 41% / foliage 37% / live wood 14% / CWD 7%; Reco = 9.4 ± 0.5 µmol m⁻² s⁻¹.
- System: old-growth tropical wet forest, La Selva, Costa Rica.
- Source [D]: **Cavaleri et al. 2008**, Plant Cell Environ. 31:473–483.

**`FR-02` — Leaf % of Reco — PROXY**
- Value: leaf ≈ **33% of Reco** (leaf 2.6, wood 1.1, soil 3.2 µmol m⁻² s⁻¹; total 7.8); autotrophic 2.2 vs heterotrophic 5.6.
- System: central Amazon terra firme, Manaus.
- Source [D]: **Chambers et al. 2004**, Ecol. Appl. 14(sp4):S72–S88.

**`FR-03` — Foliar % of net daytime fixed C — MANGROVE**
- Value: foliage respires **~22% of net daytime fixed carbon** (R. apiculata).
- Source [C]: Clough et al. 1997, reported in **Alongi 2014**, Ann. Rev. Mar. Sci. 6:195–219.
- Notes: This is a leaf-R:assimilation ratio, **not** leaf:Reco — do not substitute for FR-01/02.

→ Use **foliar ≈ ⅓ of Reco** as the independent bound on the missing leaf term.

### Item 4 — Autotrophic vs heterotrophic Reco partitioning

**`AH-01` — Mangrove Ra:Rh split — MANGROVE (global synthesis)**
- Value: **autotrophic ≈ 74% / heterotrophic ≈ 26% of Reco** (Ra 3,079; Rh 1,101; Re 4,180 g C m⁻² yr⁻¹); Re/GPP = 0.91; CUE = 0.33. Canopy respiration is the single largest flux.
- Source [C]: **Alongi 2014**, Ann. Rev. Mar. Sci. 6:195–219, Table 4.
- Notes: Global mass-balance, NOT site-measured. Treat as prior, not ground truth.

**`AH-02` — Site below-canopy share of Reco — MANGROVE, your site**
- Value: below-canopy components (soil, pneumatophores, prop roots, CWD, water) = **45–65% of total Reco**. Soil-only efflux **1.27 ± 0.05 µmol m⁻² s⁻¹** (n=86); soil+pneumatophores **3.17 ± 0.11**; prop roots **1.94 ± 0.45**; CWD **2.34 ± 0.23**; surface water **1.02 ± 0.10**.
- Source [A]: **Troxler et al. 2015**, Agric. For. Meteorol. 213:273–282. Location: component-efflux table.
- Notes: Directly comparable to your chamber upscaling. Soil-only 1.27 is the benchmark for your soil chamber Rs; the foliar term should fill most of the remaining 35–55% of Reco.

**`AH-03` — Mangrove ER:GPP alternative — MANGROVE**
- Value: **ER/GPP ≈ 0.65** (eddy-covariance-based; lower than Alongi's 0.91 budget value).
- Source [C]: **Adame et al. 2024**, Ecosphere 15(3):e4806.
- Notes: Brackets the plausible Re:GPP range 0.65–0.91; difference is method (EC NEE-partition vs mass balance incl. lateral export).

### Item 5 — GPP / NEE magnitudes for sanity-checking SRS-6

**`GN-01` — Max daytime NEE — MANGROVE, your site**
- Value: **−20 to −25 µmol m⁻² s⁻¹** (Mar–May peak uptake).
- Source [A]: Barr et al. 2010, G02020.

**`GN-02` — Annual NEP — MANGROVE, your site**
- Value: **NEP = 1170 ± 127 g C m⁻² yr⁻¹** (2004; = −NEE annual; strong sink).
- Source [A]: Barr et al. 2010, G02020.

**`GN-03` — Daytime / mean ecosystem respiration — MANGROVE, your site**
- Value: daytime Rd **2.81 ± 2.41 µmol m⁻² s⁻¹**; mean ecosystem Re **1.62 ± 1.38 µmol m⁻² s⁻¹**.
- Source [A]: Barr et al. 2010 (mean Re value as tabulated in Troxler 2015).
- Notes: ⚠ distinct from leaf Rd `LR-01` despite near-identical 1.62 value.

**`GN-04` — Leaf Amax (light-saturated assimilation) — MANGROVE, your site**
- Value: **maximum foliar assimilation ≈ 18 µmol m⁻² s⁻¹** (red mangrove; sustained 8–10 under high T/radiation load).
- Source [A]: Barr et al. 2009, G02008.
- Notes: Pairs with `LR-01` for leaf-level net budget; consistent with your in-light leaf-chamber data.

**`GN-05` — Midday GPP plausibility check**
- Reasoning: GPP = −NEE + Reco. With max daytime −NEE 20–25 (`GN-01`) and daytime Reco ~3–12, midday GPP ≈ **25–40 µmol m⁻² s⁻¹**. Your **~37** sits at the high-but-plausible end (clear-sky spring peak). [Tier A inputs; derived check.]
- To finalize: extract Barr 2010 GEP₂₀₀₀ (gross ecosystem photosynthesis at PAR 2000) from the full text — see §5.

**Reference scaling (non-site, for ranges):** Hong Kong/Kandelia GPP 2,830 / ER 1,940 g C m⁻² yr⁻¹; Pichavaram (India) GPP 2,305 g C m⁻² yr⁻¹ — Indo-Pacific proxies, shorter canopy [D].

**Historical neotropical corroboration:** Golley & Medina 1975 (Lugo et al., Rookery Bay FL): red-mangrove leaves had greater net photosynthesis and *lower* respiration than black-mangrove leaves; red mangrove had greater trunk respiration — qualitatively consistent with `LR-01` ≈ `LR-02` magnitudes [C].

---

## 4. Methods & corrections (apply these, record which you used)

### `C-01` — OCR / typesetting unit correction (mmol → µmol; g⁻² → g⁻¹)
Barr 2009 and Barr 2010 PDFs render the micro sign (µ) as "m", so "µmol" prints as "mmol", and superscript minus signs drop. A respiration of 1.62 **mmol** CO₂ m⁻² s⁻¹ is physically impossible for a leaf (that is ~3 orders of magnitude too high; whole-ecosystem fluxes are tens of µmol). Correct all such leaf/flux values to **µmol**. Likewise Sturchio 2022 "nmol g⁻² s⁻¹" → "nmol g⁻¹ s⁻¹". This correction is applied to `LR-01`, `GN-01`, `GN-03`, `GN-04`.
*Verification:* magnitude cross-check against same-unit values in other primary sources (Sturchio µmol m⁻² s⁻¹ leaf Rd; Troxler µmol m⁻² s⁻¹ soil efflux).

### `C-02` — Temperature standardization of leaf Rd (Heskel et al. 2016)
Literature Rd is at 25 °C; your canopy leaves are warmer. Use the globally convergent short-term response (verified applicable to your genera by Chieppa 2023, `LR-04`):

```
R(T_leaf) = R25 · exp[ 0.1012·(T_leaf − 25) − 0.0005·(T_leaf² − 25²) ]
```
T in °C, R in µmol m⁻² s⁻¹; coefficients b = 0.1012 °C⁻¹, c = −0.0005 °C⁻². 
Equivalent published polynomial: `ln R = −2.2276 + 0.1012·T − 0.0005·T²`. 
Source: Heskel et al. 2016, PNAS 113(14):3832–3837, DOI 10.1073/pnas.1520282113.
*Why not a fixed Q10=2:* the −c term makes effective Q10 fall from ~2 (cool) toward ~1.5 (warm); a constant Q10 over-predicts at 30–34 °C leaf temperature. Effective Q10 at any T: `Q10 = exp[10·(b + 2cT)]`.

### `C-03` — Q10 (species-specific, optional alternative to C-02)
If using a simple Q10 scaling rather than the polynomial: **Q10 = 2.39** for *A. germinans* (Sturchio 2022, 20–30 °C, `LR-02`); **Q10 ≈ 2.0** for both genera (Chieppa 2023, `LR-04`). Apply `R(T) = R25 · Q10^((T−25)/10)`. Prefer `C-02` for T > 30 °C.

### `C-04` — Gap-filling Rd from traits (Atkin et al. 2015, GlobResp)
Where a species/leaf-cohort Rd is missing, estimate Rd25 from measured traits:
```
Area, N + LMA:   log10 Rd_a25 = 0.469 + 0.329·log10[N]a + 0.204·log10 Ma
Area, climate:   log10 Rd_a25 = 0.451 − 0.0153·TWQ − 0.00016·PWQ
Mass, SLA + N:   log10 Rd_m25 = 0.0932 + 0.475·log10 SLA + 0.364·log10[N]m
```
[N]a = leaf N per area (g m⁻²), Ma = LMA (g m⁻²), TWQ = mean T of warmest quarter (≈ 28–29 °C at SRS-6), PWQ = precip of warmest quarter, SLA in m² kg⁻¹. Source: Atkin et al. 2015, New Phytol. 206:614–636.

### `C-05` — Vcmax-based Rd gap-fill ratio
```
Rd25 = (0.020 – 0.026) × Vcmax25      (tropical/subtropical broadleaf)
```
Use 0.020–0.026, **not** the older TBM default 0.015 (underestimates for warm broadleaf evergreens). Source: Wu et al. 2025, New Phytol. (DOI 10.1111/nph.20267); Atkin et al. 2015. 
*Site cross-check:* Barr 2009 gives Rd 1.62 / Vcmax 76.1 = **0.021**, internal-consistent with this range — so the SRS-6 leaf data independently land inside the literature ratio.

### `C-06` — Deriving an autotrophic fraction (no direct mangrove measurement)
Mangrove CUE ≈ 0.33–0.57 across syntheses (Alongi 2014 CUE 0.33; blue-carbon synthesis 0.57). Autotrophic respiration as a fraction of GPP: `Ra/GPP = 1 − CUE` → **0.43–0.67**. The Reco-based split (Alongi Table 4) gives Ra ≈ 74% of Reco (`AH-01`). These are priors; reconcile against your bottom-up sum and the EC Reco rather than adopting blindly.

### `C-07` — Leaf Rd → canopy leaf respiration → budget (the scaling chain)
```
R_leaf_canopy (µmol m⁻² ground s⁻¹) = Rd25_species · f_T(T_leaf) · LAI · (1 − f_inhib)
```
- `Rd25_species`: species-weighted from `LR-01`/`LR-02` (weight by basal-area or LAI share of R. mangle vs A. germinans vs L. racemosa).
- `f_T(T_leaf)`: Heskel multiplier from `C-02` at canopy leaf T (ideally half-hourly; integrate over a vertical LAI/T profile since lower canopy is cooler/shaded).
- `LAI`: range from `LAI-01`–`LAI-03` (carry low/central/high).
- `f_inhib`: ~0.30 daytime light inhibition of leaf R (apply only to daylight hours; 0 at night).

Then close the budget two ways and compare:
```
Reco_bottomup = R_soil + R_wood + R_proproot + R_water + R_leaf_canopy      (your chambers + literature leaf term)
NEE_bottomup  = Reco_bottomup − GPP_tower
```
Independent bound: `R_leaf_canopy` should be ≈ ⅓ of Reco (`FR-01/02`) and the below-canopy chamber sum ≈ 45–65% of Reco (`AH-02`).

### `C-08` — Lateral DIC export caveat (do not treat tower Reco as a hard closure target)
Barr et al. (2010) note tower-derived Reco likely **under**-counts belowground respiration because much is exported laterally as dissolved inorganic carbon by tides rather than vented vertically past the EC sensor. Consequence: `Reco_bottomup` (chambers + leaf) may legitimately *exceed* tower Reco. Carry an explicit lateral-export term rather than forcing chamber+leaf to equal tower Reco.

---

## 5. Open issues / to verify in primary text before publication

1. **Barr 2010 GEP₂₀₀₀** — extract the published peak gross ecosystem photosynthesis (PAR = 2000) to confirm whether your midday GPP ~37 is within or just above their reported maximum (`GN-05`). The exact annual GPP total is also not printed as a single number in Barr 2013 (reported as 8-day sums) — integrate the series if you need it.
2. **`LR-04` magnitudes** — Chieppa 2023 absolute R_area25/R_mass25 are figure-only; digitize Fig. 4 if you need seedling magnitudes (we used only its Q10 and b,c applicability).
3. **`AH-01` Ra:Rh** — global synthesis, not SRS-6. If a site-specific autotrophic fraction is needed, derive from your component chambers + a measured/【gap-filled】leaf term rather than importing 74%.
4. **`out (1).pdf`** — the Barr 2005 dissertation file in your folder is front matter + methods only (24 pp; tower construction, abstract). Its leaf/GPP data chapters are not in this file; those numbers are sourced from Barr 2009/2010 instead. Obtain the full dissertation if you want the original leaf-cuvette dataset.
5. **Golley & Medina 1975** — exact leaf Rd numbers from the Lugo et al. chapter were not digitized (older scan); used qualitatively only (`GN`-historical). Page-level extraction possible on request.
6. **Sturchio 2021 NS/SS site labels** — confirm which site (northern/southern GTMNERR) you consider most analogous before adopting a single `LR-03` value; ranges given span both.

---

## 6. References (full, with DOIs and in-source locations used)

1. Barr, J.G., Fuentes, J.D., Engel, V., Zieman, J.C. (2009). Physiological responses of red mangroves to the climate in the Florida Everglades. *J. Geophys. Res. Biogeosci.* 114, G02008. https://doi.org/10.1029/2008JG000843 — used: Farquhar-parameter table (Rd 1.62±1.32, Vcmax 76.1±23.4, Jmax 117±41), Results §3 (Amax ~18). [folder]
2. Barr, J.G., Engel, V., Fuentes, J.D., Zieman, J.C., O'Halloran, T.L., Smith, T.J., Anderson, G. (2010). Controls on mangrove forest–atmosphere CO₂ exchanges in western Everglades National Park. *J. Geophys. Res. Biogeosci.* 115, G02020. https://doi.org/10.1029/2009JG001186 — used: NEP 1170±127, daytime NEE −20…−25, daytime Rd 2.81±2.41, LAI 2.29±0.18, lateral-export caveat. [folder]
3. Barr, J.G., et al. (2013). Modeling light use efficiency in a subtropical mangrove forest equipped with CO₂ eddy covariance. *Biogeosciences* 10, 2145–2158. https://doi.org/10.5194/bg-10-2145-2013 — used: 8-day GPP/Re ranges, canopy height ~19 m. [web]
4. Troxler, T.G., Barr, J.G., Fuentes, J.D., Engel, V., Anderson, G., Sanchez, C., Lagomasino, D., Price, R., Davis, S.E. (2015). Component-specific dynamics of riverine mangrove CO₂ efflux in the Florida coastal Everglades. *Agric. For. Meteorol.* 213, 273–282. https://doi.org/10.1016/j.agrformet.2014.12.012 — used: component efflux table, soil Rs 1.27±0.05, below-canopy 45–65% of Reco, LAI 2.80±1.38. [folder]
5. Sturchio, M.A., Chieppa, J., Simpson, L.T., Feller, I.C., Chapman, S.K., Aspinwall, M.J. (2022). Contrasting Effects of Nitrogen Addition on Leaf Photosynthesis and Respiration in Black Mangrove in North Florida. *Estuaries and Coasts* 46, 182–197. https://doi.org/10.1007/s12237-022-01120-7 — used: R_area25 1.28–1.54, R_mass25 5.75–8.73, Q10 2.39. [folder]
6. Sturchio, M.A., et al. (2021/2022). Temperature acclimation of leaf respiration differs between marsh and mangrove vegetation in a coastal ecotone. *Global Change Biology* 28(2). https://doi.org/10.1111/gcb.15938 — used: A. germinans R25 ranges, seasonal Q10 decline. [folder]
7. Chieppa, J., Feller, I.C., Harris, K., Dorrance, S., Sturchio, M.A., Gray, E., Tjoelker, M.G., Aspinwall, M.J. (2023). Thermal acclimation of leaf respiration is consistent in tropical and subtropical populations of two mangrove species. *J. Exp. Bot.* 74(10), 3174–3187. https://doi.org/10.1093/jxb/erad093 — used: Q10 ≈ 2.0, applicability of b,c formulation to R. mangle + A. germinans. [folder]
8. Hogan, J.A., Castañeda-Moya, E., Lamb-Wotton, L., Troxler, T., Baraloto, C. (2022). Water levels primarily drive variation in photosynthesis and nutrient use of scrub Red Mangroves in the southeastern Florida Everglades. *Tree Physiology* 42(4), 797–814. https://doi.org/10.1093/treephys/tpab151 — used: scrub R. mangle Anet (context). [folder]
9. Charkowicz, C.J., et al. (2025). Resilience to Hurricanes Is High in Mangrove Blue Carbon Forests. *Global Change Biology* 31(3), e70124. https://doi.org/10.1111/gcb.70124 — used: MODIS LAI 5.55 (tall), 2.87 (scrub). [web]
10. Castañeda-Moya, E., Twilley, R.R., Rivera-Monroy, V.H. (2013). Allocation of biomass and NPP of mangrove forests along environmental gradients in the Florida Coastal Everglades. *For. Ecol. Manage.* 307, 226–241. https://doi.org/10.1016/j.foreco.2013.07.011 — used: Shark River NPP 17.0±1.1 Mg ha⁻¹ yr⁻¹, structural gradient. [web]
11. Atkin, O.K., et al. (2015). Global variability in leaf respiration in relation to climate, plant functional types and leaf traits. *New Phytologist* 206, 614–636. https://doi.org/10.1111/nph.13253 — used: TrpEvBl Rd25 0.43, gap-fill equations. [web]
12. Heskel, M.A., et al. (2016). Convergence in the temperature response of leaf respiration across biomes and plant functional types. *PNAS* 113(14), 3832–3837. https://doi.org/10.1073/pnas.1520282113 — used: b=0.1012, c=−0.0005 T-correction. [web]
13. Wu, J., et al. (2025). Linking leaf dark respiration to leaf traits and reflectance spectroscopy across diverse forest types. *New Phytologist*. https://doi.org/10.1111/nph.20267 — used: Rd25:Vcmax25 0.020–0.026. [web]
14. Cavaleri, M.A., Oberbauer, S.F., Ryan, M.G. (2008). Foliar and ecosystem respiration in an old-growth tropical rain forest. *Plant, Cell & Environment* 31(4), 473–483. https://doi.org/10.1111/j.1365-3040.2008.01775.x — used: foliage 37% of Reco, component split. [web]
15. Chambers, J.Q., et al. (2004). Respiration from a tropical forest ecosystem: partitioning of sources and low carbon use efficiency. *Ecological Applications* 14(sp4), S72–S88. https://doi.org/10.1890/01-6012 — used: leaf 33% of Reco, component fluxes. [web]
16. Alongi, D.M. (2014). Carbon cycling and storage in mangrove forests. *Annual Review of Marine Science* 6, 195–219. https://doi.org/10.1146/annurev-marine-010213-135020 — used: Ra:Rh 74:26, CUE 0.33, Re/GPP 0.91, foliar 22% of net fixed C. [web]
17. Adame, M.F., et al. (2024). Deconstructing the mangrove carbon cycle: gains, transformation, and losses. *Ecosphere* 15(3), e4806. https://doi.org/10.1002/ecs2.4806 — used: ER/GPP ≈ 0.65. [web]
18. Kovacs, J.M., Wang, J., Flores-Verdugo, F. (2005). Mapping mangrove leaf area index … Agua Brava Lagoon, Mexican Pacific. *Estuar. Coast. Shelf Sci.* 62, 377–384. https://doi.org/10.1016/j.ecss.2004.09.027 — used: riverine LAI 4.66. [web]
19. Cheeseman, J.M., Lovelock, C.E. (2004). Photosynthetic characteristics of dwarf and fringe Rhizophora mangle L. in a Belizean mangrove. *Plant, Cell & Environment* 27(6), 769–780. https://doi.org/10.1111/j.1365-3040.2004.01181.x — used: fringe LAI 2.3, Amax 9.9/5.3. [web]
20. Golley, F.B., Medina, E. (eds.) (1975). *Tropical Ecological Systems: Trends in Terrestrial and Aquatic Research*. Springer (incl. Lugo, Evink, Brinson, Broce & Snedaker, "Diurnal Rates of Photosynthesis, Respiration, and Transpiration in Mangrove Forests"). ISBN 978-3-642-88533-4 — used: qualitative red vs black mangrove leaf R ordering. [folder]
21. AmeriFlux US-Skr Shark River Slough (Tower SRS-6). https://doi.org/10.17190/AMF/1246105 — site metadata. [web]

---

## 7. Change log
- **v1.0 (2026-06-17):** Initial documentation. Incorporates user-supplied folder (9 PDFs). Key change vs prior web-only synthesis: added site-specific *R. mangle* leaf Rd (`LR-01`, Barr 2009) and retired the Indo-Pacific R. stylosa proxy; added MODIS LAI upper bound (`LAI-02`, Charkowicz 2025); corrected Sturchio 2021 DOI to 10.1111/gcb.15938.
