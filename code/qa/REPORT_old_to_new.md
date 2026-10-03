# Legacy → rebuild: what changed, by how much, and proposed manuscript edits

Branch `claude/flux-workflow-rebuild`, 2026-10-02. All numbers come from the
current outputs of `Rscript run_all.R`. Legacy numbers come from the frozen
baseline in `output/qa/baseline/`. The supporting tables are:

| File | What it holds |
|---|---|
| `output/qa/report_component_class_campaign.csv` | n, mean, median, % below detection, % QC-flagged, detection classes, per gas × component × class × campaign |
| `output/qa/report_manuscript_results_diff.csv` | Every line of `manuscript/text/manuscript_results.txt` whose numbers moved |
| `output/qa/net_forcing_attribution.csv` | Net-forcing change split by component |
| `output/qa/ebullition_vs_legacy_*.csv` | Water placements, legacy vs stage 04 |
| `output/qa/scripted_windows_review.{csv,pdf}` | 92 tree closures on scripted windows that differ from legacy by > 20 % |

Section 4 lists the manuscript edits (approved and applied).

---

## 1. Why the numbers move

Each item below is a data or processing correction. Each is recorded as a
tracked table in `data/flux_metadata/` or in pipeline code.

**Wrong traces**

The legacy pipeline fitted these closures on the wrong part of the record:

- **Cross-analyzer merges.** `import2RData(merge = TRUE)` mixed analyzers' rows,
  so some fits used another analyzer's data. Examples:
  - 2022-10-16 SRS6, crew JG/SC, recorded on LGR3 (not LGR2 as entered).
  - Three "Picarro" SRS5 water closures that were LGR tree closures.
- **The LGR2 2022-10-23 clock offset was wrong** (−1091 s). The legacy CP40
  October placements were therefore matched to the wrong closures.
- **Legacy re-trims onto a neighbour.** Five windows re-trimmed to replace a
  negative flux had been moved onto a neighbouring closure's trace. Examples:
  - Mar_23_90: 21.3 → −0.07 (its own trace).
  - Mar_23_137: 20.4 → −0.03.
- **Saved windows on another closure.** 17 saved windows lay on another
  closure's trace, across a data gap, or across a chamber lift.
- **Start-time typos (4).** Each was checked against the scanned or
  photographed datasheet. Example: Mar_22_T1_78, logged 15:05 instead of
  13:05, was fitted on soil 31's closure (60.9 → 0.08).
- **One-closure day offset.** The LGR2 offset for 2023-03-17 was taken from a
  single floating-chamber window, which shifted the SE1 leaf windows by 4 min.

**Duplicates and non-measurements, now excluded**

| Exclusion | What it was |
|---|---|
| Oct_22_278-283 | Re-entry of the 12 March 2023 SRS5 stems |
| BL60 water 168-177 | The 2022-03-24 BL60 *soil* sheet, re-entered as water dated 3/22/23 |
| Closures with no trace of their own | Oct_22_7, 8, 23 (later re-included), 25, 256 ("possible leak, redone") and Mar_23_21 |
| Water 122 | Analyzer fit failure at high CH4 (agreed 2026-10-01) |

**Water surfaces: one flux per floating-chamber placement (stage 04)**

| Before (legacy) | Now (stage 04) |
|---|---|
| 89–98 water rows | 47 placements |
| Diffusive and ebullitive split by a hand-built jump detector | goAquaFlux (goFlux fork 2ed7224, de-ebulliated diffusive window) |

- Long runs use their first 10 min for the diffusive flux. Ebullition is
  counted over the whole placement.
- The 40 legacy "added" placements were mostly duplicates or stem traces. Five
  real unlogged placements were kept.

**Picarro G4301**

The analyzer measures one gas per logged row. Fits, noise and bubble detection
now use only each gas's fresh readings. For the Picarro, ebullition is not
separable, so the total is the two-point flux.

**Detection limits and QC**

Detection uses an empirical MDF = 1.96 σ / t × flux term. σ is the MAD of
first differences within the window, centred per closure and pooled per
analyzer × campaign (works around jgewirtzman/fluxqc#1 and #2). fluxqc QC
screens are carried as flags, not applied.

**Conventions (Jon, 2026-10-01)**

- Tower air temperature and pressure for every closure.
- Hutchinson–Mosier (HM) model only with ≥ 30 points.
- No H2O dilution correction.
- Mar 2022 pilot chambers (R2, RA, pneumatophore) excluded.

**Gap-filled water fluxes**

Water CH4 and CO2 at SRS5/SRS6 in October 2022 come from dissolved gas × k600.
Legacy used the three mislabelled tree closures.

---

## 2. Headline numbers

| Quantity | Legacy | Rebuild |
|---|---|---|
| Fluxes in the analysis (study sites) | 867 in the text; 855 in the baseline | 752 |
| Water fluxes | 92 | 47 placements |
| Stem CH4, nmol m⁻² s⁻¹ (95 % CI) | 8.0 (4.9–12.1), n = 486 | 8.0 (4.8–12.1), n = 476 |
| Intact stem CH4 | 0.5 | 0.29 |
| Ghost / regenerating stem amplification vs intact | 23× / 28× | 41× / 50× |
| Water CH4, regenerating | 34.5 | 56.9 |
| Ebullitive share of water CH4 | CP40 20.4 %, BL60 7.3 %, FLM30 5.0 % | CP40 34.0 %, BL60 3.9 %, FLM30 not separable (Picarro); 14.1 % overall; bubbles also in 5 of 32 dry-season placements |
| Intact CH4, g m⁻² yr⁻¹ | 2.2 | 1.5 (area-weighted flooding; 0.9–2.2 across choices, Table S10) |
| Ghost CH4, g m⁻² yr⁻¹ | 11.7 | 10.7 |
| Intact CO2, g m⁻² yr⁻¹ | −4,713 (−1,285 g C) | −3,772 (−1,030 g C; tower 2022–23 −1,288 g C) |
| Ghost CO2, g m⁻² yr⁻¹ | +1,437 | +1,775 |
| Intact net forcing, 100 yr | −4,653 | −3,731 (vertical); −1,841 NECB with alkalinity retained (S.T10) |
| Ghost net forcing, 100 yr / 20 yr | +1,763 / +2,386 | +2,107 / +2,741 (GWP* +3,104) |
| Sink-to-source switch | ~6,400–6,900 | ~6,400 GWP20 / ~5,800 GWP100 (vertical); ~5,500 / ~4,900 (NECB, alkalinity retained, central lateral scenario) |
| Monte Carlo, GWP100 | intact −6,133…−3,521; ghost +1,474…+2,045 | intact −5,309…−2,052; ghost +1,453…+3,213 (still disjoint) |
| CH4 share of forcing | intact 1.3 % / 3.6 %; ghost 18.5 % / 39.8 % | intact 1.1 % / 3.1 % (vertical), 1.8 % / 5.2 % (NECB, alkalinity retained); ghost 15.8 % / 35.2 % |
| Regenerating budget, g m⁻² yr⁻¹ | ~29 | ~33 |

The intact net forcing change is +250 (a weaker sink). Of that, in g CO2 m⁻² yr⁻¹:

- **Tower GPP: +1,188.** Legacy tower GPP was clipped at zero, which put
  1.2–1.4 µmol m⁻² s⁻¹ of GPP into the night. Unclipped, with GPP = 0 at night,
  the 24-h mean is 7.8 instead of 8.7.
- **Soil: −804.** The floor was flooded 98–100 % of the time in Oct 2022 and
  ~70 % in Mar 2023 (FCE LTER water level), not 50 %.
- **Stem respiration: −644.** The breath bump (Mar_23_191) and the mixed-analyzer
  SRS6 fits are gone, partly offset by the 24-h temperature correction (Q10 1.15).
- **Leaf: +242.** Central LAI 2.8 (SRS-6 ground) instead of 2.3.
- **Water CO2: +227.** Gap-filled water fluxes and more water surface under
  measured flooding.
- **Downed wood: +128.** Krauss et al. 2005 volume, exposed only above water.
- **Roots: −52**, including the submerged prop-root area at high tide.
- **CH4: −35.**

`output/qa/net_forcing_attribution.csv` has the full split.

Intact NEE (−1,208 g C m⁻² yr⁻¹) is within ~6 % of the 2022–23 tower NEE
(−1,288) and ~3 % of the long-term tower NEP (1,170 ± 127, Barr et al. 2010), so
"closely matched" (D:65) holds again. Bottom-up respiration is 4.63 vs tower
4.45 µmol m⁻² s⁻¹.

> Numbers above and in section 4 were refreshed 2026-10-02 to the area-weighted flooding central case. Every analytical choice is compared one at a time in SI Table S10 (`output/qa/sensitivity_summary.csv`); carbon-balance framings are in `output/upscaling/forcing_framings.csv` (SI S.T10).

### Upscaling changes after the dataset rebuild

| Change | Basis | Where |
|---|---|---|
| Tower GPP not clipped; GPP = 0 at night | Clipping biases GPP high (noise in NEE) | `07_upscaling/01_tower_gpp.R` |
| Chamber stem, root, soil and CWD CO2 scaled from measurement time to 24 h, Q10 1.15 | Tower within-month night-respiration Q10 (2004–2023) | `01_tower_gpp.R`, `03_upscale_co2.R` 4b |
| Downed CWD area from Krauss et al. 2005 (67; 13–181 m³ ha⁻¹), 4V/d with d = 10 cm | No plot inventory; method of Troxler et al. 2015 | `00_lib/cwd_scaling.R` |
| Downed CWD and submerged prop roots exchange with the air only above water | FCE LTER water level; mean flooded depth | `00_lib/cwd_scaling.R`, `00_lib/tide_weights.R` |
| High/low tide weighted by the share of the plot floor under water (SRS5 0.62/0.37, SRS6 0.74/0.55) | FCE LTER knb-lter-fce.1168.15 × censored floor-height model fitted to our depth readings | `07_upscaling/01b_flood_fraction.R` |
| Central LAI 2.8 (2.3–5.55); Kok inhibition 30 % (MC 20–50 %) | Troxler 2015 / Barr; Reed et al. 2025; Atkin et al. 2014 | `03_upscale_co2.R`, `05_mc_forcing.R` |
| Sensitivities | Q10 × CWD grid; flooding ±5 cm; CH4 night/day 1.7 | `output/qa/budget_scenarios.csv`; `FLOOD_FRAC`; `net_forcing_by_class.csv` |

---

## 3. By component × class × campaign (CH4, nmol m⁻² s⁻¹)

Full table, including CO2, % below detection, % flagged and detection classes:
`output/qa/report_component_class_campaign.csv`. The largest moves:

| Component, class, campaign | n | Mean CH4 | Main reason |
|---|---|---|---|
| Stem, healthy, Oct 2022 | 73 → 70 | 0.90 → 0.35 | SRS6 crew JG/SC on LGR3; legacy fitted them on mixed data |
| Stem, regenerating, Mar 2022 | 17 → 17 | 5.64 → 2.01 | T1_78 was soil 31's closure (time typo) |
| Water, ghost, Mar 2023 | 41 → 23 | 8.18 → 6.33 | One flux per placement; water 122 excluded |
| Water, ghost, Oct 2022 | 19 → 10 | 42.6 → 38.8 | One flux per placement; CP40 clock fixed |
| Water, regenerating, Mar 2023 | 4 → 0 | 6.75 → none | The rows were the 2022 soil sheet |
| Root, healthy, Oct 2022 | 28 → 28 | 6.27 → 5.48 | |
| Stem, ghost (all campaigns) | ≈ | ≈ | ≈ |

Share below detection rises for most groups. Legacy used goFlux's datasheet
MDF; the rebuild uses the empirical σ. For example:

- Healthy stems, Mar 2023: 3 % → 38 %.
- Healthy soils, Mar 2023: 15 % → 42 %.

Below-detection fluxes keep their measured value in the analysis set.

---

## 4. Manuscript edits (approved and applied 2026-10-02)

Applied to the draft and SI with values from the current outputs. Rows marked with the earlier 50/50-tide values (intact CH4 2.0 g, composition 67/23/10 %, plot means, extrapolation) were superseded by the measured-flooding values: intact CH4 0.9 g (roots 44 %, water 35 %, soil 20 %, stems 1.5 %), intact plots 0.9–4.7 mg m⁻² d⁻¹, extrapolation ghost 29.3 vs 30.5 and intact 2.43 vs 2.43, Abstract methane share ~2 % → ~36 % (GWP20).

Line numbers refer to `manuscript/drafts/manuscript_NCC_draft.md` (D) and
`manuscript/SI/manuscript_NCC_SI.md` (SI). Status key:

- **C**: changed.
- **S**: already stale (did not match the legacy outputs either).
- **M**: methods statement no longer describes the pipeline.

### Abstract / Results numbers

| Where | Now in text | Proposed | Status |
|---|---|---|---|
| D:15, D:103 | "seven ecosystem pathways" | "six" (pneumatophores are part of soil; Methods D:131 already says six) | C/S |
| D:35 | "867 methane and CO2 fluxes" | 752 | S + C |
| D:35 | soils 17.6 (10.5–26.1) | 17.1 (10.2–25.5), n = 118 | C |
| D:35 | water 16.0 (10.7–22.0), n = 92 | 17.6 (10.3–26.5), n = 47 (one per floating-chamber placement) | C |
| D:35 | stems 8.0 (4.9–12.1), n = 486 | 8.0 (4.8–12.1), n = 476 | C |
| D:35 | roots 3.8 (1.1–8.9) | 3.5 (1.2–7.8) | C |
| D:35 | CWD 1.1 (0.1–2.7) | 1.0 (0.0–2.6) | C |
| D:35 | leaves 0.02 (0.01–0.03) | 0.02 (0.01–0.04) | ≈ |
| D:35 | 92 % of stem and root fluxes positive | 93 % of stem and 91 % of root fluxes | C |
| D:37 | "Disturbance elevated every pathway" | "elevated stem, soil and water-surface emission" (roots: ghost 0.61 < intact 3.53) | S |
| D:37 | stems ghost / regen / intact 11.7 / 14.6 / 0.5; "23- and 28-fold" | 11.8 (7.5–17.4) / 14.3 (3.3–33.9) / 0.29 (0.19–0.41); "41- and 50-fold" | C |
| D:37 | soil intact / ghost / regen 6.6 / 12.7 / 54.5 | 5.9 / 11.7 / 55.6 | C |
| D:37 | water intact / ghost / regen 0.7 / 19.1 / 34.5 | 0.9 / 16.2 / 56.9 | C |
| D:37 | wet vs dry stem 14.4 vs 3.7 (3.9×) | 14.7 (7.1–25.5) vs 3.5 (2.2–5.0) (4.2×) | C |
| D:37 | wet vs dry water 40.5 vs 5.8 (7.0×) | 44.8 (28.9–64.7) vs 4.8 (3.7–6.0) (9.3×) | C |
| D:37 | "Ebullition was confined to wet-season water surfaces … 5–20 % (20.4 % CP40, 7.3 % BL60, 5.0 % FLM30)" | "Ebullition occurred mainly on wet-season water surfaces at disturbed sites (bubbles in 6 of 15 wet and 5 of 32 dry placements), contributing up to 34 % of water-surface methane (34.0 % at CP40, 3.9 % at BL60; 14 % overall; not separable at FLM30, where the Picarro was used)" | C |
| D:37 | stem CO2 2.7 (2.4–3.0), n = 486 | 2.6 (2.3–2.9), n = 476 | C |
| D:37 | leaf CO2 −1.3 (−2.0 to −0.6) | −1.5 (−2.3 to −0.8); "only component with net uptake" still holds | C |
| D:41 | *R. mangle* 1.19 → 0.41 (0 → 100 cm) | 1.10 → 0.30 | C |
| D:41 | stem CO2 height p = 0.98 | p = 0.93 | C |
| D:41 | species p = 0.006 | p = 0.02 | C |
| D:41 | *L. racemosa* 0–50 cm 5.08 (1.92–12.84) | 4.04 (1.61–9.60) | C |
| D:41 | LARA vs RHMA 1.16 vs 0.51, p = 0.01; vs *C. erectus* 0.26, p = 0.02 | 1.05 vs 0.44, p = 0.02; 0.29, p = 0.05 | C |
| D:41 | dead vs living *A. germinans* 1.68 vs 0.74, p = 0.07; *R. mangle* 0.58 vs 0.98, p = 0.81 | 1.60 vs 0.70, p = 0.06; 0.59 vs 0.89, p = 0.92 | C |
| D:47 | ghost plots mean ~32 (7.7–74.0); intact ~6 (1.0–17.6) | ~29 (7.5–70.5); ~5.4 (1.1–15.5) | C |
| D:47 | wet-season ghost plots 74.0 (45.4–102.9); 26.7 (16.6–37.1) | 70.8 (29.7–111.4); 29.0 (15.5–42.5) | C |
| D:47, D:75 | intact ~2.2 g; ~4 at Shark River | ~2.0; ~3.5 | C |
| D:47 | intact composition soil 69 / root 23 / water 7 % | 67 / 23 / 10 % | C |
| D:49, SI:83 | extrapolation sensitivity ≤ ~3 %: ghost 32.0 vs 33.1, intact 5.89 vs 5.93 | ≤ ~4 %: ghost 29.3 vs 30.4, intact 5.41 vs 5.40 | C |
| D:51, D:81, SI:107 | regenerating soil 55, water 35; budget ~29 g | soil 56, water 57; ~33 g | C |
| D:51, SI:115 | Marco Island soil 2.3 vs up to 34; stems 0.5 vs 12–13 | 2.6 vs up to 31; 0.5 vs 11–14 | C |
| D:63 | bottom-up wet 36.3 vs 6.9; dry ghost 9.9, intact 1.6 | 35.9 vs 6.1; 6.4 and 1.7 | C |
| D:63 | intact bottom-up CO2 −2.9 to −3.9 | −3.1 to −3.3; recheck "within one standard error" of top-down (−2.2 ± 2) | C |
| D:65 | intact CO2 −4,713 g (−1,285 g C), "closely matched" tower NEP | −4,427 g (−1,208 g C); "closely matched" still holds | C |
| D:65 | intact CH4 2.2 g, +60 / +175 CO2-eq, 1.3 % / 3.6 % | 0.9 g, +25 / +72, 0.6 % / 1.6 % | C |
| D:65 | ghost +1,763 / +2,386; CO2 +1,437; CH4 11.7 g, +326 / +950; 18.5 % / 39.8 % | +2,107 / +2,741; +1,775; 11.9 g, +332 / +966; 15.8 % / 35.2 % (text now leads with GWP20; ghost floor partly exposed in Mar 2023) | C |
| D:67, D:87 | switch ~6,400–6,900 | ~6,200–6,700 | C |
| D:67, SI:97 | Monte Carlo intact −6,133…−3,521; ghost +1,474…+2,045 | GWP20 −5,249…−1,963 / +1,891…+3,925; GWP100 −5,309…−2,052 / +1,453…+3,213 | C |
| D:81 | amplification stem 23–28×, soil 2–8×, water 28–50× | stem 41–50×, soil 2–9×, water 19–66× | C |
| D:89 | CH4 share "nearly doubles" GWP100 → GWP20 | "more than doubles" (ghost 2.2×, intact 2.7×) | S |
| SI:49, SI:87 | ghost water depth dry 8.7 and 6.6 | 8.7 and 6.3 | C |
| SI:89 | exposed-soil ghost ≈ 17.5; budget 13.0 / 7.7 / 13.7 | ≈ 16.2; 12.0 / 7.3 / 13.4 (the flooded case still differs from the main ghost budget, 10.7; it did in legacy too) | C/S |
| SI:107 | BL60 soil 55 (27–89); water 35 (15–55); stem 15 (4–33) | 56 (27–91); 57 (33–89); 14 (3–34) | C |
| SI:115 | RB10 soil 1.2 (0.5–2.0); core intact 0.6–13 | 1.0 (0.5–1.7); 0.6–12 | C |
| SI:115 | SE-1 stem 0.16; water 1.0 (n = 5) | 0.09; 1.6 (n = 1: the other four were not separate placements) | C |
| SI:115 | leaf CO2 SE1 −1.38; BL60 −1.23 | SE1 −2.27; BL60 −1.25 | C |
| SI:115 | "all comparisons below are dry-season" | the core-site rates pool both seasons: drop the qualifier or recompute | S |

Unchanged (no edit needed):

- Abstract ~1 % / ~40 %.
- TLS areas.
- Ghost composition ~99 % water.
- Stems < 1 % and roots ~23 % of the intact budget.
- Porewater PCA, salinity and correlations.
- Intact GPP 8.7.
- Height and species × height p < 0.001.
- Chamber areas and the floating footprint.
- 34 % of BL60 chambers with standing water.
- CWD < 1 %.

### Methods text (M)

Methods describe the analysis only. Work on our own field records (time typos, mislabelled analyzers or dates, duplicate entries, window curation) is data preparation upstream of the analysis; it is documented in this report, the commit history and `data/flux_metadata/`, not in the manuscript.

| Where | Now in text | Proposed |
|---|---|---|
| D:131, SI:29 | "Los Gatos Research … cavity ring-down" / "Ultra-Portable … (UGGA)" | "three ABB/LGR GLA131 microportable greenhouse-gas analyzers (off-axis ICOS, 28 cm³ cell) and a Picarro G4301 cavity ring-down analyzer" |
| D:131 | 1 Hz / 0.17 Hz | "1 Hz (LGR; 0.1 Hz in March 2022) or ~0.2 Hz (Picarro, which measures CH4 and CO2 on alternate logged rows; only fresh readings of each gas were used)" |
| D:131, SI:31 | volumes 0.5–39.6 L; stems 0.5–3.1, roots 0.5–2.0, soil 2.1–39.6 | 0.8–39.6 L; stems 0.8–3.1, roots 0.8–2.0, soil 1.5–39.6 |
| D:131, SI:33 | n = 867 = 486 + 118 + 92 + 65 + 27 + 19 (sums to 807) | 752 = stems 476 + soil 118 + water 47 placements + roots 65 + CWD 27 + leaves 19 |
| D:131, SI:33 | median incubation 180 s (water 300 s); range 60–1,800 s | fit windows 180–210 s (median by component); floating-chamber placements median ~6.5 min, up to 79 min |
| D:135, SI:37 | best model by AICc | goFlux `best.flux` criteria; HM considered only with ≥ 30 points, else linear |
| D:135, SI:37 | field air temperature, gap-filled; default pressure | air temperature and pressure from the co-located US-Skr tower at each measurement time (101.325 kPa where the tower had no pressure, March 2022) |
| D:135, SI:29, SI:37 | MDF from volume, analyzer precision and duration | "Minimum detectable fluxes were computed per measurement as 1.96 σ / t × the flux term, with σ the median absolute deviation of within-window first differences, centred per closure and pooled by analyzer and campaign; below-detection fluxes were retained at their measured values" |
| D:135, SI:37 | 7 artefacts excluded | "Measurements with analyzer artefacts were excluded." (Pilot chambers, duplicate entries and data-sheet corrections are data preparation upstream of the analysis and are not described.) |
| D:135, SI:41 (S.M5) | jump detector > 0.10 ppm, 15 s buffer, step correction | "Floating-chamber placements (chamber on to lift) were identified in the analyzer record, giving one flux per placement. Ebullition was separated with goAquaFlux (goFlux; de-ebulliated diffusive window): the diffusive flux was fitted on the de-ebulliated series (first 10 min for placements > 12 min) and ebullition was the summed bubble steps over the whole placement. For the Picarro, whose ~5 s readings do not resolve bubbles, the total is the two-point flux over the placement." |
| SI:37 | 14 manually trimmed windows; manual recovery; post hoc volume corrections | Delete these sentences (data preparation, not analysis). If a sentence is wanted: "Each flux was fitted over the chamber closure recorded in the field log; system volumes were computed from measured chamber dimensions." |
| D:143 (new sentence) | n/a | "Wet-season water-surface fluxes at the intact sites, where no floating-chamber record exists, were estimated from dissolved CH4 and CO2 and a gas-transfer velocity calibrated on paired chamber and dissolved-gas measurements (k600 median 1.1 cm h⁻¹)." |

---

## 5. Decisions taken in the rebuild (for review)

1. **Crew JG/SC on LGR3** for 2022-10-16 SRS6 (analyzer correction; the sheet
   leaves the analyzer blank). Crew MN/ST's Oct_22_7 and 8 are excluded: their
   saved windows were JG/SC's closures, and the LGR3 record ends 13:04.
2. **Water 122 excluded** (analyzer fit failure; agreed).
3. **Unlogged placements.**
   - Kept: CP40 P01, FLM30 P01, BL60 P02, FLM30 P09 and P10.
   - Rejected: CP40 P10 (stem trace) and FLM30 P08 (handling).
4. **Weak traces.**
   - **Kept with QC flags (keep-and-flag rule):** Mar_23_191 (SRS5 stem). Its
     window shows only a breath bump, so CO2 is −2.1; legacy's 29.7 was a fit
     to the rising limb of that bump.
   - **Excluded:** Oct_22_25. Its logged time falls on a CO2 plateau left by
     the previous placement, and its legacy window was another closure.
5. **LGR2 tree closures on scripted windows** (92 differ from legacy by more
   than 20 %). The rebuild windows sit on the logged closure. Legacy's longer
   windows took in pre-closure air or the placement step, so legacy slopes were
   lower: CP40 stems in Mar 2023 are typically +40–100 %, e.g. Mar_23_132 at
   36.6 vs 24.0. Traces: `output/qa/scripted_windows_review.pdf`.
6. **fluxqc noise and MDF issues** are reported as jgewirtzman/fluxqc#1 and #2.
   The rebuild works around both on its own side.

## 6. Reproducibility

- A fresh clone plus raw data runs `Rscript run_all.R` end to end and
  reproduces every tracked output.
- Differences between runs are limited to run timestamps and the fork-library
  path in the settings files. The figure jitter is now seeded.
- The goFlux fork installs itself from `vendor/` into `.Rlib/`.
