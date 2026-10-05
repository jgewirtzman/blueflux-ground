# Verification of the Methane-Cycler Synthesis Against Ground Data

**Scope.** Per instruction, the sequencing-derived numbers (relative abundances, Qubit-weighted loads, gen:troph ratios) are taken as given from the synthesis writeup — no re-derivation from ASV/count tables was attempted. This document tests the checklist items that depend on data held in this repo: porewater chemistry and gas profiles (`data/porewater/merged_porewater_all_parameters.csv`, Oct–Nov 2025 campaign), transect salinity (`data/environmental/salinity/Blueflux Salinity.xlsx`, Oct 2022 + Mar 2023), and chamber fluxes (`intermediate/combined_gas_flux_dataset_complete.csv`, 2022–2023 campaigns).

**Site name mapping:** BL = BL60 (early regenerating), Cp = CP40 (ghost forest / die-off), SRS5/SRS6 as-is.

**Timing caveat (applies throughout).** The three datasets are from different campaigns (fluxes 2022–23; sequencing samples ≤ May 2024; porewater chemistry Oct 2025). All cross-dataset comparisons assume spatial patterns are persistent.

---

## Check 2 — Cp sulfate profile: **prediction FAILS**

Porewater sulfate at CP40 does **not** decline below the surface. It is the highest of any site at every depth, and essentially flat:

| Depth (cm) | SO₄ (ppm) | PSU | SO₄/Br (seawater ≈ 40) |
|---|---|---|---|
| 0 | 3200 | 41.4 | 29.6 |
| 15 | **3253** | 54.5 | 29.9 |
| 45 | 2981 | 51.7 | 29.4 |
| 90 | 2638 | 50.3 | 27.3 |

The Methanocellaceae (hydrogenotrophic) spike at 15 cm coincides with the **maximum sulfate concentration in the entire dataset** (~34 mM). Salinity and sulfate are coupled at Cp, not decoupled. SO₄/Br ratios (~27–30 vs. ~40 conservative) show only ~25–30% sulfate depletion — active sulfate reduction, but nowhere near exhaustion. The spike therefore needs another explanation: microniches, inactive/relic DNA, or taxonomic misassignment — checking the ASVs behind it (checklist item 2's fallback) is now the priority.

Independent isotopic evidence points the same way: porewater δ¹³C-CH₄ at Cp is −61 to −64‰ at all depths — the acetoclastic/methylotrophic range, not the strongly depleted (−80 to −110‰) signature of dominant hydrogenotrophic production. (Partial oxidation could have enriched an initially lighter pool, so this is suggestive, not conclusive.)

## Check 3 — SRS transect discontinuity: **CONFIRMED (dry season), threshold-like**

March 2023 (dry season) surface-water salinity along the river:

| Site | PSU |
|---|---|
| SRS1 | 0.2 |
| SRS4 | 6.0–6.8 |
| SRS4.5 | 8.8 |
| **SRS4.6** | **24.1** |
| SRS4.8 | 27.5 |
| SRS5 | 28.1 |
| SRS5.5 | 28.3 |
| SRS6 | 28.3 |
| Gulf | 31.7 |

There is a sharp salt front between SRS4.5 and SRS4.6 — i.e., exactly in the SRS4→SRS5 gap where the water gen:troph ratio drops an order of magnitude (~0.19 → ~0.02). The pattern is a **step, not a continuum**: salinity is roughly flat on both sides of the front, as is the community ratio (~0.16–0.22 fresh side; ~0.02–0.03 saline side). Note that in October 2022 (wet season) the entire reach up to SRS5.5 was fresh (<2.4 PSU), so the community break should track the dry-season front position; the sampling date of the sequenced water matters for interpreting this. No transect sulfate exists (anions were only run on the four porewater sites), so the salinity–sulfate distinction can't be made from current data.

## Check 7 — Porewater CH₄ vs. community profiles: **mostly consistent; BL is the exception**

CH₄ (µM), depths 0 / 15 / 45 / 90 cm:

- **CP40: 59.9 / 44.0 / 42.4 / 22.3** — peaks at 0–15 cm, where the DNA is. **Consistent** with explanation (b) (production concentrated in the thin surface layer). Caveat: 42 µM persists at 45 cm where methanogen DNA is near zero — plausibly diffusion from above, but not diagnostic.
- **BL60: 11.1 / 16.1 / 4.8 / 23.5** — **not** cleanly surface-loaded. The CH₄ minimum at 45 cm does line up with BL's gen:troph dip (1.39 at 45 cm), but the profile maximum is at 90 cm where Qubit-weighted methanogen DNA is ~zero. Either deep production by sparse-but-active cells, transport, or a deep DNA-extraction failure (see item 6, untestable here). Separately, BL surface water CH₄ is 86.6 µM — by far the highest surface-water value — consistent with BL as #1 emitter.
- **SRS6: 0.18 / 1.04 / 0.66 / 1.62** — increases downcore, max at 90 cm. **Consistent** with SRS6's ratio crossover to methanogen dominance at 75/90 cm (2.10).
- **SRS5: 3.7 / 5.3 / 0.8 / 3.7** — peaks at 15 cm, matching SRS5's methanogen-load peak (89 pg/µL at 15 cm). **Consistent.**

δ¹³C-CH₄ adds a clean site contrast: SRS5 is strongly depleted (−80 to −99‰; hydrogenotrophic signature), SRS6 becomes depleted with depth (−65 → −93‰), while BL and Cp sit at −58 to −65‰ (acetoclastic/methylotrophic and/or partially oxidized).

**Data-quality flag:** δ¹³C-CO₂ is unusable for most samples (values of −200 to −4700‰ are physically impossible — likely low-CO₂ analyzer readings or a headspace-correction error in the merge). The handful of plausible values (BL60 0/15/45 cm: −34/−47/−28‰; SRS6 15 cm: −29‰) give apparent εc ≈ 15–49‰, low values consistent with acetoclastic-type production and/or oxidation — which sides with slide 10's "acetoclastic" claim for BL over the Methanocellaceae family reading (item 5), but fix the δ¹³C-CO₂ column before leaning on this.

## Check 8 — Which community metric predicts the emission ranking: **the writeup's prediction holds**

Chamber ground-surface CH₄ fluxes (soil + water surfaces, `CH4_best.flux`):

| Site | n | median | mean | rank |
|---|---|---|---|---|
| BL60 | 27 | 40.6 | 72.7 | 1 |
| CP40 | 17 | 6.1 | 23.5 | 2 |
| SRS6 | 33 | 3.4 | 12.2 | 3 |
| SRS5 | 29 | 0.3 | 0.6 | 4 |

(Ranking identical by mean or median; BL #1 and Cp #2 confirmed, matching the deck's stated ordering.)

Against the writeup's metrics (Spearman ρ vs. flux rank):

- **Methanogen load (Qubit-weighted, 0–5 cm):** predicted order BL > SRS5 > SRS6 > Cp, **ρ = 0.20**. Badly wrong in both directions — ranks Cp last (it's #2) and SRS5 second (it's last, despite 77 pg/µL).
- **Soil gen:troph (0–5 cm):** predicted BL > SRS5 > Cp > SRS6, **ρ = 0.40**.
- **Weak water-column filter (inverse water methanotroph %, 3 sites with water data):** predicted Cp > SRS6 > SRS5, **ρ = 1.00** — perfect ordering of the three sites where it exists.

So production-side metrics alone underpredict Cp and overpredict SRS5, exactly as the synthesis anticipated; the water-filter metric resolves both. SRS5 is the mirror image of Cp: large subsurface methanogen reservoir, measurable porewater CH₄, hydrogenotrophic isotope signature — but the lowest flux and the strongest water methanotroph filter (troph:gen ≈ 50). With n = 4 sites this is illustrative, not statistical. Note on item 9: per the PI, there is **no BL surface-water sample** — BL doesn't flood the way the tidal sites do — so the water-filter metric structurally cannot apply at BL (its slide-11 "water" diamonds must be something else); this closes item 9 rather than leaving it as a data gap.

## Check 10 — Substrate data: **not available**

No acetate, H₂, TMA, or methylated-amine measurements exist in any porewater file. Available proxies: DOC is dramatically elevated at BL (252–318 mg/L vs. 59–77 at Cp and 15–96 at SRS sites) and alkalinity at BL is ~2× Cp; the δ¹³C constraints are described under Check 7. Discriminating the Cp methylotroph hypothesis (item 4/10) still requires either genus-level ASV work or targeted porewater chemistry (TMA/betaine/DMSO-lineage osmolytes) on a future campaign.

## Check 11 — Nitrite at BL: **untestable; NO₂ data are corrupted**

- The CP40 "NO₂-N" values (~13,000 ppm) in `merged_porewater_all_parameters.csv` are a **misassigned chloride peak**: the raw IC run (`Anions_251121.csv`) shows a massive peak at RT 6.8 labeled NO₂-N in exactly the samples where Cl was over-range and unreported. Nitrite at 13 g/L in sulfidic, −375 mV porewater is impossible. These cells should be set to NA (and Cl re-run at higher dilution if it matters).
- Real NO₂/NO₃ are otherwise below detection everywhere except NO₃-N = 0.88 ppm at BL60 90 cm — the wrong depth for the shallow Methylomirabilaceae peak. The nitrite-AOM hypothesis is neither supported nor refuted; shallow porewater NOₓ at BL would need to be measured (ideally at the 15–45 cm gen:troph dip).

## Items requiring raw sequencing data (not attempted, per instruction)

Items **1, 4, 5 (count side), 6, 12** all need the ASV/count tables and Qubit spreadsheet and remain open (item 9 is closed — no BL surface water exists; see Check 8). Highest-value ones given the results above:

1. **Item 4 (genus-level Cp Methanosarcinaceae)** — now doubly important because the sulfate profile kills the "sulfate drawdown" rescue of the hydrogenotrophic reading, and the isotopes lean acetoclastic/methylotrophic.
2. **Item 6 (per-gram normalization)** — the BL deep CH₄ maximum over near-zero DNA makes an extraction artifact at depth a live concern.

## Data-quality flags raised during this pass

1. `NO2_N_ppm` at CP40 in the merged porewater files = misassigned chloride (see Check 11). Also explains why `Cl_ppm` is NA in all porewater rows.
2. `d13C_CO2_mean` is physically impossible (< −130‰) for 14 of 20 samples; only BL60 0/15/45, SRS5 Surface, SRS6 Surface/15 look real. Check the headspace/keeling processing in the merge script.
3. Porewater chemistry (Oct–Nov 2025) postdates the sequencing samples by ~2 years; the Check 2/3/7 conclusions assume the geochemical structure is stable.
