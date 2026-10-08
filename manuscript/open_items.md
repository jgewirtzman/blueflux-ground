# Open items (moved out of the drafts)

Working notes that used to sit inside the Science draft and supplement. The drafts
now carry only content placeholders (missing data or text).

## Authors and affiliations
All author affiliations confirmed by J. Gewirtzman (2026-10-03).

## Checks
- E. Delaria: airborne uncertainty definition.
- Inorganic-N dilution.
- D. Lagomasino: definition of the 2017 dieback layer (173 km2 vs the paper's 790 km2 of persistent damage).
- TLS scan positions per plot; Powell et al. methods paper (in preparation).
- Tower instruments, measurement height, processing and u* threshold (US-Skr PIs).
- References verified (2026-10-03). Remaining: the 2017 dieback layer vs the public Taillie archive (PANGAEA); CARAFE dataset title now reads 2022-2024 on ORNL DAAC — recheck at submission.

## Agreed, to implement (after the SI figure walk-through)
- Ghost-forest lateral export: state as unmeasured (non-tidal because ponded; overland flow likely causes episodic lateral loss). Bounding scenarios: zero; literature (dissolved export ~halved after mortality, Sippo 2019/2020, Stegehuis 2026: ~150-250 g C m-2 yr-1); full intact-stand rate (346, 235-938).
- CH4 additionality: CH4 increment (845 GWP20 / 290 GWP100 g CO2-eq m-2 yr-1) as a percentage of warming from carbon lost by the dead stand (vertical CO2 + lateral scenario): ~47%/16% (zero lateral), ~30-35%/11-12% (literature), ~27%/9% (intact rate). Lost uptake treated as a recovery debt. Fig 5b/c redesign (regenerating CH4; switch split into lost uptake / carbon released / CH4) and text edits.
- Global hypothetical scaling (CH4-centred, per 100 km2 of persistently dead stand), framed as hypothetical.

## QA flags (water chambers)
- Bubble detection misses steps in the final seconds of a placement (no post-bubble baseline): 1 of 30 bubble-free LGR placements (44857_CP40_Water_78, CP40 Oct 2022) has a ~2.4 ppm step at 336 s inside the diffusive window; diffusive flux ~21% high (25.2 vs ~20.8 nmol m-2 s-1). Option: trim the last ~15 s or flag end steps.
- LGR1_2023-03-18_FLM30_P09: ~80 s flat lag at the start included in the diffusive window (flux likely underestimated). Check stage-02 window start for lagged placements.
- Gradual bubble releases are missed: 45000_CP40_Water_124 (CP40 Mar 2023) has a ~80 ppb rise at 490-525 s, just before the detected bubble at 528 s, not flagged (outside the diffusive window, so diffusive flux unaffected). Bound: the whole-placement total (goFlux over the full placement) is within 1% for this placement and 2-9% of diffusive + detected ebullition at the site x campaign level (CP40 Oct +6%, CP40 Mar +9%, FLM30 Mar +2%), so missed ebullition is <~10%. Now shown in fig. S4a as an upper-bound segment with the ebullitive share as a range, and stated in M7.
- Low R2 (<0.95) bubble-free placements are mostly SRS5 Mar 2023 near-zero fluxes (expected near detection).

## Data gaps
- No porewater CH4 for SRS5 and SRS6 in March 2023: the GC run file (GC Run_Dec_2023) holds only SRS1 surface water for that month. Check whether those samples were collected and run elsewhere.

## Optional analyses
- Global extension: apply the per-area switch to cyclone-driven mangrove mortality worldwide (global loss products x storm tracks), with bounds.
- Fig. 1C artist illustration: labels pending.
