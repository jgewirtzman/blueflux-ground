# BlueFlux — Mangrove Component CO₂ Budget: Literature Outputs

Literature values and provenance supporting a component-resolved CO₂ budget (`NEE = Reco − GPP`) for hurricane-disturbed Florida Everglades mangroves (NASA BlueFlux). Site: Shark River Slough, tower SRS-6 / AmeriFlux US-Skr. Species: *Rhizophora mangle*, *Avicennia germinans*, *Laguncularia racemosa*, *Conocarpus erectus*.

Generated 2026-06-17. Sign convention: negative NEE = net ecosystem uptake.

## Contents

| File | What it is | Start here if… |
|---|---|---|
| `CO2_budget_literature_provenance.md` | **Primary document.** Full provenance + methods. Every value has a stable ID, verbatim published quantity, exact in-source location, method, verification tier (A–D), and corrections applied. Includes the correction/scaling equations (Heskel T-correction, GlobResp gap-fill, leaf→canopy→budget chain). | …you want to understand or cite any number. |
| `value_catalog.csv` | Machine-readable catalog (22 rows, 16 cols) keyed by the same IDs. Columns include `value_as_published`, `units`, `species`, `site`, `method`, `class` (mangrove/proxy), `tier`, `doi`, `source_location`, `corrections_applied`. | …you want to load values programmatically. |
| `Mangrove_leaf_respiration_literature_values.xlsx` | Annotated data table, 6 tabs (Leaf_Rd, Leaf_Photosynthesis, GPP_Scaling_Ratios, FCE_Site_Fluxes, Temp_Scaling, References). Green/orange confidence flags. | …you want to browse values in a spreadsheet. |
| `Mangrove_leaf_respiration_synthesis_memo.docx` | Narrative memo on how to estimate the missing leaf-respiration term and partition tower GPP. | …you want the prose walkthrough. |

The `.md` and `.csv` share value IDs (e.g. `LR-01`, `LAI-02`, `AH-02`) — use them to cross-reference. The `.md` is the source of truth; the spreadsheet predates the folder-PDF additions, so where they differ, the `.md`/`.csv` win.

## Key results (one-liners)

- **Leaf dark respiration, dominant species, at-site:** `LR-01` *R. mangle* Rd = 1.62 ± 1.32 µmol m⁻² s⁻¹ (Barr 2009, LI-6400 / Farquhar fit). `LR-02` *A. germinans* Rd₂₅ = 1.28–1.54 µmol m⁻² s⁻¹, Q10 = 2.39 (Sturchio 2022). Both at 25 °C — temperature-correct with §4 `C-02`.
- **LAI:** carry a range — `LAI-01` 2.3 (ground, hurricane-suppressed) → `LAI-02` 5.55 (MODIS, recovered). It's the dominant scaling lever for the leaf term.
- **Foliar fraction of Reco:** ~⅓ (proxy, `FR-01/02`). **Below-canopy chamber share of Reco:** 45–65% at this site (`AH-02`, Troxler 2015).
- **Midday GPP ~37 µmol m⁻² s⁻¹:** plausible high-end for tall riverine Shark River (`GN-05`).

## Important caveats (see provenance §4–§5)

- **OCR unit fix (`C-01`):** Barr 2009/2010 PDFs print "µmol" as "mmol" and drop superscript minus signs. All leaf/flux values corrected to µmol.
- **Distinct-but-identical values:** leaf Rd 1.62 (Barr 2009, `LR-01`) ≠ mean ecosystem Re 1.62 (Barr 2010, `GN-03`). Coincidental.
- **Lateral DIC export (`C-08`):** tower Reco likely under-counts belowground respiration (tidal export), so a bottom-up chamber+leaf sum may legitimately exceed tower Reco — don't force closure.
- **Proxies vs mangrove:** tier D rows are tropical-forest analogs used only for ratios with no mangrove measurement; flagged per row.

## Source PDFs

The primary-source PDFs are in the parent folder (`../`, the connected `literature` directory). The provenance `.md` §1.1 maps each filename to its citation. Note `out (1).pdf` is only the front matter of Barr's 2005 dissertation (not the data chapters).

## Provenance / reproducibility

`CO2_budget_literature_provenance.md` §1 logs the PDF text-extraction commands and the re-runnable web-search queries; §6 is the full reference list with DOIs and the exact in-source location used for each value; §7 is the change log.
