# Open questions for Jon (ORNL DAAC draft)

Resolved 2026-10-02: no tree IDs (not tagged); field clocks were local civil time; include all fluxes and all sites; SRS5/SRS6 coordinates from FCE LTER.

1. **Species code COPE**: 15 stem measurements at BL60, March 2023 (chambers A, B, C). BL60 stems were coded COER (*Conocarpus erectus*) in October 2022 and COPE in March 2023, with no COER in March, so COPE is probably *Conocarpus erectus* under another code. Confirm? Codes Cyprus / Mahogany / Slash Pine are mapped to *Taxodium distichum*, *Swietenia mahagoni* and *Pinus elliottii*: confirm.
2. **Chamber classes HA and HB**: 16 measurements, all March 2023 (BL60: 3 roots, 2 stems; SE1: 6 roots, 5 stems). The geometry rule is 'A (or B) chamber minus stem cylinder', with area from the stem diameter, which reads as the A or B chamber fitted around the whole circumference of a small stem or root. One record is written 'Hollow HB'; if H means 'hollow' (open-ended chamber around a small stem or root), the rule fits. Correct?
3. **Coordinates** for BL60, CP40, FLM30, MI, RB10 and SE1 come from the BlueFlux site list (3-4 decimals); better GPS values? Cypress Boardwalk, Long Pine Key and Mahogany Hammock have none.
4. **Pneumatophores** are counted inside soil collars (count, density), not chambered separately. Fine as is?
5. **Leaf area basis**: 15 leaves x literature mean leaf area (Lin & Sternberg 1992), not measured. Fine as is?
6. **Raw analyzer records** (~1 GB) as a second granule?
7. **Authors, funding, permits**: see the proposal below.
8. **Non-mangrove comparison sites.** Cypress Boardwalk stems (*Taxodium*) were measured from the water surface over 34 cm of standing water, so 'upland' is wrong for it; the package now says 'non-mangrove comparison sites'. What habitat label should each of Cypress Boardwalk, Long Pine Key and Mahogany Hammock carry?
9. **BL60 Mar_23_26/27/28** (2023-03-19): component stem, but status recorded as 'CWD' (two COPE, one RHMA; 9-12.5 cm diameter, 30-60 cm height). Standing dead stems, or downed wood?
10. **Negative prop-root heights**: Oct_22_80 (FLM30), Oct_22_119/125/130/137 (SRS5) were entered as stems at -25 or -50 cm 'from the water surface' with 3-20 cm water, and are treated as roots with height above sediment 0. What does the negative height mean (distance down a prop root from the stem? below the water line?)
11. **Downed wood without status**: 7 downed-wood closures have no tissue status (Mar_23_199-202, 207 at SRS5; Mar_23_150-151 at CP40). Should they be labelled dead?
12. **Height datum check** (raised by the compilation): height_above_sediment_cm is now computed as chamber height + water depth where the datum is the water surface (it previously repeated chamber_height_cm). Some records then give chambers below the water line (height_above_water_cm < 0), e.g. sediment-referenced 14 cm with 11 cm water. Confirm the datum convention in `above`.

## Proposed dataset metadata (approve or correct)

- **Title:** BlueFlux: Ground-Based Chamber CH4 and CO2 Fluxes from Mangrove Components, Florida Everglades, 2022-2023
- **Authors (data producers, proposed order):** Gewirtzman, J.; Adams, F.; Charles, S.; Peterman, J.; Lindquist, A.; Carruthers, L.; Colwell, A.; Powell, E.; Stovall, A.; Malone, S.; Lagomasino, D.; Poulter, B.; Raymond, P. A. [field and lab people from the manuscript list; add or remove]
- **Contact:** Jonathan Gewirtzman (jongewirtzman@gmail.com)
- **Project:** NASA Carbon Monitoring System (CMS), BlueFlux. [CMS award number(s): to fill]
- **Funding:** NASA CMS (BlueFlux); NSF Graduate Research Fellowship (J.G.); NASA Connecticut Space Grant Graduate Fellowship (J.G.). [Other co-author awards: to fill]
- **Permits:** Everglades National Park research permit [number]; Rookery Bay NERR [permit/access]; [Marco Island site access].
- **Acknowledgements:** field team (N. Bendavid, K. Blumenthal, R. D'Ascanio, A. Stemberger, Q. Ying, J. Hirsch); FCE LTER; Everglades National Park; Rookery Bay NERR; Yale Analytical and Stable Isotope Center.
- **Related data:** CARAFE airborne fluxes (Delaria et al., ORNL DAAC [DOI]); AmeriFlux US-Skr (doi:10.17190/AMF/1246105); FCE LTER water levels (knb-lter-fce.1168).
- **Related publication:** Gewirtzman et al., Hurricane-induced mortality switches mangroves from carbon sink to methane source [preprint DOI].
- **Keywords:** methane; carbon dioxide; mangrove; tree stem flux; ebullition; chamber; Everglades; hurricane; ghost forest; blue carbon.
