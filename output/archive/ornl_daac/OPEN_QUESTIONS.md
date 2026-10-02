# Open questions for Jon (ORNL DAAC draft)

Resolved 2026-10-02: no tree IDs (not tagged); field clocks were local civil time; include all fluxes and all sites; SRS5/SRS6 coordinates from FCE LTER.

1. **Species code COPE**: 15 stem measurements at BL60, March 2023 (chambers A, B, C), alongside COER (*Conocarpus erectus*) at the same site. Which species? Codes Cyprus / Mahogany / Slash Pine are mapped to *Taxodium distichum*, *Swietenia mahagoni* and *Pinus elliottii*: confirm.
2. **Chamber classes HA and HB**: 16 measurements, all March 2023 (BL60: 3 roots, 2 stems; SE1: 6 roots, 5 stems). The geometry rule is 'A (or B) chamber minus stem cylinder', with area from the stem diameter, which reads as the A or B chamber fitted around the whole circumference of a small stem or root. Correct?
3. **Coordinates** for BL60, CP40, FLM30, MI, RB10 and SE1 come from the BlueFlux site list (3-4 decimals); better GPS values? Cypress Boardwalk, Long Pine Key and Mahogany Hammock have none.
4. **Pneumatophores** are counted inside soil collars (count, density), not chambered separately. Fine as is?
5. **Leaf area basis**: 15 leaves x literature mean leaf area (Lin & Sternberg 1992), not measured. Fine as is?
6. **Raw analyzer records** (~1 GB) as a second granule?
7. **Authors, funding, permits**: see the proposal below.

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
