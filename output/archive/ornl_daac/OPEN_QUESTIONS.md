# Open questions for Jon (ORNL DAAC draft)

Resolved 2026-10-02: no tree IDs (not tagged); field clocks were local civil time; include all fluxes and all sites; SRS5/SRS6 coordinates from FCE LTER; COPE = *Conocarpus erectus*; HA/HB = hollow (open-ended) chambers fitted around a small stem or root.

1. **Species codes** Cyprus / Mahogany / Slash Pine are mapped to *Taxodium distichum*, *Swietenia mahagoni* and *Pinus elliottii*: confirm.
2. (resolved: HA/HB are hollow chambers)
3. (resolved: field GPS for BL60, CP40, FLM30, MI and SE1; RB10 from imagery; the non-mangrove comparison sites still have none)
4. (resolved: pneumatophores counted in soil collars, fine as is)
5. (resolved: literature leaf area, fine as is)
6. (resolved: raw analyzer records go in as a second granule)
7. **Authors and funding**: see the proposal below (permits are not needed for the DAAC package).
8. **Non-mangrove comparison sites.** Cypress Boardwalk stems (*Taxodium*) were measured from the water surface over 34 cm of standing water, so 'upland' is wrong for it; the package now says 'non-mangrove comparison sites'. What habitat label should each of Cypress Boardwalk, Long Pine Key and Mahogany Hammock carry?
9. (resolved: standing dead stems; tissue_status dead)
10. **Height datum by campaign**: see output/qa/chamber_heights_review.png. Negative prop-root heights are now datum root_crown with height above sediment unknown. Open: in March and October 2022 stems were chambered at nominal 0/50/100 cm; for *Rhizophora* was 0 the root crown rather than the sediment?
11. (resolved: all downed wood is dead)
12. (resolved: convention kept; two downed-wood chambers below the recorded water line carry a height_note)

## Proposed dataset metadata (approve or correct)

- **Title:** BlueFlux: Ground-Based Chamber CH4 and CO2 Fluxes from Mangrove Components, Florida Everglades, 2022-2023
- **Authors (data producers, proposed order):** Gewirtzman, J.; Adams, F.; Charles, S.; Peterman, J.; Lindquist, A.; Carruthers, L.; Colwell, A.; Powell, E.; Stovall, A.; Malone, S.; Lagomasino, D.; Poulter, B.; Raymond, P. A. [field and lab people from the manuscript list; add or remove]
- **Contact:** Jonathan Gewirtzman (jongewirtzman@gmail.com)
- **Project:** NASA Carbon Monitoring System (CMS), BlueFlux. NASA grant 80NSSC21K1564
- **Funding:** NASA CMS (BlueFlux); NSF Graduate Research Fellowship (J.G.); NASA Connecticut Space Grant Graduate Fellowship (J.G.). [Other co-author awards: to fill]
- **Acknowledgements:** field team (N. Bendavid, K. Blumenthal, R. D'Ascanio, A. Stemberger, Q. Ying, J. Hirsch); FCE LTER; Everglades National Park; Rookery Bay NERR; Yale Analytical and Stable Isotope Center.
- **Related data:** CARAFE airborne fluxes (Delaria et al., ORNL DAAC [DOI]); AmeriFlux US-Skr (doi:10.17190/AMF/1246105); FCE LTER water levels (knb-lter-fce.1168).
- **Related publication:** Gewirtzman et al., Hurricane-induced mortality switches mangroves from carbon sink to methane source [preprint DOI].
- **Keywords:** methane; carbon dioxide; mangrove; tree stem flux; ebullition; chamber; Everglades; hurricane; ghost forest; blue carbon.
