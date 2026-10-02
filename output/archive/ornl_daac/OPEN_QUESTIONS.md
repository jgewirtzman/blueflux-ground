# Open questions for Jon (ORNL DAAC draft)

1. **Tree IDs.** The dataset has no tree identifier. Diameter is recorded at each chamber height, so stems at 0/50/100/150 cm cannot be grouped into trees reliably. Do the datasheets carry a tree number or tag? Without it, `measurement_id` is the only key and per-tree profiles cannot be reconstructed.
2. **Height units and datum.** `chamber_height_cm` is in cm (0-185 on stems; roots -50 to 80, where negatives are below the reference surface). `height_datum` = sediment_surface or water_surface as recorded in `above`; please confirm that `above = water` means the height was measured from the water surface.
3. **Site coordinates.** Site-level only (no per-tree positions). The SRS5 coordinate in `data/sites/site_metadata.csv` (25.364, -81.077) is ~100 m from SRS6; FCE LTER lists SRS5 at 25.3770, -81.0323. Which is right for our plots? Coordinates are missing for Long Pine Key, Mahogany Hammock, Cypress Boardwalk (single-visit upland comparison trees: keep, or drop from this package?).
4. **Time zone.** Times are treated as local civil time (America/New_York; EST/EDT switch on 2023-03-12, inside the March 2023 campaign). Please confirm that field clocks were local civil time, not EST year-round.
5. **Species code COPE** (BL60): which species? Codes Cyprus / Mahogany / Slash Pine are mapped to *Taxodium distichum*, *Swietenia mahagoni* and *Pinus elliottii*: confirm.
6. **Pneumatophores.** They are not a separate chamber component: soil collars include pneumatophores within the footprint, with count and density per collar. The compilation asked for pneumatophore fluxes; is collar-level count enough, or should collars with pneumatophores be marked as a separate tissue class?
7. **Leaf area basis.** Leaf fluxes are per one-sided leaf area estimated as 15 leaves x literature mean leaf area (Lin & Sternberg 1992), not measured leaf area. Keep as is with this note?
8. **Scope.** Includes March 2022 (dropped from the manuscript budgets) and the context sites (MI, RB10, SE1, upland trees). Archive everything measured, or only the manuscript set?
9. **Authors, funding, permit numbers, related-publication DOI** for the guide.
10. **Chamber classes HA and HB** (16 tree measurements, enclosed 48 and 126 cm2): what are they, for the geometry table?
11. **Raw analyzer files.** ORNL DAAC often asks for the raw concentration records. Include the 1 Hz / 0.2 Hz analyzer files (~1 GB) as a second granule?
