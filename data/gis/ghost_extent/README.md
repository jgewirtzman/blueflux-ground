# Hurricane-driven mangrove loss with little recovery (2017 storms)

`CIFOR_shortTermLoss_2017_GMW_V1_wCountry_Area.*` (ESRI shapefile, WGS 84 /
World Mercator), from D. Lagomasino (ECU), received 2026-08-25 as
`Mangrove_ShortTermLoss_2017.zip`. Mangrove area (Global Mangrove Watch v1
baseline) damaged by the 2017 hurricane season that showed little recovery
afterwards; mainly Florida, Cuba and Puerto Rico. Per D. Lagomasino, it misses
part of the Florida die-off (different method) but captures the bulk.
Attributes: COUNTRY, `median` (meaning to confirm with D. Lagomasino), `area`
(m2 as supplied). Used by `code/07_upscaling/09_regional_scaling.R`.

Source analysis: Taillie PJ, Roman-Cuesta R, Lagomasino D, Cifuentes-Jara M, Fatoyinbo T, Ott LE, Poulter B (2020)
Widespread mangrove damage resulting from the 2017 Atlantic mega hurricane season. Environ. Res. Lett. 15, 064010.
doi:10.1088/1748-9326/ab82cf (Landsat dNDVI < -0.2 within GMW v1; persistent damage = no NDVI recovery over the
7-month post-season). File lineage (ArcGIS metadata): CIFOR_recoveryYear_2017_GMW_V1_dissolved_shortTermLoss unioned
with CIFOR Caribbean country boundaries, selected median > 0 (D. Lagomasino, 2020-07-09). Relation of this layer
(173 km2) to the paper's 790 km2 of persistent damage, and the meaning of `median`, to confirm with D. Lagomasino.
