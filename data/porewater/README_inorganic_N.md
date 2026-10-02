# Porewater inorganic nitrogen (October 2025 profiles)

`porewater_inorganic_N_Oct2025.csv` holds the 20 BlueFlux porewater samples
(SRS5, SRS6, CP40, BL60; surface water and 0, 15, 45, 90 cm) copied verbatim
from `Yale_inorgN_2026 (2).xlsx`, sheet "Final data" (received 2026-10-02). The
source workbook also contains samples from another project (N mineralisation
incubations), which are not copied. Columns are as in the source: Sample ID,
mg N-NO3/L, mg N-NH4/L. Values are not edited here; negative values (below
detection) and the "CP40 C" label (taken as 0 cm) are handled in
`code/05_dataset/04_porewater_nitrogen.R`.
