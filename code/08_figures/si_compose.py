"""Stack SI figure parts into one image per supplementary figure.

Each part is drawn by its own script at 7.2 in wide (300 dpi) with the panel letters it has
in the combined figure; this step only stacks them top to bottom on a white page.
Writes output/figures/other/si_grp_<name>.png (read by manuscript/SI/build_supplement.py).
"""
import os
from PIL import Image

OUT = "output/figures/other"
GROUPS = {
    "dissolved": ["water_positions_by_campaign", "si_k600_compare"],                     # chamber vs dissolved-gas water flux; k600
    "tide":      ["si_flood_fraction", "si_tide_states"],                                # flooded share; CH4 by tide state
    "porewater": ["ed_porewater_rounds", "si_S12_carbonate"],                            # three rounds; alkalinity and sulfate
}
for name, parts in GROUPS.items():
    ims = [Image.open(os.path.join(OUT, p + ".png")).convert("RGB") for p in parts]
    w = max(i.width for i in ims)
    page = Image.new("RGB", (w, sum(i.height for i in ims)), "white")
    y = 0
    for i in ims:
        page.paste(i, ((w - i.width) // 2, y)); y += i.height
    page.save(os.path.join(OUT, f"si_grp_{name}.png"), dpi=(300, 300))
    print(name, page.size, f"{page.height / 300:.1f} in tall at 7.2 in wide")
