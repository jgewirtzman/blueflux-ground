"""Write the committed Fig. 2c slab subsets from the full plot clips (tls_clip.py).

For each site: points within the 40 m (x) by 20 m (depth, y) slab through the plot centre,
heights relative to the 1st-percentile point (-0.5 to 20.5 m kept), isolated returns removed
(< 3 points per 0.25 m voxel), then thinned to one point per 2 cm x 2 cm (x, z) cell per 0.5 m
of depth (no visible change at panel size). Writes data/tls/point_clouds/<site>_slab.npz.
"""
import numpy as np
for s in ["SRS6", "CP40"]:
    d = np.load(f"data/tls/point_clouds/{s}_clip.npz"); x, y, z = d["x"], d["y"], d["z"]
    z = z - np.percentile(z, 1); m = (np.abs(y) < 10) & (np.abs(x) < 20) & (z >= -0.5) & (z < 20.5)
    x, y, z = x[m], y[m], z[m]
    c = 0.25; k = (np.floor(x / c).astype(np.int64) * 100003 + np.floor(y / c).astype(np.int64)) * 100003 + np.floor(z / c).astype(np.int64)
    _, inv, cnt = np.unique(k, return_inverse=True, return_counts=True); m = cnt[inv] >= 3
    x, y, z = x[m], y[m], z[m]
    k = (np.floor(x / 0.02).astype(np.int64) * 10007 + np.floor(z / 0.02).astype(np.int64)) * 101 + np.floor((y + 10) / 0.5).astype(np.int64)
    _, i = np.unique(k, return_index=True)
    np.savez_compressed(f"data/tls/point_clouds/{s}_slab.npz", x=x[i], y=y[i], z=z[i], centre=d["centre"])
    print(s, len(i))
