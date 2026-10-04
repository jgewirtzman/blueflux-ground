"""Render terrestrial laser scans of intact and ghost plots for Fig. 2c.

Reads data/tls/point_clouds/<site>_clip.npz (tls_clip.py) and draws, for each site, a
side view of a 40 m long, 5 m deep slab through the plot centre (class colour, paler
with distance from the viewer; heights above the plot's 2nd-percentile point), at the same scale for all sites.
Isolated returns are removed (fewer than 4 points per 0.3 m voxel).
Writes output/figures/other/tls_render.png and a 3-D oblique variant tls_render_3d.png.
"""
import sys
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

SITES = sys.argv[1:] or ["SRS6", "CP40"]
CLS = {"SRS5": "intact", "SRS6": "intact", "CP40": "ghost", "FLM30": "ghost"}
COL = {"intact": "#1B6B4A", "ghost": "#6B6585"}
ZMAX, SLAB = 22, 2.5

def load(s, cell=0.3, min_pts=4):
    """Points, with isolated returns (noise, insects, spray) dropped: keep points in 0.3 m
    voxels that hold at least min_pts points."""
    d = np.load(f"data/tls/point_clouds/{s}_clip.npz"); x, y, z = d["x"], d["y"], d["z"]
    k = (np.floor(x / cell).astype(np.int64) * 100003 + np.floor(y / cell).astype(np.int64)) * 100003 + np.floor(z / cell).astype(np.int64)
    u, inv, cnt = np.unique(k, return_inverse=True, return_counts=True)
    m = cnt[inv] >= min_pts
    return x[m], y[m], z[m]

cmap = matplotlib.colormaps["YlGnBu_r"]
fig, axes = plt.subplots(1, len(SITES), figsize=(7.2, 2.6), sharey=True, gridspec_kw=dict(wspace=0.05))
for ax, s in zip(np.atleast_1d(axes), SITES):
    x, y, z = load(s)
    m = (np.abs(y) < SLAB) & (z < ZMAX) & (z > -1)
    o = np.argsort(-y[m])                              # far points first, near points on top
    # class hue, lightness by depth in the slab (near dark, far pale)
    base = np.array(matplotlib.colors.to_rgb(COL[CLS[s]]))
    f = ((y[m][o] + SLAB) / (2 * SLAB))[:, None] * 0.75
    ax.scatter(x[m][o], z[m][o], c=base * (1 - f) + f, s=0.25, lw=0, rasterized=True)
    ax.set_xlim(-20, 20); ax.set_ylim(-1, ZMAX); ax.set_aspect("equal")
    ax.set_title(f"{s} ({CLS[s]})", fontsize=8, color=COL[CLS[s]], fontweight="bold", loc="left")
    ax.set_xlabel("m", fontsize=7); ax.tick_params(labelsize=6)
    for sp in ["top", "right"]: ax.spines[sp].set_visible(False)
np.atleast_1d(axes)[0].set_ylabel("Height (m)", fontsize=7)
fig.savefig("output/figures/other/tls_render.png", dpi=400, bbox_inches="tight", facecolor="white")

fig = plt.figure(figsize=(7.2, 3.2))
for k, s in enumerate(SITES):
    ax = fig.add_subplot(1, len(SITES), k + 1, projection="3d")
    x, y, z = load(s)
    r = np.sqrt(x ** 2 + y ** 2); m = (r < 15) & (z < ZMAX) & (z > -1)
    sel = np.flatnonzero(m); sel = sel[np.random.default_rng(1).permutation(len(sel))[:400_000]]
    ax.scatter(x[sel], y[sel], z[sel], c=z[sel], cmap=cmap, vmin=0, vmax=ZMAX * 0.9, s=0.05, lw=0, depthshade=False, rasterized=True)
    ax.set_xlim(-15, 15); ax.set_ylim(-15, 15); ax.set_zlim(0, ZMAX); ax.set_box_aspect((30, 30, ZMAX))
    ax.view_init(elev=14, azim=-60); ax.set_axis_off()
    ax.set_title(f"{s} ({CLS[s]})", fontsize=8, color=COL[CLS[s]], fontweight="bold")
fig.savefig("output/figures/other/tls_render_3d.png", dpi=400, bbox_inches="tight", facecolor="white")

# stacked panel for Fig. 2c: intact above ghost, the same 40 m wide, 20 m deep slab for both,
# paler with distance (linear, up to 92% towards white); height above the 1st-percentile point
# (removes water-surface returns below the floor); nothing below 0 m drawn
SLAB2 = 10.0
fig, axes = plt.subplots(2, 1, figsize=(2.4, 2.6), gridspec_kw=dict(hspace=0.04))
for ax, s in zip(axes, SITES):
    d = np.load(f"data/tls/point_clouds/{s}_clip.npz")
    x, y, z = load(s); z = z - np.percentile(d["z"], 1)
    m = (np.abs(y) < SLAB2) & (z < 20) & (z >= 0)
    o = np.argsort(-y[m])
    base = np.array(matplotlib.colors.to_rgb(COL[CLS[s]]))
    f = ((y[m][o] + SLAB2) / (2 * SLAB2))[:, None] * 0.92
    ax.scatter(x[m][o], z[m][o], c=base * (1 - f) + f, s=0.03, lw=0, rasterized=True)
    ax.set_xlim(-20, 20); ax.set_ylim(0, 22.5); ax.set_aspect("equal"); ax.axis("off")
    ax.text(20, 22.5, f"{s} ({CLS[s]})", fontsize=5, color=COL[CLS[s]], fontweight="bold", va="top", ha="right")
axes[1].plot([15, 20], [13, 13], color="grey", lw=1); axes[1].text(17.5, 13.8, "5 m", fontsize=4.5, ha="center", color="grey")
fig.savefig("output/figures/other/tls_render_panel.png", dpi=600, bbox_inches="tight", facecolor="white", pad_inches=0.01)
