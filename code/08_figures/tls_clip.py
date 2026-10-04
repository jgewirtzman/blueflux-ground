"""Clip and thin terrestrial laser scans around each plot for display (Fig. 2c).

Reads the single-scan LAS files of the BlueFlux TLS dataset (Xiong, Lagomasino & Poulter
2024, ORNL DAAC 2311; ~101 GB, not stored in the repository; set BLUEFLUX_TLS_LAS to the
download folder) for the scan date used in the surface-area analysis, keeps points within a
square of half-width HALF metres around the plot centre (mean of the scans' bounding-box
centres), thins them to one point per VOX-metre voxel, and writes
data/tls/point_clouds/<site>_clip.npz (x, y, z in metres relative to the centre and to the
2nd-percentile height, plus reflectance).
"""
import csv, os, sys
import numpy as np
import laspy

LAS = os.environ.get("BLUEFLUX_TLS_LAS", os.path.expanduser("~/Downloads/TLS_Lidar_BlueFlux_Mangroves_2311_1-20261004_061002"))
OUT = "data/tls/point_clouds"
HALF, VOX = 20.0, 0.10
SCANS = {"SRS6": ["2022-10-15", "2022-10-18"], "CP40": ["2023-03-10"], "SRS5": ["2022-10-21"], "FLM30": ["2023-03-12"]}

rows = list(csv.DictReader(open(os.path.join(LAS, "TLS_Mangrove_Forests_Everglades_File_Characteristics.csv"))))
os.makedirs(OUT, exist_ok=True)
for site in (sys.argv[1:] or SCANS):
    out = os.path.join(OUT, f"{site}_clip.npz")
    if os.path.exists(out):
        print(site, "cached"); continue
    r = [x for x in rows if x["Filename"].startswith(site + "_") and x["Date"] in SCANS[site]]
    cx = np.mean([(float(x["Min_East"]) + float(x["Max_East"])) / 2 for x in r])
    cy = np.mean([(float(x["Min_North"]) + float(x["Max_north"])) / 2 for x in r])
    parts = []
    for x in r:
        with laspy.open(os.path.join(LAS, x["Filename"])) as f:
            for ch in f.chunk_iterator(2_000_000):
                X, Y, Z = np.asarray(ch.x), np.asarray(ch.y), np.asarray(ch.z)
                m = (np.abs(X - cx) < HALF) & (np.abs(Y - cy) < HALF)
                if not m.any(): continue
                refl = np.asarray(ch["Reflectance"])[m] if "Reflectance" in ch.point_format.dimension_names else np.zeros(m.sum())
                xs, ys, zs = X[m] - cx, Y[m] - cy, Z[m]
                key = (np.floor(xs / VOX).astype(np.int64) * 1_000_003 + np.floor(ys / VOX).astype(np.int64)) * 1_000_003 + np.floor(zs / VOX).astype(np.int64)
                _, i = np.unique(key, return_index=True)
                parts.append(np.column_stack([key[i].astype(np.float64), xs[i], ys[i], zs[i], refl[i]]))
        print(site, x["Filename"], sum(len(p) for p in parts), flush=True)
    a = np.concatenate(parts)
    _, i = np.unique(a[:, 0], return_index=True)
    a = a[i, 1:]
    z0 = np.percentile(a[:, 2], 2)
    np.savez_compressed(out, x=a[:, 0].astype(np.float32), y=a[:, 1].astype(np.float32), z=(a[:, 2] - z0).astype(np.float32),
                        refl=a[:, 3].astype(np.float32), centre=np.array([cx, cy]))
    print(site, "written", len(a))
