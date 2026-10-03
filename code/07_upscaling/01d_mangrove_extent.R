# =============================================================================
# South Florida mangrove extent before the 2017 storms, for the Fig. 1 map.
# Global Mangrove Watch v3.0, 2016 extent (Bunting et al. 2022, Remote Sens.
# 14, 3657; Zenodo 10.5281/zenodo.6894273, CC BY 4.0). Downloads the 2016
# GeoTIFF archive (66 MB) to a temporary file only if the cropped product is
# missing, mosaics the south Florida 1-degree tiles, crops to the Fig. 1 frame
# aggregates to ~100 m (display layer) and polygonises. Writes data/gis/mangrove_extent/gmw_v3_2016_sfl.gpkg.
# =============================================================================
suppressMessages({library(terra); library(sf)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
out <- "data/gis/mangrove_extent/gmw_v3_2016_sfl.gpkg"
if (!file.exists(out)) {
  zf <- tempfile(fileext = ".zip")
  download.file("https://zenodo.org/records/6894273/files/gmw_v3_2016_gtiff.zip?download=1", zf, mode = "wb", quiet = TRUE)
  tiles <- paste0("gmw_v3_2016/GMW_", c("N25W082", "N25W081", "N26W082", "N26W081", "N27W082", "N27W081"), "_2016_v3.tif")
  td <- tempfile(); unzip(zf, files = tiles, exdir = td)
  r <- mosaic(sprc(lapply(file.path(td, tiles), rast)))
  r <- crop(r, ext(-82.0, -80.0, 24.7, 26.4))
  r[r != 1] <- NA
  r <- aggregate(r, fact = 4, fun = "max", na.rm = TRUE)   # ~100 m cells: display layer only
  p <- as.polygons(r, dissolve = TRUE) |> st_as_sf() |> st_make_valid()
  dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
  st_write(p, out, delete_dsn = TRUE, quiet = TRUE)
}
x <- st_read(out, quiet = TRUE)
cat(sprintf("GMW v3 2016 south Florida mangrove: %.0f km2\n", sum(as.numeric(st_area(st_transform(x, 32617)))) / 1e6))
