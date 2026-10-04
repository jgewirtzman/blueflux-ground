# =============================================================================
# Site greenness from Sentinel-2 L2A (10 m; plot scale), context for Table S1 and
# the ghost-forest GPP note. Microsoft Planetary Computer STAC (public; anonymous
# SAS token). For each site, scenes over the plot from Sep 2022 to Apr 2023 with
# < 30% scene cloud cover; a 50 x 50 m window (5 x 5 pixels) centred on the site
# coordinates (data/sites/site_metadata.csv). Pixels flagged cloud, cloud shadow,
# cirrus, saturated or no-data in the scene classification (SCL 0, 1, 3, 8, 9, 10)
# are dropped. NDVI = (B08 - B04) / (B08 + B04) from surface reflectance (BOA
# offset -1000 applied for processing baseline >= 04.00). Also the share of
# pixels classed vegetation (SCL 4) and water (SCL 6).
# Extracted pixel statistics are cached in data/environmental/satellite/sentinel2/
# (one CSV per site) and only re-extracted when absent.
# Writes output/analysis/si/site_greenness_s2.csv (median and range over clear dates,
# by campaign window: wet = Sep-Nov 2022, dry = Feb-Apr 2023).
# =============================================================================
suppressMessages({library(dplyr); library(jsonlite); library(httr); library(terra)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
cache <- "data/environmental/satellite/sentinel2"; dir.create(cache, recursive = TRUE, showWarnings = FALSE)
meta <- read.csv("data/sites/site_metadata.csv")
STAC <- "https://planetarycomputer.microsoft.com/api/stac/v1/search"
token <- function() content(GET("https://planetarycomputer.microsoft.com/api/sas/v1/token/sentinel-2-l2a"))$token
setGDALconfig("GDAL_HTTP_MAX_RETRY", "5"); setGDALconfig("GDAL_HTTP_RETRY_DELAY", "3")

extract_site <- function(site, lat, lon) {
  f <- file.path(cache, paste0(site, ".csv"))
  if (file.exists(f)) return(read.csv(f))
  body <- list(collections = list("sentinel-2-l2a"), intersects = list(type = "Point", coordinates = c(lon, lat)),
               datetime = "2022-09-01/2023-04-30", limit = 200,
               query = list(`eo:cloud_cover` = list(lt = 30)))
  items <- fromJSON(content(POST(STAC, body = toJSON(body, auto_unbox = TRUE), content_type_json()), as = "text", encoding = "UTF-8"), simplifyVector = FALSE)$features
  tok <- token()
  out <- bind_rows(lapply(items, function(it) {
    href <- function(b) paste0("/vsicurl/", it$assets[[b]]$href, "?", tok)
    pb <- as.numeric(it$properties$`s2:processing_baseline`)
    off <- if (!is.na(pb) && pb >= 4) -1000 else 0
    r <- tryCatch({
      b4 <- rast(href("B04")); pt <- project(vect(cbind(lon, lat), crs = "EPSG:4326"), crs(b4))
      xy <- geom(pt)[, c("x", "y")]; e <- ext(xy[1] - 25, xy[1] + 25, xy[2] - 25, xy[2] + 25)
      red <- values(crop(b4, e)); nir <- values(crop(rast(href("B08")), e))
      scl <- values(resample(crop(rast(href("SCL")), e + 20), crop(b4, e), method = "near"))
      list(red = (as.numeric(red) + off) / 1e4, nir = (as.numeric(nir) + off) / 1e4, scl = as.numeric(scl))
    }, error = function(e) NULL)
    if (is.null(r)) return(NULL)
    ok <- !(r$scl %in% c(0, 1, 3, 8, 9, 10))
    nd <- (r$nir - r$red) / (r$nir + r$red)
    data.frame(site = site, date = substr(it$properties$datetime, 1, 10), scene_cloud = it$properties$`eo:cloud_cover`,
               n_px = length(nd), n_clear = sum(ok), ndvi = if (any(ok)) median(nd[ok]) else NA,
               frac_veg = if (any(ok)) mean(r$scl[ok] == 4) else NA, frac_water = if (any(ok)) mean(r$scl[ok] == 6) else NA)
  }))
  write.csv(out, f, row.names = FALSE)
  out
}
px <- bind_rows(lapply(seq_len(nrow(meta)), function(i) extract_site(meta$site_id[i], meta$latitude[i], meta$longitude[i])))
s <- px %>% filter(n_clear >= 0.8 * n_px) %>% group_by(site, date) %>%   # overlapping tiles / reprocessings: one value per date
  summarise(ndvi = median(ndvi), frac_veg = median(frac_veg), frac_water = median(frac_water), .groups = "drop") %>%
  mutate(window = ifelse(substr(date, 1, 7) %in% c("2022-09", "2022-10", "2022-11"), "wet (Sep-Nov 2022)",
                         ifelse(substr(date, 1, 7) %in% c("2023-02", "2023-03", "2023-04"), "dry (Feb-Apr 2023)", NA))) %>%
  filter(!is.na(window)) %>% group_by(site, window) %>%
  summarise(ndvi_lo = min(ndvi), ndvi_hi = max(ndvi), ndvi = median(ndvi), frac_veg = median(frac_veg),
            frac_water = median(frac_water), n_scenes = n(), .groups = "drop")
dir.create("output/analysis/si", showWarnings = FALSE, recursive = TRUE)
write.csv(s, "output/analysis/si/site_greenness_s2.csv", row.names = FALSE)
print(as.data.frame(s %>% mutate(across(where(is.numeric), ~ round(.x, 2)))))
