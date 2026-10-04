# =============================================================================
# Landsat NDVI grain sensitivity at the river-edge intact plots (SRS5, SRS6).
# Their coordinates are on the Shark River bank, so the 90 m (3 x 3 pixel) window
# of 09_site_ndvi_history.R mixes forest with river water. From one read per scene
# of a +/- 150 m window (same scenes and QA as 09_site_ndvi_history.R):
#   centre   the single 30 m pixel at the coordinates
#   win90    the 3 x 3 window (as 09_site_ndvi_history.R)
#   inland   the 3 x 3 window centred 100 m from the plot in the direction (of 8)
#            with the highest median NDVI over all clear scenes (i.e. away from
#            the river into continuous forest)
# Extractions cached in data/environmental/satellite/landsat_ndvi_grain/<site>.csv.
# Writes output/analysis/si/site_ndvi_grain.csv (annual dry-season medians).
# =============================================================================
suppressMessages({library(dplyr); library(jsonlite); library(httr); library(terra)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
meta <- read.csv("data/sites/site_metadata.csv")
STAC <- "https://planetarycomputer.microsoft.com/api/stac/v1/search"
tok_for <- function(coll) { for (i in 1:6) { r <- tryCatch(fromJSON(content(GET(paste0("https://planetarycomputer.microsoft.com/api/sas/v1/token/", coll)),
                                                                 as = "text", encoding = "UTF-8"))$token, error = function(e) NULL)
  if (!is.null(r)) return(r); Sys.sleep(10 * i) }; stop("no SAS token for ", coll) }
setGDALconfig("GDAL_HTTP_MAX_RETRY", "5"); setGDALconfig("GDAL_HTTP_RETRY_DELAY", "3")
search <- function(lon, lat, dt) {
  body <- list(collections = list("landsat-c2-l2"), intersects = list(type = "Point", coordinates = c(lon, lat)), datetime = dt, limit = 200,
               query = list(`eo:cloud_cover` = list(lt = 40), platform = list(`in` = list("landsat-5", "landsat-7", "landsat-8", "landsat-9"))))
  for (i in 1:5) { r <- tryCatch(fromJSON(content(POST(STAC, body = toJSON(body, auto_unbox = TRUE), content_type_json()), as = "text", encoding = "UTF-8"),
                                          simplifyVector = FALSE)$features, error = function(e) NULL); if (!is.null(r)) return(r); Sys.sleep(10 * i) }
  list()
}
cache <- "data/environmental/satellite/landsat_ndvi_grain"; dir.create(cache, recursive = TRUE, showWarnings = FALSE)
dirs <- expand.grid(dx = c(-1, 0, 1), dy = c(-1, 0, 1)) %>% filter(!(dx == 0 & dy == 0)) %>%
  mutate(len = sqrt(dx^2 + dy^2), ox = 100 * dx / len, oy = 100 * dy / len, dir = paste0(dx, ",", dy))
grain_site <- function(site, lat, lon) {
  f <- file.path(cache, paste0(site, ".csv")); if (file.exists(f)) return(read.csv(f))
  out <- bind_rows(lapply(1995:2025, function(y) {
    tok <- tok_for("landsat-c2-l2")
    its <- search(lon, lat, sprintf("%d-01-01/%d-04-30", y, y))
    its <- head(its[order(sapply(its, function(i) i$properties$`eo:cloud_cover`))], 8)
    bind_rows(lapply(its, function(it) tryCatch({
      h <- function(b) paste0("/vsicurl/", it$assets[[b]]$href, "?", tok)
      r0 <- rast(h("red")); pt <- project(vect(cbind(lon, lat), crs = "EPSG:4326"), crs(r0)); xy <- geom(pt)[, c("x", "y")]
      e <- ext(xy[1] - 150, xy[1] + 150, xy[2] - 150, xy[2] + 150)
      red <- crop(r0, e) * 2.75e-5 - 0.2; nir <- crop(rast(h("nir08")), e) * 2.75e-5 - 0.2
      qa <- crop(rast(h("qa_pixel")), e)
      nd <- (nir - red) / (nir + red); nd[bitwAnd(as.integer(values(qa)), 31L) > 0] <- NA
      win <- function(cx, cy, half) { v <- values(crop(nd, ext(cx - half, cx + half, cy - half, cy + half))); v <- v[is.finite(v)]
        if (length(v) >= max(1, round(0.5 * (2 * half / 30)^2))) median(v) else NA }
      cen <- extract(nd, matrix(xy, ncol = 2))[1, 1]
      d <- data.frame(site = site, date = substr(it$properties$datetime, 1, 10), variant = c("centre", "win90"),
                      ndvi = c(cen, win(xy[1], xy[2], 45)))
      dd <- bind_rows(lapply(seq_len(nrow(dirs)), function(k) data.frame(site = site, date = d$date[1], variant = paste0("dir", dirs$dir[k]),
                                                                         ndvi = win(xy[1] + dirs$ox[k], xy[2] + dirs$oy[k], 45))))
      bind_rows(d, dd)
    }, error = function(e) NULL)))
  }))
  write.csv(out, f, row.names = FALSE); out
}
g <- bind_rows(lapply(c("SRS5", "SRS6"), function(s) { m <- meta[meta$site_id == s, ]; grain_site(s, m$latitude, m$longitude) }))
best <- g %>% filter(grepl("^dir", variant), !is.na(ndvi)) %>% group_by(site, variant) %>% summarise(m = median(ndvi), .groups = "drop") %>%
  group_by(site) %>% slice_max(m, n = 1) %>% ungroup() %>% select(site, best = variant)
print(best)
ann <- g %>% left_join(best, by = "site") %>% filter(variant %in% c("centre", "win90") | variant == best) %>%
  mutate(variant = ifelse(grepl("^dir", variant), "inland", variant), year = as.integer(substr(date, 1, 4))) %>%
  filter(!is.na(ndvi)) %>% group_by(site, variant, year) %>% summarise(ndvi = median(ndvi), n = n(), .groups = "drop")
dir.create("output/analysis/si", showWarnings = FALSE, recursive = TRUE)
write.csv(ann, "output/analysis/si/site_ndvi_grain.csv", row.names = FALSE)
print(ann %>% group_by(site, variant) %>% summarise(med = round(median(ndvi), 2), sd = round(sd(ndvi), 2), .groups = "drop"))
