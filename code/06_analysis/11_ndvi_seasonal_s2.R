# =============================================================================
# Wet- vs dry-season greenness of each plot after Hurricane Irma (Sentinel-2 L2A,
# 10 m). For each site, year 2018-2025 and season (dry: Jan-Apr; wet: Aug-Nov),
# the three clearest scenes (scene cloud < 40%) with >= 8 of 9 valid, cloud-free
# pixels (SCL not 0, 1, 3, 8, 9, 10) in a 3 x 3 pixel (30 m) window at the site
# coordinates; NDVI = median over the window (BOA offset applied for processing
# baseline >= 04.00). Microsoft Planetary Computer STAC (public; anonymous token).
# Extractions cached in data/environmental/satellite/sentinel2_seasonal/<site>.csv.
# Writes output/analysis/si/site_ndvi_seasonal.csv and
# output/figures/other/si_ndvi_seasonal.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(jsonlite); library(httr); library(terra); library(ggplot2)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
meta <- read.csv("data/sites/site_metadata.csv")
STAC <- "https://planetarycomputer.microsoft.com/api/stac/v1/search"
tok <- function() { for (i in 1:6) { r <- tryCatch(fromJSON(content(GET("https://planetarycomputer.microsoft.com/api/sas/v1/token/sentinel-2-l2a"),
                                                              as = "text", encoding = "UTF-8"))$token, error = function(e) NULL)
  if (!is.null(r)) return(r); Sys.sleep(10 * i) }; stop("no SAS token") }
search <- function(lon, lat, dt) {
  body <- list(collections = list("sentinel-2-l2a"), intersects = list(type = "Point", coordinates = c(lon, lat)), datetime = dt, limit = 100,
               query = list(`eo:cloud_cover` = list(lt = 40)))
  for (i in 1:5) { r <- tryCatch(fromJSON(content(POST(STAC, body = toJSON(body, auto_unbox = TRUE), content_type_json()), as = "text", encoding = "UTF-8"),
                                          simplifyVector = FALSE)$features, error = function(e) NULL); if (!is.null(r)) return(r); Sys.sleep(10 * i) }
  list()
}
setGDALconfig("GDAL_HTTP_MAX_RETRY", "5"); setGDALconfig("GDAL_HTTP_RETRY_DELAY", "3")
cache <- "data/environmental/satellite/sentinel2_seasonal"; dir.create(cache, recursive = TRUE, showWarnings = FALSE)
seasons <- c(dry = "%d-01-01/%d-04-30", wet = "%d-08-01/%d-11-30")
site_series <- function(site, lat, lon) {
  f <- file.path(cache, paste0(site, ".csv")); if (file.exists(f)) return(read.csv(f))
  out <- bind_rows(lapply(2018:2025, function(y) bind_rows(lapply(names(seasons), function(sn) {
    tk <- tok(); its <- search(lon, lat, sprintf(seasons[[sn]], y, y))
    its <- its[order(sapply(its, function(i) i$properties$`eo:cloud_cover`))]
    got <- list()
    for (it in its) {
      if (length(got) >= 3) break
      r <- tryCatch({
        h <- function(b) paste0("/vsicurl/", it$assets[[b]]$href, "?", tk)
        pb <- as.numeric(it$properties$`s2:processing_baseline`); off <- if (!is.na(pb) && pb >= 4) -1000 else 0
        b4 <- rast(h("B04")); pt <- project(vect(cbind(lon, lat), crs = "EPSG:4326"), crs(b4)); xy <- geom(pt)[, c("x", "y")]
        e <- ext(xy[1] - 15, xy[1] + 15, xy[2] - 15, xy[2] + 15)
        red <- (values(crop(b4, e)) + off) / 1e4; nir <- (values(crop(rast(h("B08")), e)) + off) / 1e4
        scl <- values(resample(crop(rast(h("SCL")), e + 20), crop(b4, e), method = "near"))
        ok <- !(scl %in% c(0, 1, 3, 8, 9, 10)) & is.finite(red) & is.finite(nir)
        if (sum(ok) >= 8) data.frame(site = site, year = y, season = sn, date = substr(it$properties$datetime, 1, 10),
                                     ndvi = median(((nir - red) / (nir + red))[ok])) else NULL
      }, error = function(e) NULL)
      if (!is.null(r) && !(r$date %in% sapply(got, `[[`, "date"))) got[[length(got) + 1]] <- r
    }
    bind_rows(got)
  }))))
  write.csv(out, f, row.names = FALSE); out
}
s <- bind_rows(lapply(seq_len(nrow(meta)), function(i) site_series(meta$site_id[i], meta$latitude[i], meta$longitude[i])))
ann <- s %>% group_by(site, year, season) %>% summarise(ndvi = median(ndvi), n = n(), .groups = "drop")
dir.create("output/analysis/si", showWarnings = FALSE, recursive = TRUE)
write.csv(ann, "output/analysis/si/site_ndvi_seasonal.csv", row.names = FALSE)

site_cls <- c(SRS5 = "intact", SRS6 = "intact", RB10 = "intact", BL60 = "regenerating", CP40 = "ghost", FLM30 = "ghost", MI = "ghost", SE1 = "scrub")
d <- ann %>% mutate(site = factor(site, names(site_cls)), cls = site_cls[as.character(site)], season = factor(season, c("wet", "dry")))
avg <- d %>% group_by(site, cls, season) %>% summarise(m = mean(ndvi), lo = min(ndvi), hi = max(ndvi), .groups = "drop")
p <- ggplot(d, aes(season, ndvi, colour = cls)) +
  geom_line(aes(group = year), colour = "grey80", linewidth = 0.25) +
  geom_point(size = 0.9, alpha = 0.6, position = position_nudge(x = 0)) +
  geom_errorbar(data = avg, aes(y = m, ymin = lo, ymax = hi), width = 0.12, linewidth = 0.4) +
  geom_point(data = avg, aes(y = m), shape = 23, size = 2.3, fill = "white", stroke = 0.6) +
  facet_wrap(~ site, nrow = 1) + scale_y_continuous(limits = c(-0.2, 1)) +
  scale_colour_manual(values = c(pal_class, scrub = "#9A8C7A"), guide = "none") +
  labs(x = NULL, y = "Sentinel-2 NDVI (30 m), 2018-2025") + theme_fig() + theme(strip.text = element_text(face = "bold"))
ggsave("output/figures/other/si_ndvi_seasonal.png", p, width = 7.2, height = 2.8, dpi = 300, bg = "white")
ggsave("output/figures/other/si_ndvi_seasonal.pdf", p, width = 7.2, height = 2.8, device = cairo_pdf)
print(as.data.frame(avg %>% mutate(across(where(is.numeric), ~ round(.x, 2)))))
