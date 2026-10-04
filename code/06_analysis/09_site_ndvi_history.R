# =============================================================================
# Long-term greenness history of each site (context for site history, Table S1,
# and the BL60 "regenerating" class).
#   Landsat Collection 2 Level-2 (Landsat 5, 7, 8, 9; 30 m), 1995-2025: dry-season
#   (Jan-Apr) scenes with < 40% scene cloud, up to 8 per year, NDVI from surface
#   reflectance (scale 2.75e-5, offset -0.2) over a 3 x 3 pixel (90 m) window at the
#   site coordinates, pixels flagged fill, cloud, cloud shadow, dilated cloud or
#   cirrus in QA_PIXEL dropped (scenes with < 5 clear pixels skipped).
# Microsoft Planetary Computer STAC (public; anonymous SAS token). Extractions are
# cached in data/environmental/satellite/landsat_ndvi/ (one CSV per site).
# Writes output/analysis/si/site_ndvi_history.csv (annual dry-season medians) and
# output/figures/other/si_ndvi_history.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(jsonlite); library(httr); library(terra); library(ggplot2)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))
meta <- read.csv("data/sites/site_metadata.csv")
STAC <- "https://planetarycomputer.microsoft.com/api/stac/v1/search"
tok_for <- function(coll) { for (i in 1:6) { r <- tryCatch(fromJSON(content(GET(paste0("https://planetarycomputer.microsoft.com/api/sas/v1/token/", coll)),
                                                                 as = "text", encoding = "UTF-8"))$token, error = function(e) NULL)
  if (!is.null(r)) return(r); Sys.sleep(10 * i) }; stop("no SAS token for ", coll) }
setGDALconfig("GDAL_HTTP_MAX_RETRY", "5"); setGDALconfig("GDAL_HTTP_RETRY_DELAY", "3")
search <- function(coll, lon, lat, dt, cc = 40, extra = list()) {
  body <- c(list(collections = list(coll), intersects = list(type = "Point", coordinates = c(lon, lat)),
                 datetime = dt, limit = 200, query = c(list(`eo:cloud_cover` = list(lt = cc)), extra)))
  fromJSON(content(POST(STAC, body = toJSON(body, auto_unbox = TRUE), content_type_json()), as = "text", encoding = "UTF-8"),
           simplifyVector = FALSE)$features
}
window_vals <- function(href, lon, lat, half) {
  r <- rast(href); pt <- project(vect(cbind(lon, lat), crs = "EPSG:4326"), crs(r)); xy <- geom(pt)[, c("x", "y")]
  as.numeric(values(crop(r, ext(xy[1] - half, xy[1] + half, xy[2] - half, xy[2] + half))))
}

# ---- Landsat, all sites ----------------------------------------------------------------------
lcache <- "data/environmental/satellite/landsat_ndvi"; dir.create(lcache, recursive = TRUE, showWarnings = FALSE)
landsat_site <- function(site, lat, lon) {
  f <- file.path(lcache, paste0(site, ".csv")); if (file.exists(f)) return(read.csv(f))
  out <- bind_rows(lapply(1995:2025, function(y) {
    tok <- tok_for("landsat-c2-l2")
    its <- search("landsat-c2-l2", lon, lat, sprintf("%d-01-01/%d-04-30", y, y),
                  extra = list(platform = list(`in` = list("landsat-5", "landsat-7", "landsat-8", "landsat-9"))))
    its <- head(its[order(sapply(its, function(i) i$properties$`eo:cloud_cover`))], 8)
    bind_rows(lapply(its, function(it) tryCatch({
      h <- function(b) paste0("/vsicurl/", it$assets[[b]]$href, "?", tok)
      red <- window_vals(h("red"), lon, lat, 45); nir <- window_vals(h("nir08"), lon, lat, 45)
      qa <- window_vals(h("qa_pixel"), lon, lat, 45)
      bad <- bitwAnd(as.integer(qa), 1L + 2L + 4L + 8L + 16L) > 0          # fill, dilated cloud, cirrus, cloud, shadow
      red <- red * 2.75e-5 - 0.2; nir <- nir * 2.75e-5 - 0.2
      nd <- (nir - red) / (nir + red)
      data.frame(site = site, date = substr(it$properties$datetime, 1, 10), platform = it$properties$platform,
                 n_clear = sum(!bad), ndvi = if (sum(!bad) >= 5) median(nd[!bad]) else NA)
    }, error = function(e) NULL)))
  }))
  write.csv(out, f, row.names = FALSE); out
}
ls_all <- bind_rows(lapply(seq_len(nrow(meta)), function(i) landsat_site(meta$site_id[i], meta$latitude[i], meta$longitude[i])))
ann <- ls_all %>% filter(!is.na(ndvi)) %>% mutate(year = as.integer(substr(date, 1, 4))) %>%
  group_by(site, year) %>% summarise(ndvi = median(ndvi), n = n(), .groups = "drop")
dir.create("output/analysis/si", showWarnings = FALSE, recursive = TRUE)
write.csv(ann, "output/analysis/si/site_ndvi_history.csv", row.names = FALSE)

# river-edge plots: use the inland window (10_ndvi_grain_srs.R), as Fig. 1c
gr <- "output/analysis/si/site_ndvi_grain.csv"
if (file.exists(gr)) { inl <- read.csv(gr) %>% filter(variant == "inland") %>% transmute(site, year, ndvi, n)
  ann <- bind_rows(ann %>% filter(!site %in% unique(inl$site)), inl) }
# ---- figure -----------------------------------------------------------------------------------
site_cls <- c(SRS5 = "intact", SRS6 = "intact", RB10 = "intact", BL60 = "regenerating", CP40 = "ghost", FLM30 = "ghost", MI = "ghost", SE1 = "scrub")
storms <- data.frame(name = c("Andrew", "Wilma", "Irma", "Ian"), date = as.Date(c("1992-08-24", "2005-10-24", "2017-09-10", "2022-09-28")))
sv <- storms %>% filter(name != "Andrew") %>% mutate(x = as.numeric(format(date, "%Y")) + as.numeric(format(date, "%j")) / 365)
pa <- ggplot(ann %>% mutate(cls = site_cls[site], site = factor(site, names(site_cls))), aes(year, ndvi, colour = cls)) +
  geom_vline(data = sv, aes(xintercept = x), colour = "grey60", linetype = "dashed", linewidth = 0.3) +
  geom_text(data = sv, aes(x = x, y = 0.02, label = name), inherit.aes = FALSE, angle = 90, hjust = 0, vjust = -0.4, size = 1.8, colour = "grey45") +
  geom_line(linewidth = 0.4) + geom_point(size = 0.8) + facet_wrap(~ site, ncol = 4) +
  scale_colour_manual(values = c(pal_class, scrub = "#9A8C7A"), guide = "none") +
  labs(x = NULL, y = "Landsat NDVI (dry season, Jan-Apr)") + scale_y_continuous(limits = c(0, 1)) + theme_fig()
p <- pa
saveRDS(pa, "output/figures/other/si_ndvi_history.rds")
ggsave("output/figures/other/si_ndvi_history.png", p, width = 7.2, height = 4.4, dpi = 300, bg = "white")
ggsave("output/figures/other/si_ndvi_history.pdf", p, width = 7.2, height = 4.4, device = cairo_pdf)
print(as.data.frame(ann %>% filter(site == "BL60")))
