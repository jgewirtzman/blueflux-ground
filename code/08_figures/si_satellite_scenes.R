# =============================================================================
# Fig. S? | Sentinel-2 true-colour scenes of each site through time.
# Columns: before Hurricane Irma (dry season, Jan-Apr 2017), after Irma (Oct 2017 -
# Jan 2018), the wet-season campaign (Sep-Nov 2022), the dry-season campaign
# (Feb-Apr 2023) and the latest dry season (Jan-Apr 2025). Rows: sites
# (data/sites/site_metadata.csv). For each site x window, the Sentinel-2 L2A scene
# (Microsoft Planetary Computer; scene cloud < 30%) with the most cloud-free pixels
# in a 600 x 600 m chip centred on the plot (scene classification SCL not 3, 8, 9, 10)
# is shown as a true-colour composite (B04, B03, B02; reflectance stretched 0-0.15,
# BOA offset applied for processing baseline >= 04.00). Circle: 50 m around the plot.
# Chips are cached in data/environmental/satellite/sentinel2_chips/ (one RDS per
# site x window).
# Writes output/figures/other/si_satellite_scenes.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(jsonlite); library(httr); library(terra); library(ggplot2)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))
cache <- "data/environmental/satellite/sentinel2_chips"; dir.create(cache, recursive = TRUE, showWarnings = FALSE)
meta <- read.csv("data/sites/site_metadata.csv")
site_lv <- c("SRS5", "SRS6", "RB10", "BL60", "CP40", "FLM30", "MI", "SE1")
cls <- c(SRS5 = "intact", SRS6 = "intact", RB10 = "intact", BL60 = "regenerating", CP40 = "ghost", FLM30 = "ghost", MI = "ghost", SE1 = "scrub")
windows <- data.frame(win = c("Before Irma\n(Jan-Apr 2017)", "After Irma\n(Oct 2017-Jan 2018)", "Wet campaign\n(Sep-Nov 2022)",
                              "Dry campaign\n(Feb-Apr 2023)", "Dry season\n(Jan-Apr 2025)"),
                      dt = c("2017-01-01/2017-04-30", "2017-10-01/2018-01-31", "2022-09-01/2022-11-30",
                             "2023-02-01/2023-04-30", "2025-01-01/2025-04-30"), key = c("w1", "w2", "w3", "w4", "w5"))
STAC <- "https://planetarycomputer.microsoft.com/api/stac/v1/search"
token <- function() content(GET("https://planetarycomputer.microsoft.com/api/sas/v1/token/sentinel-2-l2a"))$token
setGDALconfig("GDAL_HTTP_MAX_RETRY", "5"); setGDALconfig("GDAL_HTTP_RETRY_DELAY", "3")
HALF <- 300

chip <- function(site, lat, lon, key, dt) {
  f <- file.path(cache, sprintf("%s_%s.rds", site, key))
  if (file.exists(f)) return(readRDS(f))
  body <- list(collections = list("sentinel-2-l2a"), intersects = list(type = "Point", coordinates = c(lon, lat)),
               datetime = dt, limit = 100, query = list(`eo:cloud_cover` = list(lt = 30)))
  items <- fromJSON(content(POST(STAC, body = toJSON(body, auto_unbox = TRUE), content_type_json()), as = "text", encoding = "UTF-8"),
                    simplifyVector = FALSE)$features
  items <- items[order(sapply(items, function(it) it$properties$`eo:cloud_cover`))]
  tok <- token(); best <- NULL
  for (it in head(items, 8)) {
    href <- function(b) paste0("/vsicurl/", it$assets[[b]]$href, "?", tok)
    res <- tryCatch({
      r4 <- rast(href("B04")); pt <- project(vect(cbind(lon, lat), crs = "EPSG:4326"), crs(r4))
      xy <- geom(pt)[, c("x", "y")]; e <- ext(xy[1] - HALF, xy[1] + HALF, xy[2] - HALF, xy[2] + HALF)
      scl <- crop(rast(href("SCL")), e + 40)
      clear <- mean(!(values(scl) %in% c(0, 3, 8, 9, 10)))
      list(e = e, clear = clear, it = it)
    }, error = function(e) NULL)
    if (is.null(res)) next
    if (is.null(best) || res$clear > best$clear) best <- res
    if (res$clear > 0.98) break
  }
  if (is.null(best)) return(NULL)
  it <- best$it; href <- function(b) paste0("/vsicurl/", it$assets[[b]]$href, "?", tok)
  pb <- as.numeric(it$properties$`s2:processing_baseline`); off <- if (!is.na(pb) && pb >= 4) -1000 else 0
  rgb <- c(crop(rast(href("B04")), best$e), crop(rast(href("B03")), best$e), crop(rast(href("B02")), best$e))
  df <- as.data.frame(rgb, xy = TRUE); names(df) <- c("x", "y", "r", "g", "b")
  ctr <- c(mean(c(best$e[1], best$e[2])), mean(c(best$e[3], best$e[4])))
  out <- df %>% mutate(x = x - ctr[1], y = y - ctr[2], across(c(r, g, b), ~ (.x + off) / 1e4)) %>%
    mutate(site = site, key = key, date = substr(it$properties$datetime, 1, 10), clear = best$clear)
  saveRDS(out, f); out
}
all <- bind_rows(lapply(site_lv, function(s) { m <- meta[meta$site_id == s, ]
  bind_rows(lapply(seq_len(nrow(windows)), function(j) chip(s, m$latitude, m$longitude, windows$key[j], windows$dt[j]))) }))
st <- function(v) { v[!is.finite(v)] <- 0; pmin(pmax(v / 0.15, 0), 1) }
all <- all %>% mutate(col = rgb(st(r), st(g), st(b)),
                      site = factor(site, site_lv), win = factor(windows$win[match(key, windows$key)], windows$win))
lab <- all %>% distinct(site, win, date, clear) %>% mutate(txt = format(as.Date(date), "%d %b %Y"))
circ <- data.frame(t = seq(0, 2 * pi, length.out = 60)) %>% mutate(x = 50 * cos(t), y = 50 * sin(t))
strip_y <- ggh4x::strip_themed(text_y = lapply(pal_class[ifelse(cls[site_lv] %in% names(pal_class), cls[site_lv], "intact")],
                                                function(cc) element_text(colour = cc, face = "bold", angle = 0, hjust = 0)))
p <- ggplot(all, aes(x, y)) + geom_raster(aes(fill = col)) + scale_fill_identity() +
  geom_path(data = circ, aes(x, y), colour = "yellow", linewidth = 0.35) +
  geom_label(data = lab, aes(x = -HALF + 8, y = -HALF + 8, label = txt), hjust = 0, vjust = 0, size = 1.6,
             linewidth = 0, label.padding = unit(0.8, "pt"), fill = alpha("white", 0.75)) +
  ggh4x::facet_grid2(site ~ win, strip = strip_y, switch = "y") + coord_equal(expand = FALSE) +
  labs(x = NULL, y = NULL) + theme_void(base_size = 7) +
  theme(strip.text.x = element_text(size = 6.5, face = "bold", margin = margin(0, 0, 2, 0)),
        strip.placement = "outside", panel.spacing = unit(1.5, "pt"), plot.margin = margin(2, 2, 2, 2))
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/si_satellite_scenes.png", p, width = 6.4, height = 9.6, dpi = 300, bg = "white")
ggsave("output/figures/other/si_satellite_scenes.pdf", p, width = 6.4, height = 9.6, device = cairo_pdf)
print(as.data.frame(lab %>% select(site, win, date, clear) %>% mutate(win = gsub("\n", " ", win), clear = round(clear, 2))))
