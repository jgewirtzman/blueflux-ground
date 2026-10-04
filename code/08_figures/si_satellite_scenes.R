# =============================================================================
# Fig. S? | Sentinel-2 true-colour scenes of each site through time.
# Columns: one scene per year 2016-2025, the cleanest within ~2.5 months of 1 March (dry
# season; Hurricane Irma, Sep 2017, falls between 2017 and 2018), then the wet (Oct 2022)
# and dry (Mar 2023) campaigns. Rows: sites
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
yrs <- 2016:2025
# one column per year: the cleanest scene within ~2 months of 1 March (Jan-Apr dry season, widened to
# Dec-May only when needed); Irma (Sep 2017) falls between 2017 and 2018. Then the two campaigns.
windows <- rbind(
  data.frame(win = ifelse(yrs == 2018, "2018\n(after Irma)", as.character(yrs)),
             dt = sprintf("%d-12-15/%d-05-15", yrs - 1, yrs), key = paste0("c", yrs), target = sprintf("%d-03-01", yrs)),
  data.frame(win = c("Oct 2022\n(wet campaign)", "Mar 2023\n(dry campaign)"),
             dt = c("2022-09-01/2022-11-30", "2023-02-01/2023-04-30"), key = c("cw22", "cd23"), target = c("2022-10-15", "2023-03-15")))
STAC <- "https://planetarycomputer.microsoft.com/api/stac/v1/search"
token <- function() { for (i in 1:6) { r <- tryCatch(fromJSON(content(GET("https://planetarycomputer.microsoft.com/api/sas/v1/token/sentinel-2-l2a"),
                                                                     as = "text", encoding = "UTF-8"))$token, error = function(e) NULL)
  if (!is.null(r)) return(r); Sys.sleep(15 * i) }; stop("no SAS token") }
setGDALconfig("GDAL_HTTP_MAX_RETRY", "5"); setGDALconfig("GDAL_HTTP_RETRY_DELAY", "3")
HALF <- 300

chip <- function(site, lat, lon, key, dt, target) {
  f <- file.path(cache, sprintf("%s_%s.rds", site, key))
  if (file.exists(f)) return(readRDS(f))
  body <- list(collections = list("sentinel-2-l2a"), intersects = list(type = "Point", coordinates = c(lon, lat)),
               datetime = dt, limit = 100, query = list(`eo:cloud_cover` = list(lt = 30)))
  items <- fromJSON(content(POST(STAC, body = toJSON(body, auto_unbox = TRUE), content_type_json()), as = "text", encoding = "UTF-8"),
                    simplifyVector = FALSE)$features
  items <- items[order(sapply(items, function(it) it$properties$`eo:cloud_cover`))]
  tok <- token(); best <- NULL
  cands <- list()
  for (it in head(items, 15)) {
    href <- function(b) paste0("/vsicurl/", it$assets[[b]]$href, "?", tok)
    res <- tryCatch({
      r4 <- rast(href("B04")); pt <- project(vect(cbind(lon, lat), crs = "EPSG:4326"), crs(r4))
      xy <- geom(pt)[, c("x", "y")]; e <- ext(xy[1] - HALF, xy[1] + HALF, xy[2] - HALF, xy[2] + HALF)
      scl <- crop(rast(href("SCL")), e + 40)
      clear <- mean(!(values(scl) %in% c(0, 3, 8, 9, 10)) & is.finite(values(scl)))   # no-data (tile edge) counts as not clear
      list(e = e, clear = clear, it = it)
    }, error = function(e) NULL)
    if (is.null(res)) next
    res$lag <- abs(as.numeric(as.Date(substr(it$properties$datetime, 1, 10)) - as.Date(target)))
    cands[[length(cands) + 1]] <- res
  }
  # cleanest scene (>= 98% valid, cloud-free) nearest the target date; otherwise the clearest available
  if (length(cands)) {
    clr <- sapply(cands, `[[`, "clear"); lag <- sapply(cands, `[[`, "lag")
    best <- if (any(clr >= 0.98)) cands[[which(clr >= 0.98)[which.min(lag[clr >= 0.98])]]] else cands[[which.max(clr)]]
  }
  if (is.null(best) || best$clear < 0.5) return(NULL)                  # nothing usable: do not cache
  it <- best$it; href <- function(b) paste0("/vsicurl/", it$assets[[b]]$href, "?", tok)
  pb <- as.numeric(it$properties$`s2:processing_baseline`); off <- if (!is.na(pb) && pb >= 4) -1000 else 0
  rgb <- c(crop(rast(href("B04")), best$e), crop(rast(href("B03")), best$e), crop(rast(href("B02")), best$e))
  df <- as.data.frame(rgb, xy = TRUE); names(df) <- c("x", "y", "r", "g", "b")
  ctr <- c(mean(c(best$e[1], best$e[2])), mean(c(best$e[3], best$e[4])))
  out <- df %>% mutate(x = x - ctr[1], y = y - ctr[2], across(c(r, g, b), ~ (.x + off) / 1e4)) %>%
    mutate(site = site, key = key, date = substr(it$properties$datetime, 1, 10), clear = best$clear)
  if (nrow(out) == 0) return(NULL)
  saveRDS(out, f); out
}
all <- bind_rows(lapply(site_lv, function(s) { m <- meta[meta$site_id == s, ]
  bind_rows(lapply(seq_len(nrow(windows)), function(j) chip(s, m$latitude, m$longitude, windows$key[j], windows$dt[j], windows$target[j]))) }))
st <- function(v) { v[!is.finite(v)] <- 0; pmin(pmax(v / 0.15, 0), 1) }
all <- all %>% mutate(col = rgb(st(r), st(g), st(b)),
                      site = factor(site, site_lv), win = factor(windows$win[match(key, windows$key)], windows$win))
lab <- all %>% distinct(site, win, date, clear) %>% mutate(txt = format(as.Date(date), "%d %b %Y"))
circ <- data.frame(t = seq(0, 2 * pi, length.out = 60)) %>% mutate(x = 50 * cos(t), y = 50 * sin(t))
strip_y <- ggh4x::strip_themed(text_y = lapply(c(pal_class, scrub = "#9A8C7A")[cls[site_lv]],
                                                function(cc) element_text(colour = cc, face = "bold", angle = 0, hjust = 1)))
p <- ggplot(all, aes(x, y)) + geom_raster(aes(fill = col)) + scale_fill_identity() +
  geom_path(data = circ, aes(x, y), colour = "yellow", linewidth = 0.35) +
  geom_label(data = lab, aes(x = -HALF + 8, y = -HALF + 8, label = txt), hjust = 0, vjust = 0, size = 1.15,
             linewidth = 0, label.padding = unit(0.8, "pt"), fill = alpha("white", 0.75)) +
  ggh4x::facet_grid2(site ~ win, strip = strip_y, switch = "y") + coord_equal(expand = FALSE) +
  labs(x = NULL, y = NULL) + theme_void(base_size = 7) +
  theme(strip.text.x = element_text(size = 6, face = "bold", margin = margin(0, 0, 2, 0)),
        strip.text.y.left = element_text(size = 7, angle = 0, hjust = 1, margin = margin(0, 3, 0, 0)),
        strip.placement = "outside", panel.spacing = unit(1.5, "pt"), plot.margin = margin(2, 2, 2, 2))
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/si_satellite_scenes.png", p, width = 11, height = 6.6, dpi = 300, bg = "white")
ggsave("output/figures/other/si_satellite_scenes.pdf", p, width = 11, height = 6.6, device = cairo_pdf)
print(as.data.frame(lab %>% select(site, win, date, clear) %>% mutate(win = gsub("\n", " ", win), clear = round(clear, 2))))
