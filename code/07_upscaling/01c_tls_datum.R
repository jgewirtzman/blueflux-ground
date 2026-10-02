# =============================================================================
# Water standing at the TLS "ground" surface when the plots were scanned.
# The scanner is near-infrared, which does not return from below a water
# surface, so where standing water was present at scan time the TLS height
# datum (z = 0, lowest visible points) is the water surface, not the sediment.
# Woody surface heights are then relative to the scan-time water level, and
# the waterline exposure (00_lib/exposure.R) uses depth relative to that level:
# w_eff = w - w_scan.
#
# Scan dates and times: file names of the ORNL DAAC TLS archive (Xiong,
# Lagomasino & Poulter 2024, doi:10.3334/ORNLDAAC/2311; NASA CMR collection
# C3170821246-ORNL_CLOUD), listed via the CMR API and cached in
# data/tls/tls_scan_files_ornl.csv. Scans used for the surface areas: SRS5 and
# SRS6 October 2022; CP40 and FLM30 March 2023 (Xiong et al. TLS manuscript).
# File-name times are read as local clock time (the DAAC guide's example).
#   SRS5, SRS6: FCE LTER logger level (knb-lter-fce.1168, local standard time)
#     at the scan hours plus the plot mean floor height (01b_flood_fraction.R);
#     w_scan = mean over scan hours of max(0, level + floor_mu).
#   CP40, FLM30: no logger; w_scan = median of our depth readings at stem,
#     root and downed-wood positions in March 2023 (2-6 days after the scans;
#     zeros included). A proxy.
# Writes output/upscaling/tls_datum_offset.csv.
# =============================================================================
suppressMessages(library(dplyr))
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
lst <- "data/tls/tls_scan_files_ornl.csv"
if (!file.exists(lst)) {
  js <- jsonlite::fromJSON("https://cmr.earthdata.nasa.gov/search/granules.json?collection_concept_id=C3170821246-ORNL_CLOUD&page_size=500")
  write.csv(data.frame(title = js$feed$entry$title), lst, row.names = FALSE)
}
sc <- read.csv(lst) %>%
  mutate(f = sub("^[^.]*\\.", "", title),
         site = toupper(sub("_.*$", "", f))) %>%
  filter(grepl("\\.las$", f)) %>%
  mutate(stamp = sub(".*-([0-9]{6}_[0-9]{6}).*", "\\1", f),
         t_local = as.POSIXct(stamp, format = "%y%m%d_%H%M%S", tz = "America/New_York"))
used <- list(SRS5 = "2022-10", SRS6 = "2022-10", CP40 = "2023-03", FLM30 = "2023-03")
sc <- sc %>% filter(site %in% names(used)) %>% filter(format(t_local, "%Y-%m") == unlist(used)[site])

ff <- read.csv("output/upscaling/flood_fraction.csv")
wl <- read.csv("data/environmental/water_level/FCE_LTER_1168_water_levels.csv") %>%
  filter(SITENAME %in% c("SRS5", "SRS6"), WaterLevel > -9000) %>%
  mutate(t = as.POSIXct(paste(Date, Time), tz = "Etc/GMT+5"))
fx <- read.csv("output/data_products/combined_gas_flux_dataset.csv")
out <- lapply(names(used), function(s) {
  x <- sc[sc$site == s, ]
  hrs <- unique(as.POSIXct(format(x$t_local, tz = "Etc/GMT+5", "%Y-%m-%d %H:00:00"), tz = "Etc/GMT+5"))
  if (s %in% c("SRS5", "SRS6")) {
    mu <- ff$floor_mu_cm[ff$site == s][1]
    lev <- wl$WaterLevel[wl$SITENAME == s & wl$t %in% hrs]
    w <- mean(pmax(0, lev + mu)); src <- "FCE logger at scan hours + plot mean floor height"
  } else {
    r <- fx$water_depth[fx$plot == s & fx$month_year == "2023-03" & fx$component %in% c("stem", "root", "cwd") & !is.na(fx$water_depth)]
    w <- median(pmax(0, r)); src <- "median of our March 2023 depth readings (proxy)"
  }
  data.frame(site = s, scan_dates = paste(unique(format(x$t_local, "%Y-%m-%d")), collapse = ";"),
             n_scans = nrow(x), w_scan_cm = round(w, 2), source = src)
}) %>% bind_rows()
write.csv(out, "output/upscaling/tls_datum_offset.csv", row.names = FALSE)
print(out, row.names = FALSE)
