# =============================================================================
# Above-water exposure of woody surfaces (stems, prop roots) and the stem CH4
# height profile referenced to the water surface.
# Shared by 07_upscaling/02_upscale_methane.R and 03_upscale_co2.R.
#
# Stem CH4 is fitted against height above the water surface where water was
# present (height above the soil surface where no standing water was present): emission peaks
# just above the water line, where gas diffuses through tissue and air rather
# than water, and the profile moves up and down with the water level. Bark
# below the water emits to the water (already in the water-surface flux), not
# to the air. (A simplification: transport distance, oxidation and sap flow
# also shape the profile.)
#
# Water depth at the trees, per site x campaign:
#   tidal intact sites (SRS5, SRS6): every hour of the campaign month, FCE LTER
#     level above the soil (knb-lter-fce.1168) at the mean floor height of the
#     plot (01b_flood_fraction.R): depth = max(0, level + floor_mu).
#   non-tidal sites (CP40, FLM30): the water depths we recorded at stem, root
#     and downed-wood positions (zeros included), one sample per reading.
# TLS woody surface is in 0.5 m bins above the TLS ground (z0 = bin lower
# edge). Where water stood at scan time the TLS ground is the water surface, so
# depths are taken relative to the scan-time level: w - w_scan
# (01c_tls_datum.R; TLS_DATUM=ground treats the TLS ground as the sediment).
# For a bin [z0, z1] and depth w the exposed part is [max(z0, w), z1]; with the
# fitted profile f(h) = exp(a + b h), h = z - w, the mean flux per unit bark
# area over the bin is exactly integrated and averaged over the depth samples.
# =============================================================================
depth_samples_setup <- function(flux, project_dir = ".", max_n = 60) {
  ff <- read.csv(file.path(project_dir, "output", "upscaling", "flood_fraction.csv"))
  wl <- read.csv(file.path(project_dir, "data", "environmental", "water_level", "FCE_LTER_1168_water_levels.csv"))
  wl <- wl[wl$SITENAME %in% c("SRS5", "SRS6") & wl$WaterLevel > -9000, ]
  camp_ym <- c("Oct 2022" = "2022-10", "Mar 2023" = "2023-03", "Mar 2022" = "2022-03")
  dat <- file.path(project_dir, "output", "upscaling", "tls_datum_offset.csv")
  w_scan <- if (Sys.getenv("TLS_DATUM", "water") == "ground" || !file.exists(dat)) list() else {
    d <- read.csv(dat); as.list(setNames(d$w_scan_cm, d$site)) }
  off <- function(site) if (is.null(w_scan[[site]])) 0 else w_scan[[site]]
  thin <- function(x) if (length(x) > max_n) unname(quantile(x, (seq_len(max_n) - 0.5) / max_n, type = 1)) else x
  function(site, camp) {
    camp <- as.character(camp)
    if (site %in% c("SRS5", "SRS6")) {
      mu <- ff$floor_mu_cm[ff$site == site][1]
      h <- wl$WaterLevel[wl$SITENAME == site & substr(wl$Date, 1, 7) == camp_ym[[camp]]]
      return((thin(pmax(0, h + mu)) - off(site)) / 100)
    }
    r <- flux$water_depth[flux$plot == site & as.character(flux$campaign) == camp &
                            flux$component %in% c("stem", "root", "cwd") & !is.na(flux$water_depth)]
    if (!length(r)) return(-off(site) / 100)
    (thin(pmax(0, r)) - off(site)) / 100
  }
}

# mean per-area flux over bin [z0, z1] (m above ground), profile exp(a + b h),
# averaged over depth samples w (m); a, b may be vectors (Monte Carlo draws)
stem_bin_flux <- function(a, b, z0, z1, w) {
  out <- 0
  for (wk in w) {
    lo <- max(z0, wk)
    if (lo >= z1) next
    hb <- z1 - wk; la <- lo - wk
    seg <- ifelse(abs(b) < 1e-9, exp(a) * (z1 - lo), exp(a) * (exp(b * hb) - exp(b * la)) / b)
    out <- out + seg
  }
  out / length(w) / (z1 - z0)
}

# mean exposed fraction of bin [z0, z1] over depth samples w
exposed_frac <- function(z0, z1, w) mean(pmax(0, z1 - pmax(z0, w)) / (z1 - z0))
