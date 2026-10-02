# =============================================================================
# Weights of the tide states in the upscaling. Tidal sites (SRS5, SRS6) are
# budgeted at high tide (floor flooded) and low tide (floor exposed); the class
# budget weights them by the measured fraction of time the floor is flooded in
# that campaign month (output/upscaling/flood_fraction.csv, from
# 07_upscaling/01b_flood_fraction.R). Always-flooded and dry sites ("fixed")
# weigh 1.
# Sensitivity: FLOOD_FRAC = <0-1> (e.g. 0.5, the earlier equal split) or
# "lo" / "hi" (microtopography range); FLOOD_FRAC_FILE = an alternative table
# with site, campaign, frac_flooded (code/qa/flooding_scenarios.R).
# =============================================================================
tide_weight_setup <- function(project_dir = ".") {
  f_file <- Sys.getenv("FLOOD_FRAC_FILE", file.path(project_dir, "output", "upscaling", "flood_fraction.csv"))
  ff <- read.csv(f_file)
  opt <- Sys.getenv("FLOOD_FRAC", "")
  col <- switch(opt, lo = "frac_flooded_lo", hi = "frac_flooded_hi", "frac_flooded")
  fixed <- suppressWarnings(as.numeric(opt))
  function(site, campaign, tide_state) {
    f <- if (is.finite(fixed)) rep(fixed, length(site)) else
      ff[[col]][match(paste(site, campaign), paste(ff$site, ff$campaign))]
    ifelse(tide_state == "high_tide", f, ifelse(tide_state == "low_tide", 1 - f, 1))
  }
}

# Share of prop-root surface below the water at high tide: the TLS 0-0.5 m root
# surface (as a share of all root surface) times the fraction of that bin under
# the campaign-month mean flooded depth (FCE LTER level), assuming surface is
# spread evenly within the bin. Submerged root surface exchanges with the water,
# not the air (as for soil and downed wood). Trunk surface below the water is
# < 1.5 % and is ignored.
root_submerged_setup <- function(tls_all, project_dir = ".") {
  ff <- read.csv(file.path(project_dir, "output", "upscaling", "flood_fraction.csv"))
  r <- tls_all[tls_all$segment_class == "root", ]
  share0 <- tapply(r$Total_surface_area_m2 * (r$height_bin_num == 0), r$site, sum, na.rm = TRUE) /
            tapply(r$Total_surface_area_m2, r$site, sum, na.rm = TRUE)
  function(site, campaign, tide_state) {
    if (tide_state != "high_tide") return(0)
    d <- ff$mean_depth_flooded_cm[ff$site == site & ff$campaign == campaign]
    if (!length(d) || !is.finite(d) || is.na(share0[site])) return(0)
    unname(share0[site]) * min(d, 50) / 50
  }
}
