# =============================================================================
# Weights of the tide states in the upscaling. Tidal sites (SRS5, SRS6) are
# budgeted at high tide (floor flooded) and low tide (floor exposed); the class
# budget weights them by the measured fraction of time the floor is flooded in
# that campaign month (output/upscaling/flood_fraction.csv, from
# 07_upscaling/01b_flood_fraction.R). Always-flooded and dry sites ("fixed")
# weigh 1.
# Sensitivity: FLOOD_FRAC = <0-1> (e.g. 0.5, the earlier equal split) or
# "lo" / "hi" (microtopography range).
# =============================================================================
tide_weight_setup <- function(project_dir = ".") {
  ff <- read.csv(file.path(project_dir, "output", "upscaling", "flood_fraction.csv"))
  opt <- Sys.getenv("FLOOD_FRAC", "")
  col <- switch(opt, lo = "frac_flooded_lo", hi = "frac_flooded_hi", "frac_flooded")
  fixed <- suppressWarnings(as.numeric(opt))
  function(site, campaign, tide_state) {
    f <- if (is.finite(fixed)) rep(fixed, length(site)) else
      ff[[col]][match(paste(site, campaign), paste(ff$site, ff$campaign))]
    ifelse(tide_state == "high_tide", f, ifelse(tide_state == "low_tide", 1 - f, 1))
  }
}
