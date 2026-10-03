# =============================================================================
# Ghost-forest floor (CP40, FLM30) without standing water.
# Shared by 07_upscaling/02_upscale_methane.R and 03_upscale_co2.R.
#
# Share of the floor without standing water, per site x campaign: the share of
# our water-depth readings at stem, root and downed-wood positions that were
# zero (October 2022: none; March 2023: 6 of 31 at CP40, 9 of 48 at FLM30).
# That part of the floor emits as soil; the rest through the water surface.
# No ghost soil chambers were run in the analysed campaigns, so the exposed
# soil flux comes from FLM30 soil chambers in March 2022, when the site had no
# standing water (same site and season, different year).
# Sensitivity (environment variables):
#   GHOST_SOIL    = FLM30_2022 (default) | MI (Marco Island ghost soils, Mar
#                   2022-23) | pooled (FLM30 2022 + MI) | BL60 (dieback soils,
#                   Mar 2023) | none (floor fully inundated)
#   GHOST_EXPOSED = no_water (default) | le2cm (readings <= 2 cm count as exposed)
# =============================================================================
ghost_floor_setup <- function(project_dir = ".") {
  d <- read.csv(file.path(project_dir, "output", "data_products", "combined_gas_flux_dataset.csv"))
  camp_of <- c("2022-10" = "Oct 2022", "2023-03" = "Mar 2023")
  src <- Sys.getenv("GHOST_SOIL", "FLM30_2022"); ex <- Sys.getenv("GHOST_EXPOSED", "no_water")
  soil_rows <- switch(src,
    FLM30_2022 = d$plot == "FLM30" & d$month_year == "2022-03",
    MI         = d$plot == "MI",
    pooled     = d$plot %in% c("FLM30", "MI") & d$month_year %in% c("2022-03", "2023-03"),
    BL60       = d$plot == "BL60" & d$month_year == "2023-03",
    none       = rep(FALSE, nrow(d)))
  soil <- d[soil_rows & d$component == "soil", ]
  boot <- function(x, n = 5000) { x <- x[is.finite(x)]; if (length(x) < 2) return(NULL)
    set.seed(11); b <- replicate(n, mean(sample(x, replace = TRUE)))
    list(rate = mean(x), ci_lo = unname(quantile(b, 0.025)), ci_hi = unname(quantile(b, 0.975)), n = length(x),
         source = paste0("ghost floor without standing water: ", src, " soil")) }
  pos <- d[d$plot %in% c("CP40", "FLM30") & d$component %in% c("stem", "root", "cwd") & !is.na(d$water_depth) &
             d$month_year %in% names(camp_of), ]
  pos$campaign <- camp_of[pos$month_year]
  thr <- if (ex == "le2cm") 2 else 0
  list(
    share = function(site, camp) {
      if (src == "none") return(0)
      x <- pos$water_depth[pos$plot == site & pos$campaign == as.character(camp)]
      if (!length(x)) 0 else mean(x <= thr)
    },
    soil_flux = function(gas) if (src == "none") NULL else boot(soil[[paste0(gas, "_best.flux")]])
  )
}
