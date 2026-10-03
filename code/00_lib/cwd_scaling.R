# =============================================================================
# Downed coarse woody debris (CWD): plot surface area and exposure to the air.
# Shared by 07_upscaling/02_upscale_methane.R and 03_upscale_co2.R.
#
# Area: no plot inventory of downed wood exists (TLS models standing trees
# only; standing dead trees are in the stem term). Following Troxler et al.
# (2015, SRS-6), the wood volume of Krauss et al. (2005) -- South Florida
# mangroves 9-10 yr after Hurricane Andrew, all-site mean 67 m3 ha-1, range
# 13-181 -- is converted to lateral surface with a 10 cm piece diameter:
#   SA (m2 per m2 ground) = 4 V / d.
# Context (SI): FCE LTER woody litterfall at SRS4-6 was 68-95 g m-2 yr-1 in
# 2001-04, peaked after Wilma (2005) and Irma (2017), and was 47-56 in 2022-23;
# our campaigns were ~5 yr after Irma.
#
# Exposure: downed wood exchanges gas with the air only above the water.
#   tidal sites: low tide 1, high tide 0 (as soil);
#   always-flooded (ghost) sites: arc of a lying log of the median measured CWD
#   diameter above the waterline at the site x campaign mean water depth,
#   acos((h - r) / r) / pi.
# Overrides for sensitivity: CWD_VOL_M3HA, CWD_D_M (environment variables).
# =============================================================================
CWD_VOL_CENTRAL <- 67; CWD_VOL_LO <- 13; CWD_VOL_HI <- 181      # Krauss et al. 2005, m3 ha-1
CWD_VOL_M3HA <- suppressWarnings(as.numeric(Sys.getenv("CWD_VOL_M3HA", as.character(CWD_VOL_CENTRAL))))
CWD_D_M      <- as.numeric(Sys.getenv("CWD_D_M", "0.10"))       # Troxler et al. 2015
# lognormal spread matching the Krauss range as ~95 % bounds (for Monte Carlo)
CWD_SDLOG <- mean(c(log(CWD_VOL_HI / CWD_VOL_CENTRAL), log(CWD_VOL_CENTRAL / CWD_VOL_LO))) / 1.96

cwd_sa_of <- function(plot_area, vol = CWD_VOL_M3HA) 4 * vol / 1e4 / CWD_D_M * plot_area   # m2 wood per plot

# flux: the analysis dataset (needs plot, component, diameter, water_depth, and
# a campaign column labelled like the upscaling scripts' campaigns)
cwd_exposure_setup <- function(flux, campaign_col = "campaign") {
  d_obs <- stats::median(flux$diameter[flux$component == "cwd"], na.rm = TRUE) / 100
  dep <- flux[!is.na(flux$water_depth), ]
  depth <- stats::aggregate(dep$water_depth, by = list(plot = dep$plot, campaign = as.character(dep[[campaign_col]])), FUN = mean)
  names(depth)[3] <- "h_cm"
  function(site, camp, tide) {
    if (tide == "high_tide") return(0)
    if (tide == "low_tide") return(1)
    h <- depth$h_cm[depth$plot == site & depth$campaign == as.character(camp)] / 100
    if (!length(h) || !is.finite(h)) return(0)
    r <- d_obs / 2
    if (h <= 0) return(1)
    if (h >= 2 * r) return(0)
    acos((h - r) / r) / pi
  }
}
