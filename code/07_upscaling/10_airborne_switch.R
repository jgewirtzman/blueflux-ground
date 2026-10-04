# =============================================================================
# The intact-to-ghost switch by three independent routes:
#   chamber   site budgets (this study; forcing_switch_per_m2.csv, Monte Carlo 95%)
#   airborne  CARAFE class fluxes only (Delaria et al. 2024;
#             data/carafe_topdown/delaria_endmembers_campaign.csv), four deployments,
#             annual = mean of wet (Oct 2022) and dry (mean of Apr 2022, Feb and Apr 2023)
#             as the chamber budgets; midday CO2 converted to daily with the tower
#             regression of Doughty et al. 2026 (fig. S7: daily = 0.27 midday + 0.58
#             umol m-2 s-1), applied to both classes; CH4 taken as daily. Sensitivity:
#             ghost midday = daily (no night-time respiration). Monte Carlo on each
#             deployment's SE (10,000 draws).
#   model     regional MODIS-reflectance upscaling (Doughty et al. 2026, fig. S2C/S3C):
#             mangrove-forest vs ghost-forest pixel classes, 2022-2024 means read from
#             the published figures (CO2 -2.5 vs -1.5 umol m-2 s-1; CH4 18.5 vs 27 nmol
#             m-2 s-1); ghost pixels are the Lagomasino et al. 2021 dieback map at 500 m,
#             recovering stands included. No interval (point read from figures).
# GWP20 81.2, GWP100 27.9. Writes output/upscaling/switch_by_method.csv.
# =============================================================================
suppressMessages(library(dplyr))
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/00_lib/seed.R")
u_ch4 <- 16.04e-9 * 3.156e7; u_co2 <- 44.01e-6 * 3.156e7
gwp <- c(GWP20 = 81.2, GWP100 = 27.9)
deps <- c("Apr 2022", "Oct 2022", "Feb 2023", "Apr 2023")
e <- read.csv("data/carafe_topdown/delaria_endmembers_campaign.csv") %>% filter(campaign %in% deps) %>%
  mutate(k = ifelse(grepl("ghost", class), "ghost", "intact"))
set.seed(seed_for("airborne_switch")); N <- 10000
ann <- function(gas, kk, conv) { d <- e[e$gas == gas & e$k == kk, ]
  m <- sapply(seq_len(nrow(d)), function(i) { x <- rnorm(N, d$flux[i], d$se[i]); if (conv) 0.27 * x + 0.58 else x })
  as.vector(m %*% ifelse(d$campaign == "Oct 2022", 0.5, 0.5 / 3)) }
q <- function(x) c(median(x), quantile(x, c(0.025, 0.975)))
air <- function(label, ghost_conv) {
  ic <- ann("CO2", "intact", TRUE) * u_co2; gc <- ann("CO2", "ghost", ghost_conv) * u_co2
  im <- ann("CH4", "intact", FALSE) * u_ch4; gm <- ann("CH4", "ghost", FALSE) * u_ch4
  bind_rows(lapply(names(gwp), function(h) { sw <- (gc - ic) + gwp[[h]] * (gm - im)
    data.frame(method = label, horizon = h, co2_part = median(gc - ic), ch4_part = median(gwp[[h]] * (gm - im)),
               switch = q(sw)[1], lo = q(sw)[2], hi = q(sw)[3], p_pos = mean(sw > 0)) }))
}
sw <- with(read.csv("output/upscaling/forcing_switch_per_m2.csv"), setNames(g_co2eq_m2_yr, term))
nf <- read.csv("output/upscaling/net_forcing_by_class.csv")
dco2 <- nf$co2_g_yr[nf$disturbance_level == "ghost"] - nf$co2_g_yr[nf$disturbance_level == "healthy"]
dch4g <- nf$ch4_g_yr[nf$disturbance_level == "ghost"] - nf$ch4_g_yr[nf$disturbance_level == "healthy"]
chamber <- data.frame(method = "Chamber budgets (this study)", horizon = names(gwp), co2_part = dco2, ch4_part = gwp * dch4g,
                      switch = c(sw[["gwp20"]], sw[["gwp100"]]), lo = c(sw[["gwp20_lo"]], sw[["gwp100_lo"]]),
                      hi = c(sw[["gwp20_hi"]], sw[["gwp100_hi"]]), p_pos = NA)
mod <- data.frame(method = "Regional model (Doughty et al. 2026)", horizon = names(gwp),
                  co2_part = (-1.5 - -2.5) * u_co2, ch4_part = gwp * (27 - 18.5) * u_ch4, lo = NA, hi = NA, p_pos = NA) %>%
  mutate(switch = co2_part + ch4_part)
res <- bind_rows(chamber, air("Airborne (Delaria et al. 2024; Doughty conversion)", TRUE),
                 air("Airborne, ghost midday = daily (sensitivity)", FALSE), mod)
write.csv(res, "output/upscaling/switch_by_method.csv", row.names = FALSE)
print(res %>% mutate(across(c(co2_part, ch4_part, switch, lo, hi), round), p_pos = round(p_pos, 3)))
