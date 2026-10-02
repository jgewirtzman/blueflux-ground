# =============================================================================
# QA / sensitivity: class CO2 budget and net forcing under combinations of the
# day -> 24 h temperature sensitivity (Q10) and the downed-wood volume, against
# the tower (US-Skr 2022-23 campaign NEE; Barr et al. 2010) and CARAFE.
# Re-runs 07_upscaling/03_upscale_co2.R and 04_net_forcing.R with the
# CO2_Q10 / CWD_VOL_M3HA overrides, then restores the defaults.
# Writes output/qa/budget_scenarios.csv.
# =============================================================================
suppressMessages(library(dplyr))
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
q10s <- c(none = "1", tower_within_month = "tower", literature = "2", stem_chambers = "stem")   # "tower" = default
vols <- c(negligible = "0.01", krauss_lo = "13", krauss_mean = "67", krauss_eyewall = "132", krauss_hi = "181")
u <- 12.011e-6 * 3.156e7                       # umol m-2 s-1 -> g C m-2 yr-1
run <- function(q, v) {
  env <- c(paste0("CO2_Q10=", q), paste0("CWD_VOL_M3HA=", v))
  for (s in c("code/07_upscaling/03_upscale_co2.R", "code/07_upscaling/04_net_forcing.R"))
    stopifnot(system2("Rscript", s, env = env, stdout = FALSE, stderr = FALSE) == 0)
  x <- read.csv("output/upscaling/plot_level_CO2_totals.csv"); f <- read.csv("output/upscaling/net_forcing_by_class.csv")
  tq <- read.csv("output/upscaling/co2_temperature_correction.csv")
  cls <- function(k) x[x$disturbance_level == k, ]
  tibble(q10_used = tq$Q10[1], healthy_Reco_umol = mean(cls("healthy")$Reco), healthy_cwd_umol = mean(cls("healthy")$cwd),
         healthy_NEE_gC = mean(cls("healthy")$NEE_bottomup) * u, healthy_net100 = f$net100[f$disturbance_level == "healthy"],
         ghost_Reco_umol = mean(cls("ghost")$Reco), ghost_cwd_umol = mean(cls("ghost")$cwd),
         ghost_NEE_gC = mean(cls("ghost")$NEE_bottomup) * u, ghost_net100 = f$net100[f$disturbance_level == "ghost"],
         tower_NEE_gC = mean(unique(cls("healthy")$NEE_tower)) * u, tower_Reco_umol = mean(unique(cls("healthy")$Reco_tower_part)))
}
out <- bind_rows(lapply(names(q10s), function(qn) bind_rows(lapply(names(vols), function(vn)
  run(q10s[[qn]], vols[[vn]]) %>% mutate(q10_case = qn, cwd_case = vn, cwd_vol_m3ha = as.numeric(vols[[vn]]))))))
src <- read.csv("output/upscaling/budget_sources_totals.csv")
car <- function(k) src$value[src$term == "NEE" & grepl("CARAFE", src$source) & src$class == k][1]
out <- out %>% mutate(barr2010_NEE_gC = -1170, carafe_NEE_healthy_gC = car("Healthy"), carafe_NEE_ghost_gC = car("Ghost")) %>%
  select(q10_case, q10_used, cwd_case, cwd_vol_m3ha, everything())
write.csv(out, "output/qa/budget_scenarios.csv", row.names = FALSE)
# restore the defaults
for (s in c("code/07_upscaling/03_upscale_co2.R", "code/07_upscaling/04_net_forcing.R")) system2("Rscript", s, stdout = FALSE, stderr = FALSE)
print(as.data.frame(out %>% transmute(q10_case, cwd_case, h_NEE = round(healthy_NEE_gC), h_net100 = round(healthy_net100),
                                      g_NEE = round(ghost_NEE_gC), g_net100 = round(ghost_net100))), row.names = FALSE)
