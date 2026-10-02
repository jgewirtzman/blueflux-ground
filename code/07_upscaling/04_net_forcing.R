# =============================================================================
# Class-level net radiative forcing (point estimates): bottom-up CH4 budget as
# CO2-equivalents + bottom-up net CO2 exchange, for the intact and ghost classes.
#   CH4: plot_level_CH4_totals.csv (exponential stem scenario), tide states
#        averaged 50/50 within site x campaign, then class mean.
#   CO2: plot_level_CO2_totals.csv NEE_bottomup, class mean over site x campaign.
# Output: output/upscaling/net_forcing_by_class.csv (read by plot_closure.R and assemble_carbon_budget.R;
# Monte Carlo intervals for the same quantities come from mc_co2_forcing.R).
# =============================================================================
suppressMessages({library(dplyr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

GWP100 <- 27.9; GWP20 <- 81.2
umol_to_g_yr <- 44e-6*3.156e7          # umol CO2 m-2 s-1 -> g CO2 m-2 yr-1
mgd_to_gyr   <- 365/1000               # mg CH4 m-2 d-1 -> g CH4 m-2 yr-1

ch4 <- read.csv("output/upscaling/plot_level_CH4_totals.csv") %>%
  filter(scenario=="exponential") %>%
  group_by(site,campaign,disturbance_level) %>%
  summarise(total_mg=weighted.mean(total_mg,tide_weight),.groups="drop") %>%   # tide average, weighted by flooded fraction
  group_by(disturbance_level) %>%
  summarise(ch4_g_yr=mean(total_mg)*mgd_to_gyr,.groups="drop")

co2 <- read.csv("output/upscaling/plot_level_CO2_totals.csv") %>%
  group_by(disturbance_level) %>%
  summarise(nee_umol=mean(NEE_bottomup),.groups="drop") %>%
  mutate(co2_g_yr=nee_umol*umol_to_g_yr)

out <- inner_join(ch4,co2,by="disturbance_level") %>%
  mutate(ch4_co2eq100=ch4_g_yr*GWP100, ch4_co2eq20=ch4_g_yr*GWP20,
         net100=co2_g_yr+ch4_co2eq100, net20=co2_g_yr+ch4_co2eq20,
         # CH4 share of the combined (absolute) CO2 + CH4 forcing
         ch4_pct100=round(100*ch4_co2eq100/(abs(co2_g_yr)+ch4_co2eq100),1),
         ch4_pct20 =round(100*ch4_co2eq20 /(abs(co2_g_yr)+ch4_co2eq20),1))

write.csv(out,"output/upscaling/net_forcing_by_class.csv",row.names=FALSE)
cat("=== Class net forcing (g CO2-eq m-2 yr-1) ===\n")
out %>% mutate(across(where(is.numeric),~round(.x,2))) %>% as.data.frame() %>% print()
