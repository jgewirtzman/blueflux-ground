# =============================================================================
# QA / sensitivity: how the intact (SRS5, SRS6) budgets depend on the flooding
# representation and on the tidal-phase water flux.
#
# Flooding (weights of the high- and low-tide states):
#   equal_split       0.5 (the earlier assumption)
#   switch_campaign   whole floor flooded when the logger level > 0, campaign month
#   switch_longterm   the same, 2010 onward
#   area_campaign     area-weighted (censored floor-height model; central; 01b)
#   area_campaign_lo / _hi   floor mean -/+ 1.96 SE
#   area_longterm     area-weighted, 2010 onward
# Tidal-phase water flux (applied to the intact water term after the run):
#   x1 (as measured), x2, x3.
# Writes output/qa/flooding_scenarios.csv (and restores the default outputs).
# =============================================================================
suppressMessages(library(dplyr))
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
TIDAL <- c("SRS5", "SRS6"); CAMPS <- c("2022-10" = "Oct 2022", "2023-03" = "Mar 2023")
u_gC <- 12.011e-6 * 3.156e7; GWP100 <- 27.9

wl <- read.csv("data/environmental/water_level/FCE_LTER_1168_water_levels.csv") %>%
  filter(SITENAME %in% TIDAL, WaterLevel > -9000) %>%
  mutate(t = as.numeric(as.POSIXct(paste(Date, Time), tz = "Etc/GMT+5")), ym = substr(Date, 1, 7), mo = substr(Date, 6, 7))
ff0 <- read.csv("output/upscaling/flood_fraction.csv")
off_cal <- setNames(ff0$offset_cm[match(TIDAL, ff0$site)], TIDAL)

tab <- function(fun) bind_rows(lapply(TIDAL, function(s) bind_rows(lapply(names(CAMPS), function(m)
  data.frame(site = s, campaign = CAMPS[[m]], frac_flooded = fun(s, m))))))
lvl <- function(s, filt) { w <- wl[wl$SITENAME == s, ]; w$WaterLevel[filt(w)] + off_cal[[s]] }
col <- function(cc) function(s, m) ff0[[cc]][ff0$site == s & ff0$campaign == CAMPS[[m]]]
scen <- list(
  equal_split          = tab(function(s, m) 0.5),
  switch_campaign      = tab(col("frac_flooded_switch")),
  switch_longterm      = tab(function(s, m) mean(lvl(s, function(w) w$Date >= "2010-01-01") > 0)),
  area_campaign        = tab(col("frac_flooded")),          # central
  area_campaign_lo     = tab(col("frac_flooded_lo")),
  area_campaign_hi     = tab(col("frac_flooded_hi")),
  area_longterm        = tab(col("frac_flooded_longterm"))
)
set.seed(1)

tmp <- tempfile(fileext = ".csv")
run <- function(tbl) {
  write.csv(tbl, tmp, row.names = FALSE)
  env <- paste0("FLOOD_FRAC_FILE=", tmp)
  for (s in c("code/07_upscaling/02_upscale_methane.R", "code/07_upscaling/03_upscale_co2.R", "code/07_upscaling/04_net_forcing.R"))
    stopifnot(system2("Rscript", s, env = env, stdout = FALSE, stderr = FALSE) == 0)
  ch <- read.csv("output/upscaling/summary_CH4_by_component.csv") %>% filter(scenario == "exponential", disturbance_level == "healthy")
  co <- read.csv("output/upscaling/plot_level_CO2_totals.csv") %>% filter(disturbance_level == "healthy")
  nf <- read.csv("output/upscaling/net_forcing_by_class.csv") %>% filter(disturbance_level == "healthy")
  data.frame(frac_SRS5 = paste(round(tbl$frac_flooded[tbl$site == "SRS5"], 2), collapse = "/"),
             frac_SRS6 = paste(round(tbl$frac_flooded[tbl$site == "SRS6"], 2), collapse = "/"),
             ch4_g = mean(ch$total) * 0.365, ch4_water_g = mean(ch$water) * 0.365, ch4_soil_g = mean(ch$soil) * 0.365,
             ch4_root_g = mean(ch$root) * 0.365, reco = mean(co$Reco), co2_water = mean(co$water), co2_soil = mean(co$soil),
             nee_gC = mean(co$NEE_bottomup) * u_gC, net100 = nf$net100)
}
res <- bind_rows(lapply(names(scen), function(n) run(scen[[n]]) %>% mutate(flooding = n, .before = 1)))
# tidal-phase multipliers on the intact water term (CH4 and CO2)
mult <- bind_rows(lapply(c(1, 2, 3), function(k) res %>% mutate(
  water_multiplier = k,
  ch4_g = ch4_g + (k - 1) * ch4_water_g,
  nee_gC = nee_gC + (k - 1) * co2_water * u_gC,
  net100 = net100 + (k - 1) * (co2_water * 44.01e-6 * 3.156e7 + ch4_water_g * GWP100))))
write.csv(mult, "output/qa/flooding_scenarios.csv", row.names = FALSE)
unlink(tmp)
for (s in c("code/07_upscaling/02_upscale_methane.R", "code/07_upscaling/03_upscale_co2.R", "code/07_upscaling/04_net_forcing.R"))
  system2("Rscript", s, stdout = FALSE, stderr = FALSE)
print(mult %>% transmute(flooding, water_x = water_multiplier, frac_SRS5, frac_SRS6, CH4_g = round(ch4_g, 2),
                         CH4_water = round(ch4_water_g * water_multiplier, 2), CH4_soil = round(ch4_soil_g, 2), CH4_root = round(ch4_root_g, 2),
                         Reco = round(reco, 2), NEE_gC = round(nee_gC), net100 = round(net100)), row.names = FALSE)
