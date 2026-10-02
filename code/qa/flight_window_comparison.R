# =============================================================================
# QA: like-for-like comparison with CARAFE (Delaria et al.) flight fluxes.
# CARAFE flies around midday, so its fluxes are compared here with a midday
# bottom-up product (product A), and the midday -> 24 h adjustments that turn it
# into the daily budget (product B, 07_upscaling) are tabulated term by term.
#
# Product A, per site x campaign (umol CO2 m-2 s-1; nmol CH4 m-2 s-1):
#   chamber stem, root, soil, water, CWD CO2 at measurement time (the 24-h
#   temperature factor of 03_upscale_co2.R removed; chambers ran ~10:00-15:00);
#   leaf respiration at midday tower TA with 30 % light inhibition;
#   tower GPP and NEE averaged over the midday window; ghost GPP = 0;
#   CH4 as measured (daytime chambers).
# Midday window: 11:00-15:00 local standard time (10:00-16:00 as sensitivity).
# CARAFE: Oct 2022 flights; Mar 2023 analog = mean of Feb and Apr 2023 flights;
# SE as reported per flight period.
# Writes output/qa/flight_window_comparison.csv and
# output/qa/midday_to_daily_adjustments.csv.
# =============================================================================
suppressMessages(library(dplyr))
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

LAI <- 2.8; K_EXT <- 0.5; Rd25 <- 1.55; INHIB <- 0.30
effLAI <- function(L) (1 - exp(-K_EXT * L)) / K_EXT
f_T <- function(T) exp(0.1012 * (T - 25) - 0.0005 * (T^2 - 25^2))
camps <- c("Oct 2022", "Mar 2023")

tw <- read.csv("output/gpp/US-Skr_GPP_halfhourly_Mar2022_Oct2022_Mar2023.csv") %>%
  mutate(campaign = case_when(year == 2022 & month == 10 ~ "Oct 2022", year == 2023 & month == 3 ~ "Mar 2023"),
         h = hour + minute / 60, TA = ifelse(!is.na(TA) & TA > -900, TA, TA_model)) %>%
  filter(!is.na(campaign))
midday <- function(lo, hi) tw %>% filter(h >= lo, h < hi) %>% group_by(campaign) %>%
  summarise(GPP = mean(GPP, na.rm = TRUE), NEE_tower = mean(NEE_gapfilled, na.rm = TRUE),
            Reco_tower = mean(Reco, na.rm = TRUE), TA = mean(TA, na.rm = TRUE), .groups = "drop") %>%
  mutate(leaf = Rd25 * f_T(TA) * (1 - INHIB) * effLAI(LAI))
daily <- tw %>% group_by(campaign) %>%
  summarise(GPP = mean(GPP, na.rm = TRUE), NEE_tower = mean(NEE_gapfilled, na.rm = TRUE),
            Reco_tower = mean(Reco, na.rm = TRUE), .groups = "drop")

# chamber components back to measurement time
fac <- read.csv("output/upscaling/co2_temperature_correction.csv") %>% select(campaign, component, factor)
comp <- read.csv("output/upscaling/summary_CO2_by_component.csv") %>% filter(campaign %in% camps)
undo <- function(d) {
  for (cc in c("stem", "root", "soil", "cwd")) {
    f <- fac$factor[match(paste(d$campaign, cc), paste(fac$campaign, fac$component))]
    d[[cc]] <- d[[cc]] / ifelse(is.na(f), 1, f)
  }
  d
}
comp_meas <- undo(comp)

ch4 <- read.csv("output/upscaling/summary_CH4_by_component.csv") %>%
  filter(scenario == "exponential", campaign %in% camps) %>%
  mutate(CH4_nmol = total / 16.04 * 1e6 / 86400) %>%                # mg m-2 d-1 -> nmol m-2 s-1
  group_by(campaign, disturbance_level) %>% summarise(CH4_nmol = mean(CH4_nmol), .groups = "drop")

build <- function(lo, hi) {
  md <- midday(lo, hi)
  comp_meas %>% select(-leaf, -Reco, -Reco_noleaf, -Reco_g) %>% left_join(md, by = "campaign") %>%
    mutate(leaf = ifelse(disturbance_level == "healthy", leaf, 0),
           GPP = ifelse(disturbance_level == "healthy", GPP, 0),
           Reco = stem + root + soil + water + cwd + leaf, NEE = Reco - GPP) %>%
    group_by(campaign, class = disturbance_level) %>%
    summarise(across(c(stem, root, soil, water, cwd, leaf, Reco, GPP, NEE, NEE_tower, Reco_tower), mean), .groups = "drop") %>%
    mutate(window = sprintf("%02d-%02d h", lo, hi))
}
A <- bind_rows(build(11, 15), build(10, 16)) %>%
  left_join(ch4 %>% rename(class = disturbance_level), by = c("campaign", "class"))

# CARAFE flight means, aligned to our campaigns
cf <- read.csv("data/carafe_topdown/delaria_endmembers_campaign.csv") %>%
  mutate(class = recode(class, mangrove_forest = "healthy", ghost_forest = "ghost"),
         cmp = case_when(campaign == "Oct 2022" ~ "Oct 2022", campaign %in% c("Feb 2023", "Apr 2023") ~ "Mar 2023")) %>%
  filter(!is.na(cmp)) %>% group_by(gas, campaign = cmp, class) %>%
  summarise(carafe = mean(flux), carafe_se = sqrt(sum(se^2)) / n(), .groups = "drop")
cmpA <- A %>% left_join(cf %>% filter(gas == "CO2") %>% select(campaign, class, CARAFE_CO2 = carafe, CARAFE_CO2_se = carafe_se), by = c("campaign", "class")) %>%
  left_join(cf %>% filter(gas == "CH4") %>% select(campaign, class, CARAFE_CH4 = carafe, CARAFE_CH4_se = carafe_se), by = c("campaign", "class")) %>%
  mutate(CO2_diff_in_SE = (NEE - CARAFE_CO2) / CARAFE_CO2_se, CH4_diff_in_SE = (CH4_nmol - CARAFE_CH4) / CARAFE_CH4_se)
dir.create("output/qa", showWarnings = FALSE)
write.csv(cmpA, "output/qa/flight_window_comparison.csv", row.names = FALSE)

# midday (A, 11-15 h) -> daily (B) adjustments, healthy and ghost
B <- read.csv("output/upscaling/plot_level_CO2_totals.csv") %>% filter(campaign %in% camps) %>%
  group_by(campaign, class = disturbance_level) %>%
  summarise(across(c(stem, root, soil, water, cwd, leaf, Reco, GPP_used, NEE_bottomup), mean), .groups = "drop")
adj <- A %>% filter(window == "11-15 h") %>% inner_join(B, by = c("campaign", "class"), suffix = c("_midday", "_daily")) %>%
  transmute(campaign, class,
            chamber_temperature = (stem_daily + root_daily + soil_daily + cwd_daily) - (stem_midday + root_midday + soil_midday + cwd_midday),
            leaf_temperature_and_light = leaf_daily - leaf_midday,
            GPP_diel = -(GPP_used - GPP),
            NEE_midday = NEE, NEE_daily = NEE_bottomup,
            check = NEE + chamber_temperature + leaf_temperature_and_light + GPP_diel - NEE_daily)
write.csv(adj, "output/qa/midday_to_daily_adjustments.csv", row.names = FALSE)

cat("Midday (11-15 h) bottom-up vs CARAFE flights (umol CO2 / nmol CH4 m-2 s-1):\n")
print(as.data.frame(cmpA %>% filter(window == "11-15 h") %>%
  transmute(campaign, class, Reco = round(Reco, 2), GPP = round(GPP, 2), NEE = round(NEE, 2), tower_NEE = round(NEE_tower, 2),
            CARAFE = sprintf("%.1f +/- %.1f", CARAFE_CO2, CARAFE_CO2_se), CO2_z = round(CO2_diff_in_SE, 1),
            CH4 = round(CH4_nmol, 1), CARAFE_CH4 = sprintf("%.1f +/- %.1f", CARAFE_CH4, CARAFE_CH4_se), CH4_z = round(CH4_diff_in_SE, 1))), row.names = FALSE)
cat("\nMidday -> daily adjustments (umol CO2 m-2 s-1):\n")
print(as.data.frame(adj %>% mutate(across(where(is.numeric), ~ round(.x, 2)))), row.names = FALSE)
