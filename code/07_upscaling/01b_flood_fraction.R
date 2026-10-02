# =============================================================================
# Fraction of time the forest floor is flooded at the tidal sites (SRS5, SRS6),
# per campaign month, from the FCE LTER hourly water level above the soil
# surface (knb-lter-fce.1168.15; Castaneda-Moya et al., "Water Levels and
# Porewater Temperature data from the Shark River and Taylor River Slough
# mangrove sites", May 2001 - ongoing).
#
# The upscaling weights its high-tide (floor flooded: water flux, no soil/CWD)
# and low-tide (floor exposed) states by this fraction instead of 50/50.
#
# Central (frac_flooded): AREA-WEIGHTED. The floor is uneven (at one time our
# depth readings differ by up to 27 cm), so part of the plot is under water and
# part exposed. Every depth reading we took at SRS5/SRS6 is a sample of floor
# height relative to the logger: a wet reading gives it exactly (depth -
# logger); a reading without standing water only says the floor was above the water then (left-
# censored at -logger). A normal distribution of floor height is fitted per
# site by censored maximum likelihood (survival::survreg), and the flooded share
# of the plot each hour is Phi((level + mu) / sigma), averaged over the campaign
# month. Range (lo/hi): mu -/+ 1.96 SE. The samples sit where chambers went
# (stems, roots, collars), not at random points.
# Alternatives: frac_flooded_switch (whole floor flooded when the offset-
# corrected logger level > 0) and frac_flooded_longterm (area-weighted, 2010
# onward). The logger is checked against our wet readings (offset_cm).
# Writes output/upscaling/flood_fraction.csv.
# =============================================================================
suppressMessages({library(dplyr); library(survival)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

wl_file <- "data/environmental/water_level/FCE_LTER_1168_water_levels.csv"
wl_url  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-fce/1168/15/c410dd3aba0813c672d813ad57906f8a?key=4otuhJIXqV6DgHooWPpQZ6DPUig"
if (!file.exists(wl_file)) {
  dir.create(dirname(wl_file), recursive = TRUE, showWarnings = FALSE)
  download.file(wl_url, wl_file, mode = "wb", quiet = TRUE)
}
TIDAL <- c("SRS5", "SRS6")
CAMPS <- c("2022-03" = "Mar 2022", "2022-10" = "Oct 2022", "2023-03" = "Mar 2023")

wl <- read.csv(wl_file) %>% filter(SITENAME %in% TIDAL) %>%
  mutate(WaterLevel = ifelse(WaterLevel < -9000, NA, WaterLevel),
         t = as.numeric(as.POSIXct(paste(Date, Time), tz = "Etc/GMT+5")),   # logger: local standard time
         ym = substr(Date, 1, 7)) %>%
  filter(!is.na(WaterLevel))

# logger vs our water-depth readings at the same time
fx <- read.csv("output/data_products/combined_gas_flux_dataset.csv") %>%
  filter(plot %in% TIDAL, !is.na(water_depth), !is.na(start_time)) %>%
  mutate(t = as.numeric(as.POSIXct(paste(date, start_time), tz = "America/New_York")))
fx$logger <- NA_real_
for (p in TIDAL) {
  i <- fx$plot == p; w <- wl[wl$SITENAME == p, ]
  fx$logger[i] <- approx(w$t, w$WaterLevel, fx$t[i])$y
}
cal <- fx %>% filter(water_depth > 0, !is.na(logger)) %>% group_by(site = plot) %>%
  summarise(offset_cm = median(water_depth - logger), n_cal = n(), r_cal = cor(water_depth, logger), .groups = "drop")

# floor-height distribution relative to the logger (censored normal)
fx <- fx %>% filter(!is.na(logger)) %>%
  mutate(o = ifelse(water_depth > 0, water_depth - logger, -logger), wet = as.integer(water_depth > 0))
floor <- bind_rows(lapply(TIDAL, function(p) {
  m <- survreg(Surv(o, wet, type = "left") ~ 1, data = fx[fx$plot == p, ], dist = "gaussian")
  data.frame(site = p, floor_mu_cm = coef(m)[[1]], floor_mu_se = sqrt(vcov(m)[1, 1]), floor_sd_cm = m$scale,
             n_floor = sum(fx$plot == p), n_floor_wet = sum(fx$wet[fx$plot == p]))
}))
area <- function(h, mu, sd) mean(pnorm((h + mu) / sd))

ff <- wl %>% filter(ym %in% names(CAMPS)) %>% rename(site = SITENAME) %>%
  left_join(cal, by = "site") %>% left_join(floor, by = "site") %>% mutate(h = WaterLevel + offset_cm) %>%
  group_by(site, campaign = unname(CAMPS[ym])) %>%
  summarise(frac_flooded = area(WaterLevel, first(floor_mu_cm), first(floor_sd_cm)),
            frac_flooded_lo = area(WaterLevel, first(floor_mu_cm) - 1.96 * first(floor_mu_se), first(floor_sd_cm)),
            frac_flooded_hi = area(WaterLevel, first(floor_mu_cm) + 1.96 * first(floor_mu_se), first(floor_sd_cm)),
            frac_flooded_switch = mean(h > 0), frac_flooded_switch_raw = mean(WaterLevel > 0),
            mean_level_cm = mean(h), mean_depth_flooded_cm = mean(h[h > 0]),
            n_hours = n(), .groups = "drop") %>%
  left_join(floor, by = "site") %>%
  left_join(wl %>% filter(Date >= "2010-01-01") %>% rename(site = SITENAME) %>% left_join(floor, by = "site") %>%
              group_by(site) %>% summarise(frac_flooded_longterm = area(WaterLevel, first(floor_mu_cm), first(floor_sd_cm)),
                                           .groups = "drop"), by = "site") %>%
  left_join(cal, by = "site") %>%
  left_join(wl %>% filter(Date >= "2010-01-01") %>% group_by(site = SITENAME) %>%
              summarise(frac_flooded_2010_on = mean(WaterLevel > 0), .groups = "drop"), by = "site")
dir.create("output/upscaling", showWarnings = FALSE, recursive = TRUE)
write.csv(ff, "output/upscaling/flood_fraction.csv", row.names = FALSE)
cat("Flooded share of the plot floor (FCE LTER water level x censored floor-height model from our depth readings):\n")
print(as.data.frame(ff %>% select(site, campaign, frac_flooded, frac_flooded_lo, frac_flooded_hi, frac_flooded_switch,
                                    frac_flooded_longterm, floor_mu_cm, floor_sd_cm) %>%
                      mutate(across(where(is.numeric), ~ round(.x, 2)))), row.names = FALSE)
