# =============================================================================
# Fraction of time the forest floor is flooded at the tidal sites (SRS5, SRS6),
# per campaign month, from the FCE LTER hourly water level above the soil
# surface (knb-lter-fce.1168.15; Castaneda-Moya et al., "Water Levels and
# Porewater Temperature data from the Shark River and Taylor River Slough
# mangrove sites", May 2001 - ongoing).
#
# The upscaling weights its high-tide (floor flooded: water flux, no soil/CWD)
# and low-tide (floor exposed) states by this fraction instead of 50/50.
# The logger level is checked against our own water-depth readings: where we
# recorded standing water, offset = median(field depth - logger) (~1 cm), and
# the flooded fraction uses the offset-corrected level. Floor microtopography
# (soil collars sat on raised microsites, ~0 cm while the logger read 4-6 cm)
# is carried as a range: flooded above +5 cm (lo) and above -5 cm (hi).
# Writes output/upscaling/flood_fraction.csv.
# =============================================================================
suppressMessages(library(dplyr))
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

wl_file <- "data/environmental/water_level/FCE_LTER_1168_water_levels.csv"
wl_url  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-fce/1168/15/c410dd3aba0813c672d813ad57906f8a?key=4otuhJIXqV6DgHooWPpQZ6DPUig"
if (!file.exists(wl_file)) {
  dir.create(dirname(wl_file), recursive = TRUE, showWarnings = FALSE)
  download.file(wl_url, wl_file, mode = "wb", quiet = TRUE)
}
TIDAL <- c("SRS5", "SRS6")
CAMPS <- c("2022-03" = "Mar 2022", "2022-10" = "Oct 2022", "2023-03" = "Mar 2023")
MICRO_CM <- 5

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

ff <- wl %>% filter(ym %in% names(CAMPS)) %>% rename(site = SITENAME) %>%
  left_join(cal, by = "site") %>% mutate(h = WaterLevel + offset_cm) %>%
  group_by(site, campaign = unname(CAMPS[ym])) %>%
  summarise(frac_flooded = mean(h > 0), frac_flooded_lo = mean(h > MICRO_CM), frac_flooded_hi = mean(h > -MICRO_CM),
            frac_flooded_raw = mean(WaterLevel > 0), mean_level_cm = mean(h), mean_depth_flooded_cm = mean(h[h > 0]),
            n_hours = n(), .groups = "drop") %>%
  left_join(cal, by = "site") %>%
  left_join(wl %>% filter(Date >= "2010-01-01") %>% group_by(site = SITENAME) %>%
              summarise(frac_flooded_2010_on = mean(WaterLevel > 0), .groups = "drop"), by = "site")
dir.create("output/upscaling", showWarnings = FALSE, recursive = TRUE)
write.csv(ff, "output/upscaling/flood_fraction.csv", row.names = FALSE)
cat("Flooded fraction of the forest floor (FCE LTER water level, offset-corrected to our depth readings):\n")
print(as.data.frame(ff %>% select(site, campaign, frac_flooded, frac_flooded_lo, frac_flooded_hi, offset_cm, n_cal) %>%
                      mutate(across(where(is.numeric), ~ round(.x, 2)))), row.names = FALSE)
