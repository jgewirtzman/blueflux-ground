# =============================================================================
# Table S1 | Study sites: location, class, sampling, stand structure, hydrology
# and porewater salinity, one column per site.
#   location, class, species   data/sites/site_metadata.csv
#   sampling                   closures per campaign (combined_gas_flux_dataset.csv)
#   stand structure            TLS (data/tls/tree_stats_per_site.csv,
#                              all_sites_summary.csv): stem density, basal area,
#                              mean and 95th-percentile tree height, mean DBH, and
#                              woody surface per m2 ground (stems + branches; prop roots)
#   hydrology                  tidal sites: mean high / low water of each campaign
#                              month above the mean floor (FCE LTER logger + fitted
#                              floor, as si_waterline.R) and the flooded share of the
#                              floor (flood_fraction.csv); other sites: mean (max)
#                              recorded water depth at chamber positions
#   salinity                   porewater PSU by season
#                              (data/environmental/site_characterization_salinity_ch4.csv)
# Writes output/analysis/si/si_site_table.csv (long: site, row, value).
# =============================================================================
suppressMessages({library(dplyr); library(tidyr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

meta <- read.csv("data/sites/site_metadata.csv")
site_lv <- c("SRS5", "SRS6", "BL60", "CP40", "FLM30", "RB10", "SE1", "MI")
role <- c(SRS5 = "intact", SRS6 = "intact", BL60 = "regenerating", CP40 = "ghost", FLM30 = "ghost",
          RB10 = "intact (context)", SE1 = "scrub (context)", MI = "ghost (context)")
camp_lab <- c("2022-03" = "Mar 22", "2022-10" = "Oct 22", "2023-03" = "Mar 23", "2025-10" = "Oct 25")
sg <- function(x) sub("^[+]0$", "0", sprintf("%+.0f", x))
f1 <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = ",")
row <- function(site, r, v) data.frame(site = site, row = r, value = v)

# ---- sampling
fx <- read.csv("output/data_products/combined_gas_flux_dataset.csv") %>% filter(plot %in% site_lv)
samp <- fx %>% count(site = plot, month_year) %>% arrange(site, month_year) %>% group_by(site) %>%
  summarise(value = paste(sprintf("%s (%d)", camp_lab[month_year], n), collapse = "; "), .groups = "drop") %>%
  mutate(row = "Campaigns (closures)")

# ---- stand structure (TLS)
tst <- read.csv("data/tls/tree_stats_per_site.csv")
tls <- read.csv("data/tls/all_sites_summary.csv")
wsa <- tls %>% mutate(part = ifelse(segment_class == "root", "root", "stem")) %>% group_by(site, part) %>%
  summarise(sa = sum(Total_surface_area_m2), .groups = "drop") %>%
  left_join(tst %>% select(site, area_m2), by = "site") %>% mutate(v = sa / area_m2) %>%
  select(site, part, v) %>% pivot_wider(names_from = part, values_from = v)
struct <- tst %>% left_join(wsa, by = "site") %>% transmute(site,
  `Stem density (ha⁻¹)` = f1(tree_density_ha),
  `Basal area (m² ha⁻¹)` = f1(total_ba_m2 / Plot_Area_ha, 1),
  `Tree height, mean / 95th pct. (m)` = sprintf("%s / %s", f1(mean_height, 1), f1(p95_height, 1)),
  `Mean DBH (cm)` = f1(100 * mean_dbh, 1),
  `Woody surface, stem / root (m² m⁻²)` = sprintf("%s / %s", f1(stem, 2), f1(root, 2))) %>%
  pivot_longer(-site, names_to = "row", values_to = "value")

# ---- hydrology
ff <- read.csv("output/upscaling/flood_fraction.csv")
wl <- read.csv("data/environmental/water_level/FCE_LTER_1168_water_levels.csv") %>%
  filter(SITENAME %in% c("SRS5", "SRS6"), WaterLevel > -9000, substr(Date, 1, 7) %in% c("2022-10", "2023-03")) %>%
  left_join(ff %>% distinct(site, floor_mu_cm), by = c(SITENAME = "site")) %>%
  mutate(rel = WaterLevel + floor_mu_cm, ym = substr(Date, 1, 7), t = as.POSIXct(paste(Date, Time), tz = "Etc/GMT+5")) %>%
  arrange(SITENAME, t)
tide <- wl %>% group_by(site = SITENAME, ym) %>% group_modify(function(d, ...) {
  y <- d$rel; n <- length(y)
  pk <- sapply(seq_len(n), function(i) { r <- y[max(1, i - 5):min(n, i + 5)]; c(y[i] == max(r), y[i] == min(r)) })
  data.frame(mhw = mean(y[pk[1, ]]), mlw = mean(y[pk[2, ]])) }) %>% ungroup() %>%
  left_join(ff %>% mutate(ym = c("Oct 2022" = "2022-10", "Mar 2023" = "2023-03")[campaign]) %>% select(site, ym, frac_flooded),
            by = c("site", "ym"))
hyd_tidal <- tide %>% group_by(site) %>% arrange(ym, .by_group = TRUE) %>%
  summarise(value = paste(sprintf("%s: %s / %s (%.0f%%)", camp_lab[ym], sg(mhw), sg(mlw), 100 * frac_flooded), collapse = "; "),
            .groups = "drop")
hyd_other <- fx %>% filter(!plot %in% c("SRS5", "SRS6"), is.finite(water_depth)) %>%
  group_by(site = plot, month_year) %>% summarise(m = mean(water_depth), mx = max(water_depth), .groups = "drop") %>%
  group_by(site) %>% summarise(value = paste(sprintf("%s: %.0f (%.0f)", camp_lab[month_year], m, mx), collapse = "; "), .groups = "drop")
hyd <- bind_rows(hyd_tidal, hyd_other) %>% mutate(row = "Water level (cm)")

# ---- porewater salinity
sal <- read.csv("data/environmental/site_characterization_salinity_ch4.csv") %>%
  filter(sample_type == "porewater", site %in% site_lv, is.finite(PSU_mean)) %>%
  mutate(ym = case_when(grepl("Oct 2022", season) ~ "2022-10", grepl("Mar 2023", season) ~ "2023-03", grepl("Oct 2025", season) ~ "2025-10")) %>%
  arrange(site, ym) %>%
  group_by(site) %>% summarise(value = paste(sprintf("%s: %.0f", camp_lab[ym], PSU_mean), collapse = "; "), .groups = "drop") %>%
  mutate(row = "Porewater salinity (PSU)")

out <- bind_rows(
  meta %>% transmute(site = site_id, row = "Name", value = site_name),
  data.frame(site = names(role), row = "Class", value = unname(role)),
  meta %>% transmute(site = site_id, row = "Latitude, longitude", value = sprintf("%.4f, %.4f", latitude, longitude)),
  meta %>% transmute(site = site_id, row = "Dominant species", value = dominant_species),
  samp, struct, hyd, sal) %>%
  filter(site %in% site_lv, value != "")
row_lv <- unique(out$row)
out <- out %>% mutate(site = factor(site, site_lv), row = factor(row, row_lv)) %>% arrange(row, site)
dir.create("output/analysis/si", showWarnings = FALSE, recursive = TRUE)
write.csv(out, "output/analysis/si/si_site_table.csv", row.names = FALSE)
print(out %>% pivot_wider(names_from = site, values_from = value) %>% as.data.frame())
