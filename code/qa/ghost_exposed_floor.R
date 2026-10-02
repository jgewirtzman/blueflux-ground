# =============================================================================
# QA / sensitivity (for decision): ghost-forest floor without standing water.
# The central budget treats the ghost floor (CP40, FLM30) as fully inundated in
# both analysed campaigns. In March 2023 our depth readings at stem, root and
# downed-wood positions show that part of the floor had no standing water
# (6 of 31 positions at CP40, 9 of 48 at FLM30), and no ghost soil chambers
# were run in the analysed campaigns. This script re-budgets the ghost class
# with that part of the floor emitting as soil, under alternative, data-based
# estimates of the exposed-soil flux:
#   same site, other year : FLM30 soil chambers, March 2022 (site not inundated)
#   ghost analogue        : Marco Island (MI) ghost soils, March 2022 + 2023
#   pooled ghost soils    : FLM30 2022 + MI (the earlier S.T2 analogue)
#   dieback soil          : BL60 (regenerating, hurricane-affected) soils, March 2023
# and two exposed shares: positions with no standing water (central) and, as an
# upper bound, positions with <= 2 cm treated as exposed.
# October 2022 stays fully inundated (all positions under 2-31 cm of water).
# CO2 soil rates are scaled to 24 h with the campaign soil temperature factor.
# Writes output/qa/ghost_exposed_floor.csv. Does not change the central budget.
# =============================================================================
suppressMessages(library(dplyr))
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
GWP20 <- 81.2; GWP100 <- 27.9
u_gCO2 <- 44.01e-6 * 3.156e7; nmol_to_mgd <- 16.04e-9 * 1e3 * 86400
GHOST <- c("CP40", "FLM30")

d <- read.csv("output/data_products/combined_gas_flux_dataset.csv")
boot <- function(x, n = 2000) { x <- x[is.finite(x)]; b <- replicate(n, mean(sample(x, replace = TRUE)))
  c(mean = mean(x), lo = unname(quantile(b, 0.025)), hi = unname(quantile(b, 0.975)), n = length(x)) }
soil_src <- list(
  "FLM30 soil, Mar 2022 (same site)"      = d %>% filter(plot == "FLM30", month_year == "2022-03", component == "soil"),
  "MI ghost soil, Mar 2022-23 (analogue)" = d %>% filter(plot == "MI", component == "soil"),
  "Pooled ghost soil (FLM30 2022 + MI)"   = d %>% filter(plot %in% c("FLM30", "MI"), month_year %in% c("2022-03", "2023-03"), component == "soil"),
  "BL60 dieback soil, Mar 2023"           = d %>% filter(plot == "BL60", month_year == "2023-03", component == "soil"))
rates <- bind_rows(lapply(names(soil_src), function(k) { x <- soil_src[[k]]
  data.frame(source = k, t(boot(x$CH4_best.flux)) %>% as.data.frame() %>% setNames(c("CH4", "CH4_lo", "CH4_hi", "n")),
             t(boot(x$CO2_best.flux)) %>% as.data.frame() %>% setNames(c("CO2", "CO2_lo", "CO2_hi", "nCO2"))) }))

# exposed share in March 2023 from the depth readings at tree positions
pos <- d %>% filter(plot %in% GHOST, month_year == "2023-03", component %in% c("stem", "root", "cwd"), !is.na(water_depth))
expo <- pos %>% group_by(site = plot) %>% summarise(no_water = mean(water_depth == 0), le2cm = mean(water_depth <= 2), n = n(), .groups = "drop")

# central ghost budget pieces (per m2 of plot)
ch4 <- read.csv("output/upscaling/summary_CH4_by_component.csv") %>% filter(scenario == "exponential", site %in% GHOST)
co2 <- read.csv("output/upscaling/plot_level_CO2_totals.csv") %>% filter(site %in% GHOST)
ft  <- read.csv("output/upscaling/flux_rates_with_gapfills.csv") %>% filter(site %in% GHOST, component == "water") %>% select(site, campaign, water_rate = flux_rate)
tf  <- read.csv("output/upscaling/co2_temperature_correction.csv") %>% filter(component == "soil") %>% select(campaign, soil_factor = factor)
base <- ch4 %>% select(site, campaign, stem, root, cwd, water, total) %>%
  left_join(co2 %>% select(site, campaign, co2_stem = stem, co2_root = root, co2_cwd = cwd, co2_water = water, NEE = NEE_bottomup), by = c("site", "campaign")) %>%
  left_join(ft, by = c("site", "campaign")) %>% left_join(tf, by = "campaign") %>%
  mutate(ground_frac = water / (water_rate * nmol_to_mgd), soil_factor = coalesce(soil_factor, 1))

nf <- read.csv("output/upscaling/net_forcing_by_class.csv")
intact20 <- nf$net20[nf$disturbance_level == "healthy"]; intact100 <- nf$net100[nf$disturbance_level == "healthy"]

scen <- function(label, src, share_col) {
  r <- if (is.null(src)) NULL else rates[rates$source == src, ]
  b <- base %>% left_join(expo %>% select(site, e = all_of(share_col)), by = "site") %>%
    mutate(e = ifelse(campaign == "Mar 2023" & !is.null(r), e, 0),
           soil_ch4 = if (is.null(r)) 0 else r$CH4 * ground_frac * e * nmol_to_mgd,
           ch4_tot = stem + root + cwd + water * (1 - e) + soil_ch4,
           soil_co2 = if (is.null(r)) 0 else r$CO2 * soil_factor * ground_frac * e,
           nee = NEE - co2_water * e + soil_co2)
  ch4_g <- mean(b$ch4_tot) * 0.365; co2_g <- mean(b$nee) * u_gCO2
  data.frame(scenario = label, exposed_mar23 = paste(round(unique(b$e[b$campaign == "Mar 2023"]), 2), collapse = "/"),
             soil_CH4_nmol = if (is.null(r)) NA else r$CH4, soil_CO2_umol = if (is.null(r)) NA else r$CO2,
             ghost_CH4_g = ch4_g, ghost_CO2_g = co2_g,
             ghost_net20 = co2_g + ch4_g * GWP20, ghost_net100 = co2_g + ch4_g * GWP100,
             switch20 = co2_g + ch4_g * GWP20 - intact20, switch100 = co2_g + ch4_g * GWP100 - intact100)
}
out <- bind_rows(
  scen("Central: fully inundated", NULL, "no_water"),
  bind_rows(lapply(rates$source, function(k) scen(k, k, "no_water"))),
  bind_rows(lapply(rates$source, function(k) scen(paste0(k, "; <=2 cm exposed"), k, "le2cm"))))
write.csv(out, "output/qa/ghost_exposed_floor.csv", row.names = FALSE)
write.csv(rates, "output/qa/ghost_exposed_soil_rates.csv", row.names = FALSE)
cat("Exposed share (March 2023):\n"); print(as.data.frame(expo))
cat("\nCandidate exposed-soil fluxes (CH4 nmol, CO2 umol m-2 s-1; mean [95% CI]):\n")
print(rates %>% transmute(source, n, CH4 = sprintf("%.1f [%.1f, %.1f]", CH4, CH4_lo, CH4_hi), CO2 = sprintf("%.2f [%.2f, %.2f]", CO2, CO2_lo, CO2_hi)), row.names = FALSE)
cat("\nGhost budget and switch:\n")
print(out %>% mutate(across(where(is.numeric), ~ round(.x, 1))), row.names = FALSE)
