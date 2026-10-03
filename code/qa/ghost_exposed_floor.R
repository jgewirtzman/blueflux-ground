# =============================================================================
# QA / sensitivity: ghost-forest floor without standing water (00_lib/ghost_floor.R).
# Central: in March 2023 the share of our depth readings at stem, root and
# downed-wood positions without standing water (0.19 at CP40 and FLM30) emits
# as soil, using FLM30 soil chambers from March 2022 (same site and season, no
# standing water then); October 2022 was fully inundated. This script reruns
# the CH4, CO2 and net-forcing steps under the alternatives:
#   exposed-soil flux: FLM30 2022 (central) | MI ghost soils | pooled FLM30 + MI |
#                      BL60 dieback soils | none (floor fully inundated)
#   exposed share    : no standing water (central) | readings <= 2 cm
# and restores the defaults. Writes output/qa/ghost_exposed_floor.csv and the
# candidate soil rates to output/qa/ghost_exposed_soil_rates.csv.
# =============================================================================
suppressMessages(library(dplyr))
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
steps <- c("code/07_upscaling/02_upscale_methane.R", "code/07_upscaling/03_upscale_co2.R", "code/07_upscaling/04_net_forcing.R")
u_gCO2 <- 44.01e-6 * 3.156e7
grid <- expand.grid(soil = c("FLM30_2022", "MI", "pooled", "BL60", "none"), exposed = c("no_water", "le2cm"), stringsAsFactors = FALSE) %>%
  filter(!(soil == "none" & exposed == "le2cm"))
lab <- c(FLM30_2022 = "FLM30 soil, Mar 2022 (same site)", MI = "Marco Island ghost soil, Mar 2022-23",
         pooled = "Pooled ghost soil (FLM30 2022 + MI)", BL60 = "BL60 dieback soil, Mar 2023", none = "Floor fully inundated")
run <- function(soil, exposed) {
  env <- c(paste0("GHOST_SOIL=", soil), paste0("GHOST_EXPOSED=", exposed))
  for (s in steps) stopifnot(system2("Rscript", s, env = env, stdout = FALSE, stderr = FALSE) == 0)
  nf <- read.csv("output/upscaling/net_forcing_by_class.csv"); g <- nf[nf$disturbance_level == "ghost", ]; h <- nf[nf$disturbance_level == "healthy", ]
  ft <- read.csv("output/upscaling/flux_rates_with_gapfills.csv")
  sr <- ft[ft$site == "FLM30" & ft$campaign == "Mar 2023" & ft$component == "soil", ]
  data.frame(soil_source = lab[[soil]], exposed = exposed,
             soil_CH4_nmol = if (nrow(sr)) sr$flux_rate else NA,
             ghost_CH4_g = g$ch4_g_yr, ghost_CO2_g = g$co2_g_yr, ghost_net20 = g$net20, ghost_net100 = g$net100,
             ghost_netstar = g$net_gwpstar, switch20 = g$net20 - h$net20, switch100 = g$net100 - h$net100)
}
out <- bind_rows(lapply(seq_len(nrow(grid)), function(i) run(grid$soil[i], grid$exposed[i])))
for (s in steps) system2("Rscript", s, stdout = FALSE, stderr = FALSE)          # restore defaults
write.csv(out, "output/qa/ghost_exposed_floor.csv", row.names = FALSE)
print(out %>% mutate(across(where(is.numeric), ~ round(.x, 1))), row.names = FALSE)
