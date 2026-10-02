# =============================================================================
# QA: class net CO2 exchange (bottom-up NEE = respiration components - tower
# GPP), legacy vs rebuild, split into the contribution of each respiration
# component (class mean over site x campaign, umol m-2 s-1 and g CO2 m-2 yr-1),
# plus the CH4 term of the net forcing. Writes output/qa/net_forcing_attribution.csv.
# =============================================================================
suppressMessages(library(dplyr))
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
u2g <- 44e-6 * 3.156e7
o <- read.csv("output/qa/baseline/output__upscaling__plot_level_CO2_totals.csv")
n <- read.csv("output/upscaling/plot_level_CO2_totals.csv")
comps <- c("stem", "root", "soil", "water", "cwd", "leaf", "GPP_used")
d <- inner_join(o, n, by = c("site", "campaign", "disturbance_level"), suffix = c(".old", ".new"))
att <- bind_rows(lapply(comps, function(cc) d %>% group_by(disturbance_level) %>%
  summarise(component = cc, legacy = mean(.data[[paste0(cc, ".old")]]), rebuild = mean(.data[[paste0(cc, ".new")]]), .groups = "drop"))) %>%
  mutate(sign = ifelse(component == "GPP_used", -1, 1), delta_NEE_umol = sign * (rebuild - legacy), delta_NEE_g = delta_NEE_umol * u2g)
fo <- read.csv("output/qa/baseline/output__upscaling__net_forcing_by_class.csv"); fn <- read.csv("output/upscaling/net_forcing_by_class.csv")
ch4 <- inner_join(fo, fn, by = "disturbance_level", suffix = c(".old", ".new")) %>%
  transmute(disturbance_level, component = "CH4 (CO2-eq 100)", legacy = ch4_co2eq100.old, rebuild = ch4_co2eq100.new,
            delta_NEE_umol = NA_real_, delta_NEE_g = ch4_co2eq100.new - ch4_co2eq100.old)
tot <- inner_join(fo, fn, by = "disturbance_level", suffix = c(".old", ".new")) %>%
  transmute(disturbance_level, component = "net forcing (100 yr)", legacy = net100.old, rebuild = net100.new,
            delta_NEE_umol = NA_real_, delta_NEE_g = net100.new - net100.old)
out <- bind_rows(att %>% select(-sign), ch4, tot) %>% arrange(disturbance_level)
write.csv(out, "output/qa/net_forcing_attribution.csv", row.names = FALSE)
print(out %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>% as.data.frame(), row.names = FALSE)
