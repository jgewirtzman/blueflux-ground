# =============================================================================
# Intact-to-ghost switch from the airborne (CARAFE) class fluxes alone, beside the
# chamber-based value. Inputs: data/carafe_topdown/delaria_endmembers_campaign.csv
# (CH4, CO2 class means and SE by deployment; Delaria et al. 2024) and
# delaria_CO2_daily_converted.csv (midday CO2 converted to daily for intact forest;
# ghost midday ~ daily as GPP ~ 0). CH4 is taken as daily (no diel correction, as the
# chamber central case). Annual values: (i) wet = Oct 2022, dry = mean of Apr 2022,
# Feb 2023 and Apr 2023, annual = mean of wet and dry (as the chamber budgets);
# (ii) plain mean of the four deployments. Monte Carlo (10,000 draws, normal on each
# deployment's SE). GWP20 81.2, GWP100 27.9 (as 04_net_forcing.R).
# Writes output/upscaling/airborne_switch.csv.
# =============================================================================
suppressMessages(library(dplyr))
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/00_lib/seed.R")
u_ch4 <- 16.04e-9 * 3.156e7          # nmol m-2 s-1 -> g CH4 m-2 yr-1
u_co2 <- 44.01e-6 * 3.156e7          # umol m-2 s-1 -> g CO2 m-2 yr-1
deps <- c("Apr 2022", "Oct 2022", "Feb 2023", "Apr 2023")
ch4 <- read.csv("data/carafe_topdown/delaria_endmembers_campaign.csv") %>% filter(gas == "CH4", campaign %in% deps) %>%
  transmute(class = ifelse(grepl("ghost", class), "ghost", "intact"), campaign, v = flux, se)
co2 <- read.csv("data/carafe_topdown/delaria_CO2_daily_converted.csv") %>% filter(campaign %in% deps) %>%
  transmute(class = ifelse(grepl("ghost", class), "ghost", "intact"), campaign, v = daily, se = daily_se)
annual <- function(x, scheme) if (scheme == "wet/dry") 0.5 * x[2] + 0.5 * mean(x[c(1, 3, 4)]) else mean(x)
set.seed(seed_for("airborne_switch")); N <- 10000
draw <- function(d) sapply(deps, function(cp) { r <- d[d$campaign == cp, ]; rnorm(N, r$v, r$se) })
res <- bind_rows(lapply(c("wet/dry", "four deployments"), function(sc) {
  out <- list()
  for (cl in c("intact", "ghost")) {
    m4 <- draw(ch4[ch4$class == cl, ]); c4 <- draw(co2[co2$class == cl, ])
    out[[cl]] <- list(ch4 = apply(m4, 1, annual, scheme = sc) * u_ch4, co2 = apply(c4, 1, annual, scheme = sc) * u_co2)
  }
  bind_rows(lapply(c(20, 100), function(h) { g <- ifelse(h == 20, 81.2, 27.9)
    net <- function(cl) out[[cl]]$co2 + g * out[[cl]]$ch4
    sw <- net("ghost") - net("intact"); dch4 <- g * (out$ghost$ch4 - out$intact$ch4)
    q <- function(x) c(median(x), quantile(x, c(0.025, 0.975)))
    data.frame(scheme = sc, horizon = paste0("GWP", h),
               term = c("intact CH4 (g m-2 yr-1)", "ghost CH4 (g m-2 yr-1)", "intact CO2 (g m-2 yr-1)", "ghost CO2 (g m-2 yr-1)",
                        "intact net", "ghost net", "switch", "switch CH4 part"),
               rbind(q(out$intact$ch4), q(out$ghost$ch4), q(out$intact$co2), q(out$ghost$co2), q(net("intact")), q(net("ghost")), q(sw), q(dch4)),
               p_switch_pos = mean(sw > 0)) }))
}))
names(res)[4:6] <- c("median", "lo", "hi")
write.csv(res, "output/upscaling/airborne_switch.csv", row.names = FALSE)
print(res %>% mutate(across(c(median, lo, hi), ~ round(.x))))
