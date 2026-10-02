# =============================================================================
# QA / SI: one-at-a-time sensitivity of the intact and ghost budgets and of the
# net forcing to each analytical choice, around the central case. Collects the
# scenario outputs already produced (budget_scenarios.csv, flooding_scenarios.csv,
# net_forcing_by_class.csv, forcing_framings.csv, mc_net_forcing_by_class.csv)
# and adds the leaf-respiration (Rd25 x LAI) and tidal-phase ranges.
# Writes output/qa/sensitivity_summary.csv (SI Table S10).
# Run after code/qa/budget_scenarios.R and code/qa/flooding_scenarios.R.
# =============================================================================
suppressMessages(library(dplyr))
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
u_gC <- 12.011e-6 * 3.156e7; u_gCO2 <- 44.01e-6 * 3.156e7; GWP100 <- 27.9

nf  <- read.csv("output/upscaling/net_forcing_by_class.csv")
co2 <- read.csv("output/upscaling/plot_level_CO2_totals.csv")
ch4 <- read.csv("output/upscaling/summary_CH4_by_component.csv") %>% filter(scenario == "exponential")
hv  <- function(k) nf[nf$disturbance_level == k, ]
cen <- list(ch4 = hv("healthy")$ch4_g_yr, nee = mean(co2$NEE_bottomup[co2$disturbance_level == "healthy"]) * u_gC,
            net = hv("healthy")$net100, gnet = hv("ghost")$net100)
row <- function(choice, setting, ch4_g = cen$ch4, nee = cen$nee, net = cen$net, gnet = cen$gnet, basis = "")
  data.frame(choice, setting, intact_CH4_g = ch4_g, intact_NEE_gC = nee, intact_net100 = net, ghost_net100 = gnet,
             switch_net100 = gnet - net, basis)

out <- list(row("central", "central case", basis = "area-weighted campaign flooding; Q10 1.15; Krauss CWD 67; LAI 2.8; tidal phase 1.0; daytime CH4"))

b <- read.csv("output/qa/budget_scenarios.csv")
for (q in c("none", "literature", "stem_chambers")) { r <- b[b$q10_case == q & b$cwd_case == "krauss_mean", ]
  out[[length(out) + 1]] <- row("Q10 (day -> 24 h, chamber CO2)", sprintf("%s (%.2f)", q, r$q10_used),
                                nee = r$healthy_NEE_gC, net = r$healthy_net100, gnet = r$ghost_net100, basis = "tower within-month 1.15 central") }
for (v in c("negligible", "krauss_lo", "krauss_eyewall", "krauss_hi")) { r <- b[b$q10_case == "tower_within_month" & b$cwd_case == v, ]
  out[[length(out) + 1]] <- row("Downed CWD volume", sprintf("%s (%g m3/ha)", v, r$cwd_vol_m3ha),
                                nee = r$healthy_NEE_gC, net = r$healthy_net100, gnet = r$ghost_net100, basis = "Krauss et al. 2005, 67 (13-181)") }

f <- read.csv("output/qa/flooding_scenarios.csv") %>% filter(water_multiplier == 1, flooding != "area_campaign")
for (i in seq_len(nrow(f))) out[[length(out) + 1]] <- row("Flooding representation (intact)",
  sprintf("%s (SRS5 %s; SRS6 %s)", f$flooding[i], f$frac_SRS5[i], f$frac_SRS6[i]),
  ch4_g = f$ch4_g[i], nee = f$nee_gC[i], net = f$net100[i], basis = "FCE LTER water level x floor-height model")

wat <- mean(ch4$water[ch4$disturbance_level == "healthy"]) * 0.365
for (k in c(0.6, 2.0)) out[[length(out) + 1]] <- row("Tidal phase, intact water CH4", sprintf("x %.1f", k),
  ch4_g = cen$ch4 + (k - 1) * wat, net = cen$net + (k - 1) * wat * GWP100, basis = "Bouillon 2007; Reithmaier 2020; Lin 2024; Yong 2024")

out[[length(out) + 1]] <- row("CH4 day -> 24 h", "x 1.30 (all CH4)", ch4_g = hv("healthy")$ch4_g_yr_24h,
  net = hv("healthy")$net100_ch4_24h, gnet = hv("ghost")$net100_ch4_24h, basis = "Zhu et al. 2024 (mangrove EC)")

effLAI <- function(L) (1 - exp(-0.5 * L)) / 0.5
leaf <- mean(co2$leaf[co2$disturbance_level == "healthy"])
for (s in list(c("Rd25 1.28, LAI 2.3", 1.28, 2.3), c("Rd25 1.62, LAI 5.55", 1.62, 5.55))) {
  d <- leaf * (as.numeric(s[2]) * effLAI(as.numeric(s[3]))) / (1.55 * effLAI(2.8)) - leaf
  out[[length(out) + 1]] <- row("Leaf respiration", s[1], nee = cen$nee + d * u_gC, net = cen$net + d * u_gCO2,
                                basis = "Barr 2009; Sturchio 2022; Troxler 2015 / Reed 2025 LAI") }

fr <- read.csv("output/upscaling/forcing_framings.csv")
for (x in c("necb_alk_retained", "necb_all_export", "storage")) { r <- fr[fr$class == "Healthy" & fr$framing == x, ]
  out[[length(out) + 1]] <- row("Carbon-balance framing", x, ch4_g = r$CH4_g, nee = r$CO2_gCO2 * 12.011 / 44.01,
                                net = r$net100, basis = "lateral export and storage from the literature (Table S9); ghost lateral not available") }

mc <- read.csv("output/upscaling/mc_net_forcing_by_class.csv")
out[[length(out) + 1]] <- data.frame(choice = "Monte Carlo 95 % interval", setting = "all propagated terms",
  intact_CH4_g = NA, intact_NEE_gC = NA, intact_net100 = sprintf("%.0f to %.0f", mc$net100_lo[mc$class == "healthy"], mc$net100_hi[mc$class == "healthy"]),
  ghost_net100 = sprintf("%.0f to %.0f", mc$net100_lo[mc$class == "ghost"], mc$net100_hi[mc$class == "ghost"]), switch_net100 = NA,
  basis = "chamber fluxes, TLS areas, CWD area, leaf term, tower GPP, tidal phase")
res <- bind_rows(lapply(out, function(d) mutate(d, across(c(intact_net100, ghost_net100), as.character))))
write.csv(res, "output/qa/sensitivity_summary.csv", row.names = FALSE)
print(res %>% mutate(across(c(intact_CH4_g), ~ round(.x, 2)), intact_NEE_gC = round(intact_NEE_gC),
                     switch_net100 = round(switch_net100)) %>% select(-basis), row.names = FALSE)
