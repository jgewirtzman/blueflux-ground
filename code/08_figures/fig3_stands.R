# =============================================================================
# Fig. 3 | Stand budgets and their independent closure.
#   (a) CH4 and (b) CO2 stand budgets by forest class (intact = SRS5, SRS6;
#       ghost = CP40, FLM30; site means, tide-weighted) and campaign, stacked by
#       component (CO2: respiration above zero, tower GPP below). Bottom-up
#       total / net exchange: white diamond with Monte Carlo 95% interval.
#       Independent estimates beside each bar: airborne eddy covariance (CARAFE,
#       two-class disaggregation, Delaria et al. 2024; March 2023 = mean of the
#       February and April 2023 deployments; CO2 converted to daily; +/- 1 SE)
#       and the US-Skr tower (intact; campaign-month NEE, 95% CI over days).
#       Units: CH4 nmol m-2 s-1; CO2 umol m-2 s-1 (daily mean).
# Per-site budgets are in Extended Data. Writes output/figures/other/fig3_stands.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(patchwork)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
f <- 1e-3 / 16.04 * 1e9 / 86400          # mg CH4 m-2 d-1 -> nmol m-2 s-1
CAMP <- c("Oct 2022", "Mar 2023"); cls <- c("intact", "ghost")
comp_order <- c("water", "soil", "prop root", "stem", "downed wood", "leaf")
lab_camp <- function(x) sub(" 20", "\n'", x)

# ---- bottom-up CH4 ----
ch <- read.csv("output/upscaling/plot_level_CH4_totals.csv") %>% filter(scenario == "exponential", campaign %in% CAMP) %>%
  group_by(site, campaign, disturbance_level) %>%
  summarise(across(c(water_mg, soil_mg, root_mg, stem_mg, cwd_mg, total_mg), ~ weighted.mean(.x, tide_weight)), .groups = "drop") %>%
  group_by(campaign, disturbance_level) %>% summarise(across(ends_with("_mg"), mean), .groups = "drop") %>%
  mutate(class = factor(recode(disturbance_level, healthy = "intact"), cls), campaign = factor(campaign, CAMP))
mc <- read.csv("output/upscaling/mc_component_uncertainty.csv") %>% filter(component == "total", campaign %in% CAMP) %>%
  group_by(site, campaign, disturbance_level) %>%
  summarise(lo = weighted.mean(mc_ci_lo, tide_weight), hi = weighted.mean(mc_ci_hi, tide_weight), .groups = "drop") %>%
  group_by(campaign, disturbance_level) %>% summarise(lo = mean(lo) * f, hi = mean(hi) * f, .groups = "drop") %>%
  mutate(class = factor(recode(disturbance_level, healthy = "intact"), cls), campaign = factor(campaign, CAMP))
ch_long <- ch %>% pivot_longer(c(water_mg, soil_mg, root_mg, stem_mg, cwd_mg), names_to = "component", values_to = "mg") %>%
  mutate(v = mg * f, comp = factor(recode(sub("_mg", "", component), root = "prop root", cwd = "downed wood"), comp_order))
ch_tot <- ch %>% transmute(class, campaign, total = total_mg * f) %>% left_join(mc %>% select(class, campaign, lo, hi), by = c("class", "campaign"))

# ---- CARAFE and tower ----
cara <- function(file, gas, val, se) {
  x <- read.csv(file)
  if (!is.null(gas)) x <- x[x$gas == gas, ]
  x <- x %>% mutate(class = factor(recode(class, mangrove_forest = "intact", ghost_forest = "ghost"), cls))
  bind_rows(x %>% filter(campaign == "Oct 2022") %>% transmute(class, campaign, v = .data[[val]], se = .data[[se]]),
            x %>% filter(campaign %in% c("Feb 2023", "Apr 2023")) %>% group_by(class) %>%
              summarise(v = mean(.data[[val]]), se = sqrt(mean(.data[[se]]^2)), .groups = "drop") %>% mutate(campaign = "Mar 2023")) %>%
    mutate(campaign = factor(campaign, CAMP), source = "aircraft (CARAFE)", lo = v - se, hi = v + se)
}
ca_ch4 <- cara("data/carafe_topdown/delaria_endmembers_campaign.csv", "CH4", "flux", "se")
ca_co2 <- cara("data/carafe_topdown/delaria_CO2_daily_converted.csv", NULL, "daily", "daily_se")
tower <- read.csv("output/gpp/US-Skr_campaign_fluxes.csv") %>% filter(gas == "NEE") %>%
  mutate(campaign = case_when(year == 2022 & month == 10 ~ "Oct 2022", year == 2023 & month == 3 ~ "Mar 2023")) %>%
  filter(!is.na(campaign)) %>% transmute(class = factor("intact", cls), campaign = factor(campaign, CAMP), v = mean, lo, hi, source = "tower (US-Skr)")

# ---- bottom-up CO2 ----
comp <- read.csv("output/upscaling/summary_CO2_by_component.csv") %>% filter(campaign %in% CAMP) %>%
  group_by(campaign, disturbance_level) %>% summarise(across(c(water, soil, root, stem, cwd, leaf), mean), .groups = "drop")
gpp <- read.csv("output/upscaling/plot_level_CO2_totals.csv") %>% filter(campaign %in% CAMP) %>%
  group_by(campaign, disturbance_level) %>% summarise(GPP = mean(GPP_used), NEE = mean(NEE_bottomup), .groups = "drop")
co <- comp %>% left_join(gpp, by = c("campaign", "disturbance_level")) %>%
  mutate(class = factor(recode(disturbance_level, healthy = "intact"), cls), campaign = factor(campaign, CAMP))
co_long <- co %>% pivot_longer(c(water, soil, root, stem, cwd, leaf), names_to = "component", values_to = "v") %>%
  mutate(comp = factor(recode(component, root = "prop root", cwd = "downed wood"), comp_order))
mcc <- read.csv("output/upscaling/mc_CO2_forcing.csv") %>%
  transmute(class = factor(recode(class, healthy = "intact"), cls), campaign = factor(campaign, CAMP), lo = nee_lo, hi = nee_hi)
co_net <- co %>% transmute(class, campaign, total = NEE) %>% left_join(mcc, by = c("class", "campaign"))

# ---- plotting ----
nud <- c(`aircraft (CARAFE)` = 0.34, `tower (US-Skr)` = 0.46)
src_shape <- c(`aircraft (CARAFE)` = 24, `tower (US-Skr)` = 22)
panel <- function(long, net, refs, ylab, gpp_df = NULL) {
  refs <- refs %>% mutate(xx = as.numeric(campaign) + nud[source])
  p <- ggplot() + geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3)
  if (!is.null(gpp_df)) p <- p + geom_col(data = gpp_df, aes(campaign, -GPP), width = 0.5, fill = "#A9C6B3")
  p + geom_col(data = long, aes(campaign, v, fill = comp), width = 0.5, colour = "white", linewidth = 0.15,
               position = position_stack(reverse = TRUE)) +
    geom_errorbar(data = net, aes(campaign, ymin = lo, ymax = hi), width = 0.12, linewidth = 0.4) +
    geom_point(data = net, aes(campaign, total), shape = 23, fill = "white", colour = col_ink, size = 2.2) +
    geom_errorbar(data = refs, aes(x = xx, ymin = lo, ymax = hi), width = 0.06, linewidth = 0.4, colour = "grey30") +
    geom_point(data = refs, aes(x = xx, y = v, shape = source), fill = "grey30", colour = "white", size = 2) +
    facet_grid(~ class) +
    scale_x_discrete(labels = lab_camp) +
    scale_fill_manual(values = pal_comp, limits = comp_order, name = "component") +
    scale_shape_manual(values = src_shape, limits = names(src_shape), name = "independent estimate") +
    labs(x = NULL, y = ylab) +
    theme_fig() + theme(panel.grid.major.x = element_blank(), strip.text = element_text(hjust = 0.5))
}
pa <- panel(ch_long, ch_tot, ca_ch4, expression("Stand CH"[4]*" (nmol m"^-2*" s"^-1*")")) + guides(fill = "none", shape = "none")
pb <- panel(co_long, co_net, bind_rows(ca_co2, tower), expression("Stand CO"[2]*" ("*mu*"mol m"^-2*" s"^-1*", daily)"), gpp_df = co) +
  geom_text(data = data.frame(class = factor("intact", cls)), aes(x = 1.5, y = -0.5, label = "GPP (tower)"),
            size = 2.1, colour = "grey30", vjust = 1, inherit.aes = FALSE)
fig <- (pa + labs(tag = "a")) + (pb + labs(tag = "b")) + plot_layout(guides = "collect") & theme(legend.position = "right")
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/fig3_stands.png", fig, width = 7.2, height = 3.4, dpi = 300, bg = "white")
ggsave("output/figures/other/fig3_stands.pdf", fig, width = 7.2, height = 3.4, device = cairo_pdf)
