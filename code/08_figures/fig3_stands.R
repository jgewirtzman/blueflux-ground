# =============================================================================
# Fig. 3 | Stand budgets and their independent closure. Panel order in the figure:
#   a = airborne by deployment, b = annual budgets, c, d = component shares,
#   e, f = closure (stacked bottom-up components + totals + independent estimates;
#   CH4 on a linear axis cropped at 70, the ghost October airborne interval runs off).
#   (a) Share of stand CH4 by component, and (b) share of stand respiration
#       (CO2) by component, for intact (SRS5, SRS6) and ghost (CP40, FLM30)
#       forest by campaign (site means, tide-weighted).
#   (c) CH4 and (d) net CO2 exchange: bottom-up totals (white diamonds, Monte
#       Carlo 95% intervals) beside independent estimates at the same 95% level:
#       airborne eddy covariance (CARAFE two-class disaggregation, Delaria et al.
#       2024; March 2023 = mean of February and April 2023; +/- 1.96 SE; CO2
#       converted to daily) and the US-Skr tower (intact; campaign-month NEE,
#       95% CI over days). CH4 on the inverse-hyperbolic-sine axis of Fig. 2.
#   (e) Airborne CH4 and CO2 for intact and ghost forest by deployment
#       (PLACEHOLDER until the two-class values for all five deployments,
#       including July 2024, arrive).
#   (f) Annual stand budgets: CH4 (g CH4 m-2 yr-1) and net CO2 exchange
#       (g C m-2 yr-1), bottom-up with Monte Carlo 95% intervals; intact NEE
#       beside the tower-based estimate (Barr et al. 2010, NEP 1,170 +/- 127).
# Writes output/figures/other/fig3_stands.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(patchwork)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
f <- 1e-3 / 16.04 * 1e9 / 86400          # mg CH4 m-2 d-1 -> nmol m-2 s-1
CAMP <- c("Oct 2022", "Mar 2023"); cls <- c("intact", "ghost")
comp_order <- c("water", "soil", "prop root", "stem", "downed wood", "leaf")
lab_camp <- function(x) sub(" 20", "\n'", x)
z95 <- 1.96
to_class <- function(x) factor(recode(x, healthy = "intact", mangrove_forest = "intact", ghost_forest = "ghost"), cls)

# ---- bottom-up CH4 by class x campaign ----
ch <- read.csv("output/upscaling/plot_level_CH4_totals.csv") %>% filter(scenario == "exponential", campaign %in% CAMP) %>%
  group_by(site, campaign, disturbance_level) %>%
  summarise(across(c(water_mg, soil_mg, root_mg, stem_mg, cwd_mg, total_mg), ~ weighted.mean(.x, tide_weight)), .groups = "drop") %>%
  group_by(campaign, disturbance_level) %>% summarise(across(ends_with("_mg"), mean), .groups = "drop") %>%
  mutate(class = to_class(disturbance_level), campaign = factor(campaign, CAMP))
mc <- read.csv("output/upscaling/mc_component_uncertainty.csv") %>% filter(component == "total", campaign %in% CAMP) %>%
  group_by(site, campaign, disturbance_level) %>%
  summarise(lo = weighted.mean(mc_ci_lo, tide_weight), hi = weighted.mean(mc_ci_hi, tide_weight), .groups = "drop") %>%
  group_by(campaign, disturbance_level) %>% summarise(lo = mean(lo) * f, hi = mean(hi) * f, .groups = "drop") %>%
  mutate(class = to_class(disturbance_level), campaign = factor(campaign, CAMP))
ch_share <- ch %>% pivot_longer(c(water_mg, soil_mg, root_mg, stem_mg, cwd_mg), names_to = "component", values_to = "mg") %>%
  group_by(class, campaign) %>% mutate(pct = 100 * mg / sum(mg)) %>% ungroup() %>%
  mutate(comp = factor(recode(sub("_mg", "", component), root = "prop root", cwd = "downed wood"), comp_order))
ch_tot <- ch %>% transmute(class, campaign, v = total_mg * f, source = "bottom-up (chambers x area)") %>%
  left_join(mc %>% select(class, campaign, lo, hi), by = c("class", "campaign"))

# ---- bottom-up CO2 ----
comp <- read.csv("output/upscaling/summary_CO2_by_component.csv") %>% filter(campaign %in% CAMP) %>%
  group_by(campaign, disturbance_level) %>% summarise(across(c(water, soil, root, stem, cwd, leaf), mean), .groups = "drop")
co_share <- comp %>% pivot_longer(c(water, soil, root, stem, cwd, leaf), names_to = "component", values_to = "v") %>%
  group_by(campaign, disturbance_level) %>% mutate(pct = 100 * v / sum(v)) %>% ungroup() %>%
  mutate(class = to_class(disturbance_level), campaign = factor(campaign, CAMP),
         comp = factor(recode(component, root = "prop root", cwd = "downed wood"), comp_order))
nee <- read.csv("output/upscaling/plot_level_CO2_totals.csv") %>% filter(campaign %in% CAMP) %>%
  group_by(campaign, disturbance_level) %>% summarise(v = mean(NEE_bottomup), .groups = "drop") %>%
  mutate(class = to_class(disturbance_level), campaign = factor(campaign, CAMP))
mcc <- read.csv("output/upscaling/mc_CO2_forcing.csv") %>%
  transmute(class = to_class(class), campaign = factor(campaign, CAMP), lo = nee_lo, hi = nee_hi)
co_tot <- nee %>% select(class, campaign, v) %>% left_join(mcc, by = c("class", "campaign")) %>%
  mutate(source = "bottom-up (chambers x area)")

# ---- independent estimates (95%) ----
cara <- function(file, gas, val, se) {
  x <- read.csv(file); if (!is.null(gas)) x <- x[x$gas == gas, ]
  x <- x %>% mutate(class = to_class(class))
  bind_rows(x %>% filter(campaign == "Oct 2022") %>% transmute(class, campaign, v = .data[[val]], se = .data[[se]]),
            x %>% filter(campaign %in% c("Feb 2023", "Apr 2023")) %>% group_by(class) %>%
              summarise(v = mean(.data[[val]]), se = sqrt(mean(.data[[se]]^2)), .groups = "drop") %>% mutate(campaign = "Mar 2023")) %>%
    mutate(campaign = factor(campaign, CAMP), source = "aircraft (CARAFE)", lo = v - z95 * se, hi = v + z95 * se)
}
ca_ch4 <- cara("data/carafe_topdown/delaria_endmembers_campaign.csv", "CH4", "flux", "se")
ca_co2 <- cara("data/carafe_topdown/delaria_CO2_daily_converted.csv", NULL, "daily", "daily_se")
tower <- read.csv("output/gpp/US-Skr_campaign_fluxes.csv") %>% filter(gas == "NEE") %>%
  mutate(campaign = case_when(year == 2022 & month == 10 ~ "Oct 2022", year == 2023 & month == 3 ~ "Mar 2023")) %>%
  filter(!is.na(campaign)) %>% transmute(class = factor("intact", cls), campaign = factor(campaign, CAMP), v = mean, lo, hi, source = "tower (US-Skr)")

src_lev <- c("bottom-up (chambers x area)", "aircraft (CARAFE)", "tower (US-Skr)")
src_shape <- c(23, 24, 22); names(src_shape) <- src_lev
src_fill <- c("white", "grey30", "grey30"); names(src_fill) <- src_lev
dodge <- c(-0.2, 0, 0.2); names(dodge) <- src_lev

share_panel <- function(df, ylab) {
  ggplot(df, aes(campaign, pct, fill = comp)) +
    geom_col(width = 0.65, colour = "white", linewidth = 0.15, position = position_stack(reverse = TRUE)) +
    facet_grid(~ class) + scale_x_discrete(labels = lab_camp) +
    scale_y_continuous(expand = c(0, 0), breaks = c(0, 50, 100)) +
    scale_fill_manual(values = pal_comp, limits = comp_order, name = "component") +
    labs(x = NULL, y = ylab) + theme_fig() + theme(panel.grid.major.x = element_blank(), strip.text = element_text(hjust = 0.5))
}
closure_panel <- function(stack, df, ylab, ylim = NULL, gpp_df = NULL) {
  df <- df %>% mutate(source = factor(source, src_lev), x = as.numeric(campaign) + c(0, 0.36, 0.5)[as.integer(source)])
  p <- ggplot() + geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3)
  if (!is.null(gpp_df)) p <- p + geom_col(data = gpp_df, aes(as.numeric(campaign), -GPP), width = 0.5, fill = "#A9C6B3") +
    geom_text(data = data.frame(class = factor("intact", cls)), aes(x = 1.5, y = -7.6, label = "GPP (tower)"),
              size = 2, colour = "grey30", vjust = 1)
  p <- p + geom_col(data = stack, aes(as.numeric(campaign), v, fill = comp), width = 0.5, colour = "white", linewidth = 0.15,
                    position = position_stack(reverse = TRUE)) +
    geom_errorbar(data = df, aes(x = x, ymin = lo, ymax = hi, colour = source), width = 0.07, linewidth = 0.45) +
    geom_point(data = df, aes(x = x, y = v, shape = source), fill = ifelse(df$source == src_lev[1], "white", "grey30"),
               colour = col_ink, size = 2.1, stroke = 0.4) +
    facet_grid(~ class) +
    scale_x_continuous(breaks = 1:2, labels = lab_camp(CAMP), limits = c(0.6, 2.65)) +
    scale_fill_manual(values = pal_comp, limits = comp_order, name = "component") +
    scale_shape_manual(values = src_shape, limits = src_lev, name = "estimate") +
    scale_colour_manual(values = c(col_ink, "grey30", "grey30"), limits = src_lev, guide = "none") +
    labs(x = NULL, y = ylab) + theme_fig() + theme(panel.grid.major.x = element_blank(), strip.text = element_text(hjust = 0.5))
  if (!is.null(ylim)) p <- p + coord_cartesian(ylim = ylim)
  p
}
pa <- share_panel(ch_share, expression("Share of stand CH"[4]*" (%)"))
pb <- share_panel(co_share, expression("Share of stand respiration (%)"))
ch_stack <- ch %>% pivot_longer(c(water_mg, soil_mg, root_mg, stem_mg, cwd_mg), names_to = "component", values_to = "mg") %>%
  mutate(v = mg * f, comp = factor(recode(sub("_mg", "", component), root = "prop root", cwd = "downed wood"), comp_order))
co_stack <- co_share %>% select(class, campaign, comp, v)
gpp_df <- read.csv("output/upscaling/plot_level_CO2_totals.csv") %>% filter(campaign %in% CAMP) %>%
  group_by(campaign, disturbance_level) %>% summarise(GPP = mean(GPP_used), .groups = "drop") %>%
  mutate(class = to_class(disturbance_level), campaign = factor(campaign, CAMP))
pc <- closure_panel(ch_stack, bind_rows(ch_tot, ca_ch4), expression("Stand CH"[4]*" (nmol m"^-2*" s"^-1*")"), ylim = c(-18, 70))
pd <- closure_panel(co_stack, bind_rows(co_tot, ca_co2, tower), expression("Stand CO"[2]*" ("*mu*"mol m"^-2*" s"^-1*", daily)"),
                    gpp_df = gpp_df)

# ---- (e) aircraft by deployment (placeholder) ----
deps <- c("Apr 2022", "Oct 2022", "Feb 2023", "Apr 2023", "Jul 2024")
air <- read.csv("data/carafe_topdown/delaria_endmembers_campaign.csv") %>%
  mutate(class = to_class(class), campaign = factor(campaign, deps), gas = ifelse(gas == "CH4", "CH4\n(nmol m-2 s-1)", "CO2, midday\n(umol m-2 s-1)"))
pend <- expand.grid(campaign = factor("Jul 2024", deps), gas = unique(air$gas))
pe <- ggplot(air, aes(campaign, flux, colour = class)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_errorbar(aes(ymin = flux - z95 * se, ymax = flux + z95 * se), position = position_dodge(0.5), width = 0.15, linewidth = 0.4) +
  geom_point(position = position_dodge(0.5), size = 1.8) +
  geom_text(data = pend, aes(campaign, -Inf, label = "pending"), inherit.aes = FALSE, angle = 90, hjust = -0.2, size = 2.2, colour = "grey50") +
  facet_wrap(~ gas, scales = "free_y") + scale_x_discrete(drop = FALSE, labels = function(x) sub(" 20", "\n'", x)) +
  scale_colour_manual(values = pal_class[cls], name = "forest class") +
  labs(x = "Airborne deployment", y = "Airborne flux (95% CI)",
       subtitle = "PLACEHOLDER: two-class values for all five deployments pending") +
  theme_fig() + theme(strip.text = element_text(hjust = 0.5), plot.subtitle = element_text(size = 6.5, colour = "firebrick"))

# ---- (f) annual budgets ----
nf <- read.csv("output/upscaling/net_forcing_by_class.csv")
mca <- read.csv("output/upscaling/mc_component_uncertainty.csv") %>% filter(component == "total") %>%
  group_by(site, campaign, disturbance_level) %>% summarise(lo = weighted.mean(mc_ci_lo, tide_weight), hi = weighted.mean(mc_ci_hi, tide_weight), .groups = "drop") %>%
  group_by(disturbance_level) %>% summarise(lo = mean(lo) * 365 / 1000, hi = mean(hi) * 365 / 1000, .groups = "drop")
k_c <- 12e-6 * 3.156e7                                     # umol CO2 m-2 s-1 -> g C m-2 yr-1
mcy <- read.csv("output/upscaling/mc_CO2_forcing.csv") %>% group_by(class) %>% summarise(lo = mean(nee_lo) * k_c, hi = mean(nee_hi) * k_c, .groups = "drop")
ann <- bind_rows(
  nf %>% transmute(class = to_class(disturbance_level), var = "CH4\n(g CH4 m-2 yr-1)", v = ch4_g_yr) %>%
    left_join(mca %>% transmute(class = to_class(disturbance_level), lo, hi), by = "class"),
  nf %>% transmute(class = to_class(disturbance_level), var = "net CO2 exchange\n(g C m-2 yr-1)", v = co2_g_yr * 12 / 44) %>%
    left_join(mcy %>% transmute(class = to_class(class), lo, hi), by = "class")) %>% mutate(source = "bottom-up (chambers x area)")
twr <- data.frame(class = factor("intact", cls), var = "net CO2 exchange\n(g C m-2 yr-1)", v = -1170, lo = -1297, hi = -1043, source = "tower (US-Skr)")
pf <- ggplot(bind_rows(ann, twr) %>% mutate(source = factor(source, src_lev)), aes(class, v)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_errorbar(aes(ymin = lo, ymax = hi, group = source), position = position_dodge(0.45), width = 0.12, linewidth = 0.45) +
  geom_point(aes(shape = source, fill = source), position = position_dodge(0.45), size = 2.1, colour = col_ink, stroke = 0.4) +
  facet_wrap(~ var, scales = "free_y") +
  scale_shape_manual(values = src_shape, limits = src_lev, guide = "none") +
  scale_fill_manual(values = src_fill, limits = src_lev, guide = "none") +
  labs(x = NULL, y = "Annual stand budget") + theme_fig() + theme(strip.text = element_text(hjust = 0.5), panel.grid.major.x = element_blank())

pa <- pa + guides(fill = "none"); pb <- pb + guides(fill = "none"); pc <- pc + guides(shape = "none", fill = "none")
pd <- pd + guides(shape = guide_legend(override.aes = list(fill = c("white", "grey30", "grey30"))))
fig <- (pe + labs(tag = "a")) + (pf + labs(tag = "b")) + (pa + labs(tag = "c")) + (pb + labs(tag = "d")) +
  (pc + labs(tag = "e")) + (pd + labs(tag = "f")) +
  plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "right")
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/fig3_stands.png", fig, width = 7.2, height = 8, dpi = 300, bg = "white")
ggsave("output/figures/other/fig3_stands.pdf", fig, width = 7.2, height = 8, device = cairo_pdf)
