# =============================================================================
# Carbon and methane budget figure (new style; candidate main-text panel).
#   (a) Carbon flows for intact and ghost forest on one scale (g C m-2 yr-1):
#       GPP, ecosystem respiration by component, CH4 emission, lateral export
#       (DIC, DOC, POC, dissolved CH4; literature, intact only), storage
#       (burial, wood increment; literature, intact only) and the closure
#       residual. Arrow width proportional to flux.
#   (b) Methane budget by pathway (g CH4 m-2 yr-1): stand emission by
#       component (bottom-up, Monte Carlo 95% interval), lateral dissolved CH4
#       export (intact), and the airborne mean of four deployments
#       (2022-2023, daytime) for comparison.
# Inputs: output/upscaling/summary_CO2_by_component.csv, plot_level_CO2_totals.csv,
#   summary_CH4_by_component.csv, carbon_budget_full.csv, carbon_budget_summary.csv,
#   mc_component_uncertainty.csv, data/carafe_topdown/delaria_endmembers_campaign.csv.
# Writes output/figures/other/fig_carbon_budget.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(patchwork)})
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
col_ch4 <- "#A23B72"; col_co2 <- "#9A9DA1"; col_lat <- "#2C7BB6"; col_stor <- "#6B4226"
umol_to_gC <- 12.011e-6 * 3.156e7                 # umol C m-2 s-1 -> g C m-2 yr-1
mgch4d_to_gC <- 365 / 1000 * 12.011 / 16.043      # mg CH4 m-2 d-1 -> g C m-2 yr-1
mgch4d_to_gCH4 <- 365 / 1000

cls <- c(healthy = "intact", ghost = "ghost")
co2 <- read.csv("output/upscaling/summary_CO2_by_component.csv") %>% filter(disturbance_level %in% names(cls)) %>%
  mutate(class = cls[disturbance_level]) %>% group_by(class) %>%
  summarise(across(c(stem, root, soil, water, cwd, leaf), mean), .groups = "drop") %>%
  pivot_longer(-class, names_to = "comp", values_to = "v") %>% mutate(gC = v * umol_to_gC)
gpp <- read.csv("output/upscaling/plot_level_CO2_totals.csv") %>% filter(disturbance_level %in% names(cls)) %>%
  mutate(class = cls[disturbance_level]) %>% group_by(class) %>% summarise(gpp = mean(GPP_used) * umol_to_gC, .groups = "drop")
ch4 <- read.csv("output/upscaling/summary_CH4_by_component.csv") %>% filter(scenario == "exponential", disturbance_level %in% names(cls)) %>%
  mutate(class = cls[disturbance_level]) %>% group_by(class) %>%
  summarise(across(c(stem, root, soil, water, cwd), mean), .groups = "drop") %>%
  pivot_longer(-class, names_to = "comp", values_to = "mg") %>% mutate(gC = mg * mgch4d_to_gC, gCH4 = mg * mgch4d_to_gCH4)
cb <- read.csv("output/upscaling/carbon_budget_full.csv") %>% filter(class == "Healthy")
lit <- setNames(cb$value, cb$term)
sumr <- read.csv("output/upscaling/carbon_budget_summary.csv") %>% filter(class == "Healthy")

comp_lab <- c(leaf = "leaf", stem = "stem + branch", root = "prop root", soil = "soil", water = "water", cwd = "downed wood")
pal_flow <- c(pal_comp_data, stem = pal_comp[["stem"]])

# ---------------------------------------------------------------- (a) carbon budget schematic
# Cross-sections of intact and ghost forest with annual flows as arrows; drawing
# helpers live in fig_carbon_stockflow.R.
co2w <- co2 %>% select(class, comp, gC) %>% pivot_wider(names_from = comp, values_from = gC)
ch4w <- ch4 %>% select(class, comp, gC) %>% pivot_wider(names_from = comp, values_from = gC)
gppw <- gpp
src <- read.csv("output/upscaling/budget_sources_totals.csv")
sv <- function(t) src$value[src$term == t & src$class == "Healthy"][1]
LITTER <- sv("Litterfall"); MORT <- sv("CWD prod."); ROOTP <- sv("Root NPP")
# uncertainties: measured terms +/- 1 SE across the four plot x campaign estimates;
# literature terms as published ranges. The closure residual (NEE - lateral -
# burial - wood increment) gets a range from the asymmetric half-widths of its
# terms combined in quadrature.
svr <- function(t, s) unlist(src[src$term == t & src$source == s, c("lo", "hi")])
cmp <- read.csv("output/upscaling/budget_sources_components.csv") %>% filter(source %in% c("This study: total ER", "Chambers (this study)"))
SE <- lapply(c(intact = "Healthy", ghost = "Ghost"), function(C) {
  s <- cmp %>% filter(class == C, term == "Reco"); o <- as.list(setNames(s$se, s$component))
  ch <- src %>% filter(term == "CH4", source == "Chambers (this study)", class == C)
  o$ch4 <- (ch$hi - ch$lo) / 2; o })
GPP_SE <- with(src %>% filter(term == "GPP", source == "US-SKR tower (2022-23)"), (hi - lo) / 2)
RNG <- as.matrix(cb[, c("ci_lo", "ci_hi")]); rownames(RNG) <- cb$term
LIT_R <- svr("Litterfall", "Castaneda 2013"); MORT_R <- svr("CWD prod.", "Mortality x AGB (FCE)")
nee <- src %>% filter(term == "NEE", source == "Chambers (this study)", class == "Healthy"); nee_se <- (nee$hi - nee$lo) / 2
dn <- c(nee_se, sumr$flux_lateral_hi - sumr$flux_lateral, lit[["Soil C burial"]] - RNG["Soil C burial", 1], lit[["dBiomass C"]] - RNG["dBiomass C", 1])
dn[3:4] <- c(RNG["Soil C burial", 2] - lit[["Soil C burial"]], RNG["dBiomass C", 2] - lit[["dBiomass C"]])
up_ <- c(nee_se, sumr$flux_lateral - sumr$flux_lateral_lo, lit[["Soil C burial"]] - RNG["Soil C burial", 1], lit[["dBiomass C"]] - RNG["dBiomass C", 1])
RES_R <- sumr$closure_resid + c(-sqrt(sum(dn^2)), sqrt(sum(up_^2)))
cat(sprintf("closure residual %.0f (%.0f to %.0f)\n", sumr$closure_resid, RES_R[1], RES_R[2]))
source("code/08_figures/fig_carbon_stockflow.R")
ttl <- function(k, sub) labs(title = paste(k, "forest"), subtitle = sub)
th_t <- function(k) theme(plot.title = element_text(face = "bold", size = 9, colour = pal_class[[k]], margin = margin(0, 0, 1, 0)),
                          plot.subtitle = element_text(size = 7, colour = "grey30", margin = margin(0, 0, 0, 0)),
                          panel.border = element_rect(fill = NA, colour = "grey55", linewidth = 0.4))
r_i <- co2w[co2w$class == "intact", ]; r_g <- co2w[co2w$class == "ghost", ]
M_g <- sum(ch4w[ch4w$class == "ghost", -1])
pa <- ((schematic("intact") + ttl("intact", sprintf("net uptake %s; retained %s (wood %s, burial %s)", fmtv(-sumr$flux_measured),
          fmtv(sumr$NECB_full), fmtv(lit[["dBiomass C"]]), fmtv(lit[["Soil C burial"]]))) + th_t("intact")) + plot_spacer() +
       (schematic("ghost") + ttl("ghost", sprintf("net loss %s, all of it respired or emitted as CH4", fmtv(sum(r_g[, -1]) + M_g))) + th_t("ghost")) + plot_layout(widths = c(1, 0.05, 1))) /
  schematic_key() + plot_layout(heights = c(1, 0.09))

# ---------------------------------------------------------------- (b) methane budget
mc <- read.csv("output/upscaling/mc_component_uncertainty.csv") %>% filter(component == "total", disturbance_level %in% names(cls)) %>%
  group_by(site, campaign, disturbance_level) %>%
  summarise(lo = weighted.mean(mc_ci_lo, tide_weight), hi = weighted.mean(mc_ci_hi, tide_weight), .groups = "drop") %>%
  group_by(class = cls[disturbance_level]) %>% summarise(lo = mean(lo) * mgch4d_to_gCH4, hi = mean(hi) * mgch4d_to_gCH4, .groups = "drop")
air <- read.csv("data/carafe_topdown/delaria_endmembers_campaign.csv") %>% filter(gas == "CH4") %>%
  mutate(class = ifelse(class == "ghost_forest", "ghost", "intact")) %>% group_by(class) %>%
  summarise(v = mean(flux) * 16.043e-9 * 3.156e7, se = sqrt(sum(se^2)) / n() * 16.043e-9 * 3.156e7, .groups = "drop")
st <- ch4 %>% mutate(comp = factor(comp_lab[comp], comp_lab[c("water", "soil", "root", "stem", "cwd")]), class = factor(class, c("intact", "ghost")))
tot <- st %>% group_by(class) %>% summarise(v = sum(gCH4), .groups = "drop") %>% left_join(mc, by = "class")
lat <- data.frame(class = factor("intact", c("intact", "ghost")), v = lit[["Lateral CH4 (aq)"]] * 16.043 / 12.011)
xk <- function(c) as.numeric(factor(c, c("intact", "ghost")))
# x positions stored in the data so the saved panel (RDS) is self-contained
st <- st %>% mutate(x = xk(class) - 0.17); tot <- tot %>% mutate(x = xk(class) - 0.17)
lat <- lat %>% mutate(x = xk(class) + 0.06); air <- air %>% mutate(x = xk(class) + 0.25)
pb <- ggplot() +
  geom_col(data = st, aes(x, gCH4, fill = comp), width = 0.3, colour = "white", linewidth = 0.25) +
  geom_errorbar(data = tot, aes(x, ymin = lo, ymax = hi), width = 0.08, linewidth = 0.4, colour = col_ink) +
  geom_point(data = tot, aes(x, v, shape = "bottom-up"), size = 2.2, fill = "white", colour = col_ink) +
  geom_col(data = lat, aes(x, v), width = 0.12, fill = col_lat, alpha = 0.8) +
  geom_text(data = lat, aes(x, v, label = "lateral\n(dissolved)"), vjust = -1.6, size = 1.8, colour = col_lat, lineheight = 0.85) +
  geom_errorbar(data = air, aes(x, ymin = v - 1.96 * se, ymax = v + 1.96 * se), width = 0.06, linewidth = 0.4, colour = "grey40") +
  geom_point(data = air, aes(x, v, shape = "airborne, mean of 4 deployments"), size = 2.2, fill = "grey40", colour = "grey40") +
  scale_x_continuous(breaks = 1:2, labels = c("intact", "ghost")) +
  scale_fill_manual(values = setNames(pal_comp[c("water", "soil", "prop root", "stem", "downed wood")], comp_lab[c("water", "soil", "root", "stem", "cwd")]), name = NULL) +
  scale_shape_manual(values = c(`bottom-up` = 23, `airborne, mean of 4 deployments` = 24), name = NULL) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
  labs(x = NULL, y = expression("CH"[4]*" (g CH"[4]*" m"^-2*" yr"^-1*")")) + theme_fig() +
  theme(axis.text.x = element_text(face = "bold", colour = pal_class[c("intact", "ghost")], size = 8), panel.grid.major.x = element_blank(),
        legend.position = "right", legend.key.size = unit(8, "pt"), legend.text = element_text(size = 7))

fig <- (wrap_elements(full = pa & theme(plot.margin = margin(2, 2, 2, 12))) + labs(tag = "a")) / ((pb + labs(tag = "b")) + plot_spacer() + plot_layout(widths = c(1, 0.35))) + plot_layout(heights = c(2.4, 1)) &
  theme(plot.tag = element_text(face = "bold", size = 11))
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
saveRDS(pa, "output/figures/other/fig_carbon_schematic.rds")   # reused as Fig 5a
saveRDS(pb, "output/figures/other/fig_ch4_budget.rds")          # methane budget panel (reusable)
ggsave("output/figures/other/fig_carbon_schematic.png", pa, width = 7.2, height = 4.15, dpi = 300, bg = "white")
ggsave("output/figures/other/fig_carbon_budget.png", fig, width = 7.2, height = 6.4, dpi = 300, bg = "white")
ggsave("output/figures/other/fig_carbon_budget.pdf", fig, width = 7.2, height = 6.4, device = cairo_pdf)
print(tot); print(air)
