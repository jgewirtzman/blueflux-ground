# =============================================================================
# Fig. 3 | From rates to stands (rate x area = budget).
#   (a) Surface per m2 of ground by component and class (intact = SRS5, SRS6;
#       ghost = CP40, FLM30; means over sites and campaigns, tide-weighted),
#       labelled by source: laser scanning (prop roots, stems = trunk + branch),
#       flooding model (water surface vs exposed soil; logger x floor survey),
#       literature (downed wood: Krauss et al. 2005 volume, above-water part;
#       leaf area: LAI 2.8).
#   (b) Stand CH4 budgets by site and campaign, stacked by component (tide-
#       weighted, exponential stem profile), with Monte Carlo 95% intervals
#       on the total (07_upscaling/02_upscale_methane.R).
#   (c) Stand CO2 budgets by class and campaign: component respiration above
#       zero, gross primary production (tower, intact) below, bottom-up net
#       exchange with Monte Carlo 95% interval (07_upscaling/03_upscale_co2.R).
# Writes output/figures/other/fig3_stands.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(patchwork)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
cls2 <- c(intact = "intact", ghost = "ghost")
site_class <- c(SRS5 = "intact", SRS6 = "intact", CP40 = "ghost", FLM30 = "ghost")
comp_order <- c("water", "soil", "prop root", "stem", "downed wood", "leaf")

# ---- (a) surface per ground area ----------------------------------------------
tw <- read.csv("output/upscaling/plot_level_CH4_totals.csv") %>% filter(scenario == "exponential") %>%
  distinct(site, campaign, tide_state, tide_weight)
bd <- read.csv("output/upscaling/budget_decomposition.csv") %>% filter(component %in% c("water", "soil", "cwd")) %>%
  left_join(tw, by = c("site", "campaign", "tide_state")) %>%
  mutate(sa = surface_area_m2 / area_m2) %>% group_by(site, campaign, component) %>%
  summarise(sa = weighted.mean(sa, tide_weight), .groups = "drop") %>%
  group_by(site, component) %>% summarise(sa = mean(sa), .groups = "drop")
ts <- read.csv("data/tls/tree_stats_per_site.csv")
tls <- read.csv("data/tls/all_sites_summary.csv") %>% left_join(ts %>% select(site, area_m2), by = "site") %>%
  mutate(component = ifelse(segment_class == "root", "root", "stem")) %>%
  group_by(site, component) %>% summarise(sa = sum(Total_surface_area_m2) / first(area_m2), .groups = "drop")
area <- bind_rows(bd, tls, data.frame(site = names(site_class), component = "leaf",
                                       sa = ifelse(site_class == "intact", 2.8, 0))) %>%
  mutate(class = factor(site_class[site], names(cls2))) %>% group_by(class, component) %>% summarise(sa = mean(sa), .groups = "drop") %>%
  mutate(comp = factor(recode(component, root = "prop root", cwd = "downed wood"), rev(comp_order)),
         source = case_when(comp %in% c("prop root", "stem") ~ "laser scanning",
                            comp %in% c("water", "soil") ~ "flooding model",
                            TRUE ~ "literature"),
         y = as.numeric(comp) + ifelse(class == "intact", 0.18, -0.18))
bands <- data.frame(y = seq_along(levels(area$comp))) %>% filter(y %% 2 == 1)
pa <- ggplot(area) +
  geom_rect(data = bands, aes(ymin = y - 0.5, ymax = y + 0.5), xmin = -Inf, xmax = Inf, fill = "grey95") +
  geom_segment(aes(x = 0, xend = sa, y = y, yend = y, colour = class), linewidth = 2.6,
               alpha = ifelse(area$source == "literature", 0.45, 1)) +
  geom_text(data = area %>% filter(class == "intact") %>% group_by(comp) %>%
              summarise(y = max(as.numeric(comp)) + 0.36, source = first(source), .groups = "drop"),
            aes(x = 3.1, y = y, label = source), hjust = 1, size = 2, colour = "grey45", fontface = "italic") +
  scale_y_continuous(breaks = seq_along(levels(area$comp)), labels = levels(area$comp), expand = c(0, 0),
                     limits = c(0.5, length(levels(area$comp)) + 0.5)) +
  scale_x_continuous(limits = c(0, 3.1), expand = c(0, 0)) +
  scale_colour_manual(values = pal_class[names(cls2)], name = "forest class") +
  labs(x = expression("Surface per ground area (m"^2*" m"^-2*")"), y = NULL) +
  theme_fig() + theme(panel.grid.major.y = element_blank(), axis.ticks.y = element_blank())

# ---- (b) CH4 budgets ------------------------------------------------------------
ch <- read.csv("output/upscaling/summary_CH4_by_component.csv") %>% filter(scenario == "exponential") %>%
  pivot_longer(c(water, soil, root, stem, cwd), names_to = "component", values_to = "mg") %>%
  mutate(comp = factor(recode(component, root = "prop root", cwd = "downed wood"), comp_order),
         class = factor(site_class[site], names(cls2)), campaign = factor(campaign, c("Oct 2022", "Mar 2023")),
         x = factor(sub(" 20", "\n'", campaign), c("Oct\n'22", "Mar\n'23")),
         site_lab = factor(paste0(site, "\n", site_class[site]), paste0(names(site_class), "\n", site_class)))
mcb <- read.csv("output/upscaling/mc_component_uncertainty.csv") %>% filter(component == "total") %>%
  group_by(site, campaign) %>% summarise(lo = weighted.mean(mc_ci_lo, tide_weight), hi = weighted.mean(mc_ci_hi, tide_weight), .groups = "drop")
tot <- ch %>% group_by(site, campaign, class, x, site_lab) %>% summarise(total = sum(mg), .groups = "drop") %>%
  left_join(mcb, by = c("site", "campaign"))
pb <- ggplot(ch, aes(x, mg)) +
  geom_col(aes(fill = comp), width = 0.7, colour = "white", linewidth = 0.15, position = position_stack(reverse = TRUE)) +
  geom_errorbar(data = tot, aes(x, ymin = lo, ymax = hi), width = 0.2, linewidth = 0.4, inherit.aes = FALSE) +
  facet_grid(~ site_lab) +
  scale_fill_manual(values = pal_comp, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = expression("Stand CH"[4]*" (mg m"^-2*" d"^-1*")")) +
  theme_fig() + theme(panel.grid.major.x = element_blank(), strip.text = element_text(hjust = 0.5))

# ---- (c) CO2 budgets ------------------------------------------------------------
co <- read.csv("output/upscaling/plot_level_CO2_totals.csv") %>% filter(site %in% names(site_class)) %>%
  mutate(class = factor(site_class[site], names(cls2))) %>% group_by(class, campaign) %>%
  summarise(across(c(soil, water, root, stem, cwd, leaf, GPP_used, NEE_bottomup), mean), .groups = "drop") %>%
  mutate(campaign = factor(campaign, c("Oct 2022", "Mar 2023")))
co_long <- co %>% pivot_longer(c(water, soil, root, stem, cwd, leaf), names_to = "component", values_to = "v") %>%
  mutate(comp = factor(recode(component, root = "prop root", cwd = "downed wood"), comp_order))
gpp <- co %>% transmute(class, campaign, v = -GPP_used)
mcc <- read.csv("output/upscaling/mc_CO2_forcing.csv") %>% mutate(class = factor(recode(class, healthy = "intact"), names(cls2)),
                                                                   campaign = factor(campaign, c("Oct 2022", "Mar 2023")))
nee <- co %>% select(class, campaign, NEE_bottomup) %>% left_join(mcc %>% select(class, campaign, nee_lo, nee_hi), by = c("class", "campaign"))
pc <- ggplot() +
  geom_col(data = co_long, aes(campaign, v, fill = comp), width = 0.6, colour = "white", linewidth = 0.15,
           position = position_stack(reverse = TRUE)) +
  geom_col(data = gpp, aes(campaign, v), width = 0.6, fill = "#A9C6B3") +
  geom_text(data = data.frame(class = factor("intact", names(cls2))), aes(x = 1.5, y = -0.4, label = "GPP (tower)"),
            size = 2.1, colour = "grey30", vjust = 1, inherit.aes = FALSE) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_errorbar(data = nee, aes(campaign, ymin = nee_lo, ymax = nee_hi), width = 0.15, linewidth = 0.4) +
  geom_point(data = nee, aes(campaign, NEE_bottomup), shape = 23, fill = "white", colour = col_ink, size = 2.2) +
  scale_x_discrete(labels = function(x) sub(" 20", "\n'", x)) +
  facet_grid(~ class) +
  scale_fill_manual(values = pal_comp, name = "component", breaks = comp_order) +
  labs(x = NULL, y = expression("Stand CO"[2]*" ("*mu*"mol m"^-2*" s"^-1*", daily mean)")) +
  theme_fig() + theme(panel.grid.major.x = element_blank(), strip.text = element_text(hjust = 0.5),
                      plot.caption = element_text(size = 6, colour = "grey40", hjust = 0))

pa <- pa + theme(legend.position = "bottom")
fig <- (pa + labs(tag = "a")) + (pb + labs(tag = "b")) + (pc + labs(tag = "c")) + guide_area() +
  plot_layout(ncol = 2, widths = c(1, 1.15), heights = c(1, 1), guides = "collect") & theme(legend.position = "right")
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/fig3_stands.png", fig, width = 7.2, height = 6, dpi = 300, bg = "white")
ggsave("output/figures/other/fig3_stands.pdf", fig, width = 7.2, height = 6, device = cairo_pdf)
