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

# ---------------------------------------------------------------- (a) carbon waterfall
# Carbon is followed in process order: fixed by photosynthesis, respired by each
# component, emitted as CH4, exported laterally, and what remains is stored.
# Each bar runs from the running total before a step to the total after it.
steps_for <- function(k) {
  r <- co2 %>% filter(class == k); m <- ch4 %>% filter(class == k)
  G <- gpp$gpp[gpp$class == k]
  rv <- setNames(r$gC, r$comp)
  s <- data.frame(proc = "Uptake", step = "GPP", d = G, fill = "GPP")
  for (c in c("leaf", "stem", "root", "soil", "water", "cwd"))
    if (!is.na(rv[c]) && rv[c] > 0.5) s <- rbind(s, data.frame(proc = "Respiration", step = comp_lab[[c]], d = -rv[[c]], fill = c))
  s <- rbind(s, data.frame(proc = "CH4", step = "CH4", d = -sum(m$gC), fill = "CH4"))
  if (k == "intact") {
    s <- rbind(s, data.frame(proc = "Lateral export", step = c("DIC", "DOC", "POC"),
                             d = -c(lit[["Lateral DIC"]], lit[["Lateral DOC"]], lit[["Lateral POC"]]), fill = c("DIC", "DOC", "POC")))
  }
  s$end <- cumsum(s$d); s$start <- s$end - s$d
  s$class <- k; s$i <- seq_len(nrow(s)); s
}
wf <- bind_rows(steps_for("intact"), steps_for("ghost"))
# subtotals: net exchange after CH4; retained carbon (NECB) after lateral export, split into its fates
sub <- wf %>% group_by(class) %>% summarise(net = end[proc == "CH4"], fin = last(end), n = n(), .groups = "drop")
stor <- data.frame(class = "intact", part = c("burial", "wood increment", "unexplained (likely lateral)"),
                   v = c(lit[["Soil C burial"]], lit[["dBiomass C"]], sumr$closure_resid))
stor <- stor %>% mutate(top = cumsum(v), bot = top - v)
pal_steps <- c(GPP = pal_class[["intact"]], leaf = pal_comp[["leaf"]], stem = pal_comp[["stem"]], root = pal_comp[["prop root"]],
               soil = pal_comp[["soil"]], water = pal_comp[["water"]], cwd = pal_comp[["downed wood"]], CH4 = col_ch4,
               DIC = "#6BAED6", DOC = "#9ECAE1", POC = "#C6DBEF")
pal_stor <- c(burial = "#4A2E16", `wood increment` = "#8C6235", `unexplained (likely lateral)` = "#D9CBB5")
proc_lev <- c("Uptake", "Respiration", "CH4", "Lateral export", "Retained")
wf <- wf %>% mutate(class = factor(class, c("intact", "ghost")), xi = i)
nx <- max(wf$i) + 2
bands <- wf %>% group_by(class, proc) %>% summarise(x0 = min(xi) - 0.5, x1 = max(xi) + 0.5, .groups = "drop") %>%
  mutate(odd = as.integer(factor(proc, proc_lev)) %% 2 == 1)
xlab <- wf %>% distinct(class, xi, step)
fin_i <- wf %>% group_by(class) %>% summarise(xi = max(xi) + 1.2, .groups = "drop")
stor$xi <- fin_i$xi[fin_i$class == "intact"]; stor$class <- factor("intact", c("intact", "ghost"))
netlab <- sub %>% mutate(class = factor(class, c("intact", "ghost")))
pa <- ggplot(wf) +
  geom_rect(data = bands %>% filter(odd), aes(xmin = x0, xmax = x1, ymin = -Inf, ymax = Inf), fill = "grey96") +
  geom_text(data = bands, aes(x = (x0 + x1) / 2, y = 3300, label = proc), size = 2.1, colour = "grey40", fontface = "italic") +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
  geom_rect(aes(xmin = xi - 0.38, xmax = xi + 0.38, ymin = pmin(start, end), ymax = pmax(start, end), fill = fill)) +
  geom_segment(data = wf %>% group_by(class) %>% filter(i < max(i)), aes(x = xi + 0.38, xend = xi + 0.62, y = end, yend = end),
               colour = "grey55", linewidth = 0.25) +
  geom_text(aes(x = xi, y = pmax(start, end), label = ifelse(abs(d) < 0.05, "0", ifelse(abs(d) >= 1, format(round(abs(d)), big.mark = ","), formatC(abs(d), format = "f", digits = 1)))),
            vjust = -0.4, size = 1.9, colour = "grey25") +
  geom_rect(data = stor, aes(xmin = xi - 0.38, xmax = xi + 0.38, ymin = bot, ymax = top, fill = part), inherit.aes = FALSE) +
  geom_text(data = stor %>% mutate(class = factor(class, c("intact", "ghost"))) %>% filter(class == "intact") %>% slice(1),
            aes(x = xi, y = max(stor$top), label = sprintf("retained\n%d", round(sumr$NECB_full))), vjust = -0.3, size = 2, fontface = "bold",
            colour = pal_class[["intact"]], lineheight = 0.9, inherit.aes = FALSE) +
  geom_text(data = netlab %>% left_join(wf %>% filter(proc == "CH4") %>% select(class, xi, end), by = "class") %>% filter(class == "intact"),
            aes(x = xi + 0.5, y = end + 330, label = sprintf("net uptake\n%s", format(round(net), big.mark = ","))), size = 2.1, fontface = "bold",
            colour = "grey25", lineheight = 0.9) +
  geom_text(data = netlab %>% filter(class == "ghost"), aes(x = 1.5, y = -820, label = sprintf("net loss %d (lateral export\nand burial not measured)", round(-net))),
            hjust = 0, size = 2.1, fontface = "bold", colour = pal_class[["ghost"]], lineheight = 0.9) +
  facet_grid(~ class, scales = "free_x", space = "free_x") +
  scale_x_continuous(breaks = function(l) seq(ceiling(l[1]), floor(l[2])), labels = NULL, expand = expansion(add = 0.3)) +
  scale_fill_manual(values = c(pal_steps, pal_stor), breaks = names(pal_stor), name = "retained as") +
  geom_text(data = xlab, aes(x = xi, y = -1070, label = step), angle = 45, hjust = 1, vjust = 1, size = 2.1, colour = "grey25") +
  geom_text(data = stor %>% slice(1), aes(x = xi, y = -1070, label = "retained"), angle = 45, hjust = 1, vjust = 1, size = 2.1, colour = "grey25", inherit.aes = FALSE) +
  coord_cartesian(ylim = c(-1000, 3450), clip = "off") +
  labs(x = NULL, y = expression("Carbon (g C m"^-2*" yr"^-1*"), running total")) + theme_fig() +
  theme(strip.text = element_text(hjust = 0.5, size = 9), panel.grid.major.x = element_blank(), axis.ticks.x = element_blank(),
        plot.margin = margin(5, 5, 48, 5), axis.text.x = element_blank(), legend.position = "right", legend.key.size = unit(8, "pt"), legend.text = element_text(size = 7))

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
pb <- ggplot() +
  geom_col(data = st, aes(xk(class) - 0.17, gCH4, fill = comp), width = 0.3, colour = "white", linewidth = 0.25) +
  geom_errorbar(data = tot, aes(xk(class) - 0.17, ymin = lo, ymax = hi), width = 0.08, linewidth = 0.4, colour = col_ink) +
  geom_point(data = tot, aes(xk(class) - 0.17, v, shape = "bottom-up (chambers × area)"), size = 2.2, fill = "white", colour = col_ink) +
  geom_col(data = lat, aes(xk(class) + 0.06, v), width = 0.12, fill = col_lat, alpha = 0.8) +
  geom_text(data = lat, aes(xk(class) + 0.06, v, label = "lateral\n(dissolved)"), vjust = -1.6, size = 1.8, colour = col_lat, lineheight = 0.85) +
  geom_errorbar(data = air, aes(xk(class) + 0.25, ymin = v - 1.96 * se, ymax = v + 1.96 * se), width = 0.06, linewidth = 0.4, colour = "grey40") +
  geom_point(data = air, aes(xk(class) + 0.25, v, shape = "airborne, mean of 4 deployments"), size = 2.2, fill = "grey40", colour = "grey40") +
  scale_x_continuous(breaks = 1:2, labels = c("intact", "ghost")) +
  scale_fill_manual(values = setNames(pal_comp[c("water", "soil", "prop root", "stem", "downed wood")], comp_lab[c("water", "soil", "root", "stem", "cwd")]), name = NULL) +
  scale_shape_manual(values = c(`bottom-up (chambers × area)` = 23, `airborne, mean of 4 deployments` = 24), name = NULL) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
  labs(x = NULL, y = expression("CH"[4]*" (g CH"[4]*" m"^-2*" yr"^-1*")")) + theme_fig() +
  theme(axis.text.x = element_text(face = "bold", colour = pal_class[c("intact", "ghost")], size = 8), panel.grid.major.x = element_blank(),
        legend.position = "right", legend.key.size = unit(8, "pt"), legend.text = element_text(size = 7))

fig <- (pa + labs(tag = "a")) / ((pb + labs(tag = "b")) + plot_spacer() + plot_layout(widths = c(1, 0.35))) + plot_layout(heights = c(1.25, 1)) &
  theme(plot.tag = element_text(face = "bold", size = 11))
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/fig_carbon_budget.png", fig, width = 7.2, height = 6.4, dpi = 300, bg = "white")
ggsave("output/figures/other/fig_carbon_budget.pdf", fig, width = 7.2, height = 6.4, device = cairo_pdf)
print(tot); print(air)
