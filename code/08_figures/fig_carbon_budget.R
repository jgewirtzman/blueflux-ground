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

# ---------------------------------------------------------------- (a) flow diagram
w <- function(g) 0.4 + 9 * g / 3000                 # arrow linewidth (pt) per g C m-2 yr-1
wrap3 <- function(x) { n <- length(x); g <- split(x, ceiling(seq_len(n) / 2)); paste(sapply(g, paste, collapse = " \u00b7 "), collapse = "\n") }
flow_panel <- function(k) {
  r <- co2 %>% filter(class == k); m <- ch4 %>% filter(class == k)
  G <- gpp$gpp[gpp$class == k]; R <- sum(r$gC); M <- sum(m$gC)
  boxes <- data.frame(x0 = c(0.2, 0.6, 7.4), x1 = c(9.8, 5.9, 9.8), y0 = c(8.7, 3.4, 3.4), y1 = c(9.8, 6.2, 6.2),
                      lab = c("ATMOSPHERE", paste(toupper(k), "FOREST"), "ESTUARY / OCEAN"),
                      fill = c("#EEF3F8", if (k == "intact") "#E8F1EC" else "#EFEEF3", "#EEF3F8"))
  if (k == "intact") boxes <- rbind(boxes, data.frame(x0 = 1.2, x1 = 4.6, y0 = 0.4, y1 = 1.4, lab = "SOIL BURIAL", fill = "#F1ECE6"))
  p <- ggplot() + geom_rect(data = boxes, aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1, fill = I(fill)), colour = "grey60", linewidth = 0.3) +
    geom_text(data = boxes, aes((x0 + x1) / 2, y1 - 0.3, label = lab), size = 2.2, fontface = "bold", colour = "grey30")
  arr <- arrow(length = unit(5, "pt"), type = "closed")
  if (G > 0) p <- p + annotate("segment", x = 1.3, xend = 1.3, y = 8.7, yend = 6.2, linewidth = w(G), colour = pal_class[["intact"]], arrow = arr) +
    annotate("text", x = 1.3, y = 4.9, label = sprintf("GPP\n%s", format(round(G), big.mark = ",")), size = 2.4, colour = pal_class[["intact"]], fontface = "bold", lineheight = 0.9)
  else p <- p + annotate("text", x = 1.3, y = 7.45, label = "GPP ~0\n(no canopy)", size = 2.2, colour = "grey45", fontface = "italic", lineheight = 0.9)
  p <- p + annotate("segment", x = 2.8, xend = 2.8, y = 6.2, yend = 8.7, linewidth = w(R), colour = col_co2, arrow = arr) +
    annotate("text", x = 3.1, y = 8.25, label = sprintf("Respiration %s", format(round(R), big.mark = ",")), hjust = 0, size = 2.4, fontface = "bold", colour = "grey30")
  rr <- r %>% filter(gC > 0.5) %>% arrange(desc(gC)) %>% mutate(lab = sprintf("%s %s", comp_lab[comp], round(gC)))
  p <- p + annotate("text", x = 3.1, y = 7.95, label = wrap3(rr$lab), hjust = 0, vjust = 1, size = 1.85, colour = "grey35", lineheight = 0.95)
  p <- p + annotate("segment", x = 6.4, xend = 6.4, y = 6.2, yend = 8.7, linewidth = max(0.6, w(M) * 3), colour = col_ch4, arrow = arr) +
    annotate("text", x = 6.65, y = 8.25, label = sprintf("CH4 %s", formatC(M, format = "f", digits = 1)), hjust = 0, size = 2.4, fontface = "bold", colour = col_ch4)
  mm <- m %>% mutate(sh = 100 * gC / M) %>% filter(sh >= 1) %>% arrange(desc(sh)) %>% mutate(lab = sprintf("%s %d%%", comp_lab[comp], round(sh)))
  p <- p + annotate("text", x = 6.65, y = 7.95, label = wrap3(mm$lab), hjust = 0, vjust = 1, size = 1.85, colour = col_ch4, lineheight = 0.95)
  if (k == "intact") {
    L <- sumr$flux_lateral
    p <- p + annotate("segment", x = 5.9, xend = 7.4, y = 4.8, yend = 4.8, linewidth = w(L), colour = col_lat, arrow = arr) +
      annotate("text", x = 8.6, y = 5.2, label = sprintf("Lateral %d\n(%d to %d)", round(L), round(sumr$flux_lateral_lo), round(sumr$flux_lateral_hi)),
               size = 2.2, colour = col_lat, fontface = "bold", lineheight = 0.9) +
      annotate("text", x = 8.6, y = 4.2, label = sprintf("DIC %d \u00b7 DOC %d\nPOC %d \u00b7 CH4 %.2f", round(lit[["Lateral DIC"]]), round(lit[["Lateral DOC"]]), round(lit[["Lateral POC"]]), lit[["Lateral CH4 (aq)"]]),
               size = 1.8, colour = col_lat, lineheight = 0.9) +
      annotate("segment", x = 2.9, xend = 2.9, y = 3.4, yend = 1.4, linewidth = w(lit[["Soil C burial"]]), colour = col_stor, arrow = arr) +
      annotate("text", x = 3.15, y = 2.4, label = sprintf("Burial %d", round(lit[["Soil C burial"]])), hjust = 0, size = 2.3, colour = col_stor, fontface = "bold") +
      annotate("text", x = 3.6, y = 4.6, label = sprintf("wood increment +%d\nclosure residual +%d", round(lit[["dBiomass C"]]), round(sumr$closure_resid)),
               size = 1.9, colour = "grey35", lineheight = 0.95) +
      annotate("text", x = 3.6, y = 3.85, label = sprintf("retains %d (NECB)", round(sumr$NECB_full)), size = 2.2, fontface = "bold", colour = pal_class[["intact"]])
  } else {
    p <- p + annotate("text", x = 8.6, y = 4.8, label = "lateral export\nnot measured", size = 2.0, colour = "grey50", fontface = "italic", lineheight = 0.9) +
      annotate("text", x = 3.25, y = 2.4, label = "burial and peat loss\nnot measured", size = 2.0, colour = "grey50", fontface = "italic", lineheight = 0.9) +
      annotate("text", x = 3.25, y = 4.4, label = sprintf("net loss %d\n(vertical only)", round(R + M - G)), size = 2.2, fontface = "bold", colour = pal_class[["ghost"]], lineheight = 0.9)
  }
  p + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10), expand = FALSE) + theme_void()
}

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
  geom_text(data = lat, aes(xk(class) + 0.06, v, label = "lateral\n(dissolved)"), vjust = -0.3, size = 1.8, colour = col_lat, lineheight = 0.85) +
  geom_errorbar(data = air, aes(xk(class) + 0.25, ymin = v - 1.96 * se, ymax = v + 1.96 * se), width = 0.06, linewidth = 0.4, colour = "grey40") +
  geom_point(data = air, aes(xk(class) + 0.25, v, shape = "airborne, mean of 4 deployments"), size = 2.2, fill = "grey40", colour = "grey40") +
  scale_x_continuous(breaks = 1:2, labels = c("intact", "ghost")) +
  scale_fill_manual(values = setNames(pal_comp[c("water", "soil", "prop root", "stem", "downed wood")], comp_lab[c("water", "soil", "root", "stem", "cwd")]), name = NULL) +
  scale_shape_manual(values = c(`bottom-up (chambers × area)` = 23, `airborne, mean of 4 deployments` = 24), name = NULL) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
  labs(x = NULL, y = expression("CH"[4]*" (g CH"[4]*" m"^-2*" yr"^-1*")")) + theme_fig() +
  theme(axis.text.x = element_text(face = "bold", colour = pal_class[c("intact", "ghost")], size = 8), panel.grid.major.x = element_blank(),
        legend.position = "right", legend.key.size = unit(8, "pt"), legend.text = element_text(size = 7))

top <- (flow_panel("intact") | flow_panel("ghost"))
fig <- (wrap_elements(full = top) + labs(tag = "a", subtitle = "Carbon flows, g C m\u207b\u00b2 yr\u207b\u00b9 (arrow width \u221d flux); lateral export and storage from the literature, intact forest only") + theme(plot.subtitle = element_text(size = 7, colour = "grey35"))) / ((pb + labs(tag = "b")) + plot_spacer() + plot_layout(widths = c(1, 0.15))) + plot_layout(heights = c(1.15, 1)) &
  theme(plot.tag = element_text(face = "bold", size = 11))
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/fig_carbon_budget.png", fig, width = 7.2, height = 6.6, dpi = 300, bg = "white")
ggsave("output/figures/other/fig_carbon_budget.pdf", fig, width = 7.2, height = 6.6, device = cairo_pdf)
print(tot); print(air)
