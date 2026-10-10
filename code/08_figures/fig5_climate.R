# =============================================================================
# Fig. 5 | Climate consequence. (The carbon budget schematic is Fig. 3d.)
#   (a) Net forcing per m2 of intact and ghost forest at GWP20, GWP100 and GWP*:
#       net CO2 exchange and CH4 (as CO2-eq) stacked; diamond = net with Monte
#       Carlo 95% interval (GWP20/100); open circle = intact net ecosystem carbon
#       balance with alkalinity export retained (forcing_framings.csv).
#   (b) The intact-to-ghost switch per m2, split into lost uptake (the intact
#       stand's net CO2 uptake that stops: a recovery debt until regrowth), CO2
#       released by the dead stand (measured, vertical) and the added CH4, with
#       CH4 as a share of the warming from the measured carbon release and the
#       regional total. Unmeasured losses of the dead stand (lateral export,
#       whose effect depends on its form, and peat collapse) are not included
#       (text S6).
#   (c) Induced CH4 from 2017 hurricane dieback per 0.25 degree cell, with
#       little recovery after the 2017 hurricanes, and the Irma/Maria tracks.
# Inputs from 07_upscaling (net_forcing_by_class, mc_net_forcing_by_class,
# forcing_framings, forcing_switch_per_m2, regional_ghost_forcing{,_grid},
# hurricane_tracks_2017). Writes output/figures/other/fig5_climate.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(patchwork); library(sf)})
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
pal_gas <- c(CO2 = "#9A9DA1", CH4 = "#A23B72")
metrics <- c("GWP20", "GWP100", "GWP*")

nf <- read.csv("output/upscaling/net_forcing_by_class.csv") %>% mutate(class = class_labels[disturbance_level])
mc <- read.csv("output/upscaling/mc_net_forcing_by_class.csv") %>% mutate(class = class_labels[class])
fr <- read.csv("output/upscaling/forcing_framings.csv")
sw <- with(read.csv("output/upscaling/forcing_switch_per_m2.csv"), setNames(g_co2eq_m2_yr, term))
reg <- read.csv("output/upscaling/regional_ghost_forcing.csv") %>% filter(country == "Total")

# ---- (a) net forcing by class and metric ----
gas <- nf %>% transmute(class, CO2 = co2_g_yr, GWP20 = ch4_co2eq20, GWP100 = ch4_co2eq100, `GWP*` = ch4_co2we_gwpstar) %>%
  pivot_longer(all_of(metrics), names_to = "metric", values_to = "CH4") %>%
  pivot_longer(c(CO2, CH4), names_to = "gas", values_to = "v") %>%
  mutate(metric = factor(metric, metrics), gas = factor(gas, c("CO2", "CH4")), class = factor(class, c("intact", "ghost")))
net <- gas %>% group_by(class, metric) %>% summarise(net = sum(v), .groups = "drop") %>%
  left_join(mc %>% select(class, net20_lo, net20_hi, net100_lo, net100_hi), by = "class") %>%
  mutate(lo = case_when(metric == "GWP20" ~ net20_lo, metric == "GWP100" ~ net100_lo),
         hi = case_when(metric == "GWP20" ~ net20_hi, metric == "GWP100" ~ net100_hi))
necb <- fr %>% filter(class == "Healthy", framing == "necb_alk_retained") %>%
  transmute(GWP20 = net20, GWP100 = net100, `GWP*` = netstar, lo20 = net20_lo, hi20 = net20_hi, lo100 = net100_lo, hi100 = net100_hi) %>%
  pivot_longer(all_of(metrics), names_to = "metric", values_to = "net") %>%
  mutate(lo = case_when(metric == "GWP20" ~ lo20, metric == "GWP100" ~ lo100), hi = case_when(metric == "GWP20" ~ hi20, metric == "GWP100" ~ hi100),
         metric = factor(metric, metrics), class = factor("intact", c("intact", "ghost")), est = "intact, incl. lateral export")
pa <- ggplot(gas, aes(class, v)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_col(aes(fill = gas), width = 0.62, colour = "white", linewidth = 0.25) +
  geom_errorbar(data = net, aes(y = net, ymin = lo, ymax = hi), width = 0.12, linewidth = 0.4, colour = col_ink) +
  geom_point(data = net, aes(y = net, shape = "net (95% interval)"), size = 2.4, fill = "white", colour = col_ink, stroke = 0.6) +
  geom_errorbar(data = necb, aes(x = as.numeric(class) + 0.36, y = net, ymin = lo, ymax = hi), width = 0.08, linewidth = 0.35,
                colour = "grey35", linetype = "22") +
  geom_point(data = necb, aes(x = as.numeric(class) + 0.36, y = net, shape = est), size = 1.9, fill = "white", colour = "grey35", stroke = 0.6) +
  facet_grid(~ metric) +
  scale_fill_manual(values = pal_gas, labels = c(CO2 = expression("net CO"[2]), CH4 = expression("CH"[4])), name = NULL) +
  scale_shape_manual(values = c(`net (95% interval)` = 23, `intact, incl. lateral export` = 21), name = NULL,
                     breaks = c("net (95% interval)", "intact, incl. lateral export")) +
  scale_y_continuous(breaks = seq(-4000, 4000, 2000), labels = scales::label_comma()) +
  labs(x = NULL, y = expression("Net forcing (g CO"[2]*"-eq m"^-2*" yr"^-1*")")) + theme_fig() +
  theme(axis.text.x = element_text(colour = pal_class[c("intact", "ghost")], face = "bold"),
        strip.text = element_text(hjust = 0.5), panel.grid.major.x = element_blank(), legend.direction = "vertical")

# ---- (b) the switch, per m2: lost uptake + carbon released + CH4 ----
lost <- -nf$co2_g_yr[nf$class == "intact"]; rel <- nf$co2_g_yr[nf$class == "ghost"]
dch4 <- c(GWP20 = nf$ch4_co2eq20[nf$class == "ghost"] - nf$ch4_co2eq20[nf$class == "intact"],
          GWP100 = nf$ch4_co2eq100[nf$class == "ghost"] - nf$ch4_co2eq100[nf$class == "intact"],
          `GWP*` = nf$ch4_co2we_gwpstar[nf$class == "ghost"] - nf$ch4_co2we_gwpstar[nf$class == "intact"])
parts <- c(lost = "lost uptake (recovery debt)", rel = "carbon released by the dead stand", CH4 = "added CH4")
swd <- expand.grid(metric = factor(metrics, metrics), part = factor(names(parts), names(parts))) %>%
  mutate(v = case_when(part == "lost" ~ lost, part == "rel" ~ rel, TRUE ~ dch4[as.character(metric)]))
swt <- data.frame(metric = factor(metrics, metrics), tot = c(sw[["gwp20"]], sw[["gwp100"]], sw[["gwpstar"]]),
                  lo = c(sw[["gwp20_lo"]], sw[["gwp100_lo"]], NA), hi = c(sw[["gwp20_hi"]], sw[["gwp100_hi"]], NA),
                  Tg = c(reg$switch_gwp20_Tg, reg$switch_gwp100_Tg, reg$switch_gwpstar_Tg), ch4 = dch4) %>%
  mutate(pct_rel = 100 * ch4 / rel)
pal_part <- c(lost = "#D3D5D8", rel = pal_gas[["CO2"]], CH4 = pal_gas[["CH4"]])
pb <- ggplot(swd, aes(v, metric)) +
  geom_col(aes(fill = part), width = 0.6, colour = "white", linewidth = 0.25, position = position_stack(reverse = TRUE)) +
  geom_errorbar(data = swt, aes(x = tot, xmin = lo, xmax = hi), width = 0.15, linewidth = 0.4, colour = col_ink, orientation = "y") +
  geom_text(data = swt, aes(x = pmax(tot, hi, na.rm = TRUE) + 250,
                            label = sprintf("CH\u2084 = +%.0f%%\nof CO\u2082 released\n%s Tg yr\u207b\u00b9", pct_rel, formatC(Tg, format = "f", digits = 2))),
            hjust = 0, size = 2.1, colour = "grey20", lineheight = 0.92) +
  scale_fill_manual(values = pal_part, name = NULL,
                    labels = c(lost = "lost uptake (recovery debt)", rel = expression("CO"[2]*" released by the dead stand"),
                               CH4 = expression("added CH"[4]))) +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, 0.5))) +
  labs(x = expression("Added forcing, ghost minus intact (g CO"[2]*"-eq m"^-2*" yr"^-1*")"), y = NULL) +
  coord_cartesian(clip = "off") +
  theme_fig() + theme(panel.grid.major.y = element_blank())

# ---- (c) regional induced CH4 ----
sf_use_s2(FALSE)
land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
grid <- read.csv("output/upscaling/regional_ghost_forcing_grid.csv")
tracks <- read.csv("output/upscaling/hurricane_tracks_2017.csv")
lab_pts <- tracks %>% group_by(storm) %>% filter(lon > -97, lon < -60, lat > 9, lat < 30.5) %>%
  slice(if (first(storm) == "Irma") which.min(abs(lat - 27.5)) else which.min(abs(lat - 27))) %>% ungroup() %>%
  mutate(hjust = ifelse(storm == "Irma", 1.1, -0.12))
# 2017 hurricane dieback (Taillie et al. 2020 layer, ~25 m polygons) aggregated to
# 0.25 degree cells; induced CH4 = dieback area x the per-area increase
seq_ch4 <- grDevices::colorRampPalette(c("#F3E1EA", pal_gas[["CH4"]], "#4A1533"))(5)
pc <- ggplot() +
  geom_sf(data = land, fill = "grey93", colour = "grey75", linewidth = 0.12) +
  geom_tile(data = grid, aes(lon, lat, fill = ch4_Mg), width = grid$res_deg[1], height = grid$res_deg[1], colour = "white", linewidth = 0.05) +
  geom_path(data = tracks, aes(lon, lat, group = storm), colour = "grey35", linewidth = 0.35, linetype = "22") +
  geom_text(data = lab_pts, aes(lon, lat, label = paste(storm, "2017"), hjust = hjust), colour = "grey25", size = 2.3, fontface = "italic") +
  scale_fill_gradientn(colours = seq_ch4, trans = "log10", breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100"),
                       name = expression(atop("Induced CH"[4]*" (Mg", "yr"^-1*" per 0.25"*degree*" cell)"))) +
  coord_sf(xlim = c(-98, -59), ylim = c(8, 31), expand = FALSE) +
  labs(x = NULL, y = NULL) +
  guides(fill = guide_colourbar(title.position = "top", direction = "vertical")) +
  theme_fig() + theme(legend.position = "right", legend.justification = c(0, 0.5),
                      legend.key.width = unit(6, "pt"), legend.key.height = unit(22, "pt"), legend.title = element_text(size = 6.5),
                      panel.grid.major = element_line(colour = "grey88", linewidth = 0.2), axis.line = element_blank())

row1 <- ((pa + labs(tag = "a") + theme(legend.position = "bottom", legend.direction = "vertical")) |
         (pb + labs(tag = "b") + theme(legend.position = "bottom", legend.direction = "vertical"))) +
  plot_layout(widths = c(1.25, 1))
fig <- row1 / (pc + labs(tag = "c")) + plot_layout(heights = c(1, 1.1)) & theme(plot.tag = element_text(face = "bold", size = 11))
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/fig5_climate.png", fig, width = 7.2, height = 7.4, dpi = 300, bg = "white")
ggsave("output/figures/other/fig5_climate.pdf", fig, width = 7.2, height = 7.4, device = cairo_pdf)
print(swt)
