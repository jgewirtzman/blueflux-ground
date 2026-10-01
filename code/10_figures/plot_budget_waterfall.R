# =============================================================================
# plot_budget_waterfall.R
# -----------------------------------------------------------------------------
# Residual-decomposition waterfall (Healthy): start from the vertical net uptake
# (NECB_vertical), step down through the independently-measured sinks (burial,
# biomass, conservative lateral export), and show what is left unaccounted.
# The residual == the additional lateral export the bottom-up budget implies.
#
# Reads: output/upscaling/carbon_budget_summary.csv, carbon_budget_scenarios.csv
# =============================================================================
suppressMessages({library(dplyr); library(ggplot2)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

h  <- read.csv("output/upscaling/carbon_budget_summary.csv") %>% filter(class == "Healthy")
sc <- read.csv("output/upscaling/carbon_budget_scenarios.csv")
NECB <- h$NECB_vertical_only; BUR <- h$burial_accum; BIO <- h$dbiomass_accum
LATc <- h$flux_lateral; RES <- h$closure_resid
LAThi <- sc$lateral_total[sc$scenario == "high"]          # Reithmaier/Ho-upper

# waterfall levels (cumulative top-down): each sink removes a slice from NECB_vert
lv <- tibble::tribble(
  ~step,                    ~drop, ~fill,
  "Net uptake\n(NECB vert)", NA,    "#238b45",
  "- Burial",                BUR,   "#762a83",
  "- Biomass gain",          BIO,   "#8073ac",
  "- Lateral export\n(conservative)", LATc, "#2166ac",
  "Residual\n(unaccounted)", NA,    "#e08214"
) %>%
  mutate(step = factor(step, levels = step),
         # floating waterfall bars: each deduction spans from the previous level
         # down to the new level; NECB and residual are grounded at 0.
         top = c(NECB, NECB,       NECB - BUR,       NECB - BUR - BIO,        RES),
         bot = c(0,    NECB - BUR, NECB - BUR - BIO, NECB - BUR - BIO - LATc,  0),
         lab = c(sprintf("%.0f", NECB), sprintf("-%.0f", BUR), sprintf("-%.0f", BIO),
                 sprintf("-%.0f", LATc), sprintf("%.0f", RES)),
         x = seq_along(step))

req_lat <- NECB - BUR - BIO            # lateral needed to fully close (== 960)

p <- ggplot(lv) +
  geom_hline(yintercept = 0, color = "grey55", linewidth = 0.3) +
  # connector lines between steps
  geom_segment(aes(x = x + 0.4, xend = x + 0.6, y = top, yend = top),
               data = lv[1:4, ], color = "grey60", linewidth = 0.3, linetype = "22") +
  geom_rect(aes(xmin = x - 0.4, xmax = x + 0.4, ymin = bot, ymax = top, fill = fill),
            color = "grey30", linewidth = 0.2) +
  geom_text(aes(x = x, y = pmax(top, bot) + 35, label = lab), size = 3.3, fontface = "bold") +
  # "required lateral to close" reference: conservative bar + residual would need to reach here
  geom_hline(yintercept = 0, color = "grey55") +
  annotate("segment", x = 3.6, xend = 5.4, y = req_lat, yend = req_lat,
           linetype = "dashed", color = "grey25", linewidth = 0.4) +
  annotate("text", x = 4.5, y = req_lat + 55, size = 2.9, color = "grey25",
           label = sprintf("lateral needed to close = %.0f  (conservative %.0f + residual %.0f)", req_lat, LATc, RES)) +
  annotate("text", x = 5, y = RES/2, size = 2.9, color = "#8a4b00", fontface = "italic",
           label = sprintf("= implied extra\nlateral export\n(high scenario %.0f\ncloses to ~0)", LAThi)) +
  scale_fill_identity() +
  scale_x_continuous(breaks = lv$x, labels = lv$step) +
  labs(x = NULL, y = expression("g C "*m^-2*" "*yr^-1),
       title = "Where the Healthy budget residual comes from",
       subtitle = "Net vertical uptake stepped down by independently-measured sinks; the residual is the lateral export the budget implies") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 8.5), plot.subtitle = element_text(color = "grey40", size = 9))

dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures/presentation", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/budget_residual_waterfall.pdf", p, width = 8.5, height = 5.5)
ggsave("output/figures/presentation/budget_residual_waterfall.png", p, width = 8.5, height = 5.5, dpi = 200)
cat("Written: budget_residual_waterfall.{pdf,png}\n")
