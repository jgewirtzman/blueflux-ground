# =============================================================================
# plot_carbon_budget.R
# -----------------------------------------------------------------------------
# Full ecosystem carbon (CO2 + CH4) budget per disturbance class, in a single
# carbon currency (g C m-2 yr-1). Renders the measured skeleton now and fills in
# as literature lateral-export / burial / biomass terms are added upstream in
# code/08_upscaling/assemble_carbon_budget.R.
#
# Reads:  output/upscaling/carbon_budget_full.csv, carbon_budget_summary.csv
# Sign:   + up  = C LOST from ecosystem (respired / emitted / exported)
#         - down = C GAINED / retained  (photosynthesis, burial, biomass)
# Literature (unfilled) terms are drawn as dashed "TBD" placeholders.
# =============================================================================
suppressMessages({library(dplyr); library(ggplot2); library(tidyr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

budget <- read.csv("output/upscaling/carbon_budget_full.csv")
summ   <- read.csv("output/upscaling/carbon_budget_summary.csv")

term_lv <- c("GPP (uptake)", "Reco (CO2)", "CH4 emission",
             "Lateral DIC", "Lateral DOC", "Lateral POC", "Lateral CH4 (aq)",
             "Soil C burial", "dBiomass C")
pal <- c("GPP (uptake)"="#006837", "Reco (CO2)"="#8B4513", "CH4 emission"="#d6604d",
         "Lateral DIC"="#4682B4", "Lateral DOC"="#74add1", "Lateral POC"="#abd9e9",
         "Lateral CH4 (aq)"="#e08214", "Soil C burial"="#542788", "dBiomass C"="#8073ac")

d <- budget %>%
  mutate(class   = factor(class, levels = c("Ghost", "Healthy")),
         term    = factor(term, levels = term_lv),
         pending = as.logical(pending),
         plotval = ifelse(pending, 0, value))            # NA -> 0 for geom; flagged below

# Net vertical C exchange = algebraic sum of the plotted flux bars.
# On this atmosphere-referenced axis (+ = C loss), net < 0 is a sink.
# NECB is storage-referenced (+ = ecosystem gain), so net_flux == -NECB.
necb <- summ %>%
  transmute(class = factor(class, levels = c("Ghost", "Healthy")),
            NECB    = NECB_vertical_only,
            net_flux = -NECB_vertical_only,                # axis-consistent position
            tag      = ifelse(net_flux < 0, "sink", "source"),
            lateral_pending = as.logical(lateral_pending))

# placeholder label height: sit just above the zero line so the reader sees the slot
lab_y <- max(abs(d$plotval), na.rm = TRUE) * 0.06 + 1

p <- ggplot(d, aes(term, plotval, fill = term)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.4) +
  # measured terms: solid
  geom_col(data = ~subset(.x, !pending), width = 0.72, color = "grey25", linewidth = 0.2) +
  # literature terms not yet filled: dashed hollow slot + TBD tag
  geom_col(data = ~subset(.x,  pending), aes(y = 0), width = 0.72,
           fill = NA, color = "grey55", linetype = "22", linewidth = 0.4) +
  geom_text(data = ~subset(.x, pending), aes(y = lab_y, label = "lit:\nTBD"),
            size = 2.6, color = "grey45", lineheight = 0.85) +
  # net vertical C exchange (= sum of bars = -NECB) as a reference line
  geom_hline(data = necb, aes(yintercept = net_flux),
             linetype = "dashed", color = "grey20", linewidth = 0.4) +
  geom_text(data = necb, aes(x = length(term_lv) - 0.3, y = net_flux,
                             label = sprintf("Net vertical = %+.0f (%s)", net_flux, tag)),
            vjust = -0.5, hjust = 1, size = 2.8, color = "grey20", inherit.aes = FALSE) +
  facet_wrap(~class, nrow = 1) +
  scale_fill_manual(values = pal, guide = "none") +
  labs(x = NULL,
       y = expression("Carbon flux (g C "*m^-2*" "*yr^-1*")   [+ loss / - gain]"),
       title = "Ecosystem carbon budget (CO2 + CH4)",
       subtitle = "Solid = measured (this study) | dashed slot = literature term to be added | net line = -NECB") +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey40", size = 9))

dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures/presentation", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/carbon_budget.pdf",        p, width = 10, height = 5.2)
ggsave("output/figures/presentation/carbon_budget.png", p, width = 10, height = 5.2, dpi = 200)
cat("Written: output/figures/{other/carbon_budget.pdf, presentation/carbon_budget.png}\n")
