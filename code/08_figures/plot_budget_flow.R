# =============================================================================
# plot_budget_flow.R
# -----------------------------------------------------------------------------
# Box-and-flow (Sankey-style) diagram of the Healthy mangrove carbon budget.
# Pools = boxes (Atmosphere, Mangrove ecosystem, Coastal ocean, Long-term
# burial); fluxes = arrows with width proportional to magnitude (g C m-2 yr-1).
# Numbers read from the assembled budget so the diagram stays in sync.
#
# Reads: output/upscaling/carbon_budget_full.csv, carbon_budget_summary.csv
# =============================================================================
suppressMessages({library(dplyr); library(ggplot2)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

bud <- read.csv("output/upscaling/carbon_budget_full.csv") %>% filter(class == "Healthy")
sm  <- read.csv("output/upscaling/carbon_budget_summary.csv") %>% filter(class == "Healthy")
gv  <- function(tm) bud$value[bud$term == tm]
GPP <- abs(gv("GPP (uptake)")); RECO <- gv("Reco (CO2)"); CH4 <- gv("CH4 emission")
LAT <- sum(bud$value[bud$category == "lateral"], na.rm = TRUE)
BUR <- gv("Soil C burial"); DBIO <- gv("dBiomass C")
RESID <- sm$closure_resid
# internal NPP-allocation fluxes (from the multi-source table)
src    <- read.csv("output/upscaling/budget_sources_totals.csv")
sval   <- function(tm) src$value[src$term == tm & src$class == "Healthy"][1]
LITTER <- sval("Litterfall")                     # 389 (Castaneda 2013)
CWDP   <- sval("CWD prod.")                       # 150 (mortality x AGB, FCE anchor)

# --- boxes -------------------------------------------------------------------
box <- tibble::tribble(
  ~id,        ~xmin, ~xmax, ~ymin, ~ymax, ~fill,      ~label,
  "atm",       0.3,   9.7,   8.6,   9.8,  "#eaf2f8",  "ATMOSPHERE",
  "eco",       2.0,   6.2,   3.0,   6.6,  "#eafaf0",  "MANGROVE ECOSYSTEM\n(live biomass + soil)",
  "ocean",     7.4,   9.7,   3.3,   6.2,  "#eaf2f8",  "COASTAL OCEAN /\nESTUARY",
  "burial",    2.6,   5.6,   0.4,   1.7,  "#f4ecf7",  "LONG-TERM SOIL\nBURIAL"
)

# --- flows: x1,y1 -> x2,y2, value, colour ------------------------------------
# arrow width LINEARLY proportional to flux (GPP sets the scale). Tiny fluxes
# (CH4 ~1.6) render as a hairline by design — that is their true relative size.
WSCALE <- 11 / max(GPP, RECO)          # widest arrow ~= 11 units
lw <- function(v) pmax(0.15, abs(v) * WSCALE)
flow <- tibble::tribble(
  ~x1, ~y1, ~x2, ~y2, ~value, ~col,        ~lx,  ~ly,  ~label,
  3.0, 8.6, 3.0, 6.6, GPP,   "#1b7837",    2.35, 7.7,  sprintf("GPP\n%.0f", GPP),      # atm->eco (uptake)
  4.3, 6.6, 4.3, 8.6, RECO,  "#8B4513",    4.75, 7.7,  sprintf("Reco\n%.0f", RECO),    # eco->atm
  5.4, 6.6, 5.4, 8.6, CH4,   "#d6604d",    5.85, 7.7,  sprintf("CH4\n%.1f", CH4),      # eco->atm
  6.2, 5.2, 7.4, 5.2, LAT,   "#2166ac",    6.55, 5.6,  sprintf("Lateral export\n%.0f", LAT), # eco->ocean
  3.6, 3.0, 3.6, 1.7, BUR,   "#762a83",    2.9,  2.35, sprintf("Burial\n%.0f", BUR),   # eco->burial
  6.2, 4.1, 7.4, 4.1, RESID, "#e08214",    6.55, 4.45, sprintf("Unaccounted\nresidual %.0f", RESID), # eco->? (dashed)
  # INTERNAL NPP allocation: live biomass -> litter/CWD/soil detritus pool
  2.9, 5.6, 2.9, 4.35, LITTER, "#b8860b",  2.35, 5.0, sprintf("Litterfall\n%.0f", LITTER),
  4.7, 5.6, 4.7, 4.35, CWDP,   "#8B5A2B",  5.25, 5.0, sprintf("CWD\n%.0f", CWDP)
) %>% mutate(width = lw(value), dashed = grepl("residual", label, ignore.case = TRUE))

p <- ggplot() +
  geom_rect(data = box, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
            color = "grey45", linewidth = 0.4) +
  geom_text(data = box, aes(x = (xmin+xmax)/2, y = ymax - 0.32, label = label),
            fontface = "bold", size = 3.4, lineheight = 0.9, vjust = 1) +
  # internal detritus sub-pool + zone labels inside the ecosystem box
  geom_rect(aes(xmin = 2.25, xmax = 5.95, ymin = 3.15, ymax = 4.35), fill = "#f6edd8",
            color = "grey60", linewidth = 0.3, linetype = "22") +
  geom_text(aes(x = 4.1, y = 4.24, label = "litter + CWD + soil detritus  ->  Reco / burial / POC"),
            size = 2.7, color = "grey35", fontface = "italic", vjust = 1) +
  geom_text(aes(x = 5.1, y = 5.75, label = "live biomass"), size = 2.8, color = "grey35", fontface = "italic") +
  geom_text(aes(x = 5.1, y = 5.4, label = sprintf("gain +%.0f", DBIO)), size = 2.8, color = "#1b7837") +
  # flux arrows (solid) + residual (dashed)
  geom_segment(data = flow %>% filter(!dashed),
               aes(x = x1, y = y1, xend = x2, yend = y2, linewidth = width, color = col),
               arrow = arrow(length = unit(0.32, "cm"), type = "closed"), lineend = "butt") +
  geom_segment(data = flow %>% filter(dashed),
               aes(x = x1, y = y1, xend = x2, yend = y2, color = col),
               linewidth = 1, linetype = "22",
               arrow = arrow(length = unit(0.25, "cm"), type = "open")) +
  geom_label(data = flow, aes(x = lx, y = ly, label = label, color = col),
             size = 3, label.size = 0, fill = alpha("white", 0.75), lineheight = 0.85, fontface = "bold") +
  scale_linewidth_identity() + scale_color_identity() + scale_fill_identity() +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10), expand = FALSE) +
  labs(title = "Healthy mangrove carbon flow (g C m-2 yr-1)",
       subtitle = sprintf("arrow width ~ flux; gold = internal NPP allocation (not additive to NECB); dashed = unaccounted residual %+.0f (~ additional lateral export)", RESID)) +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), plot.subtitle = element_text(color = "grey35", size = 9),
        plot.margin = margin(8, 8, 8, 8))

dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures/presentation", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/budget_flow_healthy.pdf", p, width = 9.5, height = 6.5)
ggsave("output/figures/presentation/budget_flow_healthy.png", p, width = 9.5, height = 6.5, dpi = 200)
cat("Written: budget_flow_healthy.{pdf,png}\n")
