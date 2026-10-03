# =============================================================================
# PLACEHOLDER figure for the sediment metagenomes (Peccia lab; W. Chen).
# Lays out the intended panels on the real site x depth design, with no data:
#   (a) methanogen and methane-oxidiser families (relative abundance) by site and depth
#   (b) marker genes per g dry sediment (mcrA; mttB/mtbB/mtmB; pmoA/mmoX)
#   (c) DNA yield per g dry sediment and organic carbon (normalisation)
#   (d) mcrA abundance vs porewater CH4 and NH4
# Replace each panel with data when available.
# Output: output/figures/other/metagenome_placeholder.png
# =============================================================================
suppressMessages({library(ggplot2); library(patchwork)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
sites <- factor(c("SRS5", "SRS6", "BL60", "CP40"), levels = c("SRS5", "SRS6", "BL60", "CP40"))
depths <- factor(c("0", "15", "45", "90"), levels = c("0", "15", "45", "90"))
grid <- expand.grid(site = sites, depth = depths)
th <- theme_bw(base_size = 8) + theme(panel.grid = element_blank(), plot.tag = element_text(face = "bold"))
pend <- function(p, lab) p + annotate("text", x = Inf, y = Inf, label = lab, hjust = 1.05, vjust = 1.5, size = 2.6, colour = "grey45", fontface = "italic")
pa <- pend(ggplot(grid, aes(depth, 1)) + geom_col(fill = "grey90", colour = "grey70", linewidth = 0.2) + facet_grid(~ site) +
             labs(x = "Depth (cm)", y = "Relative abundance", tag = "a", subtitle = "Methanogen / methanotroph families") + th, "data pending")
pb <- pend(ggplot(grid, aes(depth, site)) + geom_tile(fill = "grey95", colour = "grey70", linewidth = 0.2) +
             labs(x = "Depth (cm)", y = NULL, tag = "b", subtitle = "Marker genes per g") + th, "data pending")
pc <- pend(ggplot(grid, aes(site, 0)) + geom_blank() + labs(x = NULL, y = "ng DNA per g dry sediment", tag = "c", subtitle = "DNA yield, organic C") + th + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()), "data pending")
pd <- pend(ggplot(grid, aes(1, 1)) + geom_blank() + labs(x = expression("Porewater CH"[4]*" or NH"[4]^"+"), y = "mcrA per g sediment", tag = "d", subtitle = "Genes vs porewater") + th + theme(axis.text = element_blank(), axis.ticks = element_blank()), "data pending")
fig <- pa / (pb | pc | pd) + plot_layout(heights = c(1, 1)) +
  plot_annotation(caption = "PLACEHOLDER: sediment metagenomes (Peccia lab). Panels to be filled when normalised results are available.")
ggsave("output/figures/other/metagenome_placeholder.png", fig, width = 7.2, height = 5, dpi = 300)
