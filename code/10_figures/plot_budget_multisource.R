# =============================================================================
# plot_budget_multisource.R
# -----------------------------------------------------------------------------
# Multi-source carbon budget: every independent estimate for each term shown as
# grouped bars (one bar per source), component terms (Reco, CH4) as stacked
# component bars, tower + CARAFE + chamber NEE side by side, all with CI.
# One facet per term (free scales — terms span >3 orders of magnitude).
#
# Reads:  output/upscaling/budget_sources_totals.csv, budget_sources_components.csv
# Sign:   + = C loss/source, - = C gain/uptake (per-term facets, self-contained).
# =============================================================================
suppressMessages({library(dplyr); library(ggplot2); library(tidyr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

tot <- read.csv("output/upscaling/budget_sources_totals.csv")
cmp <- read.csv("output/upscaling/budget_sources_components.csv")

term_lv <- c("GPP","NEE","Reco","CH4","Lateral DIC","Lateral TAlk","Lateral DOC","Lateral POC",
             "Lateral CH4aq","CO2 evasion","Burial","dBiomass","Litterfall","Root NPP","CWD prod.")
pal <- c(soil="#8B4513", root="#D2691E", stem="#228B22", water="#4682B4",
         cwd="#808080", leaf="#E6AB02", pneumatophore="#32CD32",
         `(total)`="#bdbdbd")

# stacked bars = component segments where we have them; else a single "(total)"
has_comp <- cmp %>% distinct(term, source, class)
single   <- tot %>% anti_join(has_comp, by = c("term","source","class")) %>%
  transmute(term, source, class, component = "(total)", value)
bars <- bind_rows(cmp, single) %>%
  mutate(term = factor(term, levels = term_lv),
         component = factor(component, levels = c(names(pal))))

# global source order: this-study first, then instruments/analogs, then literature
srank <- function(s) ifelse(grepl("this study", s, ignore.case = TRUE), 0,               # our chambers
                     ifelse(grepl("SKR|tower|Barr|Troxler|Eddy", s, ignore.case = TRUE), 1,  # tower / EC estimates
                     ifelse(grepl("CARAFE", s, ignore.case = TRUE), 2, 3)))              # top-down, then literature
lvls <- { a <- unique(c(bars$source, tot$source)); a[order(srank(a), a)] }
bars$source <- factor(bars$source, levels = lvls)
tot$source  <- factor(tot$source,  levels = lvls)

mk <- function(cls, ncol) {
  b <- bars %>% filter(class == cls) %>% mutate(term = droplevels(factor(term, levels = term_lv)))
  t <- tot  %>% filter(class == cls) %>% mutate(term = factor(term, levels = term_lv))
  ggplot(b, aes(source, value, fill = component)) +
    geom_hline(yintercept = 0, color = "grey55", linewidth = 0.3) +
    geom_col(width = 0.8, color = "grey30", linewidth = 0.2) +
    geom_errorbar(data = t, aes(source, ymin = lo, ymax = hi), inherit.aes = FALSE,
                  width = 0.25, linewidth = 0.35, color = "grey15", na.rm = TRUE) +
    facet_wrap(~ term, scales = "free", ncol = ncol) +
    scale_fill_manual(values = pal, name = "component", drop = TRUE) +
    labs(x = NULL, y = expression("g C "*m^-2*" "*yr^-1*"   [+ loss / - gain]"),
         title = paste0("Multi-source carbon budget - ", cls),
         subtitle = "one bar per estimate; Reco/CH4 stacked by component (leaf = literature canopy Rs); Litterfall/Root NPP/CWD = internal NPP allocation (NOT additive to NECB); * CARAFE dry-weighted, peak-summer forthcoming") +
    theme_bw(base_size = 11) +
    theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 7.5),
          panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"),
          plot.subtitle = element_text(color = "grey40", size = 8.5),
          legend.position = "bottom")
}

dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures/presentation", showWarnings = FALSE, recursive = TRUE)
pH <- mk("Healthy", ncol = 5)
pG <- mk("Ghost",   ncol = 4)
ggsave("output/figures/other/budget_multisource_healthy.pdf", pH, width = 13, height = 6.2)
ggsave("output/figures/presentation/budget_multisource_healthy.png", pH, width = 13, height = 6.2, dpi = 200)
ggsave("output/figures/other/budget_multisource_ghost.pdf", pG, width = 9, height = 3.6)
ggsave("output/figures/presentation/budget_multisource_ghost.png", pG, width = 9, height = 3.6, dpi = 200)
cat("Written: budget_multisource_{healthy,ghost}.{pdf,png}\n")
