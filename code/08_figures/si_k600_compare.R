# =============================================================================
# Fig. S12 | Our chamber-implied k600 against published Shark River / Everglades
# values and common parameterisations evaluated for our conditions.
#   ours        median of site medians of the chamber-implied k600 (diamond; bar, range
#               of site medians; ticks, the site medians; water_k_by_site.csv), the
#               value used for the SRS5 / SRS6 Oct 2022 water fluxes (dashed line)
#   measured    Raymond & Cole 2001 Table 1 (rivers, estuaries > 1 m deep: 1-26;
#               recommended 3-7); Shark River channel, SF6 / 3He: Ho et al. 2014 (8.3, 8.1; include
#               freshwater flushing), Ho et al. 2016 (3.3; revised 3.5, 4.2);
#               Everglades emergent wetland (sawgrass, ~0.5 m): Ho et al. 2018
#               (1.1-3.2; sheltered limnocorrals 0.52-0.61), Variano et al. 2009
#               (0.3-1.4), Happell et al. 1995 (0.77 +/- 0.55, floating dome)
#   equations   at forest-floor water (no current, h = 0.1 m) for u10 from the
#               transect wind on sampling days to the US-Skr above-canopy daytime
#               mean (1.2-2.4 m s-1; point at 1.8):
#               Ho et al. 2016:          0.77 v^0.5 h^-0.5 + 0.266 u10^2  (v = 0)
#               Rosentreter et al. 2017: CH4, -1.07 + 0.36 v + 0.99 u + 0.87 h
#               Wanninkhof 2014:         0.251 u^2 (Sc/660)^-0.5, at Sc 600
#               Cole & Caraco 1998:      2.07 + 0.215 u^1.7
#               Raymond & Cole 2001 (Fig. 2, verified from the paper): all data
#                 1.91 exp(0.35 u); floating-dome studies 2.06 exp(0.37 u);
#                 tracer (non-dome) studies 1.58 exp(0.30 u)
#               Borges et al. 2004:      1.0 + 1.719 w^0.5 h^-0.5 + 2.58 u  (w = 0)
#   Raymond et al. 2012 stream forms need channel slope x velocity and are not
#   applicable to standing forest-floor water (not shown).
# Writes output/figures/other/si_k600_compare.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(ggplot2)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))

pclass <- c(SRS5 = "intact", SRS6 = "intact", BL60 = "regenerating", CP40 = "ghost", FLM30 = "ghost", SE1 = "scrub")
pcol <- c(pal_class, scrub = "#9A8C7A", literature = "grey25", equation = "#4A6FA5")
cal <- read.csv("output/flux/03_fit/water_k_calibration.csv")
ksite <- read.csv("output/flux/03_fit/water_k_by_site.csv")
sc_co2 <- function(T_C) 1923.6 - 125.06 * T_C + 4.3773 * T_C^2 - 0.085681 * T_C^3 + 0.00070284 * T_C^4
two_pos <- cal %>% count(plot, campaign) %>% filter(n > 1) %>% mutate(two = TRUE)
ours <- cal %>% left_join(two_pos %>% select(plot, campaign, two), by = c("plot", "campaign")) %>%
  mutate(k600_co2 = F_co2_chamber / (dC_uM * 1e3) * 3600 * 100 * (sc_co2(T_C) / 600)^0.5,
         lab = sprintf("%s %s%s", plot, sub(" 20", " ’", campaign),
                       ifelse(!is.na(two), ifelse(grepl("channel", chamber_position), ", channel", ", floor"), "")),
         cls = pclass[plot])
k_used <- ksite$k600[ksite$level == "all"]
u <- c(lo = 1.2, mid = 1.8, hi = 2.4); h <- 0.1
eqs <- list(
  "Ho et al. 2016 (Shark River; no current)" = function(u) 0.266 * u^2,
  "Rosentreter et al. 2017 (mangrove creeks, CH₄)" = function(u) -1.07 + 0.99 * u + 0.87 * h,
  "Wanninkhof 2014 (wind)" = function(u) 0.251 * u^2 * (600 / 660)^-0.5,
  "Cole & Caraco 1998 (low-wind lake)" = function(u) 2.07 + 0.215 * u^1.7,
  "Raymond & Cole 2001, all data" = function(u) 1.91 * exp(0.35 * u),
  "Raymond & Cole 2001, floating domes" = function(u) 2.06 * exp(0.37 * u),
  "Raymond & Cole 2001, tracers" = function(u) 1.58 * exp(0.30 * u),
  "Borges et al. 2004 (estuary; no current)" = function(u) 1.0 + 2.58 * u)
eq <- bind_rows(lapply(names(eqs), function(n) data.frame(lab = n, k = eqs[[n]](u["mid"]), lo = eqs[[n]](u["lo"]), hi = eqs[[n]](u["hi"]))))
lit <- data.frame(
  lab = c("Shark River channel, SF₆ (Ho et al. 2014)", "Shark River channel, ³He/SF₆ (Ho et al. 2016)",
          "Everglades sawgrass wetland (Ho et al. 2018)", "  sheltered, convection only (Ho et al. 2018)",
          "Everglades wetland (Variano et al. 2009)", "Everglades, floating dome CH₄ (Happell et al. 1995)",
          "Rivers and estuaries > 1 m deep (Raymond & Cole 2001)", "  recommended for estuaries (Raymond & Cole 2001)"),
  k = c(8.2, 3.3, 2.3, 0.56, NA, 0.77, NA, NA), lo = c(8.1, 2.8, 1.1, 0.52, 0.3, 0.22, 1.0, 3), hi = c(8.3, 4.2, 3.2, 0.61, 1.4, 1.32, 26, 7))
ks_site <- ksite %>% filter(level == "site")
ours <- data.frame(lab = "This study: median of site medians", k = k_used, lo = min(ks_site$k600), hi = max(ks_site$k600), cls = "this study")
site_pts <- ks_site %>% arrange(k600) %>% transmute(lab = ours$lab, k = k600, site = name, vj = rep(c(-1.1, 2.1), length.out = n()))
ord <- c(rev(eq$lab), rev(lit$lab), ours$lab)
dd <- bind_rows(ours %>% mutate(group = "This study"),
                lit %>% mutate(cls = "literature", group = "Measured (literature)"),
                eq %>% mutate(cls = "equation", group = "Equations, forest-floor water (u₁₀ 1.2–2.4 m s⁻¹)")) %>%
  mutate(lab = factor(lab, rev(ord)), group = factor(group, unique(group)))
site_pts <- site_pts %>% mutate(lab = factor(lab, levels(dd$lab)), group = factor("This study", levels(dd$group)))
p <- ggplot(dd, aes(y = lab)) +
  geom_vline(xintercept = k_used, colour = pal_class[["intact"]], linewidth = 0.4, linetype = "dashed") +
  geom_errorbar(aes(xmin = lo, xmax = hi, colour = cls), width = 0.3, linewidth = 0.4, orientation = "y", na.rm = TRUE) +
  geom_point(data = site_pts, aes(x = k), shape = 124, size = 2.5, colour = "grey35") +
  geom_text(data = site_pts, aes(x = k, label = site, vjust = vj), size = 1.7, colour = "grey35") +
  geom_point(aes(x = k, colour = cls, shape = cls), size = 2.2, na.rm = TRUE) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  scale_shape_manual(values = c("this study" = 18, literature = 16, equation = 16), guide = "none") +
  scale_colour_manual(values = c("this study" = pal_class[["intact"]], literature = "grey25", equation = "#4A6FA5"), guide = "none") +
  scale_x_log10(breaks = c(0.3, 1, 3, 10, 30), labels = c("0.3", "1", "3", "10", "30")) +
  labs(x = expression(k[600] ~ "(cm h"^-1 * ")"), y = NULL) + theme_fig() +
  theme(strip.text.y = element_text(angle = 0, hjust = 0, size = 6.5), panel.grid.minor = element_blank())
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/si_k600_compare.png", p, width = 7.2, height = 4.6, dpi = 300, bg = "white")
ggsave("output/figures/other/si_k600_compare.pdf", p, width = 7.2, height = 4.6, device = cairo_pdf)
print(eq %>% mutate(across(where(is.numeric), ~ round(.x, 2))))
