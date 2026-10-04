# =============================================================================
# Fig. 2 | Flux rates: what each surface emits.
#   (a) CH4 and (b) CO2 per m2 of surface by component (rows) and forest class
#       (side by side, class colours, one shared axis): individual measurements
#       and bootstrapped means with 95% CIs (5,000 resamples); groups with
#       fewer than 4 closures show points only (no mean); n at the right. Soil collars
#       include pneumatophores in the footprint. Core sites (intact SRS5, SRS6;
#       regenerating BL60; ghost CP40, FLM30), all campaigns, as in the stand
#       budgets; context sites (RB10, MI, SE1) are in Extended Data 3.
#   (c) Surface per m2 of ground for the same component rows (intact, ghost;
#       regenerating not laser-scanned): water surface and exposed soil from the
#       flooding model, prop roots and stems (trunk + branch) from laser
#       scanning, downed wood (above-water part) and leaf area from the
#       literature (lighter). Rate x area along each row = the stand term.
#   (d) CH4 and (e) CO2 per m2 of woody surface (stems + prop roots) by height
#       above the water, all classes on one axis, with fitted profiles
#       (06_analysis/03_woody_height_model.R; live stem, flooded position, wet
#       season; typical rather than mean flux).
# Main-text layout (fig2_rates): row 1, rates (a, b); row 2, laser-scanned
# structure (c, point-cloud renders: PLACEHOLDER), woody flux by height (d, e) and
# woody surface by height (f); row 3, surface per m2 ground (g, pies) and the share
# of stand CH4 (h) and respiration (i) by component and campaign
# (output/figures/other/fig_component_shares.rds, saved by fig3_stands.R).
# Writes output/figures/other/fig2_rates.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(ggplot2); library(patchwork)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
set.seed(42)

comp_rows <- c(water = "water", soil = "soil", pneumatophore = "soil", root = "prop root", stem = "stem",
               cwd = "downed wood", leaves = "leaf")
d <- read.csv("output/data_products/combined_gas_flux_dataset.csv") %>%
  filter(disturbance_level %in% c("healthy", "regenerating", "ghost"),
         is.na(CO2_best.flux) | CO2_best.flux >= -10) %>%
  mutate(class = factor(class_labels[disturbance_level], names(pal_class)),
         comp = factor(comp_rows[component], rev(unique(comp_rows))),
         site_type = factor(ifelse(plot %in% c("SRS5", "SRS6", "BL60", "CP40", "FLM30"), "core site", "context site"),
                            c("core site", "context site")))
boot <- function(x, R = 5000) { x <- x[is.finite(x)]; if (length(x) < 3) return(c(mean(x), NA, NA))
  set.seed(42); b <- replicate(R, mean(sample(x, replace = TRUE))); c(mean(x), quantile(b, c(0.025, 0.975))) }
dodge <- c(intact = 0.25, regenerating = 0, ghost = -0.25)

rate_panel <- function(dd, gas, status, breaks, xlab, show_y = TRUE) {
  x <- dd %>% filter(.data[[status]] == "valid", !is.na(comp)) %>% mutate(v = .data[[paste0(gas, "_best.flux")]])
  s <- x %>% group_by(comp, class) %>% summarise(n = n(), m = boot(v)[1], lo = boot(v)[2], hi = boot(v)[3], .groups = "drop") %>%
    mutate(y = as.numeric(comp) + dodge[as.character(class)])
  x <- x %>% left_join(s %>% select(comp, class, n), by = c("comp", "class")) %>%
    mutate(y = as.numeric(comp) + dodge[as.character(class)] + runif(n(), -0.06, 0.06), small = n < 4)
  ci <- s %>% filter(n >= 4)   # bootstrap intervals only for n > 3; means shown for all groups
  bands <- data.frame(y = seq_along(levels(dd$comp))) %>% filter(y %% 2 == 1)
  ggplot() +
    geom_rect(data = bands, aes(ymin = y - 0.5, ymax = y + 0.5), xmin = -Inf, xmax = Inf, fill = "grey95") +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
    geom_point(data = x, aes(asinh(v), y, colour = class, shape = site_type), size = 0.75, alpha = 0.35, stroke = 0.35) +
    geom_errorbar(data = ci, aes(xmin = asinh(lo), xmax = asinh(hi), y = y, colour = class), width = 0, linewidth = 0.6, orientation = "y") +
    geom_point(data = s, aes(asinh(m), y, fill = class), shape = 21, colour = "white", size = 2.1, stroke = 0.5) +
    scale_y_continuous(breaks = seq_along(levels(dd$comp)), labels = if (show_y) levels(dd$comp) else NULL,
                       expand = c(0, 0), limits = c(0.5, length(levels(dd$comp)) + 0.5)) +
    scale_x_continuous(breaks = asinh(breaks), labels = breaks, expand = expansion(mult = c(0.03, 0.09))) +
    scale_colour_manual(values = pal_class, name = NULL, guide = "none") + scale_fill_manual(values = pal_class, breaks = names(pal_class), name = "forest class") +
    scale_shape_manual(values = c(`core site` = 16, `context site` = 4), guide = "none") +
    labs(x = xlab, y = NULL) + theme_fig() + theme(panel.grid.major.y = element_blank(), axis.ticks.y = element_blank())
}
w <- d %>% filter(site_type == "core site", component %in% c("stem", "root"), month_year %in% c("2022-10", "2023-03"), !is.na(height_corrected)) %>%
  mutate(surface = factor(ifelse(component == "root", "prop root", "stem"), c("stem", "prop root")),
         h = pmax(height_corrected, 0) / 100)
zoom_fill <- "#EEF1F4"; zoom_line <- "grey35"; zoom_top <- 1.9; f_xmax <- 0.17; zoom_dx <- 0.042
prof_panel <- function(gas, curves, breaks, xlab) {
  cur <- read.csv(curves) %>% mutate(class = factor(class, names(pal_class)))
  ww <- w %>% mutate(v = .data[[paste0(gas, "_best.flux")]]) %>% filter(!is.na(v))
  ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.05, fill = col_waterline) +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
    geom_point(data = ww, aes(asinh(v), h, colour = class, shape = surface), size = 0.9, alpha = 0.35, stroke = 0.4) +
    geom_ribbon(data = cur, aes(y = h / 100, xmin = fit - 1.96 * se, xmax = fit + 1.96 * se, fill = class),
                alpha = 0.18, orientation = "y") +
    geom_path(data = cur, aes(fit, h / 100, colour = class), linewidth = 1) +
    scale_x_continuous(breaks = asinh(breaks), labels = breaks) +
    scale_y_continuous(limits = c(0, zoom_top), expand = c(0, 0)) +
    scale_colour_manual(values = pal_class, guide = "none") + scale_fill_manual(values = pal_class, guide = "none") +
    scale_shape_manual(values = c(stem = 16, `prop root` = 2), name = "surface") +
    labs(x = xlab, y = "Height above water (m)") + theme_fig() +
    theme(panel.background = element_rect(fill = zoom_fill, colour = NA), panel.border = element_rect(fill = NA, colour = zoom_line, linewidth = 0.6)) +
    guides(shape = guide_legend(override.aes = list(size = 1.8, alpha = 1)))
}
pc <- prof_panel("CH4", "output/analysis/woody_height_model_curves.csv", c(0, 1, 10, 100, 1000),
                 expression("Woody CH"[4]*" (nmol m"^-2*" s"^-1*")"))
pd <- prof_panel("CO2", "output/analysis/woody_height_model_CO2_curves.csv", c(-10, -1, 0, 1, 10),
                 expression("Woody CO"[2]*" ("*mu*"mol m"^-2*" s"^-1*")")) + labs(y = NULL)

# ---- surface per ground area, by component (last panel) -------------------------------------------
site_class <- c(SRS5 = "intact", SRS6 = "intact", CP40 = "ghost", FLM30 = "ghost")
tw <- read.csv("output/upscaling/plot_level_CH4_totals.csv") %>% filter(scenario == "exponential") %>%
  distinct(site, campaign, tide_state, tide_weight)
bd <- read.csv("output/upscaling/budget_decomposition.csv") %>% filter(component %in% c("water", "soil", "cwd")) %>%
  left_join(tw, by = c("site", "campaign", "tide_state")) %>% mutate(sa = surface_area_m2 / area_m2) %>%
  group_by(site, campaign, component) %>% summarise(sa = weighted.mean(sa, tide_weight), .groups = "drop") %>%
  group_by(site, component) %>% summarise(sa = mean(sa), .groups = "drop")
tsz <- read.csv("data/tls/tree_stats_per_site.csv")
tls_seg <- read.csv("data/tls/all_sites_summary.csv") %>% left_join(tsz %>% select(site, area_m2), by = "site") %>%
  group_by(site, component = segment_class) %>% summarise(sa = sum(Total_surface_area_m2) / first(area_m2), .groups = "drop")
tls <- read.csv("data/tls/all_sites_summary.csv") %>% left_join(tsz %>% select(site, area_m2), by = "site") %>%
  mutate(component = ifelse(segment_class == "root", "root", "stem")) %>%
  group_by(site, component) %>% summarise(sa = sum(Total_surface_area_m2) / first(area_m2), .groups = "drop")
area <- bind_rows(bd, tls, data.frame(site = names(site_class), component = "leaves", sa = ifelse(site_class == "intact", 2.8, 0))) %>%
  mutate(class = factor(site_class[site], names(pal_class))) %>% group_by(class, component) %>% summarise(sa = mean(sa), .groups = "drop") %>%
  mutate(comp = factor(comp_rows[component], levels(d$comp)), lit = component %in% c("cwd", "leaves"),
         y = as.numeric(comp) + dodge[as.character(class)])
area_panel <- function() {
  bands <- data.frame(y = seq_along(levels(d$comp))) %>% filter(y %% 2 == 1)
  ggplot(area) +
    geom_rect(data = bands, aes(ymin = y - 0.5, ymax = y + 0.5), xmin = -Inf, xmax = Inf, fill = "grey95") +
    geom_segment(aes(x = 0, xend = sa, y = y, yend = y, colour = class, alpha = lit), linewidth = 2.2) +
    scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.4), guide = "none") +
    scale_colour_manual(values = pal_class, guide = "none") +
    scale_y_continuous(breaks = seq_along(levels(d$comp)), labels = NULL, expand = c(0, 0), limits = c(0.5, length(levels(d$comp)) + 0.5)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(x = expression("Surface per ground area (m"^2*" m"^-2*")"), y = NULL) +
    theme_fig() + theme(panel.grid.major.y = element_blank(), axis.ticks.y = element_blank())
}

# stacked alternative: total surface per ground area by class, components stacked
area_stack_panel <- function() {
  st <- area %>% filter(!is.na(class)) %>% mutate(comp = factor(as.character(comp), rev(levels(d$comp))))
  ggplot(st, aes(sa, class, fill = comp)) +
    geom_col(width = 0.6, colour = "white", linewidth = 0.25, position = position_stack(reverse = TRUE)) +
    scale_fill_manual(values = setNames(pal_comp, c("water", "soil", "prop root", "stem", "downed wood", "leaf")), name = "component") +
    scale_y_discrete(limits = rev(c("intact", "ghost"))) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(x = expression("Surface per ground area (m"^2*" m"^-2*")"), y = NULL) +
    theme_fig() + theme(panel.grid.major.y = element_blank(), axis.ticks.y = element_blank(),
                        axis.text.y = element_text(colour = pal_class[c("ghost", "intact")], face = "bold"))
}
# pie alternative: one pie per class, area proportional to total surface per ground area
col_branch <- "#B59A6A"
area_pie_panel <- function() {
  lv <- c("water", "soil", "prop root", "stem", "downed wood", "leaf")
  pd <- area %>% filter(!is.na(class)) %>% mutate(comp = factor(as.character(comp), lv)) %>% arrange(class, comp) %>%
    group_by(class) %>% mutate(tot = sum(sa), end = 2 * pi * cumsum(sa) / tot, start = lag(end, default = 0),
                               r = sqrt(tot / max(area %>% group_by(class) %>% summarise(t = sum(sa)) %>% pull(t))),
                               x0 = ifelse(class == "intact", 0, 1.9)) %>% ungroup()
  lab <- pd %>% distinct(class, x0, r, tot)
  ggplot(pd) +
    ggforce::geom_arc_bar(aes(x0 = x0, y0 = 0, r0 = 0, r = r, start = start, end = end, fill = comp), colour = "white", linewidth = 0.3) +
    geom_text(data = lab, aes(x0, -1.12, label = sprintf("%s\n%.1f m\u00b2 m\u207b\u00b2", class, tot), colour = class),
              size = 2.5, fontface = "bold", lineheight = 0.9, vjust = 1) +
    scale_fill_manual(values = setNames(pal_comp, lv), labels = ifelse(lv == "stem", "stem + branch", lv), breaks = lv, name = "component") +
    scale_colour_manual(values = pal_class, guide = "none") +
    coord_fixed(xlim = c(-1.05, 2.5), ylim = c(-1.6, 1.05)) + theme_void() +
    theme(plot.tag = element_text(face = "bold", size = 11), legend.key.size = unit(8, "pt"),
          legend.text = element_text(size = 7), legend.title = element_text(size = 7, face = "bold"))
}
# woody surface by height (TLS, 0.5 m bins), intact vs ghost; measured chamber zone shaded
hb <- read.csv("data/tls/all_sites_summary.csv") %>% left_join(tsz %>% select(site, area_m2), by = "site") %>%
  mutate(class = factor(site_class[site], names(pal_class)),
         part = factor(c(root = "prop root", trunk = "stem", branch = "branch")[segment_class], c("prop root", "stem", "branch")),
         h = height_bin_num) %>%
  group_by(class, site, part, h) %>% summarise(sa = sum(Total_surface_area_m2) / first(area_m2), .groups = "drop") %>%
  group_by(class, part, h) %>% summarise(sa = mean(sa), .groups = "drop")
height_panel <- function(right = FALSE) {
  # grey band = chamber height range shown enlarged in the woody-flux panels; connector lines lead to them
  con <- if (right) data.frame(class = factor("intact", levels(hb$class)), x = 0, y = c(0, zoom_top),
                               xend = -zoom_dx * 1.9, yend = c(0, 18.5)) else
    data.frame(class = factor("ghost", levels(hb$class)), x = f_xmax, y = c(0, zoom_top),
               xend = f_xmax + zoom_dx, yend = c(0, 18.5))
  ggplot(hb, aes(sa, h + 0.25, fill = part)) +
    annotate("rect", xmin = 0, xmax = f_xmax, ymin = 0, ymax = zoom_top, fill = zoom_fill, colour = NA) +
    geom_col(orientation = "y", width = 0.45, position = position_stack(reverse = TRUE), colour = NA) +
    geom_segment(data = con, aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE,
                 colour = zoom_line, linewidth = 0.45) +
    annotate("rect", xmin = 0, xmax = f_xmax, ymin = 0, ymax = zoom_top, fill = NA, colour = zoom_line, linewidth = 0.6) +
    geom_text(data = data.frame(class = factor("ghost", levels(hb$class))), aes(x = f_xmax * 0.97, y = zoom_top + 0.25),
              label = "chamber range", hjust = 1, vjust = 0, size = 2, colour = zoom_line, inherit.aes = FALSE) +
    facet_grid(~ class) +
    scale_fill_manual(values = c(`prop root` = pal_comp[["prop root"]], stem = pal_comp[["stem"]], branch = col_branch), name = "woody surface") +
    scale_y_continuous(breaks = seq(0, 20, 2), expand = c(0, 0)) +
    scale_x_continuous(breaks = c(0, 0.1)) +
    coord_cartesian(xlim = c(0, f_xmax), ylim = c(0, 18.5), expand = FALSE, clip = "off") +
    labs(x = expression("Woody surface (m"^2*" m"^-2*" per 0.5 m)"), y = "Height (m)") +
    theme_fig() + theme(strip.text = element_text(hjust = 0.5), panel.grid.major.y = element_blank())
}

build <- function(dd, file, note, stacked = FALSE, pie = FALSE, d_right = FALSE) {
  lt <- theme(legend.title = element_text(size = 7, face = "bold"), legend.text = element_text(size = 7),
              legend.key.size = unit(8, "pt"))
  horiz <- function(p, title = TRUE) cowplot::get_plot_component(p + lt + theme(legend.position = "bottom", legend.direction = "horizontal",
                                                                  legend.title.position = "left") +
                                                      (if (!title) theme(legend.title = element_blank()) else theme()),
                                                    "guide-box-bottom", return_all = TRUE)
  pa <- rate_panel(dd, "CH4", "CH4_flux_status", c(0, 1, 10, 100, 1000), expression("CH"[4]*" (nmol m"^-2*" s"^-1*")")) +
    guides(fill = guide_legend(override.aes = list(size = 2.5), nrow = 1))
  pb <- rate_panel(dd, "CO2", "CO2_flux_status", c(-10, -1, 0, 1, 10), expression("CO"[2]*" ("*mu*"mol m"^-2*" s"^-1*")"), show_y = FALSE)
  pc2 <- pc + guides(shape = guide_legend(override.aes = list(size = 1.8, alpha = 1), nrow = 1))
  pf <- height_panel(d_right)
  leg_class <- horiz(pa); leg_surf <- horiz(pc2, FALSE); leg_wood <- horiz(pf, FALSE)
  nl <- theme(legend.position = "none")
  pcc <- if (pie) area_pie_panel() else if (stacked) area_stack_panel() else area_panel()
  pcc <- pcc + lt
  if (pie) {
    pcc <- pcc + guides(fill = guide_legend(ncol = 2, byrow = FALSE)) +
      coord_fixed(xlim = c(-1.05, 2.5), ylim = c(-1.35, 1.05), clip = "off")
  } else pcc <- pcc + theme(legend.position = "right")
  # component legend sits inside panel c, under the pies
  leg_comp <- grid::nullGrob()
  if (pie) pcc <- pcc + coord_fixed(xlim = c(-1.05, 2.5), ylim = c(-2.25, 1.05), clip = "off") +
    theme(legend.position = "inside", legend.position.inside = c(0.5, -0.16), legend.justification = c(0.5, 0),
          legend.direction = "horizontal", legend.title = element_blank())
  if (!d_right) {
    design <- "
    ABC
    GGH
    FDE
    TSS
  "
    # plots are matched to design letters in alphabetical order: A B C D E F G H S T
    p_ch4 <- pc2 + nl + labs(tag = "e", y = NULL) + scale_y_continuous(limits = c(0, zoom_top), expand = c(0, 0), position = "right") +
      theme(axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank())
    p_co2 <- pd + nl + labs(tag = "f") + scale_y_continuous(limits = c(0, zoom_top), expand = c(0, 0), position = "left", name = NULL) +
      theme(axis.text.y = element_text())
    p_tls <- pf + nl + labs(tag = "d")
  } else {
    # three rows: rates | structure + woody flux by height | surfaces + component shares
    p_ch4 <- pc2 + nl + labs(tag = "d", x = expression("CH"[4]*" (nmol m"^-2*" s"^-1*")")) +
      scale_x_continuous(breaks = asinh(c(0, 10, 1000)), labels = c(0, 10, 1000))
    p_co2 <- pd + nl + labs(tag = "e", x = expression("CO"[2]*" ("*mu*"mol m"^-2*" s"^-1*")")) +
      scale_x_continuous(breaks = asinh(c(-1, 0, 10)), labels = c(-1, 0, 10))
    p_tls <- pf + nl + labs(tag = "f") + scale_y_continuous(breaks = seq(0, 20, 2), expand = c(0, 0), position = "right") +
      theme(plot.margin = margin(5.5, 5.5, 5.5, 22))
    # (c) terrestrial laser scans, intact vs ghost (tls_render.py)
    img <- png::readPNG("output/figures/other/tls_render_panel.png")
    # fill the whole design cell (no axis titles or legend to leave room for), aspect preserved
    tls_ph <- wrap_elements(full = grid::rasterGrob(img, interpolate = TRUE)) + theme(plot.margin = margin(16, 0, 0, 10))
    sh <- readRDS("output/figures/other/fig_component_shares.rds")
    design <- "
    AAABBB
    LLLLLL
    TTDEFF
    ##NNOO
    CCGGHH
  "
    # design areas take plots in alphabetical order of their letters: A B C D E F G H L N O T
    fig <- (pa + nl + labs(tag = "a")) + (pb + nl + labs(tag = "b")) + (pcc + labs(tag = "g")) +
      p_ch4 + p_co2 + p_tls + (sh$ch4 + labs(tag = "h")) + (sh$resp + labs(tag = "i")) +
      wrap_elements(full = leg_class) + wrap_elements(full = leg_surf) + wrap_elements(full = leg_wood) + (tls_ph + labs(tag = "c")) +
      plot_layout(design = design, heights = unit(c(1, 0.2, 1, 0.2, 1), c("null", "in", "null", "in", "null"))) +
      plot_annotation(caption = note, theme = theme(plot.caption = element_text(size = 6.5, colour = "grey40", hjust = 0),
                                                     plot.margin = margin(2, 2, 2, 2))) &
      theme(plot.tag = element_text(face = "bold", size = 11), plot.margin = margin(2, 3, 2, 3))
    ggsave(paste0(file, ".png"), fig, width = 7.2, height = 7.6, dpi = 300, bg = "white")
    ggsave(paste0(file, ".pdf"), fig, width = 7.2, height = 7.6, device = cairo_pdf)
    return(invisible(NULL))
  }
  fig <- (pa + nl + labs(tag = "a")) + (pb + nl + labs(tag = "b")) + (pcc + labs(tag = "c")) +
    p_ch4 + p_co2 + p_tls +
    wrap_elements(full = leg_class) + wrap_elements(full = leg_comp) + wrap_elements(full = leg_surf) + wrap_elements(full = leg_wood) +
    plot_layout(design = design, widths = c(1, 1, 1), heights = unit(c(1, 0.32, 1, 0.22), c("null", "in", "null", "in"))) +
    plot_annotation(caption = note, theme = theme(plot.caption = element_text(size = 6.5, colour = "grey40", hjust = 0)))
  ggsave(paste0(file, ".png"), fig, width = 7.2, height = 6.8, dpi = 300, bg = "white")
  ggsave(paste0(file, ".pdf"), fig, width = 7.2, height = 6.8, device = cairo_pdf)
}
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
build(d %>% filter(site_type == "core site"), "output/figures/other/fig2_rates", "", pie = TRUE, d_right = TRUE)
build(d %>% filter(site_type == "core site"), "output/figures/other/fig2_rates_dleft", "", pie = TRUE)
build(d %>% filter(site_type == "core site"), "output/figures/other/fig2_rates_rows", "")
build(d %>% filter(site_type == "core site"), "output/figures/other/fig2_rates_stacked", "", stacked = TRUE)
build(d, "output/figures/other/fig2_rates_with_context", pie = TRUE, d_right = TRUE, note =
      "a, b: includes context sites (Rookery Bay with intact, Marco Island with ghost; crosses); SE-1 (scrub) not shown.")
