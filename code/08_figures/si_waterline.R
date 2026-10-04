# =============================================================================
# Fig. S11 | Water line against woody surfaces, by site: a data-scaled cross-section
# of the lowest 1.7 m at SRS5, SRS6 (intact, tidal), CP40 and FLM30 (ghost, ponded).
# Vertical axis to scale (cm above the plot's mean floor). Every height is from
# data and matches the exposure model (si_exposure.R, code/00_lib/exposure.R):
#   floor         a smooth profile whose heights follow the floor distribution:
#                 tidal sites, fitted normal (mean 0, SD floor_sd_cm, M11);
#                 ghost sites, campaign mean depth minus each recorded depth
#   trees         number per 7 m = 7 x sqrt(TLS stem density); stems stand on the
#                 mean floor (tidal; as the exposure model) or at the mean floor
#                 height of the stem chambers (ghost); diameters from TLS (4V/SA,
#                 lowest 0.5 m)
#   prop roots    TLS heights are above the scan-time water surface (w_scan), so
#                 roots are shifted up by w_scan; apex heights are fitted so that the
#                 drawn root length below 0.5 and 1.0 m matches the TLS root surface
#   water         by campaign (wet = Oct 2022, dry = Mar 2023); tidal: mean high /
#                 low water of the month (local extremes within +/- 5 h of hourly
#                 level); ghost: mean recorded depth (the model's CWD depth).
#                 Light fill: covered at the highest line; dark: at the lowest
#   downed wood   median measured diameter, lying on the mean floor (as the model)
#   exposure      shares above water from si_exposure_values.csv (campaign mean)
# Horizontal positions, root reach and pneumatophores (no data) are illustrative.
# Writes output/figures/other/si_waterline.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(patchwork)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))

sites <- c(SRS5 = "intact", SRS6 = "intact", CP40 = "ghost", FLM30 = "ghost")
ff <- read.csv("output/upscaling/flood_fraction.csv") %>% filter(campaign %in% c("Oct 2022", "Mar 2023"))
# ---- tidal levels per site
wl <- read.csv("data/environmental/water_level/FCE_LTER_1168_water_levels.csv") %>%
  filter(SITENAME %in% c("SRS5", "SRS6"), WaterLevel > -9000) %>%
  left_join(ff %>% distinct(site, floor_mu_cm, floor_sd_cm), by = c(SITENAME = "site")) %>%
  mutate(rel = WaterLevel + floor_mu_cm, ym = substr(Date, 1, 7), t = as.POSIXct(paste(Date, Time), tz = "Etc/GMT+5")) %>%
  arrange(SITENAME, t)
tide <- wl %>% filter(ym %in% c("2022-10", "2023-03")) %>% group_by(SITENAME, ym) %>% group_modify(function(d, ...) {
  y <- d$rel; n <- length(y); w <- 5
  pk <- sapply(seq_len(n), function(i) { r <- y[max(1, i - w):min(n, i + w)]; c(y[i] == max(r), y[i] == min(r)) })
  data.frame(mhw = mean(y[pk[1, ]]), mlw = mean(y[pk[2, ]]), x99 = unname(quantile(y, 0.99)), x01 = unname(quantile(y, 0.01)))
}) %>% ungroup() %>% transmute(site = SITENAME, camp = ifelse(ym == "2022-10", "wet", "dry"), mhw, mlw, x99, x01)
# long-term context (2001-2024): annual maximum water height above the mean floor
lt <- wl %>% mutate(y = substr(Date, 1, 4)) %>% group_by(site = SITENAME, y) %>%
  summarise(mx = max(rel), n = n(), .groups = "drop") %>% filter(n > 4000) %>%
  group_by(site) %>% summarise(ann_max = median(mx), yrs80 = sum(mx > 80), nyr = n(), .groups = "drop")
# ---- ghost depths per site
fx <- read.csv("output/data_products/combined_gas_flux_dataset.csv") %>%
  filter(plot %in% c("CP40", "FLM30"), is.finite(water_depth)) %>%
  mutate(camp = ifelse(year == 2022 & month == 10, "wet", ifelse(year == 2023 & month == 3, "dry", NA))) %>% filter(!is.na(camp))
# water level = campaign mean recorded depth (the CWD exposure input); each reading's
# floor height = that level minus its depth, so the mean floor is 0 by construction
fx <- fx %>% group_by(plot, camp) %>% mutate(lev = mean(water_depth), floor_h = lev - water_depth) %>% ungroup()
gd <- fx %>% group_by(site = plot) %>%
  summarise(wet = lev[camp == "wet"][1], dry = lev[camp == "dry"][1],
            stem_floor = mean(floor_h[component == "stem"]), wet_max = max(water_depth[camp == "wet"]), .groups = "drop")
gfloor <- split(fx$floor_h, fx$plot)
# ---- TLS prop-root height profile and amount
tls <- read.csv("data/tls/all_sites_summary.csv"); tst <- read.csv("data/tls/tree_stats_per_site.csv")
rt <- tls %>% filter(segment_class == "root", Total_surface_area_m2 > 0) %>% mutate(lo = height_bin_num) %>%
  group_by(site) %>% arrange(lo, .by_group = TRUE) %>% mutate(cum = cumsum(Total_surface_area_m2) / sum(Total_surface_area_m2)) %>%
  summarise(crown = { i <- which(cum >= 0.98)[1]; prev <- c(0, cum)[i]; 100 * (lo[i] + 0.5 * (0.98 - prev) / (cum[i] - prev)) },
            root_sa = sum(Total_surface_area_m2), .groups = "drop") %>%
  left_join(tst %>% transmute(site, ground = non_tree_m2), by = "site") %>% mutate(root_ratio = root_sa / ground)
rprof <- tls %>% filter(segment_class == "root") %>% group_by(site) %>%
  summarise(f50 = sum(Total_surface_area_m2[height_bin_num < 0.5]) / sum(Total_surface_area_m2),
            f100 = sum(Total_surface_area_m2[height_bin_num < 1]) / sum(Total_surface_area_m2), .groups = "drop")
wscan <- setNames(read.csv("output/upscaling/tls_datum_offset.csv")$w_scan_cm, read.csv("output/upscaling/tls_datum_offset.csv")$site)
ntree <- setNames(round(7 * sqrt(tst$tree_density_ha / 1e4)), tst$site)
dia <- tls %>% filter(segment_class %in% c("trunk", "root"), height_bin_num < 0.5) %>%
  mutate(d = 4 * Total_volume_m3 / Total_surface_area_m2 * 100) %>% select(site, segment_class, d) %>%
  pivot_wider(names_from = segment_class, values_from = d)   # mean cylinder diameter (cm) of the lowest 0.5 m
print(dia)
ex <- read.csv("output/figures/other/si_exposure_values.csv") %>% group_by(site) %>%
  summarise(across(c(stem_bin0, root, cwd), mean), .groups = "drop")
ffl <- ff %>% group_by(site) %>% summarise(flood = mean(frac_flooded), .groups = "drop")
raw <- read.csv("output/data_products/combined_gas_flux_dataset.csv") %>% filter(plot %in% names(sites), month_year %in% c("2022-10", "2023-03"))
log_d <- median(raw$diameter[raw$component == "cwd"], na.rm = TRUE)       # as cwd_exposure_setup()
print(tide); print(lt); print(gd); print(rt)

# ---- drawing helpers
XMAX <- 7                                                  # schematic width (m)
xs <- seq(0, XMAX, by = 0.01)
mm_per_m <- 78 / XMAX                                      # approx. panel width 78 mm at 7.2 in, two columns
lw_of <- function(d_cm) d_cm / 100 * mm_per_m / 0.753     # ggplot linewidth for a true-to-scale diameter
# smooth deterministic undulation, rank-mapped onto the site's floor-height distribution
undul <- sin(2 * pi * xs / 3.3) + 0.6 * sin(2 * pi * xs / 1.7 + 1.1) + 0.25 * sin(2 * pi * xs / 1.1 + 2.3)
shape_floor <- function(qf) qf((rank(undul) - 0.5) / length(undul))
# pin the floor to height h0 within +/- 0.35 m of x0 (cosine blend), for stems and the log
# flat at h0 within +/- flat of x0, blending back to the profile over a further 0.5 m
pin <- function(fl, x0, h0, flat = 0.15, w = 0.5) { d <- abs(xs - x0); k <- d < flat + w
  b <- ifelse(d[k] < flat, 1, (1 + cos(pi * (d[k] - flat) / w)) / 2); fl[k] <- b * h0 + (1 - b) * fl[k]; fl }
smooth_keep_sd <- function(z, n = 61) { m <- mean(z); s <- sd(z)
  zz <- stats::filter(c(rev(z[1:n]), z, rev(tail(z, n))), rep(1 / n, n), sides = 2)[n + seq_along(z)]
  m + (zz - mean(zz)) / sd(zz) * s }
bez <- function(p0, p1, p2, n = 40) { s <- seq(0, 1, length.out = n)
  data.frame(x = (1 - s)^2 * p0[1] + 2 * (1 - s) * s * p1[1] + s^2 * p2[1],
             y = (1 - s)^2 * p0[2] + 2 * (1 - s) * s * p1[2] + s^2 * p2[2]) }
root_arc <- function(t, sd, h, sw, base, fy = function(x) 0) { reach <- sd * (0.2 + 0.5 * h / 100)
  bez(c(t + sd * sw / 2, base + h), c(t + reach * 0.8, base + h + 4), c(t + reach, fy(t + reach) - 3), n = 60) }
# apex heights h_k = crown * (0.3 + 0.7 ((k - 1) / (n - 1))^a) (tallest at the crown); fit a so that the drawn root
# length below 0.5 and 1.0 m (above the TLS ground) matches the TLS root surface profile
fit_apex <- function(n, crown, f50, f100) {
  hk <- function(a) crown * (0.3 + 0.7 * ((seq_len(n) - 1) / max(1, n - 1))^a)
  share <- function(a) { h <- hk(a)
    seg <- bind_rows(lapply(h, function(hh) { d <- root_arc(0, 1, hh, 0, 0) %>% filter(y >= 0)
      data.frame(y = (head(d$y, -1) + tail(d$y, -1)) / 2, len = sqrt(diff(d$x * 100)^2 + diff(d$y)^2)) }))
    c(sum(seg$len[seg$y < 50]), sum(seg$len[seg$y < 100])) / sum(seg$len) }
  a <- optimize(function(a) sum((share(a) - c(f50, f100))^2), c(0.2, 5))$minimum
  list(h = hk(a), share = share(a)) }

scene <- function(s) {
  cls <- sites[[s]]; intact <- cls == "intact"
  r <- rt[rt$site == s, ]; dd <- dia[dia$site == s, ]; ws <- wscan[[s]]
  nt <- ntree[[s]]; trees <- (seq_len(nt) - 0.5) * XMAX / nt                # evenly spaced, density from TLS
  blk_x <- if (s == "SRS6") trees[nt] else NA                              # SRS6: a live black mangrove (27 of 88 stem chambers)
  sap_x <- c(FLM30 = mean(trees))[s]
  base <- if (intact) 0 else gd$stem_floor[gd$site == s]                    # stem foot height
  fl0 <- shape_floor(if (intact) function(p) qnorm(p, 0, ff$floor_sd_cm[ff$site == s][1])
                     else function(p) quantile(gfloor[[s]], p, type = 8, names = FALSE))
  floor <- smooth_keep_sd(fl0)
  if (!is.na(sap_x)) sap_x <- trees[1] + 0.3 * diff(trees[1:2])               # sapling, then the log, between the two stems
  occ <- sort(c(trees, if (!is.na(sap_x)) sap_x)); gaps <- diff(occ); g <- which.max(gaps)
  lx <- mean(occ[g:(g + 1)])                                                    # log in the widest gap between stems
  for (tx in c(trees, if (!is.na(sap_x)) sap_x)) floor <- pin(floor, tx, base)
  floor <- pin(floor, lx, 0, flat = 0.55)                                        # log on the mean floor (as the model)
  fy <- function(x) approx(xs, floor, x, rule = 2)$y
  fl <- data.frame(x = xs, y = floor)
  # water lines by campaign: wet = Oct 2022, dry = Mar 2023
  wcol <- c(wet = "#1F5F95", dry = "#5FA3D4")
  lines <- if (intact) { tt <- tide[tide$site == s, ]
    bind_rows(lapply(c("wet", "dry"), function(cp) data.frame(camp = cp, kind = c("high", "low"),
      y = c(tt$mhw[tt$camp == cp], tt$mlw[tt$camp == cp])))) %>%
      mutate(lab = sub("[+]0$", "0", sprintf("%s %s %+.0f", camp, kind, y)))
  } else { g2 <- gd[gd$site == s, ]
    data.frame(camp = c("wet", "dry"), kind = "level", y = c(g2$wet, g2$dry)) %>% mutate(lab = sprintf("%s %+.0f", camp, y)) }
  hi <- max(lines$y); lo <- min(lines$y)                                     # fills: ever covered / always covered
  if (intact) hi <- max(lines$y[lines$kind == "high"])
  crown <- r$crown
  ybot <- -45; ytop <- 170
  wood <- if (intact) "#7B5B3E" else "#A39E96"; rootc <- "#A9592A"
  sw <- dd$trunk / 100                                                     # stem diameter (m), to scale
  red <- if (intact) setdiff(trees, blk_x) else numeric(0)
  stems <- bind_rows(lapply(setdiff(trees, blk_x), function(t)
    data.frame(t = t, x = c(t - sw * 0.75, t - sw / 2, t + sw / 2, t + sw * 0.75), y = c(base - 4, ytop, ytop, base - 4))))
  nside <- if (intact) max(1, round(1 + 3 * r$root_ratio / max(rt$root_ratio))) else 0
  rp <- rprof[rprof$site == s, ]
  apex <- if (intact) fit_apex(2 * nside, crown, rp$f50, rp$f100) else NULL
  if (intact) message(sprintf("%s root apex (cm above TLS ground): %s; drawn share <50/<100 cm %.2f/%.2f vs TLS %.2f/%.2f",
                              s, paste(round(apex$h), collapse = " "), apex$share[1], apex$share[2], rp$f50, rp$f100))
  roots <- if (intact) bind_rows(lapply(seq_along(red), function(j) { t <- red[j]
    hh <- apex$h[c(seq(1, 2 * nside, 2), seq(2, 2 * nside, 2))]               # alternate sides
    bind_rows(lapply(seq_along(hh), function(i) { sd <- if (i <= nside) -1 else 1
      root_arc(t, sd, hh[i], sw, base + ws, fy) %>% mutate(g = paste(j, i)) }))
  })) else data.frame(x = numeric(0), y = numeric(0), g = character(0))
  # pneumatophores around black mangroves (no data: 1 cm pegs, 15 cm, evenly spaced)
  pn_at <- if (!intact) trees else if (!is.na(blk_x)) blk_x else numeric(0)
  pn <- if (length(pn_at)) bind_rows(lapply(pn_at, function(t) { px <- t + c(seq(-1.3, -0.2, length.out = 8), seq(0.2, 1.3, length.out = 8))
    data.frame(x = px, y0 = fy(px), y1 = fy(px) + 15) })) else NULL
  root_lw <- lw_of(dd$root)
  sap <- if (!is.na(sap_x)) { f0 <- base
    list(stem = data.frame(x = sap_x + c(-0.025, -0.02, 0.02, 0.025), y = c(f0 - 3, f0 + 72, f0 + 72, f0 - 3)),
         roots = bind_rows(lapply(c(-1, 1), function(sd) bind_rows(lapply(1:2, function(i) {
           h <- f0 + 40 * (0.5 + 0.5 * i / 2); reach <- sd * (0.15 + 0.15 * i)
           bez(c(sap_x + sd * 0.02, h), c(sap_x + reach * 0.8, h + 2), c(sap_x + reach, fy(sap_x + reach) - 2), n = 30) %>% mutate(g = paste(sd, i))
         })))))
  } else NULL
  half <- 0.5; lb <- 0
  logp <- data.frame(xmin = lx - half, xmax = lx + half, ymin = lb, ymax = lb + log_d)
  endc <- data.frame(t = seq(0, 2 * pi, length.out = 60)) %>% mutate(x = lx + half + log_d / 400 * cos(t), y = lb + log_d / 2 + log_d / 2 * sin(t))
  rings <- crossing(r = c(0.6, 0.3), t = seq(0, 2 * pi, length.out = 40)) %>%
    mutate(x = lx + half + log_d / 400 * r * cos(t), y = lb + log_d / 2 + log_d / 2 * r * sin(t))
  # check: drawn log exposure (arc above the drawn water) vs the model
  arc <- function(h) { rr <- log_d / 2; if (h <= 0) 1 else if (h >= 2 * rr) 0 else acos((h - rr) / rr) / pi }
  message(sprintf("%s log: drawn above water at high/low %.2f/%.2f; floor below high/low %.2f/%.2f",
                  s, arc(hi), arc(lo), mean(floor < hi), mean(floor < lo)))
  e <- ex[ex$site == s, ]
  # all annotation outside the drawing: water levels as short tags in the right margin;
  # dimensions and exposure shares in a two-line subtitle
  wl_tags <- lines %>% arrange(desc(y)) %>% mutate(yl = y)
  for (i in seq_len(nrow(wl_tags))[-1]) wl_tags$yl[i] <- min(wl_tags$yl[i], wl_tags$yl[i - 1] - 7.5)   # keep tags apart,
  wl_tags$yl <- wl_tags$yl + mean(wl_tags$y - wl_tags$yl)                                              # centred on the lines
  sub <- if (intact) { l <- lt[lt$site == s, ]
    sprintf("above water: lowest 0.5 m of stem %.0f%%, prop roots %.0f%%, log %.0f%%\nfloor flooded %.0f%% of hours; annual max. %.0f cm",
            100 * e$stem_bin0, 100 * e$root, 100 * e$cwd, 100 * ffl$flood[ffl$site == s], l$ann_max) } else
    sprintf("above water: lowest 0.5 m of stem %.0f%%, log %.0f%%\nponded; deepest wet-season reading %.0f cm",
            100 * e$stem_bin0, 100 * e$cwd, gd$wet_max[gd$site == s])
  inside <- function(d) if (is.null(d)) d else d[d$x >= 0 & d$x <= XMAX, ]
  pn <- inside(pn); roots <- inside(roots)
  wfill <- function(lev) fl %>% mutate(top = pmax(y, lev))
  ggplot() +
    geom_path(data = roots, aes(x, y, group = g), linewidth = root_lw, colour = rootc, lineend = "round") +
    { if (!is.null(pn)) geom_segment(data = pn, aes(x = x, xend = x, y = y0, yend = y1), linewidth = lw_of(1), colour = "#8F8A82") } +
    geom_polygon(data = stems, aes(x, y, group = t), fill = wood, colour = "grey25", linewidth = 0.2) +
    { if (!is.na(blk_x)) { bw <- 0.21 / 2; f0 <- base
        geom_polygon(data = data.frame(x = blk_x + c(-bw * 0.7, -bw / 2, bw / 2, bw * 0.7), y = c(f0 - 4, ytop, ytop, f0 - 4)),
                     aes(x, y), fill = "#6F6A5E", colour = "grey25", linewidth = 0.2) } } +
    { if (!is.null(sap)) list(
        geom_path(data = sap$roots, aes(x, y, group = g), linewidth = lw_of(1.5), colour = "#A9592A", lineend = "round"),
        geom_polygon(data = sap$stem, aes(x, y), fill = "#7B5B3E", colour = "grey25", linewidth = 0.15)) } +
    geom_ribbon(data = fl, aes(x, ymin = ybot, ymax = y), fill = "#D7C2A6") +            # floor drawn over the buried parts
    geom_rect(data = logp, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#8E7F70", colour = "grey25", linewidth = 0.2) +
    geom_polygon(data = endc, aes(x, y), fill = "#C9B79C", colour = "grey25", linewidth = 0.2) +
    geom_path(data = rings, aes(x, y, group = r), colour = "#8E7F70", linewidth = 0.15) +
    geom_ribbon(data = wfill(hi), aes(x, ymin = y, ymax = top), fill = pal_comp[["water"]], alpha = 0.25) +
    geom_ribbon(data = wfill(lo), aes(x, ymin = y, ymax = top), fill = pal_comp[["water"]], alpha = 0.4) +
    geom_line(data = fl, aes(x, y), colour = "#7A5C3E", linewidth = 0.35) +
    geom_segment(data = lines, aes(x = 0, xend = XMAX, y = y, yend = y, colour = camp, linetype = kind), linewidth = 0.45) +
    geom_text(data = wl_tags, aes(x = XMAX + 0.08, y = yl, label = lab, colour = camp), hjust = 0, size = 1.9) +
    scale_colour_manual(values = wcol, guide = "none") +
    scale_linetype_manual(values = c(high = "22", low = "solid", level = "solid"), guide = "none") +
    scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(breaks = seq(-25, 175, 25), expand = c(0, 0)) +
    coord_cartesian(xlim = c(0, XMAX), ylim = c(ybot, ytop), clip = "off") +
    labs(x = NULL, y = "cm above mean floor", title = sprintf("%s (%s)", s, cls), subtitle = sub) +
    theme_fig() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(),
                        panel.grid = element_blank(), plot.margin = margin(2, 30, 2, 2),
                        plot.title = element_text(face = "bold", size = 8.5, colour = pal_class[[cls]], margin = margin(0, 0, 1, 0)),
                        plot.subtitle = element_text(size = 5.8, colour = "grey30", lineheight = 1, margin = margin(0, 0, 3, 0)))
}
fig <- wrap_plots(lapply(names(sites), scene), ncol = 2) + plot_annotation(tag_levels = "a")
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/si_waterline.png", fig, width = 7.2, height = 6.4, dpi = 300, bg = "white")
ggsave("output/figures/other/si_waterline.pdf", fig, width = 7.2, height = 6.4, device = cairo_pdf)
