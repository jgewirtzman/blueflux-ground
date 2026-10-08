# =============================================================================
# Carbon budget schematic: cross-sections of intact and ghost forest with the
# annual carbon flows drawn as arrows (g C m-2 yr-1; arrow width proportional to
# the square root of the flux, same scale in both panels).
#   Measured here (solid): GPP (tower), CO2 from leaves, stems + branches, prop
#     roots, soil, water and downed wood (chambers x scanned surface), CH4.
#   Literature (translucent, dashed outline; intact only): litterfall, root
#     production, wood mortality, wood increment, lateral export (DIC, DOC, POC,
#     dissolved CH4), burial; the closure residual is shown the same way.
# Sourced by fig_carbon_budget.R, which defines the flux values (co2w, ch4w, gppw,
# lit, sumr, LITTER, MORT, ROOTP) and their uncertainties (SE, GPP_SE, RNG,
# LIT_R, MORT_R, RES_R).
# Provides `schematic(k)` returning a ggplot for class k ("intact" or "ghost"),
# and `schematic_key()`.
# =============================================================================
col_gpp <- "#1E6B4E"; col_co2a <- "#8C9096"; col_ch4a <- "#A23B72"; col_tr <- "#B08A3E"
col_latf <- "#2C7BB6"; col_bur <- "#6B4226"; col_res <- "#9C8E70"
sky <- "#EEF4F9"; sed <- "#CDB59A"; sed_deep <- "#9C7E61"; water_f <- "#9CC3E0"
canopy <- c("#2F7A55", "#3E8E64", "#276B4A"); bark <- "#6B5440"; snag <- "#A8A39B"

GROUND <- 2.4; TOP <- 7.35
wfun <- function(v) 0.05 + 0.55 * sqrt(pmax(v, 0) / 3000)   # arrow shaft width (data units)
fmtv <- function(v) ifelse(abs(v) >= 10, format(round(v), big.mark = ","), formatC(v, format = "f", digits = 1))
pm <- function(v, se) paste0(fmtv(v), " \u00b1 ", fmtv(se))                       # measured: mean +/- 1 SE
rg <- function(v, lo, hi) if (is.na(lo)) fmtv(v) else paste0(fmtv(v), " (", fmtv(lo), "\u2013", fmtv(hi), ")")   # literature range

bez <- function(p0, p1, p2, n = 50) {
  t <- seq(0, 1, length.out = n)
  cbind(x = (1 - t)^2 * p0[1] + 2 * (1 - t) * t * p1[1] + t^2 * p2[1],
        y = (1 - t)^2 * p0[2] + 2 * (1 - t) * t * p1[2] + t^2 * p2[2])
}
# filled arrow polygon along a path; head scales with shaft but has a floor
arrow_poly <- function(path, w) {
  hl <- max(1.3 * w, 0.2); hw <- max(1.9 * w, 0.24)
  d <- c(0, cumsum(sqrt(diff(path[, 1])^2 + diff(path[, 2])^2))); L <- tail(d, 1)
  keep <- d < L - hl
  b <- c(approx(d, path[, 1], L - hl)$y, approx(d, path[, 2], L - hl)$y)
  sh <- rbind(path[keep, , drop = FALSE], b)
  tx <- c(diff(sh[, 1]), diff(sh[, 1])[nrow(sh) - 1]); ty <- c(diff(sh[, 2]), diff(sh[, 2])[nrow(sh) - 1])
  nl <- sqrt(tx^2 + ty^2); nx <- -ty / nl; ny <- tx / nl
  tip <- path[nrow(path), ]; u <- (tip - b) / sqrt(sum((tip - b)^2)); nn <- c(-u[2], u[1])
  o <- rbind(cbind(sh[, 1] + nx * w / 2, sh[, 2] + ny * w / 2),
             b + nn * hw / 2, tip, b - nn * hw / 2,
             cbind(rev(sh[, 1] - nx * w / 2), rev(sh[, 2] - ny * w / 2)))
  data.frame(x = unname(o[, 1]), y = unname(o[, 2]))
}
circle <- function(x, y, rx, ry = rx, n = 60) { t <- seq(0, 2 * pi, length.out = n); cbind(x = x + rx * cos(t), y = y + ry * sin(t)) }

schematic <- function(k) {
  ghost <- k == "ghost"
  r <- co2w[co2w$class == k, ]; m <- ch4w[ch4w$class == k, ]; G <- gppw$gpp[gppw$class == k]
  M <- m$stem + m$root + m$soil + m$water + m$cwd
  e <- function(cc) SE[[k]][[cc]]
  shp <- list(); ln <- list(); arr <- list(); lab <- list()
  add_shape <- function(xy, fill, col = NA, alpha = 1, lw = 0.2) shp[[length(shp) + 1]] <<- data.frame(xy, fill = fill, col = col, alpha = alpha, lw = lw)
  add_line <- function(xy, col, lw) ln[[length(ln) + 1]] <<- data.frame(xy, col = col, lw = lw)
  add_arrow <- function(path, v, col, lit = FALSE) arr[[length(arr) + 1]] <<- data.frame(arrow_poly(path, wfun(v)), col = col, lit = lit)
  add_lab <- function(x, y, txt, col, hj = 0.5, size = 2.1, face = "bold") lab[[length(lab) + 1]] <<- data.frame(x = x, y = y, txt = txt, col = col, hj = hj, size = size, face = face)

  # ---- landscape: sky, sediment with deep layer, creek on the right
  add_shape(cbind(x = c(0, 10, 10, 0), y = c(GROUND, GROUND, 9.35, 9.35)), sky)
  bank <- bez(c(7.7, GROUND), c(8.2, 1.25), c(8.9, 1.3), 30)
  ground_poly <- rbind(c(0, GROUND), bank, c(10, 1.3), c(10, 0.75), c(0, 0.75))
  add_shape(ground_poly, sed)
  add_shape(cbind(x = c(0, 10, 10, 0), y = c(0, 0, 0.75, 0.75)), sed_deep)
  wl <- if (ghost) 2.85 else 2.2                                           # water level: ponded (ghost) vs low tide
  creek <- rbind(bank[bank[, 2] <= wl, ], c(10, 1.3), c(10, wl))
  if (ghost) creek <- rbind(c(0, GROUND), bank, c(10, 1.3), c(10, wl), c(0, wl))
  add_shape(creek, water_f, alpha = 0.85)
  add_line(cbind(x = c(if (ghost) 0 else min(bank[bank[, 2] <= wl, 1]), 10), y = wl), "#5E9CC9", 0.35)

  # ---- vegetation
  trees <- data.frame(x = c(0.95, 4.7, 7.0), h = c(3.15, 3.35, 2.75), s = c(1, 1.08, 0.85))
  for (i in seq_len(nrow(trees))) {
    x0 <- trees$x[i]; h <- trees$h[i]; s <- trees$s[i]; topy <- GROUND + h
    if (!ghost) {
      for (j in c(-1, -0.55, -0.15, 0.2, 0.6, 1)) {                         # prop roots
        add_line(bez(c(x0 + 0.04 * j, GROUND + 0.75 * s), c(x0 + 0.55 * j * s, GROUND + 0.75 * s), c(x0 + 0.62 * j * s, GROUND)), bark, 0.55)
      }
      add_shape(cbind(x = x0 + c(-0.08, -0.05, 0.05, 0.08) * s, y = c(GROUND + 0.7 * s, topy - 0.2, topy - 0.2, GROUND + 0.7 * s)), bark)
      blobs <- data.frame(dx = c(-0.55, 0, 0.55, -0.3, 0.3), dy = c(-0.05, 0.25, -0.05, -0.25, -0.25), r = c(0.42, 0.5, 0.42, 0.4, 0.4))
      for (b in seq_len(nrow(blobs))) add_shape(circle(x0 + blobs$dx[b] * s, topy + blobs$dy[b] * s - 0.05, blobs$r[b] * s * 1.05), canopy[3])
      for (b in seq_len(nrow(blobs))) add_shape(circle(x0 + blobs$dx[b] * s, topy + blobs$dy[b] * s, blobs$r[b] * s), canopy[(b %% 2) + 1])
    } else {
      hh <- h * c(0.85, 1, 0.6)[i]; topy <- GROUND + hh
      for (j in c(-1, -0.4, 0.3, 0.9)) add_line(bez(c(x0 + 0.04 * j, GROUND + 0.65 * s), c(x0 + 0.5 * j * s, GROUND + 0.65 * s), c(x0 + 0.55 * j * s, GROUND)), snag, 0.5)
      add_shape(cbind(x = x0 + c(-0.08, -0.04, 0.03, 0.08) * s, y = c(GROUND + 0.6 * s, topy, topy - 0.12, GROUND + 0.6 * s)), snag)
      if (i != 3) {
        add_line(cbind(x = x0 + c(0, 0.45) * s, y = GROUND + hh * c(0.72, 0.92)), snag, 0.9)
        add_line(cbind(x = x0 + c(0, -0.38) * s, y = GROUND + hh * c(0.6, 0.78)), snag, 0.8)
      }
    }
  }
  # downed wood (one log intact, two ghost)
  logs <- if (ghost) data.frame(x = c(2.55, 5.95), y = GROUND + 0.1, len = c(0.9, 1.0), ang = c(-6, 8)) else data.frame(x = 5.95, y = GROUND + 0.09, len = 0.75, ang = 5)
  for (i in seq_len(nrow(logs))) {
    t <- seq(0, 2 * pi, length.out = 60); a <- logs$ang[i] * pi / 180
    ex <- logs$len[i] / 2 * cos(t); ey <- 0.09 * sin(t)
    add_shape(cbind(x = logs$x[i] + ex * cos(a) - ey * sin(a), y = logs$y[i] + ex * sin(a) + ey * cos(a)), if (ghost) snag else "#8B7355")
  }
  # pneumatophores (intact) for texture
  if (!ghost) for (x in seq(0.3, 7.3, by = 0.32)) add_line(cbind(x = x + c(0, 0), y = GROUND + c(0, 0.09)), bark, 0.35)

  # ---- flows to/from the atmosphere (measured). Labels sit on two staggered
  # tiers so neighbouring arrows can be close; tier-2 arrows run up to their label.
  T2 <- TOP + 1.0
  up <- function(x0, y0, x1, v, col, txt, tier = 1, cx = NULL) {
    yt <- if (tier == 1) TOP else T2
    p <- if (is.null(cx)) bez(c(x0, y0), c((x0 + x1) / 2, (y0 + yt) / 2), c(x1, yt)) else bez(c(x0, y0), c(cx, y0 + 0.3), c(x1, yt))
    add_arrow(p, v, col); add_lab(x1, yt + 0.08, txt, col)
  }
  wx <- 8.2; cx4 <- 9.3                                                   # water CO2 and CH4 columns
  if (!ghost) {
    add_arrow(bez(c(4.25, T2 + 0.05), c(4.27, 7.3), c(4.3, 6.55)), G, col_gpp); add_lab(4.25, T2 + 0.13, sprintf("GPP\n%s", pm(G, GPP_SE)), col_gpp)
    up(1.0, 3.9, 1.95, r$stem, col_co2a, sprintf("stems +\nbranches\n%s", pm(r$stem, e("stem"))), 1, cx = 1.95)
    up(1.5, GROUND + 0.4, 2.75, r$root, col_co2a, sprintf("prop roots\n%s", pm(r$root, e("root"))), 2, cx = 2.75)
    up(3.45, GROUND, 3.45, r$soil, col_co2a, sprintf("soil\n%s", pm(r$soil, e("soil"))), 1)
    up(5.15, 6.45, 5.25, r$leaf, col_co2a, sprintf("leaves\n%s", pm(r$leaf, e("leaf"))), 1)
    up(5.95, GROUND + 0.2, 5.95, r$cwd, col_co2a, sprintf("downed wood\n%s", pm(r$cwd, e("cwd"))), 2)
  } else {
    add_lab(1.2, TOP + 0.08, "no GPP", "grey45", face = "italic")
    up(4.78, 4.6, 4.3, r$stem, col_co2a, sprintf("dead stems +\nbranches\n%s", pm(r$stem, e("stem"))), 2, cx = 4.4)
    up(1.45, GROUND + 0.4, 2.75, r$root, col_co2a, sprintf("prop roots\n%s", pm(r$root, e("root"))), 2, cx = 2.75)
    up(3.45, GROUND, 3.45, r$soil, col_co2a, sprintf("soil\n%s", pm(r$soil, e("soil"))), 1)
    up(5.95, GROUND + 0.22, 5.95, r$cwd, col_co2a, sprintf("downed wood\n%s", pm(r$cwd, e("cwd"))), 2)
  }
  up(wx, wl, wx, r$water, col_co2a, sprintf("water\n%s", pm(r$water, e("water"))), 1)
  up(cx4, wl, cx4, M, col_ch4a, sprintf("CH4\n%s", pm(M, e("ch4"))), 1)
  add_lab((wx + cx4) / 2, 4.6, sprintf("CH4 from\n%d%% water\n%d%% soil\n%d%% roots", round(100 * m$water / M), round(100 * m$soil / M), round(100 * m$root / M)),
          col_ch4a, size = 1.65, face = "plain")

  # ---- internal transfers, lateral export and burial (literature)
  if (!ghost) {
    add_lab(4.7, 5.45, sprintf("wood\n+%s\n(%s–%s)", fmtv(lit[["dBiomass C"]]), fmtv(RNG["dBiomass C", 1]), fmtv(RNG["dBiomass C", 2])), "white", size = 1.75)
    add_arrow(bez(c(4.15, 4.95), c(3.95, 3.7), c(4.05, GROUND + 0.02)), LITTER, col_tr, TRUE)
    add_lab(4.85, 3.38, sprintf("litterfall\n%s\n(%s–%s)", fmtv(LITTER), fmtv(LIT_R[1]), fmtv(LIT_R[2])), col_tr, hj = 0, size = 1.75)
    add_arrow(bez(c(6.95, 3.75), c(6.45, 3.3), c(6.35, GROUND + 0.24)), MORT, col_tr, TRUE)
    add_lab(7.12, 3.22, sprintf("mortality\n%s\n(%s–%s)", fmtv(MORT), fmtv(MORT_R[1]), fmtv(MORT_R[2])), col_tr, hj = 0, size = 1.75)
    add_arrow(bez(c(0.95, GROUND - 0.05), c(0.95, 1.95), c(0.95, 1.5)), ROOTP, col_tr, TRUE)
    add_lab(1.3, 1.25, sprintf("root\nproduction\n%s", fmtv(ROOTP)), col_tr, hj = 0, size = 1.75)
    add_arrow(bez(c(0.95, 1.3), c(0.95, 0.8), c(0.95, 0.3)), lit[["Soil C burial"]], col_bur, TRUE)
    add_lab(1.3, 0.22, sprintf("burial\n%s (%s–%s)", fmtv(lit[["Soil C burial"]]), fmtv(RNG["Soil C burial", 1]), fmtv(RNG["Soil C burial", 2])), "white", hj = 0, size = 1.75)
    add_arrow(bez(c(2.9, 1.9), c(6.5, 1.95), c(10, 1.9)), sumr$flux_lateral, col_latf, TRUE)
    add_lab(2.9, 2.1, sprintf("lateral export %s (%s–%s): DIC %s, DOC %s, POC %s", fmtv(sumr$flux_lateral), fmtv(sumr$flux_lateral_lo), fmtv(sumr$flux_lateral_hi),
                              fmtv(lit[["Lateral DIC"]]), fmtv(lit[["Lateral DOC"]]), fmtv(lit[["Lateral POC"]])), "#1D5A8A", hj = 0, size = 1.75)
    add_arrow(bez(c(2.9, 1.1), c(6.5, 1.15), c(10, 1.1)), sumr$closure_resid, col_res, TRUE)
    add_lab(2.9, 1.36, sprintf("unexplained %s (%s to %s)", fmtv(sumr$closure_resid), fmtv(RES_R[1]), fmtv(RES_R[2])), "#5E5440", hj = 0, size = 1.75)
  } else {
    add_lab(5.6, 1.6, "lateral export: not measured", "grey35", face = "italic", size = 1.85)
    add_lab(3.2, 0.25, "burial or peat loss: not measured", "grey95", face = "italic", size = 1.85)
  }

  sh <- bind_rows(shp, .id = "g"); li <- bind_rows(ln, .id = "g"); ar <- bind_rows(arr, .id = "g"); lb <- bind_rows(lab)
  ggplot() +
    geom_polygon(data = sh, aes(x, y, group = g, fill = I(fill), alpha = I(alpha)), colour = NA) +
    geom_path(data = li, aes(x, y, group = g, colour = I(col), linewidth = I(lw)), lineend = "round") +
    geom_polygon(data = ar, aes(x, y, group = g, fill = I(col), alpha = I(ifelse(lit, 0.5, 0.95)), colour = I(col),
                                linetype = I(ifelse(lit, "22", "solid"))), linewidth = 0.25) +
    geom_text(data = lb, aes(x, y, label = txt, colour = I(col), hjust = hj, size = I(size), fontface = face), vjust = 0, lineheight = 0.85) +
    annotate("text", 1.3, 2.24, label = "sediment", hjust = 0, size = 1.9, colour = "#5A4632", fontface = "italic") +
    annotate("text", 9.9, 0.32, label = "to estuary", hjust = 1, size = 1.8, colour = "grey95", fontface = "italic") +
    coord_fixed(xlim = c(0, 10), ylim = c(0, 9.35), expand = FALSE, clip = "off") + theme_void()
}

schematic_key <- function() {
  kk <- data.frame(lab = c("photosynthesis", "CO2 emission", "CH4 emission", "litter / wood / roots", "lateral export", "burial", "unexplained"),
                   col = c(col_gpp, col_co2a, col_ch4a, col_tr, col_latf, col_bur, col_res), lit = c(F, F, F, T, T, T, T))
  kk$x <- c(0, 1.55, 3.0, 4.45, 6.35, 7.9, 9.0) * 1.0
  ar <- bind_rows(lapply(seq_len(nrow(kk)), function(i) data.frame(arrow_poly(cbind(x = kk$x[i] + c(0, 0.45), y = c(0.5, 0.5)), 0.12), g = i, col = kk$col[i], lit = kk$lit[i])))
  ggplot() + geom_polygon(data = ar, aes(x, y, group = g, fill = I(col), colour = I(col), alpha = I(ifelse(lit, 0.5, 0.95)), linetype = I(ifelse(lit, "22", "solid"))), linewidth = 0.25) +
    geom_text(data = kk, aes(x + 0.55, 0.5, label = lab), hjust = 0, size = 2.1, colour = "grey25") +
    annotate("text", 0, 0.05, hjust = 0, size = 1.95, colour = "grey35",
             label = "g C m⁻² yr⁻¹; arrow width ∝ √flux. Solid: measured here (tower GPP; chambers × scanned surface). Translucent, dashed: literature or closure residual.") +
    coord_cartesian(xlim = c(0, 10.4), ylim = c(-0.1, 0.75), expand = FALSE, clip = "off") + theme_void()
}
