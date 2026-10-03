# =============================================================================
# Woody-surface CH4 flux by height above the water surface (or the sediment
# where no water stood): stems and prop roots together, intact / regenerating /
# ghost forest, October 2022 and March 2023, core sites.
#
# Response: asinh(CH4 flux, nmol m-2 s-1) (keeps negative and near-zero fluxes).
# Height h (cm above water or sediment). Candidate height forms, each allowed
# to differ by class: linear; log(h + 5) (offset 5 cm chosen by AIC over
# 1-40 cm); a smooth of h (k = 5); a smooth of log(h + 5) (k = 4). A check adds
# species (where identified) to the selected model.
# Covariates: surface (stem / prop root), tissue status (alive / dead), season,
# position flooded (standing water at the chamber). Random intercepts: tree
# (reconstructed: consecutive closures on one tree are logged in sequence on
# one analyzer, same species and status, < 45 min apart, at most three stem
# heights) and site x campaign. Residual SD modelled by class (Gaussian
# location-scale, mgcv::gaulss), since scatter is far larger in ghost forest.
# Height forms compared by AIC; the selected model gives fitted profiles by
# class (stem, alive, flooded, wet season) with 95% CIs.
# Writes output/analysis/woody_height_model_{comparison,fixed,profiles,curves}.csv and
# output/figures/other/woody_height_model.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(mgcv); library(ggplot2)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
dir.create("output/analysis", showWarnings = FALSE)

d <- read.csv("output/data_products/combined_gas_flux_dataset.csv") %>%
  filter(component %in% c("stem", "root"), plot %in% c("SRS5", "SRS6", "BL60", "CP40", "FLM30"),
         month_year %in% c("2022-10", "2023-03"), !is.na(height_corrected), !is.na(CH4_best.flux),
         !is.na(status)) %>%
  mutate(class = factor(case_when(plot %in% c("SRS5", "SRS6") ~ "intact", plot == "BL60" ~ "regenerating", TRUE ~ "ghost"),
                        c("intact", "regenerating", "ghost")),
         h = pmax(height_corrected, 0), y = asinh(CH4_best.flux),
         surface = factor(ifelse(component == "root", "prop root", "stem"), c("stem", "prop root")),
         status = factor(status, c("alive", "dead")),
         season = factor(ifelse(month_year == "2022-10", "wet", "dry"), c("wet", "dry")),
         flooded = factor(ifelse(!is.na(water_depth) & water_depth > 0, "yes", "no"), c("no", "yes")),
         site_camp = factor(paste(plot, month_year)),
         t = as.POSIXct(paste(date, start_time), tz = "UTC")) %>%
  arrange(plot, date, analyzer_source, t)

# reconstruct trees
tree <- integer(nrow(d)); k <- 0; n_stem <- 0; seen <- numeric(0)
for (i in seq_len(nrow(d))) {
  new <- i == 1 || d$plot[i] != d$plot[i - 1] || d$date[i] != d$date[i - 1] ||
    d$analyzer_source[i] != d$analyzer_source[i - 1] || !identical(d$species[i], d$species[i - 1]) ||
    d$status[i] != d$status[i - 1] || as.numeric(difftime(d$t[i], d$t[i - 1], units = "mins")) > 45 ||
    (d$surface[i] == "stem" && (n_stem >= 3 || d$h[i] %in% seen))
  if (new) { k <- k + 1; n_stem <- 0; seen <- numeric(0) }
  if (d$surface[i] == "stem") { n_stem <- n_stem + 1; seen <- c(seen, d$h[i]) }
  tree[i] <- k
}
d$tree <- factor(tree)
cat("Closures:", nrow(d), " trees (reconstructed):", nlevels(d$tree), "\n")
print(table(d$class, d$surface))

fe <- "surface + status + season + flooded + s(tree, bs = 're') + s(site_camp, bs = 're')"
forms <- list(
  linear = "class + class:h",
  log    = "class + class:log(h + 5)",
  smooth = "class + s(h, by = class, k = 5)",
  smooth_log = "class + s(log(h + 5), by = class, k = 4)")
fit <- function(f) gam(list(as.formula(paste("y ~", f, "+", fe)), ~ class), family = gaulss(), data = d, method = "REML")
mods <- lapply(forms, fit)
cmp <- data.frame(height_form = names(forms), AIC = sapply(mods, AIC), edf = sapply(mods, function(m) sum(m$edf))) %>%
  mutate(dAIC = AIC - min(AIC)) %>% arrange(AIC)
print(cmp, row.names = FALSE)
best <- mods[[cmp$height_form[1]]]
d$sp <- factor(ifelse(d$species %in% "COPE", "COER", ifelse(is.na(d$species) | d$species %in% c("", "UNKN"), "unidentified", d$species)))
d$sp <- relevel(d$sp, "RHMA")
msp <- gam(list(as.formula(paste("y ~", forms[[cmp$height_form[1]]], "+ sp +", fe)), ~ class), family = gaulss(), data = d, method = "REML")
sp_tab <- summary(msp)$p.table; sp_tab <- sp_tab[grep("^sp", rownames(sp_tab)), , drop = FALSE]
cat("\nSpecies added to the selected model (reference R. mangle): AIC", round(AIC(msp), 1), "\n"); print(signif(sp_tab, 3))
write.csv(data.frame(term = rownames(sp_tab), sp_tab, check.names = FALSE), "output/analysis/woody_height_model_species_check.csv", row.names = FALSE)
sm <- summary(best)
fixed <- data.frame(term = names(sm$p.coeff), estimate = sm$p.coeff, se = sm$se[names(sm$p.coeff)], p = sm$p.pv)
print(fixed %>% mutate(across(where(is.numeric), ~ signif(.x, 3))), row.names = FALSE)
if (length(sm$s.table)) print(sm$s.table)

# fitted profiles: stem, alive, flooded, wet season; random effects excluded
nd <- expand.grid(h = seq(0, 150, 1), class = levels(d$class)) %>%
  mutate(surface = factor("stem", levels(d$surface)), status = factor("alive", levels(d$status)),
         season = factor("wet", levels(d$season)), flooded = factor("yes", levels(d$flooded)),
         tree = d$tree[1], site_camp = d$site_camp[1])
pr <- predict(best, nd, se.fit = TRUE, exclude = c("s(tree)", "s(site_camp)"))
nd <- nd %>% mutate(fit = pr$fit[, 1], se = pr$se.fit[, 1],
                    flux = sinh(fit), lo = sinh(fit - 1.96 * se), hi = sinh(fit + 1.96 * se))
prof <- nd %>% filter(h %in% c(0, 5, 10, 25, 50, 100, 150)) %>% select(class, h, flux, lo, hi)
print(prof %>% mutate(across(where(is.double), ~ signif(.x, 3))), row.names = FALSE)
write.csv(cmp, "output/analysis/woody_height_model_comparison.csv", row.names = FALSE)
write.csv(fixed, "output/analysis/woody_height_model_fixed.csv", row.names = FALSE)
write.csv(prof, "output/analysis/woody_height_model_profiles.csv", row.names = FALSE)
write.csv(nd %>% select(class, h, fit, se, flux, lo, hi), "output/analysis/woody_height_model_curves.csv", row.names = FALSE)

cols <- c(intact = "#1b7837", regenerating = "#e08214", ghost = "#542788")
br <- c(-1, 0, 1, 3, 10, 30, 100, 300)
p <- ggplot() +
  geom_point(data = d, aes(h, y, colour = class, shape = surface), alpha = 0.45, size = 1.4) +
  geom_ribbon(data = nd, aes(h, ymin = fit - 1.96 * se, ymax = fit + 1.96 * se, fill = class), alpha = 0.2) +
  geom_line(data = nd, aes(h, fit, colour = class), linewidth = 0.9) +
  coord_flip() + facet_wrap(~ class) +
  scale_y_continuous(breaks = asinh(br), labels = br) +
  scale_colour_manual(values = cols, guide = "none") + scale_fill_manual(values = cols, guide = "none") +
  scale_shape_manual(values = c(stem = 16, `prop root` = 17), name = NULL) +
  labs(x = "Height above water (or sediment), cm", y = expression("CH"[4]*" flux (nmol m"^-2*" s"^-1*", asinh scale)"),
       subtitle = sprintf("Lines: fitted profile (%s height form), live stem, flooded position, wet season; 95%% CI", cmp$height_form[1])) +
  theme_bw(base_size = 9) + theme(legend.position = "bottom")
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/woody_height_model.png", p, width = 10, height = 4.2, dpi = 200)
ggsave("output/figures/other/woody_height_model.pdf", p, width = 10, height = 4.2)
