# =============================================================================
# SI figure: context and regenerating sites.
#   (a) Component CH4 rates at the context / regenerating sites (BL60, MI, RB10,
#       SE1; site mean with bootstrap 95% CI, pooled campaigns) against the
#       pooled core-class means and CIs (intact SRS5+SRS6; ghost CP40+FLM30).
#       Same conventions as Table S12 / code/06_analysis/05_si_tables.R
#       (eight named plots; closures with CO2 < -10 umol m-2 s-1 dropped;
#       sample mean; percentile CI from 5,000 resamples, seed 42), CI only n > 3.
#   (b) BL60 stand CH4 (g CH4 m-2 yr-1) by campaign and annual mean under three
#       woody-structure bounds (none, intermediate, intact), read from
#       output/upscaling/supp_regen_budget.csv (08_supplementary_analyses.R section 2).
#   (c) Measurement coverage: chambers per site x campaign x component.
# Output: output/figures/other/si_context_regen.{png,pdf}
# =============================================================================
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(patchwork)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))

conv <- 16.04e-9 * 86400 * 1000    # nmol CH4 m-2 s-1 -> mg CH4 m-2 d-1
gyr  <- 365 / 1000                 # mg m-2 d-1 -> g m-2 yr-1
df <- read.csv("output/data_products/combined_gas_flux_dataset.csv")
df$campaign <- with(df, ifelse(year == 2022 & month == 10, "Oct 2022",
                        ifelse(year == 2023 & month == 3,  "Mar 2023",
                        ifelse(year == 2022 & month == 3,  "Mar 2022", NA))))
camps  <- c("Mar 2022", "Oct 2022", "Mar 2023")
plots8 <- c("SRS5", "SRS6", "RB10", "BL60", "SE1", "CP40", "FLM30", "MI")
comp_lab <- c(soil = "soil", water = "water", root = "prop root", stem = "stem",
              cwd = "downed wood", leaves = "leaf")
grp_col <- c(pal_class, scrub = "grey45")

# =============================================================================
# (b) BL60 budget by campaign and woody-structure bound (08_supplementary_analyses.R section 2)
# =============================================================================
rb <- read.csv("output/upscaling/supp_regen_budget.csv") %>%
  mutate(woody = factor(woody, c("none", "intermediate", "intact")),
         campaign = factor(campaign, c("Oct 2022", "Mar 2023", "annual"), c("wet", "dry", "annual")))
bud <- rb %>% select(woody, campaign, soil, water, root, stem) %>%
  pivot_longer(c(soil, water, root, stem), names_to = "component", values_to = "g") %>%
  mutate(component = factor(component, c("stem", "root", "water", "soil")))
nf <- read.csv("output/upscaling/net_forcing_by_class.csv")
ref <- data.frame(lab = c("intact", "ghost"),
                  g = c(nf$ch4_g_yr[nf$disturbance_level == "healthy"], nf$ch4_g_yr[nf$disturbance_level == "ghost"]))
print(rb)
pB <- ggplot() +
  geom_hline(data = ref, aes(yintercept = g, colour = lab), linetype = "22", linewidth = 0.45) +
  geom_col(data = bud, aes(woody, g, fill = component), width = 0.62, colour = "grey25", linewidth = 0.2) +
  geom_text(data = rb, aes(woody, total, label = sprintf("%.1f", total)), vjust = -0.5, size = 2.1, colour = col_ink) +
  facet_grid(~ campaign) +
  geom_text(data = ref %>% mutate(campaign = factor("annual", levels(rb$campaign))), aes(x = 3.45, y = g, label = lab, colour = lab),
            hjust = 0, vjust = -0.3, size = 2.1, show.legend = FALSE) +
  scale_fill_manual(values = pal_comp_data[c("soil", "water", "root", "stem")],
                    breaks = c("soil", "water", "root", "stem"), labels = comp_lab[c("soil", "water", "root", "stem")], name = NULL) +
  scale_colour_manual(values = c(intact = unname(pal_class["intact"]), ghost = unname(pal_class["ghost"])), guide = "none") +
  scale_x_discrete(labels = c(none = "none", intermediate = "mid", intact = "intact"), expand = expansion(add = c(0.5, 0.9))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, NA)) +
  coord_cartesian(clip = "off") +
  labs(x = "woody surface assumed (no laser scan)",
       y = expression("BL60 stand CH"[4]*" (g CH"[4]*" m"^-2*" yr"^-1*")")) +
  theme_fig() +
  theme(panel.grid.major.x = element_blank(), legend.position = "bottom", strip.text = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 6.5), legend.text = element_text(size = 7), legend.margin = margin(0, 0, 0, 0))

# =============================================================================
# (a) Component rates: context sites vs core-class pooled ranges
# =============================================================================
r <- df %>% filter(plot %in% plots8, !is.na(CH4_best.flux), is.na(CO2_best.flux) | CO2_best.flux >= -10)
set.seed(42)
boot_ci <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) <= 3) return(data.frame(mean = mean(x), lo = NA_real_, hi = NA_real_, n = length(x)))
  set.seed(42); b <- replicate(5000, mean(sample(x, replace = TRUE)))
  data.frame(mean = mean(x), lo = unname(quantile(b, 0.025)), hi = unname(quantile(b, 0.975)), n = length(x))
}
ctx_sites <- c("RB10", "BL60", "SE1", "MI")
site_grp <- c(RB10 = "intact", BL60 = "regenerating", SE1 = "scrub", MI = "ghost")
ctx <- r %>% filter(plot %in% ctx_sites) %>% group_by(plot, component) %>%
  do(boot_ci(.$CH4_best.flux)) %>% ungroup()
core <- r %>% filter(plot %in% c("SRS5", "SRS6", "CP40", "FLM30")) %>%
  mutate(grp = ifelse(plot %in% c("SRS5", "SRS6"), "intact", "ghost")) %>%
  group_by(grp, component) %>% do(boot_ci(.$CH4_best.flux)) %>% ungroup()
cat("\nContext-site component CH4 (nmol m-2 s-1, pooled campaigns):\n")
print(as.data.frame(ctx %>% mutate(across(c(mean, lo, hi), ~ round(.x, 2)))))
cat("Core-class pooled component CH4:\n")
print(as.data.frame(core %>% mutate(across(c(mean, lo, hi), ~ round(.x, 2)))))

comp_order <- c("soil", "water", "root", "stem", "cwd", "leaves")
ctx <- ctx %>% mutate(grp = site_grp[plot], plot = factor(plot, rev(ctx_sites)),
                      component = factor(component, comp_order, comp_lab[comp_order]))
core <- core %>% mutate(component = factor(component, comp_order, comp_lab[comp_order]),
                        grp = factor(grp, c("intact", "ghost")))
raw <- r %>% filter(plot %in% ctx_sites) %>% semi_join(ctx %>% filter(n <= 3) %>%
                                                         mutate(plot = as.character(plot), component = names(comp_lab)[match(component, comp_lab)]),
                                                       by = c("plot", "component")) %>%
  mutate(grp = site_grp[plot], plot = factor(plot, rev(ctx_sites)),
         component = factor(component, comp_order, comp_lab[comp_order]))
xr <- asinh(c(-2, 700))
pA <- ggplot() +
  geom_rect(data = core %>% filter(!is.na(lo)), aes(xmin = asinh(lo), xmax = asinh(hi), ymin = -Inf, ymax = Inf, fill = grp),
            alpha = 0.22) +
  geom_vline(data = core, aes(xintercept = asinh(mean), colour = grp), linewidth = 0.4, linetype = "22",
             show.legend = FALSE) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.25) +
  geom_errorbar(data = ctx %>% filter(!is.na(lo)), aes(y = plot, xmin = asinh(lo), xmax = asinh(hi), colour = grp),
                width = 0, linewidth = 0.5, orientation = "y") +
  geom_point(data = raw, aes(asinh(CH4_best.flux), plot, colour = grp), shape = 1, size = 1.1, stroke = 0.4) +
  geom_point(data = ctx, aes(asinh(mean), plot, colour = grp), size = 1.7) +
  geom_text(data = ctx, aes(x = xr[2], y = plot, label = paste0("n=", n)), hjust = 1, size = 2.1, colour = "grey35") +
  facet_grid(component ~ ., scales = "free_y", space = "free_y", switch = "y") +
  asinh_axis(breaks = c(-1, 0, 1, 10, 100), limits = xr) +
  scale_colour_manual(values = grp_col, breaks = c("intact", "regenerating", "scrub", "ghost"), name = "context site") +
  scale_fill_manual(values = pal_class[c("intact", "ghost")], labels = c("intact core (SRS5+SRS6)", "ghost core (CP40+FLM30)"),
                    name = "core 95% CI") +
  labs(x = expression("CH"[4]*" flux (nmol m"^-2*" s"^-1*", asinh scale)"), y = NULL) +
  theme_fig() +
  theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold"),
        panel.grid.major.y = element_blank(), panel.spacing.y = unit(2, "pt"),
        panel.border = element_rect(colour = "grey85", fill = NA, linewidth = 0.3),
        legend.position = "bottom", legend.box = "vertical", legend.spacing.y = unit(0, "pt"),
        legend.text = element_text(size = 7), legend.title = element_text(size = 7),
        legend.margin = margin(0, 0, 0, 0)) +
  guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2))

# =============================================================================
# (c) Coverage grid
# =============================================================================
we <- read.csv("output/flux/03_fit/water_flux_estimates.csv") %>% filter(gas == "CH4") %>%
  transmute(plot = site, campaign, est = TRUE)
rows <- c("soil", "soil_pn", "water", "water_dg", "root", "stem", "cwd", "leaves")
row_lab <- c(soil = "soil", soil_pn = "  of which with pneumatophores", water = "water (chamber)",
             water_dg = "water (dissolved-gas estimate)", root = "prop root", stem = "stem",
             cwd = "downed wood", leaves = "leaf")
d8 <- df %>% filter(plot %in% plots8, !is.na(CH4_best.flux))
visited <- d8 %>% distinct(plot, campaign)
n_comp <- d8 %>% count(plot, campaign, row = component)
pn <- d8 %>% filter(component == "soil") %>% group_by(plot, campaign) %>%
  summarise(n = sum(pneumatophore_count > 0, na.rm = TRUE), recorded = any(!is.na(pneumatophore_count)),
            .groups = "drop") %>% mutate(row = "soil_pn")
cov <- expand_grid(plot = plots8, campaign = camps, row = rows) %>%
  left_join(bind_rows(n_comp, pn %>% select(plot, campaign, row, n, recorded)), by = c("plot", "campaign", "row")) %>%
  left_join(we %>% mutate(row = "water_dg"), by = c("plot", "campaign", "row")) %>%
  left_join(visited %>% mutate(vis = TRUE), by = c("plot", "campaign")) %>%
  mutate(n = coalesce(n, 0L), vis = coalesce(vis, FALSE),
         status = case_when(!vis ~ "site not visited",
                            row == "water_dg" & coalesce(est, FALSE) ~ "dissolved-gas estimate",
                            row == "soil_pn" & !is.na(recorded) & !recorded ~ "not recorded",
                            n > 0 ~ "measured (n chambers)",
                            TRUE ~ "not measured (0)"),
         lab = case_when(status == "measured (n chambers)" ~ as.character(n),
                         status == "not measured (0)" & row != "water_dg" ~ "0",
                         status == "not recorded" ~ "nr",
                         status == "dissolved-gas estimate" ~ "est", TRUE ~ ""),
         campaign = factor(campaign, camps), plot = factor(plot, plots8),
         row = factor(row, rev(rows)))
st_lev <- c("measured (n chambers)", "dissolved-gas estimate", "not measured (0)", "not recorded", "site not visited")
cov$status <- factor(cov$status, st_lev)
site_cls <- c(SRS5 = "intact", SRS6 = "intact", RB10 = "intact", BL60 = "regenerating", SE1 = "scrub",
              CP40 = "ghost", FLM30 = "ghost", MI = "ghost")
band <- expand_grid(plot = factor(plots8, plots8), campaign = factor(camps, camps)) %>%
  mutate(grp = site_cls[as.character(plot)])
ax_col <- grp_col[site_cls[plots8]]
pC <- ggplot(cov, aes(plot, row)) +
  geom_tile(aes(fill = status, colour = status), linewidth = 0.3, width = 0.9, height = 0.86) +
  geom_text(aes(label = lab, fontface = ifelse(status == "dissolved-gas estimate", "italic", "plain")),
            size = 2.1, colour = col_ink) +
  geom_tile(data = band, aes(plot, y = length(rows) + 0.85, fill = NULL), fill = grp_col[band$grp],
            height = 0.35, width = 0.9, inherit.aes = FALSE) +
  facet_wrap(~campaign, nrow = 1) +
  scale_y_discrete(labels = row_lab, expand = expansion(add = c(0.6, 1.1))) +
  scale_fill_manual(values = c("measured (n chambers)" = "grey70", "dissolved-gas estimate" = "#A6CBE3",
                               "not measured (0)" = "white", "not recorded" = "white", "site not visited" = "grey95"),
                    drop = FALSE, name = NULL) +
  scale_colour_manual(values = c("measured (n chambers)" = "grey70", "dissolved-gas estimate" = "#2C7BB6",
                                 "not measured (0)" = "grey35", "not recorded" = "grey70", "site not visited" = "grey95"),
                      drop = FALSE, name = NULL) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL) +
  theme_fig() +
  theme(panel.grid.major = element_blank(), axis.line = element_blank(), axis.ticks = element_blank(),
        axis.text.x = element_text(colour = ax_col, face = "bold", size = 6.5, angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 7), strip.text = element_text(size = 8),
        panel.spacing.x = unit(6, "pt"), legend.text = element_text(size = 7),
        legend.margin = margin(0, 0, 0, 0)) +
  guides(fill = guide_legend(nrow = 1), colour = guide_legend(nrow = 1))

# =============================================================================
fig <- ((pA + labs(tag = "a")) | (pB + labs(tag = "b"))) / (pC + labs(tag = "c")) +
  plot_layout(heights = c(1.35, 1)) &
  theme(plot.tag = element_text(face = "bold", size = 11))
fig[[1]] <- fig[[1]] + plot_layout(widths = c(1.45, 1))
dir.create("output/figures/other", recursive = TRUE, showWarnings = FALSE)
ggsave("output/figures/other/si_context_regen.png", fig, width = 7.2, height = 8.2, dpi = 300, bg = "white")
ggsave("output/figures/other/si_context_regen.pdf", fig, width = 7.2, height = 8.2, device = cairo_pdf)
cat("written output/figures/other/si_context_regen.{png,pdf}\n")
