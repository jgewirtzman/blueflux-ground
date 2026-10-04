# =============================================================================
# Fig. S? | The intact-to-ghost switch by three routes (07_upscaling/10_airborne_switch.R):
# chamber site budgets (this study), CARAFE airborne class fluxes alone (Delaria et al.
# 2024, midday CO2 converted to daily with the Doughty et al. 2026 tower regression;
# open: ghost midday = daily), and the regional MODIS upscaling (Doughty et al. 2026,
# mangrove vs ghost-forest pixels, 2022-2024). Bars: CO2 and CH4 parts; points and
# whiskers: switch and 95% interval. Writes output/figures/other/si_switch_methods.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(tidyr); library(ggplot2)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R"); invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))
pal_gas <- c(CO2 = "#9A9DA1", CH4 = "#A23B72")
d <- read.csv("output/upscaling/switch_by_method.csv")
lv <- c("Chamber budgets (this study)", "Airborne (Delaria et al. 2024; Doughty conversion)",
        "Airborne, ghost midday = daily (sensitivity)", "Regional model (Doughty et al. 2026)")
lab <- c("Chamber budgets\n(this study; 5 sites)", "Airborne class fluxes\n(Delaria et al. 2024)",
         "Airborne, ghost CO2\nmidday = daily", "Regional model\n(Doughty et al. 2026)")
d <- d %>% mutate(m = factor(lab[match(method, lv)], rev(lab)), horizon = factor(horizon, c("GWP20", "GWP100")))
bars <- d %>% select(m, horizon, CO2 = co2_part, CH4 = ch4_part) %>% pivot_longer(c(CO2, CH4), names_to = "gas", values_to = "v") %>%
  mutate(gas = factor(gas, c("CO2", "CH4")))
p <- ggplot(d, aes(y = m)) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_col(data = bars %>% mutate(sens = grepl("midday", m)), aes(x = v, fill = gas, alpha = sens), width = 0.55, colour = "white",
           linewidth = 0.25, position = position_stack(reverse = TRUE)) +
  scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.4), guide = "none") +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.18, linewidth = 0.4, colour = col_ink, orientation = "y", na.rm = TRUE) +
  geom_point(aes(x = switch), shape = 23, size = 2.4, fill = "white", colour = col_ink, stroke = 0.6) +
  geom_text(aes(x = pmax(switch, hi, na.rm = TRUE) + 250, label = formatC(round(switch, -1), format = "d", big.mark = ",")),
            hjust = 0, size = 2.3, colour = "grey20") +
  facet_wrap(~ horizon, nrow = 1) +
  scale_fill_manual(values = pal_gas, labels = c(CO2 = expression("CO"[2]*" part"), CH4 = expression("CH"[4]*" part")), name = NULL) +
  scale_x_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0.05, 0.18))) +
  labs(x = expression("Intact-to-ghost switch (g CO"[2]*"-eq m"^-2*" yr"^-1*")"), y = NULL) +
  theme_fig() + theme(legend.position = "bottom", panel.grid.major.y = element_blank(), strip.text = element_text(hjust = 0.5))
ggsave("output/figures/other/si_switch_methods.png", p, width = 7.2, height = 3.2, dpi = 300, bg = "white")
ggsave("output/figures/other/si_switch_methods.pdf", p, width = 7.2, height = 3.2, device = cairo_pdf)
