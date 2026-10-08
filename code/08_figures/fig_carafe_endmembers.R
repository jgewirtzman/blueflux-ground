# =============================================================================
# Airborne (CARAFE) CO2 and CH4 fluxes for intact versus ghost mangrove, by
# deployment: two-class (mangrove forest, ghost forest) disaggregation
# (Delaria et al.). Deployments in hand: Apr 2022, Oct 2022, Feb 2023, Apr 2023
# (data/carafe_topdown/delaria_endmembers_campaign.csv).
# Output: output/figures/other/carafe_endmembers.{png,pdf} (draft panel).
# =============================================================================
suppressMessages({library(dplyr); library(ggplot2)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
deps <- c("Apr 2022", "Oct 2022", "Feb 2023", "Apr 2023")
d <- read.csv("data/carafe_topdown/delaria_endmembers_campaign.csv") %>%
  filter(campaign %in% deps) %>%
  mutate(class = recode(class, mangrove_forest = "Intact", ghost_forest = "Ghost"),
         campaign = factor(campaign, levels = deps), class = factor(class, levels = c("Intact", "Ghost")),
         gas = recode(gas, CO2 = "CO[2]~(mu*mol~m^-2~s^-1)", CH4 = "CH[4]~(nmol~m^-2~s^-1)"))
g <- ggplot(d, aes(campaign, flux, fill = class)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_col(position = position_dodge(0.75), width = 0.7) +
  geom_errorbar(aes(ymin = flux - se, ymax = flux + se), position = position_dodge(0.75), width = 0.2, linewidth = 0.3) +
  facet_wrap(~ gas, scales = "free_y", labeller = label_parsed) +
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = c(Intact = "#1b7837", Ghost = "#8c510a"), name = NULL) +
  labs(x = "CARAFE deployment", y = "Midday airborne flux (mean ± SE)") +
  theme_bw(base_size = 9) + theme(legend.position = "bottom", panel.grid.minor = element_blank(),
                                  axis.text.x = element_text(angle = 30, hjust = 1), strip.background = element_blank())
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/carafe_endmembers.png", g, width = 6.5, height = 3.2, dpi = 300)
ggsave("output/figures/other/carafe_endmembers.pdf", g, width = 6.5, height = 3.2)
