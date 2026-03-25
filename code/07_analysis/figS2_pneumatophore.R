# =============================================================================
# Figure S2: Soil CH4/CO2 flux vs pneumatophore density
# Output: pub_SI_pneumatophore_density
# =============================================================================
source("code/07_analysis/publication_figures_common.R")
library(ggpubr)

cat("\n--- Figure S2: Pneumatophore Density ---\n")

pn <- df %>%
  filter(!is.na(pneumatophore_density), component == "soil",
         CH4_flux_status == "valid") %>%
  mutate(disturbance = factor(disturbance_level,
                              levels = c("healthy", "regenerating", "ghost")))

# CH4 plot
p_ch4 <- pn %>%
  ggplot(aes(x = pneumatophore_density, y = CH4_best.flux)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(aes(color = disturbance), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.6, alpha = 0.15) +
  stat_regline_equation(size = 2.3, label.x.npc = 0.55, label.y.npc = 0.99) +
  stat_cor(method = "pearson", label.x.npc = 0.55, label.y.npc = 0.91,
           aes(label = paste(after_stat(r.label), after_stat(p.label), sep = "~`,`~")),
           size = 2.3) +
  stat_cor(method = "spearman", label.x.npc = 0.55, label.y.npc = 0.83,
           aes(label = paste(after_stat(r.label), after_stat(p.label), sep = "~`,`~")),
           size = 2.3, color = "grey40") +
  facet_wrap(~ plot, nrow = 1, scales = "free_y") +
  scale_y_continuous(trans = "asinh",
                     breaks = c(0, 1, 10, 100, 1000),
                     labels = c("0", "1", "10", "100", "1000")) +
  scale_color_manual(values = disturbance_colors, name = "Disturbance") +
  labs(x = expression(Pneumatophore~density~(m^{-2})),
       y = expression(Soil~CH[4]~flux~(nmol~m^{-2}~s^{-1})),
       tag = "a") +
  theme_pub(base_size = 10) +
  theme(legend.position = "none")

# CO2 plot
p_co2 <- pn %>%
  filter(!is.na(CO2_best.flux)) %>%
  ggplot(aes(x = pneumatophore_density, y = CO2_best.flux)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(aes(color = disturbance), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.6, alpha = 0.15) +
  stat_regline_equation(size = 2.3, label.x.npc = 0.55, label.y.npc = 0.99) +
  stat_cor(method = "pearson", label.x.npc = 0.55, label.y.npc = 0.91,
           aes(label = paste(after_stat(r.label), after_stat(p.label), sep = "~`,`~")),
           size = 2.3) +
  stat_cor(method = "spearman", label.x.npc = 0.55, label.y.npc = 0.83,
           aes(label = paste(after_stat(r.label), after_stat(p.label), sep = "~`,`~")),
           size = 2.3, color = "grey40") +
  facet_wrap(~ plot, nrow = 1, scales = "free_y") +
  scale_color_manual(values = disturbance_colors, name = "Disturbance") +
  labs(x = expression(Pneumatophore~density~(m^{-2})),
       y = expression(Soil~CO[2]~flux~(mu*mol~m^{-2}~s^{-1})),
       tag = "b") +
  theme_pub(base_size = 10) +
  theme(legend.position = "bottom")

combined <- ggarrange(p_ch4, p_co2, ncol = 1, heights = c(1, 1.15))

save_pub(combined, "SI_pneumatophore_density", width = 300, height = 200)
