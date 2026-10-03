# =============================================================================
# QA: does the stem CH4 decline with height differ between intact and disturbed
# forest? (Results text: "steeper and began from a higher base in disturbed
# forest".) Two views on heights above the water surface (or sediment):
#   1. the per site x campaign exponential fits used for scaling
#      (07_upscaling/02_upscale_methane.R: log CH4 ~ height, positive fluxes)
#   2. a mixed model asinh(CH4) ~ height x class + season + (1 | site), all
#      stem fluxes at the core sites, 0-150 cm.
# Writes output/qa/stem_height_by_class.csv.
# =============================================================================
suppressMessages({library(dplyr); library(lme4); library(lmerTest); library(emmeans)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
d <- read.csv("output/data_products/combined_gas_flux_dataset.csv") %>%
  filter(component == "stem", plot %in% c("SRS5", "SRS6", "BL60", "CP40", "FLM30"),
         month_year %in% c("2022-10", "2023-03"), !is.na(height_corrected), height_corrected >= 0,
         height_corrected <= 150, !is.na(CH4_best.flux)) %>%
  mutate(class = factor(case_when(plot %in% c("SRS5", "SRS6") ~ "intact", plot == "BL60" ~ "regenerating", TRUE ~ "ghost"),
                        c("intact", "regenerating", "ghost")),
         h = height_corrected / 100, season = ifelse(month_year == "2022-10", "wet", "dry"))

fits <- d %>% filter(CH4_best.flux > 0) %>% group_by(class, plot, month_year) %>%
  group_modify(~ { m <- lm(log(CH4_best.flux) ~ h, data = .x)
    data.frame(n = nrow(.x), base_nmol = exp(coef(m)[[1]]), slope_per_m = coef(m)[[2]],
               p_slope = summary(m)$coefficients["h", 4]) }) %>% ungroup()

m <- lmer(asinh(CH4_best.flux) ~ h * class + season + (1 | plot), data = d)
a <- anova(m)
tr <- as.data.frame(emtrends(m, ~ class, var = "h"))
base <- as.data.frame(emmeans(m, ~ class, at = list(h = 0)))
mixed <- data.frame(class = tr$class, slope_asinh_per_m = tr$h.trend, slope_lo = tr$lower.CL, slope_hi = tr$upper.CL,
                    base_at_0 = sinh(base$emmean), base_lo = sinh(base$lower.CL), base_hi = sinh(base$upper.CL),
                    p_height_x_class = a["h:class", "Pr(>F)"], p_class = a["class", "Pr(>F)"])
pc <- as.data.frame(pairs(emtrends(m, ~ class, var = "h")))
cat("Per site x campaign exponential fits:\n"); print(as.data.frame(fits %>% mutate(across(where(is.double), ~ signif(.x, 3)))), row.names = FALSE)
cat("\nMixed model: slopes and base by class\n"); print(mixed %>% mutate(across(where(is.double), ~ signif(.x, 3))), row.names = FALSE)
cat("\nSlope contrasts:\n"); print(pc[, c("contrast", "estimate", "p.value")], row.names = FALSE)
write.csv(bind_rows(fits %>% mutate(view = "exponential fit (site x campaign)"),
                    mixed %>% mutate(view = "mixed model (class)")),
          "output/qa/stem_height_by_class.csv", row.names = FALSE)
