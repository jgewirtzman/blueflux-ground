# =============================================================================
# QA: CH4 flux by height above the water surface (or sediment) for all woody
# surfaces, stems and prop roots together, by forest class. Descriptive: points,
# and medians by height band. Prop roots with a recorded height only (most
# October 2022 roots have none). Writes output/qa/woody_flux_by_height.{png,csv}.
# =============================================================================
suppressMessages({library(dplyr); library(ggplot2)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
d <- read.csv("output/data_products/combined_gas_flux_dataset.csv") %>%
  filter(component %in% c("stem", "root"), plot %in% c("SRS5", "SRS6", "BL60", "CP40", "FLM30"),
         month_year %in% c("2022-10", "2023-03"), !is.na(height_corrected), !is.na(CH4_best.flux)) %>%
  mutate(class = factor(case_when(plot %in% c("SRS5", "SRS6") ~ "intact", plot == "BL60" ~ "regenerating", TRUE ~ "ghost"),
                        c("intact", "regenerating", "ghost")),
         surface = ifelse(component == "root", "prop root", "stem"),
         campaign = ifelse(month_year == "2022-10", "Oct 2022 (wet)", "Mar 2023 (dry)"),
         band = cut(pmax(height_corrected, 0), c(0, 5, 25, 50, 100, 150, Inf), right = FALSE,
                    labels = c("0-5", "5-25", "25-50", "50-100", "100-150", ">150")))
summ <- d %>% group_by(class, band) %>%
  summarise(n = n(), n_root = sum(surface == "prop root"), median = median(CH4_best.flux),
            q25 = quantile(CH4_best.flux, 0.25), q75 = quantile(CH4_best.flux, 0.75), .groups = "drop")
write.csv(summ, "output/qa/woody_flux_by_height.csv", row.names = FALSE)
print(as.data.frame(summ %>% mutate(across(where(is.double), ~ signif(.x, 3)))), row.names = FALSE)
p <- ggplot(d, aes(asinh(CH4_best.flux), pmax(height_corrected, 0))) +
  geom_point(aes(colour = surface, shape = campaign), alpha = 0.7, size = 1.6) +
  geom_point(data = summ %>% mutate(h = c(2.5, 15, 37.5, 75, 125, 175)[as.integer(band)]),
             aes(asinh(median), h), inherit.aes = FALSE, shape = 23, size = 3, fill = "black") +
  facet_wrap(~ class) +
  scale_x_continuous(breaks = asinh(c(-1, 0, 1, 10, 100, 1000)), labels = c(-1, 0, 1, 10, 100, 1000)) +
  scale_colour_manual(values = c(stem = "#8c510a", `prop root` = "#35978f"), name = NULL) +
  labs(x = expression("CH"[4]*" flux (nmol m"^-2*" s"^-1*", asinh scale)"), y = "Height above water (or sediment), cm",
       subtitle = "Black diamonds: median by height band (0-5, 5-25, 25-50, 50-100, 100-150, >150 cm)") +
  theme_bw(base_size = 9) + theme(legend.position = "bottom")
ggsave("output/qa/woody_flux_by_height.png", p, width = 10, height = 4.5, dpi = 200)
