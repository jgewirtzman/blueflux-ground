# =============================================================================
# QA: recorded chamber heights on stems, prop roots and downed wood, by site x
# campaign, against the water level, to check the height datum.
# Heights are above the sediment (05_dataset/01_compile_datasets.R).
# Water level per site x campaign: median (line) and range (band) of the depths
# recorded at stem, root and downed-wood positions. Points below the water line
# or below the sediment are flagged (open symbols) and listed.
# Writes output/qa/chamber_heights_review.{png,csv}.
# =============================================================================
suppressMessages({library(dplyr); library(ggplot2)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
core <- c("SRS5", "SRS6", "BL60", "FLM30", "CP40", "SE1", "MI", "RB10")
d <- read.csv("output/data_products/combined_gas_flux_dataset.csv") %>%
  filter(plot %in% core, component %in% c("stem", "root", "cwd")) %>%
  mutate(campaign = recode(month_year, "2022-03" = "Mar 2022", "2022-10" = "Oct 2022", "2023-03" = "Mar 2023"),
         campaign = factor(campaign, c("Mar 2022", "Oct 2022", "Mar 2023")),
         depth = ifelse(is.na(water_depth), 0, pmax(0, water_depth)),
         h_sed = height,
         flag = case_when(is.na(height) ~ "no height",
                          h_sed < 0 ~ "below sediment",
                          h_sed < depth ~ "below water line",
                          TRUE ~ "ok"),
         component = factor(recode(component, stem = "Stem", root = "Prop root", cwd = "Downed wood"),
                             c("Stem", "Prop root", "Downed wood")))
wl <- d %>% group_by(plot, campaign) %>%
  summarise(w_med = median(depth), w_min = min(depth), w_max = max(depth), .groups = "drop")
summ <- d %>% count(plot, campaign, component, flag)
write.csv(d %>% filter(flag != "ok") %>%
            select(flux_id, plot, campaign, component, status, height,
                   water_depth, h_sed, flag),
          "output/qa/chamber_heights_review.csv", row.names = FALSE)
print(as.data.frame(summ %>% filter(flag != "ok")), row.names = FALSE)

p <- ggplot(d %>% filter(!is.na(h_sed))) +
  geom_rect(data = wl, aes(xmin = -Inf, xmax = Inf, ymin = w_min, ymax = w_max), fill = "#9ecae1", alpha = 0.35, inherit.aes = FALSE) +
  geom_hline(data = wl, aes(yintercept = w_med), colour = "#2171b5", linewidth = 0.4) +
  geom_hline(yintercept = 0, colour = "#8c510a", linewidth = 0.5) +
  geom_jitter(aes(component, h_sed, colour = status, shape = flag == "ok"), width = 0.18, height = 0, size = 1.4, alpha = 0.8) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), labels = c(`TRUE` = "ok", `FALSE` = "below water or sediment"), name = NULL) +
  scale_colour_manual(values = c(alive = "#1b7837", dead = "#762a83"), name = NULL) +
  facet_grid(campaign ~ plot) +
  labs(x = NULL, y = "Height above sediment (cm)",
       subtitle = "Brown line: sediment. Blue line and band: median and range of recorded water depth.") +
  theme_bw(base_size = 8) + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
ggsave("output/qa/chamber_heights_review.png", p, width = 10, height = 6, dpi = 200)
