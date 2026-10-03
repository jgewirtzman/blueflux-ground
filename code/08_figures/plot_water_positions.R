# =============================================================================
# Water CH4 flux by measurement position, across all campaigns.
# Shows every water chamber's flux, labeled by recorded collar_location and
# flagged in-plot vs off-plot/river, to assess whether open-water measurements
# read artificially low.
# =============================================================================
suppressMessages({library(dplyr); library(ggplot2)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

df <- read.csv("output/data_products/combined_gas_flux_dataset.csv")
df$camp <- with(df, ifelse(year==2022&month==10,"Oct 2022",
                    ifelse(year==2023&month==3,"Mar 2023",
                    ifelse(year==2022&month==3,"Mar 2022",NA))))
off_pat <- "river|open|off peir|off pier|pier|interface|edge"   # channel/open-water placements; "outside plot" at ghost sites is still over the floor
w <- df %>% filter(component=="water", !is.na(CH4_best.flux), !is.na(camp)) %>%
  mutate(position = case_when(
           is.na(collar_location) ~ "above forest floor",   # unlabelled = standard placement over the flooded floor (no channels at ghost/regenerating sites)
           grepl(off_pat, collar_location, ignore.case=TRUE) ~ "channel / open water",
           TRUE ~ "above forest floor"),
         class = recode(plot, CP40="ghost",FLM30="ghost",MI="ghost",
                        BL60="regenerating",SE1="scrub",SRS5="intact",SRS6="intact",RB10="intact"),
         site = factor(plot, levels=c("CP40","FLM30","BL60","SE1","SRS5","SRS6")),
         camp = factor(camp, levels=c("Mar 2022","Oct 2022","Mar 2023")),
         position = factor(position, c("above forest floor", "channel / open water")))

source("code/08_figures/palette.R")   # house palette + theme_fig()
pos_cols <- c("above forest floor" = "#2C7BB6", "channel / open water" = "#C2513A")
site_cls <- c(CP40 = "ghost", FLM30 = "ghost", BL60 = "regenerating", SE1 = "scrub", SRS5 = "intact", SRS6 = "intact")
w <- w %>% mutate(site_lab = factor(paste0(site, "\n", site_cls[as.character(site)]),
                                    paste0(levels(site), "\n", site_cls[levels(site)])))
# arithmetic mean per site x campaign x position (computed on the raw scale), drawn as a bar
mn <- w %>% group_by(camp, site_lab, position) %>% summarise(m = mean(CH4_best.flux), .groups = "drop")
dg <- position_dodge(width = 0.6)
p <- ggplot(w, aes(site_lab, CH4_best.flux, group = position)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_point(aes(fill = position), shape = 21, colour = "white", stroke = 0.25, size = 1.8, alpha = .9,
             position = position_jitterdodge(jitter.width = 0.12, jitter.height = 0, dodge.width = 0.6, seed = 1)) +
  geom_errorbar(data = mn, aes(x = site_lab, ymin = m, ymax = m, colour = position, group = position), inherit.aes = FALSE,
                width = 0.25, linewidth = 0.7, position = dg, show.legend = FALSE) +
  scale_colour_manual(values = pos_cols) +
  facet_wrap(~camp, nrow = 1, scales = "free_x") +
  scale_fill_manual(values = pos_cols, name = "chamber position") +
  scale_y_continuous(trans = "asinh", breaks = c(0, 1, 2, 5, 10, 20, 50, 100)) +
  labs(x = NULL, y = expression("Water-surface CH"[4]*" (nmol m"^-2*" s"^-1*")"), tag = "b") +
  guides(fill = guide_legend(override.aes = list(size = 2.2))) +
  theme_fig(base_size = 8) +
  theme(strip.text = element_text(hjust = 0.5), legend.title = element_text(face = "bold", size = 7),
        legend.text = element_text(size = 7), panel.grid.major.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.x = element_text(lineheight = 0.9))
dir.create("output/figures/other", recursive=TRUE, showWarnings=FALSE)
ggsave("output/figures/other/water_positions_by_campaign.png", p, width=7.2, height=3.2, dpi=300, bg="white")
cat("written output/figures/other/water_positions_by_campaign.png\n\n")

# full labeled table
tab <- w %>% transmute(plot, class, campaign=camp, position,
                       location=ifelse(is.na(collar_location),"",collar_location),
                       water_depth_cm=water_depth, CH4=round(CH4_best.flux,2)) %>%
  arrange(campaign, plot, position)
write.csv(tab, "output/upscaling/supp_water_positions.csv", row.names=FALSE)
cat("=== water flux by position summary (site x campaign) ===\n")
print(w %>% group_by(site, camp, position) %>%
      summarise(n=n(), CH4=round(mean(CH4_best.flux),2), .groups="drop") %>% as.data.frame())
