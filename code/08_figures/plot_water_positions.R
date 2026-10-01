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
off_pat <- "river|open|off peir|off pier|outside|pier|interface|edge"
w <- df %>% filter(component=="water", !is.na(CH4_best.flux), !is.na(camp)) %>%
  mutate(position = case_when(
           is.na(collar_location) ~ "unlabeled",
           grepl(off_pat, collar_location, ignore.case=TRUE) ~ "off-plot / open water",
           TRUE ~ "in-plot"),
         class = recode(plot, CP40="ghost",FLM30="ghost",MI="ghost",
                        BL60="regen",SE1="scrub",SRS5="healthy",SRS6="healthy",RB10="healthy"),
         site = factor(plot, levels=c("CP40","FLM30","BL60","SE1","SRS5","SRS6")),
         camp = factor(camp, levels=c("Mar 2022","Oct 2022","Mar 2023")),
         lab = ifelse(position=="off-plot / open water", gsub(" ","\n",substr(collar_location,1,18)), ""))

pos_cols <- c("in-plot"="#2166ac","off-plot / open water"="#d6604d","unlabeled"="grey60")

p <- ggplot(w, aes(site, CH4_best.flux)) +
  geom_hline(yintercept=0, color="grey70", linewidth=.3) +
  geom_point(aes(color=position, size=position), position=position_jitter(width=.18, height=0), alpha=.85) +
  ggrepel::geom_text_repel(aes(label=lab), size=2, color="#d6604d", max.overlaps=20,
                           segment.size=.2, min.segment.length=0) +
  facet_wrap(~camp, nrow=1, scales="free_x") +
  scale_color_manual(values=pos_cols, name=NULL) +
  scale_size_manual(values=c("in-plot"=2.4,"off-plot / open water"=2.8,"unlabeled"=1.8), guide="none") +
  scale_y_continuous(trans="asinh", breaks=c(0,1,2,5,10,20,50,100)) +
  labs(x=NULL, y=expression("water-surface CH"[4]*" (nmol "*m^-2*" "*s^-1*", asinh)"),
       title="Water-surface CH4 by measurement position (red = off-plot / open water)") +
  theme_bw(base_size=11) + theme(legend.position="bottom", panel.grid.minor=element_blank(),
        strip.text=element_text(face="bold"), axis.text.x=element_text(angle=45,hjust=1))

dir.create("output/figures/other", recursive=TRUE, showWarnings=FALSE)
ggsave("output/figures/other/water_positions_by_campaign.png", p, width=11, height=6, dpi=160)
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
