# Regenerate SA-by-segment-class-and-height on CONSISTENT (fixed) y scales.
suppressMessages({library(dplyr);library(ggplot2)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
TLS<-Sys.getenv("BLUEFLUX_TLS_DIR", "data/tls")
sa<-read.csv(file.path(TLS,"all_sites_summary.csv")) %>%
  mutate(height_m=height_bin_num,
         segment_label=factor(segment_class,levels=c("root","trunk","branch"),
                              labels=c("Root","Trunk","Branch")),
         site=factor(site,levels=c("CP40","FLM30","SRS5","SRS6")))
seg_colors<-c("Root"="#D2691E","Trunk"="#228B22","Branch"="#90EE90")
p<-ggplot(sa,aes(height_m,Total_surface_area_m2,fill=segment_label))+
  geom_col(position=position_stack(reverse=TRUE),width=0.45,color="grey40",linewidth=.1)+
  geom_vline(xintercept=1.5,linetype="dashed",color="grey40",linewidth=.4)+
  facet_wrap(~site,scales="free_x",nrow=1)+           # fixed y, free x (sites differ in height)
  scale_fill_manual(values=seg_colors,name="Segment")+
  labs(x="Height above ground (m); dashed line = 1.5 m chamber limit",
       y=expression("Surface area ("*m^2*" per 0.5 m bin)"))+
  theme_bw(base_size=14)+theme(legend.position="right",panel.grid.minor=element_blank(),
        strip.text=element_text(face="bold"))
ggsave("output/figures/other/SA_by_segment_height_fixedY.pdf",p,width=11,height=4.5)
ggsave("output/figures/other/SA_by_segment_height_fixedY.png",p,width=11,height=4.5,dpi=200)
ggsave("output/figures/presentation/06d_SA_by_height.png",p,width=11,height=4.5,dpi=200)
cat("written SA_by_segment_height_fixedY\n")
