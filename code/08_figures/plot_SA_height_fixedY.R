# Fig. S8 | TLS woody surface area per unit ground area, per 0.5 m height bin, by plot and
# segment class (prop root, stem, branch), on shared axes. Dashed line = 1.5 m chamber
# height limit. Plots ordered and labelled by forest class.
suppressMessages({library(dplyr);library(ggplot2)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
dir.create("output/figures/other", recursive = TRUE, showWarnings = FALSE)
TLS<-Sys.getenv("BLUEFLUX_TLS_DIR", "data/tls")
site_class<-c(SRS5="intact",SRS6="intact",CP40="ghost",FLM30="ghost")
sa<-read.csv(file.path(TLS,"all_sites_summary.csv")) %>%
  mutate(height_m=height_bin_num,
         segment_label=factor(segment_class,levels=c("root","trunk","branch"),
                              labels=c("prop root","stem","branch")),
         site=factor(site,levels=names(site_class)))
# per unit ground area (plot areas differ by up to 24%), as in Fig. 2
pa<-read.csv("output/upscaling/plot_level_CH4_totals.csv") %>% distinct(site,plot_area_m2)
sa<-sa %>% mutate(site=as.character(site)) %>% left_join(pa,by="site") %>% mutate(sa_g=Total_surface_area_m2/plot_area_m2, site=factor(site,levels=names(site_class)))
tot<-sa %>% group_by(site) %>% summarise(all=sum(sa_g), above=sum(sa_g[height_m>=1.5]), .groups="drop")
strip_lab0<-setNames(sprintf("%s (%s)\n%.1f m\u00b2 m\u207b\u00b2; %.0f%% above 1.5 m", tot$site, site_class[as.character(tot$site)],
                             tot$all, 100*tot$above/tot$all), as.character(tot$site))
seg_colors<-c(`prop root`=pal_comp[["prop root"]],stem=pal_comp[["stem"]],branch="#B59A6A")   # branch as Fig. 2
strip_lab<-strip_lab0
p<-ggplot(sa,aes(sa_g,height_m+0.25,fill=segment_label))+
  geom_col(orientation="y",position=position_stack(reverse=TRUE),width=0.45,colour=NA)+
  geom_hline(yintercept=1.5,linetype="dashed",colour="grey35",linewidth=.35)+
  ggh4x::facet_wrap2(~site,nrow=1,labeller=as_labeller(strip_lab),
                     strip=ggh4x::strip_themed(text_x=lapply(pal_class[site_class],function(cc)
                       element_text(colour=cc,face="bold",size=7.5,hjust=0.5,lineheight=0.95))))+
  scale_fill_manual(values=seg_colors,name="woody surface")+
  scale_x_continuous(expand=expansion(mult=c(0,0.04)))+
  scale_y_continuous(breaks=seq(0,20,2),expand=c(0,0))+
  labs(x=expression("Woody surface (m"^2*" m"^-2*" ground per 0.5 m)"),y="Height above ground (m)")+
  theme_fig()+theme(panel.grid.major.y=element_blank(),panel.spacing.x=unit(10,"pt"),
                    legend.title=element_text(size=7,face="bold"),legend.text=element_text(size=7),
                    legend.margin=margin(0,0,0,0),legend.box.spacing=unit(4,"pt"))
ggsave("output/figures/other/SA_by_segment_height_fixedY.pdf",p,width=7.2,height=3.6,device=cairo_pdf)
ggsave("output/figures/other/SA_by_segment_height_fixedY.png",p,width=7.2,height=3.6,dpi=300,bg="white")
ggsave("output/figures/presentation/06d_SA_by_height.png",p,width=7.2,height=3.6,dpi=300,bg="white")
cat("written SA_by_segment_height_fixedY\n")
