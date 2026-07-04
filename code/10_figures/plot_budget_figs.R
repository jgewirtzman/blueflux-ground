# =============================================================================
# Fig 4: TLS component surface areas per unit ground area, by plot/class.
# Fig 5: bottom-up CH4 budget by component (stacked), by site/season, MC CIs.
# =============================================================================
suppressMessages({library(dplyr);library(tidyr);library(ggplot2)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
TLS<-Sys.getenv("BLUEFLUX_TLS_DIR", "data/tls")
f<-1e-3/16.04*1e9/86400   # (not used; CH4 kept in mg/m2/d for Fig 5)
site_lev<-c("CP40","FLM30","SRS5","SRS6")
cls<-c(CP40="Ghost",FLM30="Ghost",SRS5="Healthy",SRS6="Healthy")

## ---- Fig 4: surface areas ----
tls<-read.csv(file.path(TLS,"all_sites_summary.csv"))
ts<-read.csv(file.path(TLS,"tree_stats_per_site.csv"))
sa<-tls %>% mutate(grp=ifelse(segment_class %in% c("trunk","branch"),"Stem (trunk+branch)",
                              ifelse(segment_class=="root","Root",NA))) %>%
  filter(!is.na(grp)) %>% group_by(site,grp) %>%
  summarise(SA=sum(Total_surface_area_m2,na.rm=TRUE),.groups="drop") %>%
  left_join(ts %>% select(site,area_m2),by="site") %>%
  mutate(SA_per_ground=SA/area_m2, class=cls[site],
         site=factor(site,levels=site_lev),
         grp=factor(grp,levels=c("Stem (trunk+branch)","Root")))
p4<-ggplot(sa,aes(site,SA_per_ground,fill=grp))+
  geom_col(position=position_dodge(.7),width=.6,color="grey20",linewidth=.2)+
  facet_grid(~class,scales="free_x",space="free_x")+
  scale_fill_manual(values=c("Stem (trunk+branch)"="#228B22","Root"="#D2691E"),name=NULL)+
  labs(x=NULL,y=expression("Surface area per ground area ("*m^2*" "*m^-2*")"))+
  theme_bw(base_size=14)+theme(legend.position="bottom",panel.grid.minor=element_blank(),
        strip.text=element_text(face="bold"))
ggsave("output/figures/other/pub_surface_areas.pdf",p4,width=7,height=4.5)
ggsave("output/figures/other/pub_surface_areas.png",p4,width=7,height=4.5,dpi=200)

## ---- Fig 5: CH4 component budget ----
comp<-read.csv("output/upscaling/plot_level_CH4_totals.csv") %>% filter(scenario=="exponential") %>%
  group_by(site,campaign) %>%
  summarise(Stem=mean(stem_mg),Root=mean(root_mg),Soil=mean(soil_mg),
            Water=mean(water_mg),CWD=mean(cwd_mg),total=mean(total_mg),.groups="drop")
mc<-read.csv("output/upscaling/mc_component_uncertainty.csv") %>% filter(component=="total") %>%
  group_by(site,campaign) %>% summarise(lo=mean(mc_ci_lo),hi=mean(mc_ci_hi),.groups="drop")
long<-comp %>% select(-total) %>%
  pivot_longer(c(Water,Soil,Root,Stem,CWD),names_to="component",values_to="mg") %>%
  mutate(component=factor(component,levels=c("Water","Soil","Root","Stem","CWD")),
         class=cls[site], site=factor(site,levels=site_lev),
         campaign=factor(campaign,levels=c("Mar 2022","Oct 2022","Mar 2023")))
tot<-comp %>% left_join(mc,by=c("site","campaign")) %>%
  mutate(class=cls[site],site=factor(site,levels=site_lev),
         campaign=factor(campaign,levels=c("Mar 2022","Oct 2022","Mar 2023")))
pal<-c(Water="#4682B4",Soil="#8B4513",Root="#D2691E",Stem="#228B22",CWD="#808080")
p5<-ggplot()+
  geom_col(data=long,aes(campaign,mg,fill=component),width=.65,color="grey30",linewidth=.15)+
  geom_errorbar(data=tot,aes(campaign,ymin=lo,ymax=hi),width=.2,linewidth=.35)+
  facet_grid(~site)+
  scale_fill_manual(values=pal,name=NULL)+
  labs(x=NULL,y=expression("CH"[4]*" emission (mg "*CH[4]*" "*m^-2*" ground "*d^-1*")"))+
  theme_bw(base_size=13)+theme(legend.position="bottom",panel.grid.minor=element_blank(),
        strip.text=element_text(face="bold"),axis.text.x=element_text(angle=35,hjust=1))
ggsave("output/figures/other/pub_component_budget_v2.pdf",p5,width=9,height=4.8)
ggsave("output/figures/other/pub_component_budget_v2.png",p5,width=9,height=4.8,dpi=200)
cat("written pub_surface_areas + pub_component_budget_v2\n")
