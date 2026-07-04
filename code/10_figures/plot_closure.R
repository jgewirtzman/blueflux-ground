# =============================================================================
# Fig 8: bottom-up (chambers x TLS, stacked components) vs top-down CARAFE
# (Delaria et al. 2024 two-endmember: ghost forest + mangrove forest).
# CH4 in nmol m-2 s-1; CO2 (NEE) in umol m-2 s-1, daily basis.
# =============================================================================
suppressMessages({library(dplyr);library(tidyr);library(ggplot2)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
f <- 1e-3/16.04*1e9/86400   # mg CH4 m-2 d-1 -> nmol m-2 s-1
CAMP <- c("Oct 2022","Mar 2023")            # wet, dry (chronological)

## ---- CH4 bottom-up components ----
buc <- read.csv("output/upscaling/plot_level_CH4_totals.csv") %>% filter(scenario=="exponential") %>%
  group_by(site,campaign,disturbance_level) %>%
  summarise(across(c(stem_mg,root_mg,soil_mg,water_mg,cwd_mg,total_mg),~mean(.x)),.groups="drop") %>%
  group_by(campaign,disturbance_level) %>%
  summarise(across(c(stem_mg,root_mg,soil_mg,water_mg,cwd_mg,total_mg),~mean(.x)),.groups="drop") %>%
  filter(campaign %in% CAMP) %>%
  transmute(campaign,class=recode(disturbance_level,healthy="Healthy",ghost="Ghost"),
            Stem=stem_mg*f,Root=root_mg*f,Soil=soil_mg*f,Water=water_mg*f,CWD=cwd_mg*f,total=total_mg*f)
mc <- read.csv("output/upscaling/mc_component_uncertainty.csv") %>% filter(component=="total") %>%
  group_by(site,campaign,disturbance_level) %>% summarise(lo=mean(mc_ci_lo),hi=mean(mc_ci_hi),.groups="drop") %>%
  group_by(campaign,disturbance_level) %>% summarise(lo=mean(lo)*f,hi=mean(hi)*f,.groups="drop") %>%
  filter(campaign %in% CAMP) %>%
  transmute(campaign,class=recode(disturbance_level,healthy="Healthy",ghost="Ghost"),lo,hi)
buc_long <- buc %>% select(-total) %>%
  pivot_longer(c(Stem,Root,Soil,Water,CWD),names_to="component",values_to="value") %>%
  mutate(method="Bottom-up",gas="CH4 (nmol m-2 s-1)")
buc_tot <- buc %>% left_join(mc,by=c("campaign","class")) %>%
  transmute(campaign,class,method="Bottom-up",gas="CH4 (nmol m-2 s-1)",total,lo,hi)

## ---- CARAFE CH4 (Delaria): Mar 2023 = mean(Feb, Apr 2023) ----
td <- read.csv("output/carafe_topdown/delaria_endmembers_campaign.csv") %>% filter(gas=="CH4") %>%
  mutate(class=recode(class,mangrove_forest="Healthy",ghost_forest="Ghost"))
td_ch4 <- bind_rows(
  td %>% filter(campaign=="Oct 2022") %>% transmute(campaign,class,total=flux,se),
  td %>% filter(campaign %in% c("Feb 2023","Apr 2023")) %>% group_by(class) %>%
    summarise(total=mean(flux),se=sqrt(mean(se^2)),.groups="drop") %>% mutate(campaign="Mar 2023")) %>%
  mutate(method="CARAFE",gas="CH4 (nmol m-2 s-1)",lo=total-se,hi=total+se,component="CARAFE")

## ---- CO2 bottom-up components (respiration up, GPP down; net = NEE) ----
comp <- read.csv("output/upscaling/summary_CO2_by_component.csv") %>%
  group_by(campaign,disturbance_level) %>%
  summarise(across(c(soil,water,root,stem,cwd,leaf),~mean(.x)),.groups="drop")
gpp <- read.csv("output/upscaling/plot_level_CO2_totals.csv") %>%
  group_by(campaign,disturbance_level) %>% summarise(GPP=mean(GPP_used),NEE=mean(NEE_bottomup),.groups="drop")
co2bu <- comp %>% left_join(gpp,by=c("campaign","disturbance_level")) %>%
  filter(campaign %in% CAMP) %>% mutate(class=recode(disturbance_level,healthy="Healthy",ghost="Ghost"))
co2_long <- co2bu %>% transmute(campaign,class,Soil=soil,Water=water,Root=root,Stem=stem,CWD=cwd,Leaf=leaf,GPP=-GPP) %>%
  pivot_longer(-c(campaign,class),names_to="component",values_to="value") %>%
  mutate(method="Bottom-up",gas="CO2 (umol m-2 s-1)")
co2_net <- co2bu %>% transmute(campaign,class,method="Bottom-up",gas="CO2 (umol m-2 s-1)",total=NEE,lo=NA,hi=NA)

## ---- CARAFE CO2 (daily) ----
tdco <- read.csv("output/carafe_topdown/delaria_CO2_daily_converted.csv") %>%
  mutate(class=recode(class,mangrove_forest="Healthy",ghost_forest="Ghost"))
tdco2 <- bind_rows(
  tdco %>% filter(campaign=="Oct 2022") %>% transmute(campaign,class,total=daily,se=daily_se),
  tdco %>% filter(campaign %in% c("Feb 2023","Apr 2023")) %>% group_by(class) %>%
    summarise(total=mean(daily),se=sqrt(mean(daily_se^2)),.groups="drop") %>% mutate(campaign="Mar 2023")) %>%
  mutate(method="CARAFE",gas="CO2 (umol m-2 s-1)",lo=total-se,hi=total+se,component="CARAFE")

## ---- assemble & plot ----
lev_comp <- c("Water","Soil","Root","Stem","CWD","Leaf","GPP","CARAFE")
# established component palette (matches pub_component_budget / decomposition figs)
pal <- c(Water="#4682B4",Soil="#8B4513",Root="#D2691E",Stem="#228B22",CWD="#808080",
         Leaf="#E6AB02",GPP="#006837",CARAFE="grey40")
stack_long <- bind_rows(buc_long,co2_long) %>%
  mutate(component=factor(component,levels=lev_comp),
         campaign=factor(campaign,levels=CAMP))
carafe <- bind_rows(td_ch4,tdco2) %>% mutate(campaign=factor(campaign,levels=CAMP))
net_bu <- bind_rows(buc_tot,co2_net) %>% mutate(campaign=factor(campaign,levels=CAMP))
xof <- function(cl,meth) as.numeric(factor(cl,levels=c("Ghost","Healthy")))+ifelse(meth=="Bottom-up",-0.21,0.21)
stack_long$x <- xof(stack_long$class,stack_long$method)
carafe$x     <- xof(carafe$class,carafe$method)
net_bu$x     <- xof(net_bu$class,net_bu$method)

dir.create("output/carafe_topdown/figures",showWarnings=FALSE)
mk_closure <- function(g, ylab){
  ggplot()+
    geom_hline(yintercept=0,color="grey55",linewidth=.3)+
    geom_col(data=stack_long %>% filter(gas==g),aes(x,value,fill=component),width=0.38)+
    geom_col(data=carafe %>% filter(gas==g),aes(x,total,fill=component),width=0.38,color="black",linewidth=.25)+
    geom_errorbar(data=carafe %>% filter(gas==g),aes(x,ymin=lo,ymax=hi),width=.12,linewidth=.4)+
    geom_errorbar(data=net_bu %>% filter(gas==g,!is.na(lo)),aes(x,ymin=lo,ymax=hi),width=.12,linewidth=.4)+
    geom_point(data=net_bu %>% filter(gas==g),aes(x,total),shape=23,fill="white",size=2.4,stroke=.7)+
    facet_wrap(~campaign,nrow=1)+
    scale_x_continuous(breaks=c(1,2),labels=c("Ghost","Healthy"))+
    scale_fill_manual(values=pal,name=NULL)+
    labs(x=NULL,y=ylab)+
    theme_bw(base_size=14)+theme(legend.position="bottom",panel.grid.minor=element_blank(),
         strip.text=element_text(face="bold"))
}
pc <- mk_closure("CH4 (nmol m-2 s-1)", expression("CH"[4]*" flux (nmol "*m^-2*" "*s^-1*")"))
po <- mk_closure("CO2 (umol m-2 s-1)", expression("CO"[2]*" NEE (umol "*m^-2*" "*s^-1*")"))
ggsave("output/figures/presentation/12a_closure_CH4.png",pc,width=8.5,height=5,dpi=200)
ggsave("output/figures/presentation/12b_closure_CO2.png",po,width=8.5,height=5,dpi=200)
ggsave("output/figures/other/closure_CH4.pdf",pc,width=8.5,height=5)
ggsave("output/figures/other/closure_CO2.pdf",po,width=8.5,height=5)
cat("written closure CH4 + CO2 separately\n")

## ---- MERGED closure + forcing figure (revised Fig 6) ----
suppressMessages(library(patchwork))
f<-read.csv("output/upscaling/net_forcing_by_class.csv") %>%
  mutate(class=factor(recode(disturbance_level,healthy="Healthy",ghost="Ghost"),levels=c("Healthy","Ghost")))
flong<-bind_rows(
  f %>% transmute(class,horizon="GWP100",CO2=co2_g_yr,CH4=ch4_co2eq100),
  f %>% transmute(class,horizon="GWP20", CO2=co2_g_yr,CH4=ch4_co2eq20)) %>%
  tidyr::pivot_longer(c(CO2,CH4),names_to="gas",values_to="val") %>%
  mutate(horizon=factor(horizon,levels=c("GWP100","GWP20")))
fnet<-bind_rows(f %>% transmute(class,horizon="GWP100",net=net100),
                f %>% transmute(class,horizon="GWP20",net=net20)) %>%
  mutate(horizon=factor(horizon,levels=c("GWP100","GWP20")))
th6<-theme_bw(base_size=13)+theme(legend.position="bottom",panel.grid.minor=element_blank(),
      strip.text=element_text(face="bold"),plot.tag=element_text(face="bold"))
p_net<-ggplot()+geom_hline(yintercept=0,color="grey55",linewidth=.3)+
  geom_col(data=flong,aes(class,val,fill=gas),width=.6)+
  geom_point(data=fnet,aes(class,net),shape=23,size=3,fill="white",stroke=.8)+
  facet_wrap(~horizon)+scale_fill_manual(values=c(CO2="#2166ac",CH4="#d6604d"),name=NULL)+
  labs(x=NULL,y=expression("Forcing (g "*CO[2]*"-eq "*m^-2*" "*yr^-1*")"),tag="c")+th6
share<-bind_rows(f %>% transmute(class,horizon="GWP100",pct=ch4_pct100),
                 f %>% transmute(class,horizon="GWP20",pct=ch4_pct20)) %>%
  mutate(horizon=factor(horizon,levels=c("GWP100","GWP20")))
p_share<-ggplot(share,aes(class,pct,fill=horizon))+
  geom_col(position=position_dodge(.7),width=.6,color="grey30",linewidth=.2)+
  geom_text(aes(label=paste0(round(pct),"%")),position=position_dodge(.7),vjust=-.4,size=3.5)+
  scale_fill_manual(values=c(GWP100="#bdbdbd",GWP20="#636363"),name=NULL)+
  labs(x=NULL,y=expression("CH"[4]*" share of forcing (%)"),tag="d")+th6+
  coord_cartesian(ylim=c(0,46))
pc2<-pc+labs(tag="a"); po2<-po+labs(tag="b")
merged<-(pc2|po2)/(p_net|p_share)+plot_layout(heights=c(1,0.95))
ggsave("output/figures/presentation/FigClosureForcing.png",merged,width=12,height=10,dpi=200)
ggsave("output/figures/other/FigClosureForcing.pdf",merged,width=12,height=10)
cat("written merged FigClosureForcing\n")
