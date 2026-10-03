# =============================================================================
# Presentation figures (title-free): tide handling, budget decomposition split
# into panels (tide-averaged 50/50 + exponential stem extrapolation), and the
# component budget split into fluxes + stacked panels.
# =============================================================================
suppressMessages({library(dplyr);library(tidyr);library(ggplot2)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
PD<-"output/presentation_figures"
lev<-c("water","soil","root","cwd","stem_measured","stem_extrapolated","stem")
lab<-c(water="Water",soil="Soil",root="Root",cwd="CWD",
       stem_measured="Stem (meas.)",stem_extrapolated="Stem (extrap.)",stem="Stem")
pal<-c("Water"="#4682B4","Soil"="#8B4513","Root"="#D2691E","CWD"="#808080",
       "Stem (meas.)"="#228B22","Stem (extrap.)"="#90EE90","Stem"="#228B22")
sites<-c("CP40","FLM30","SRS5","SRS6")
th<-theme_bw(base_size=15)+theme(legend.position="bottom",panel.grid.minor=element_blank(),
     strip.text=element_text(face="bold"),plot.title=element_blank(),plot.subtitle=element_blank())
compf<-function(x) factor(lab[x],levels=lab[lev])

## ================= TIDE HANDLING =================
tide<-data.frame(
  state=c("Ghost\n(always flooded)","Healthy\nhigh tide","Healthy\nlow tide","Healthy\n50/50 (used)"),
  Water=c(1,1,0,0.5), Soil=c(0,0,1,0.5)) %>%
  pivot_longer(c(Water,Soil),names_to="surface",values_to="frac") %>%
  mutate(state=factor(state,levels=c("Ghost\n(always flooded)","Healthy\nhigh tide","Healthy\nlow tide","Healthy\n50/50 (used)")),
         surface=factor(surface,levels=c("Water","Soil")))
pt<-ggplot(tide,aes(state,frac,fill=surface))+
  geom_col(width=.7,color="grey30",linewidth=.2)+
  scale_fill_manual(values=c(Water="#4682B4",Soil="#8B4513"),name=NULL)+
  labs(x=NULL,y="Ground-surface fraction")+th
ggsave(file.path(PD,"06e_tide_handling.png"),pt,width=8,height=5,dpi=200)

## ================= DECOMPOSITION (Oct 2022, tide-averaged) =================
b<-read.csv("output/upscaling/budget_decomposition.csv") %>% filter(campaign=="Oct 2022")
ta<-b %>% group_by(site,disturbance_level,component) %>%
  summarise(flux_rate=mean(flux_rate,na.rm=TRUE),ci_lo=mean(flux_ci_lo,na.rm=TRUE),
            ci_hi=mean(flux_ci_hi,na.rm=TRUE),sa=mean(sa_per_m2,na.rm=TRUE),
            mg=mean(total_mg_m2_d,na.rm=TRUE),.groups="drop") %>%
  mutate(comp=compf(component),site=factor(site,levels=sites))

# (rate) — non-stem components have a per-area rate
rate<-ta %>% filter(!is.na(flux_rate),!grepl("stem",component))
pr<-ggplot(rate,aes(comp,flux_rate,fill=comp))+
  geom_col(color="grey30",linewidth=.2,width=.7)+
  geom_errorbar(aes(ymin=ci_lo,ymax=ci_hi),width=.25,na.rm=TRUE)+
  facet_wrap(~site,nrow=1,scales="free_y")+scale_fill_manual(values=pal,guide="none")+
  labs(x=NULL,y=expression("Flux rate (nmol "*m^-2*" "*s^-1*")"))+th+
  theme(axis.text.x=element_text(angle=40,hjust=1))
ggsave(file.path(PD,"11c_dec_rate.png"),pr,width=11,height=4.5,dpi=200)

# (area) — surface-area density per ground area (tide-averaged)
area<-ta %>% filter(!grepl("stem",component))
parea<-ggplot(area,aes(comp,sa,fill=comp))+
  geom_col(color="grey30",linewidth=.2,width=.7)+
  facet_wrap(~site,nrow=1)+scale_fill_manual(values=pal,guide="none")+
  labs(x=NULL,y=expression("Surface area ("*m^2*" "*m^-2*" ground)"))+th+
  theme(axis.text.x=element_text(angle=40,hjust=1))
ggsave(file.path(PD,"11d_dec_area.png"),parea,width=11,height=4.5,dpi=200)

# (integrated) — rate x area = integrated flux, stacked
pint<-ggplot(ta,aes(site,mg,fill=comp))+
  geom_col(width=.7,color="grey30",linewidth=.15)+
  scale_fill_manual(values=pal,name=NULL)+
  labs(x=NULL,y=expression("CH"[4]*" (mg "*m^-2*" "*d^-1*")"))+th
ggsave(file.path(PD,"11e_dec_integrated.png"),pint,width=8,height=5,dpi=200)

# (percent) — % contribution
pct<-ta %>% group_by(site) %>% mutate(pct=100*mg/sum(mg,na.rm=TRUE)) %>% ungroup()
ppct<-ggplot(pct,aes(site,pct,fill=comp))+
  geom_col(width=.7,color="grey30",linewidth=.15)+
  scale_fill_manual(values=pal,name=NULL)+
  labs(x=NULL,y="% of plot CH4 budget")+th
ggsave(file.path(PD,"11f_dec_percent.png"),ppct,width=8,height=5,dpi=200)

## ================= COMPONENT BUDGET (both campaigns) =================
mc<-read.csv("output/upscaling/mc_component_uncertainty.csv") %>%
  filter(component!="total",campaign %in% c("Oct 2022","Mar 2023")) %>%
  group_by(site,campaign,disturbance_level,component) %>%
  summarise(mg=mean(mc_mean),lo=mean(mc_ci_lo),hi=mean(mc_ci_hi),.groups="drop") %>%
  mutate(comp=compf(component),site=factor(site,levels=sites),
         campaign=factor(campaign,levels=c("Oct 2022","Mar 2023")))
# (a) component fluxes with CI
pbf<-ggplot(mc,aes(comp,mg,fill=comp))+
  geom_col(color="grey30",linewidth=.2,width=.7)+
  geom_errorbar(aes(ymin=lo,ymax=hi),width=.25,na.rm=TRUE)+
  facet_grid(campaign~site,scales="free_y")+scale_fill_manual(values=pal,guide="none")+
  labs(x=NULL,y=expression("CH"[4]*" (mg "*m^-2*" "*d^-1*")"))+th+
  theme(axis.text.x=element_text(angle=40,hjust=1))
ggsave(file.path(PD,"11a_budget_fluxes.png"),pbf,width=11,height=6,dpi=200)

# (b) stacked budget with MC 95% CI on total
stk<-mc %>% group_by(site,campaign) %>% summarise(across(c(comp),~NULL),.groups="drop")  # placeholder
tot<-read.csv("output/upscaling/mc_component_uncertainty.csv") %>% filter(component=="total") %>%
  group_by(site,campaign) %>% summarise(lo=mean(mc_ci_lo),hi=mean(mc_ci_hi),.groups="drop") %>%
  filter(campaign %in% c("Oct 2022","Mar 2023")) %>%
  mutate(site=factor(site,levels=sites),campaign=factor(campaign,levels=c("Oct 2022","Mar 2023")))
pbs<-ggplot(mc,aes(campaign,mg,fill=comp))+
  geom_col(width=.65,color="grey30",linewidth=.15)+
  geom_errorbar(data=tot,aes(campaign,ymin=lo,ymax=hi),inherit.aes=FALSE,width=.2)+
  facet_wrap(~site,nrow=1,scales="free_y")+scale_fill_manual(values=pal,name=NULL)+
  labs(x=NULL,y=expression("CH"[4]*" (mg "*m^-2*" ground "*d^-1*")"))+th+
  theme(axis.text.x=element_text(angle=35,hjust=1))
ggsave(file.path(PD,"11b_budget_stacked.png"),pbs,width=11,height=4.8,dpi=200)
cat("written tide + decomposition + budget panels (title-free)\n")
