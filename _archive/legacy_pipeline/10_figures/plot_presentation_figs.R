# =============================================================================
# Presentation-optimized figures: simplified flux results + biology (stem).
# =============================================================================
suppressMessages({library(dplyr);library(tidyr);library(ggplot2);library(scales)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
set.seed(1)
PD<-"output/presentation_figures"
asinh_trans<-trans_new("asinh",asinh,sinh)
dist_pal<-c(Healthy="#1a9850",Regenerating="#f1a340",Ghost="#762a83")
theme_pres<-theme_bw(base_size=15)+theme(legend.position="bottom",panel.grid.minor=element_blank(),
   plot.title=element_text(face="bold"),strip.text=element_text(face="bold"))
boot_ci<-function(x,n=2000){x<-x[!is.na(x)];if(length(x)<2)return(c(mean(x),NA,NA));
  m<-replicate(n,mean(sample(x,replace=TRUE)));c(mean(x),unname(quantile(m,c(.025,.975))))}

d<-read.csv("output/data_products/combined_gas_flux_dataset.csv") %>%
  filter(disturbance_level %in% c("ghost","healthy","regenerating")) %>%
  mutate(Class=factor(recode(disturbance_level,healthy="Healthy",regenerating="Regenerating",ghost="Ghost"),
                      levels=c("Healthy","Regenerating","Ghost")),
         component=recode(component,pneumatophore="soil"))

## ---------- 1. Flux results: component x class, bootstrapped mean+CI ----------
comp_lev<-c("leaves","cwd","root","stem","water","soil")
flux_summ<-function(gas){
  d %>% filter(component %in% comp_lev, !is.na(.data[[gas]])) %>%
    group_by(Class,component) %>%
    reframe(as.data.frame(t(boot_ci(.data[[gas]]))) %>% setNames(c("m","lo","hi"))) %>%
    mutate(component=factor(component,levels=comp_lev))
}
ch4<-flux_summ("CH4_best.flux") %>% mutate(gas="CH4 flux (nmol m-2 s-1)")
co2<-flux_summ("CO2_best.flux") %>% mutate(gas="CO2 flux (umol m-2 s-1)")

pflux<-function(dat,ttl){
  ggplot(dat,aes(m,component,color=Class))+
    geom_vline(xintercept=0,linetype=2,color="grey60")+
    geom_errorbarh(aes(xmin=lo,xmax=hi),height=.25,position=position_dodge(.6),linewidth=.7)+
    geom_point(size=3,position=position_dodge(.6))+
    scale_color_manual(values=dist_pal)+scale_x_continuous(trans=asinh_trans,breaks=c(0,1,10,100))+
    labs(x=NULL,y=NULL,color=NULL,title=ttl)+theme_pres}
# CH4-only
ggsave(file.path(PD,"08_flux_results_CH4.png"),
       pflux(ch4,"Component CH4 fluxes across the disturbance gradient")+
         labs(x="CH4 flux (nmol m-2 s-1)  [asinh scale]"),width=8,height=5,dpi=200)
# CH4 + CO2
both<-bind_rows(ch4,co2)
pb<-ggplot(both,aes(m,component,color=Class))+
  geom_vline(xintercept=0,linetype=2,color="grey60")+
  geom_errorbarh(aes(xmin=lo,xmax=hi),height=.25,position=position_dodge(.6),linewidth=.7)+
  geom_point(size=2.6,position=position_dodge(.6))+
  facet_wrap(~gas,scales="free_x")+
  scale_color_manual(values=dist_pal)+scale_x_continuous(trans=asinh_trans)+
  labs(x="flux  [asinh scale]",y=NULL,color=NULL,
       title="Component CH4 and CO2 fluxes across the disturbance gradient")+theme_pres
ggsave(file.path(PD,"08_flux_results_CH4_CO2.png"),pb,width=11,height=5,dpi=200)

## ---------- 2. Biology: stem CH4 vs height + alive/dead ----------
st<-d %>% filter(component=="stem",!is.na(CH4_best.flux))
h<-st$height; if(median(h,na.rm=TRUE)<10) h<-h*100      # m -> cm if needed
st$hbin<-cut(h,breaks=c(-Inf,50,100,150,Inf),labels=c("0-50","50-100","100-150",">150 cm"))
# panel A: height x class
pa_d<-st %>% filter(!is.na(hbin)) %>% group_by(Class,hbin) %>%
  reframe(as.data.frame(t(boot_ci(CH4_best.flux))) %>% setNames(c("m","lo","hi")))
pA<-ggplot(pa_d,aes(m,hbin,color=Class))+
  geom_vline(xintercept=0,linetype=2,color="grey60")+
  geom_errorbarh(aes(xmin=lo,xmax=hi),height=.22,position=position_dodge(.55),linewidth=.7)+
  geom_point(size=3,position=position_dodge(.55))+
  scale_color_manual(values=dist_pal)+scale_x_continuous(trans=asinh_trans,breaks=c(0,1,5,20,100))+
  labs(x="Stem CH4 flux (nmol m-2 s-1)  [asinh]",y="Measurement height",color=NULL,
       title="Stem CH4 declines with height, elevated in disturbed forest")+theme_pres
# height beat only (clean, height effect shown directly). Alive/dead beat = use the
# model-based emmeans figure 09c_alive_dead.png (height-controlled; raw means are
# confounded because dead stems were measured lower in the canopy).
ggsave(file.path(PD,"09_biology_stem_height.png"),pA,width=9,height=5,dpi=200)
cat("written presentation flux + biology (height) figures\n")
