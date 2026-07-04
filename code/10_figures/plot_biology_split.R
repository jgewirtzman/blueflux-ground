# Split biology panels (title-free, established colors): stem CH4 by height
# (distribution) and by species. Disturbance palette matches the repo scheme.
suppressMessages({library(dplyr);library(ggplot2);library(scales)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
set.seed(1)
PD<-"output/presentation_figures"
asinh_trans<-trans_new("asinh",asinh,sinh)
dist_pal<-c(Healthy="#228B22",Regenerating="#999999",Ghost="#8B4513")
th<-theme_bw(base_size=15)+theme(legend.position="bottom",panel.grid.minor=element_blank(),
     plot.title=element_blank(),plot.subtitle=element_blank(),strip.text=element_text(face="bold"))
boot_ci<-function(x,n=2000){x<-x[!is.na(x)];if(length(x)<2)return(c(mean(x),NA,NA));
  m<-replicate(n,mean(sample(x,replace=TRUE)));c(mean(x),unname(quantile(m,c(.025,.975))))}

st<-read.csv("output/data_products/combined_gas_flux_dataset.csv") %>%
  filter(component=="stem",!is.na(CH4_best.flux),!is.na(height),
         disturbance_level %in% c("ghost","healthy","regenerating")) %>%
  mutate(Class=factor(recode(disturbance_level,healthy="Healthy",regenerating="Regenerating",ghost="Ghost"),
                      levels=c("Healthy","Regenerating","Ghost")),
         hbin=cut(height,breaks=c(-Inf,50,100,150,Inf),
                  labels=c("0-50","50-100","100-150",">150 cm")))

## (a) stem CH4 by height x disturbance: bootstrapped mean +/- CI (robust; shows decline)
hd<-st %>% filter(!is.na(hbin)) %>% group_by(Class,hbin) %>%
  reframe(as.data.frame(t(boot_ci(CH4_best.flux))) %>% setNames(c("m","lo","hi")))
pa<-ggplot(hd,aes(m,hbin,color=Class))+
  geom_vline(xintercept=0,linetype=2,color="grey60")+
  geom_errorbarh(aes(xmin=lo,xmax=hi),height=.22,position=position_dodge(.6),linewidth=.8)+
  geom_point(size=3.4,position=position_dodge(.6))+
  scale_color_manual(values=dist_pal,name=NULL)+
  scale_x_continuous(trans=asinh_trans,breaks=c(0,1,5,20,100))+
  labs(x=expression("Stem CH"[4]*" flux (nmol "*m^-2*" "*s^-1*")"),y="Measurement height")+th
ggsave(file.path(PD,"09a_stem_by_height.png"),pa,width=8,height=5,dpi=200)

## (b) stem CH4 by species: bootstrapped mean +/- CI
sp_lab<-c(RHMA="R. mangle",AVGE="A. germinans",LARA="L. racemosa",COER="C. erectus")
sd<-st %>% filter(species %in% names(sp_lab)) %>% mutate(Species=sp_lab[species]) %>%
  group_by(Species) %>% reframe(as.data.frame(t(boot_ci(CH4_best.flux))) %>% setNames(c("m","lo","hi"))) %>%
  arrange(m) %>% mutate(Species=factor(Species,levels=Species))
pb<-ggplot(sd,aes(m,Species))+
  geom_vline(xintercept=0,linetype=2,color="grey60")+
  geom_errorbarh(aes(xmin=lo,xmax=hi),height=.2,linewidth=.8,color="#33691e")+
  geom_point(size=3.6,color="#33691e")+
  scale_x_continuous(trans=asinh_trans,breaks=c(0,1,3,10))+
  labs(x=expression("Stem CH"[4]*" flux (nmol "*m^-2*" "*s^-1*")"),y=NULL)+th
ggsave(file.path(PD,"09b_stem_by_species.png"),pb,width=8,height=4.2,dpi=200)
cat("written biology split panels\n")
