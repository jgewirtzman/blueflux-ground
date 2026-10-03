# Fig. S7a | Stem-flux height extrapolation: measured stem CH4 below the
# ~1.5 m chamber limit, exponential-decay fit per forest class (fit to positive
# fluxes <= 1.5 m) extrapolated above it (clamped >= 0). One panel per class.
suppressMessages({library(dplyr);library(ggplot2)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
dir.create("output/figures/other", recursive = TRUE, showWarnings = FALSE)
d<-read.csv("output/data_products/combined_gas_flux_dataset.csv") %>%
  filter(component=="stem", disturbance_level %in% c("ghost","healthy","regenerating"),
         !is.na(CH4_best.flux), !is.na(height_corrected)) %>%
  mutate(h=height_corrected/100,                                       # cm -> m; same datum as the upscaling (02_upscale_methane.R)
         Class=factor(class_labels[disturbance_level], levels=names(pal_class)))
Hmax<-1.5; Htop<-8
# exponential-decay fit per class on measured (<=Hmax) positive fluxes; predict + clamp>=0
grid<-do.call(rbind,lapply(levels(d$Class),function(cl){
  dc<-d %>% filter(Class==cl, h<=Hmax, CH4_best.flux>0)
  if(nrow(dc)<5) return(NULL)
  fit<-lm(log(CH4_best.flux)~h,data=dc)
  hh<-seq(0,Htop,0.1)
  data.frame(Class=cl,h=hh,pred=pmax(exp(predict(fit,newdata=data.frame(h=hh))),0),
             measured=hh<=Hmax)
}))
grid$Class<-factor(grid$Class,levels=levels(d$Class))
ytop<-asinh(max(d$CH4_best.flux[d$h<=Hmax],na.rm=TRUE))
lab<-data.frame(Class=factor(levels(d$Class)[1],levels(d$Class)),x=Hmax+0.2,y=ytop,
                label="extrapolated")
p<-ggplot()+
  annotate("rect",xmin=Hmax,xmax=Htop,ymin=-Inf,ymax=Inf,fill="grey94")+
  geom_vline(xintercept=Hmax,linetype="dashed",colour="grey45",linewidth=0.3)+
  geom_text(data=lab,aes(x,y,label=label),hjust=0,vjust=1,size=2.4,colour="grey35")+
  geom_point(data=d %>% filter(h<=Hmax),aes(h,asinh(CH4_best.flux),colour=Class),alpha=.45,size=0.7,stroke=0)+
  geom_line(data=grid %>% filter(measured),aes(h,asinh(pred),colour=Class),linewidth=0.8)+
  geom_line(data=grid %>% filter(!measured),aes(h,asinh(pred),colour=Class),linewidth=0.8,linetype="22")+
  scale_colour_manual(values=pal_class,guide="none")+
  scale_x_continuous(breaks=0:8,expand=c(0,0))+
  scale_y_continuous(breaks=asinh(c(-1,0,1,10,100)),labels=c(-1,0,1,10,100))+
  coord_cartesian(xlim=c(0,Htop))+
  labs(x="Height on stem (m)",y=expression("Stem CH"[4]*" flux (nmol m"^-2*" s"^-1*")"),tag="a")+
  theme_fig()+theme(strip.text=element_text(hjust=0.5,size=8),panel.spacing.x=unit(10,"pt"),
                    plot.tag.position=c(0,1))
# strip text coloured by class
p<-p+ggh4x::facet_wrap2(~Class,nrow=1,strip=ggh4x::strip_themed(
  text_x=lapply(pal_class,function(cc) element_text(colour=cc,face="bold",size=8))))
ggsave("output/figures/other/stem_extrap_clean.pdf",p,width=7.2,height=2.5,device=cairo_pdf)
ggsave("output/figures/other/stem_extrap_clean.png",p,width=7.2,height=2.5,dpi=300,bg="white")
ggsave("output/figures/presentation/09e_stem_extrapolation.png",p,width=7.2,height=2.5,dpi=300,bg="white")
cat("written stem_extrap_clean\n")
