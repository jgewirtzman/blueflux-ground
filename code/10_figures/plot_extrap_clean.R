# Clean presentation figure of the stem-flux height-extrapolation approach:
# measured stem CH4 below the ~1.5 m chamber limit, exponential-decay fit
# extrapolated above it (clamped >= 0). By disturbance class.
suppressMessages({library(dplyr);library(ggplot2)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
d<-read.csv("output/data_products/combined_gas_flux_dataset.csv") %>%
  filter(component=="stem", disturbance_level %in% c("ghost","healthy","regenerating"),
         !is.na(CH4_best.flux), !is.na(height)) %>%
  mutate(h=ifelse(median(height,na.rm=TRUE)<10,height,height/100),   # -> meters
         Class=factor(recode(disturbance_level,healthy="Healthy",regenerating="Regenerating",ghost="Ghost"),
                      levels=c("Healthy","Regenerating","Ghost")))
Hmax<-1.5
pal<-c(Healthy="#228B22",Regenerating="#999999",Ghost="#8B4513")
# exponential-decay fit per class on measured (<=Hmax) positive fluxes; predict + clamp>=0
grid<-do.call(rbind,lapply(levels(d$Class),function(cl){
  dc<-d %>% filter(Class==cl, h<=Hmax, CH4_best.flux>0)
  if(nrow(dc)<5) return(NULL)
  fit<-lm(log(CH4_best.flux)~h,data=dc)
  hh<-seq(0,8,0.1)
  data.frame(Class=cl,h=hh,pred=pmax(exp(predict(fit,newdata=data.frame(h=hh))),0),
             measured=hh<=Hmax)
}))
grid$Class<-factor(grid$Class,levels=levels(d$Class))
p<-ggplot()+
  annotate("rect",xmin=Hmax,xmax=8,ymin=-Inf,ymax=Inf,fill="grey92")+
  annotate("text",x=Hmax+0.15,y=Inf,label="extrapolated",hjust=0,vjust=1.4,size=4,color="grey40")+
  geom_vline(xintercept=Hmax,linetype="dashed",color="grey45")+
  geom_point(data=d %>% filter(h<=Hmax),aes(h,CH4_best.flux,color=Class),alpha=.25,size=1.1)+
  geom_line(data=grid %>% filter(measured),aes(h,pred,color=Class),linewidth=1.1)+
  geom_line(data=grid %>% filter(!measured),aes(h,pred,color=Class),linewidth=1.1,linetype="22")+
  scale_color_manual(values=pal,name=NULL)+
  coord_cartesian(ylim=c(0,quantile(d$CH4_best.flux[d$h<=Hmax],.97,na.rm=TRUE)))+
  labs(x="Height on stem (m)",y=expression("Stem CH"[4]*" flux (nmol "*m^-2*" "*s^-1*")"),
       title="Stem CH4 height extrapolation",
       subtitle="Solid = fit to chamber measurements (<=1.5 m); dashed = exponential-decay extrapolation above")+
  theme_bw(base_size=14)+theme(legend.position="bottom",panel.grid.minor=element_blank(),
        plot.title=element_text(face="bold"))
ggsave("output/figures/other/stem_extrap_clean.pdf",p,width=8,height=5)
ggsave("output/figures/other/stem_extrap_clean.png",p,width=8,height=5,dpi=200)
ggsave("output/figures/presentation/09e_stem_extrapolation.png",p,width=8,height=5,dpi=200)
cat("written stem_extrap_clean\n")
