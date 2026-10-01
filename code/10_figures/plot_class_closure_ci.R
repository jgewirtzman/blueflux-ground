# =============================================================================
# Class closure with uncertainty bands: bottom-up stacked bar (+ bootstrap 95%
# CI on the total) beside the CARAFE class bar (+/- SE), per campaign, so the
# overlap of the two uncertainty bands is visible. Bottom-up CIs are wide
# because component fluxes are strongly right-skewed (hotspot-driven).
# =============================================================================
suppressMessages({library(dplyr); library(tidyr); library(ggplot2)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

df <- read.csv("output/data_products/combined_gas_flux_dataset.csv")
df$campaign <- with(df, ifelse(year==2022&month==10,"Oct 2022",
                        ifelse(year==2023&month==3,"Mar 2023",
                        ifelse(year==2022&month==3,"Mar 2022",NA))))
cls <- c(CP40="Ghost",FLM30="Ghost",MI="Ghost",SRS5="Healthy",SRS6="Healthy",
         BL60="Regen",SE1="Scrub",RB10="Healthy")
df$class <- cls[df$plot]
df <- df[!is.na(df$class) & !is.na(df$campaign), ]   # drop unclassified plots / off-campaign
comps <- c("soil","water","root","stem","cwd")

# TLS ratios (m2/m2 ground)
tls<-read.csv("data/tls/all_sites_summary.csv"); tstat<-read.csv("data/tls/tree_stats_per_site.csv")
sr <- tstat %>% transmute(site,ground=non_tree_m2) %>%
  left_join(tls%>%filter(segment_class%in%c("trunk","branch"))%>%group_by(site)%>%summarise(stem=sum(Total_surface_area_m2),.groups="drop"),by="site")%>%
  left_join(tls%>%filter(segment_class=="root")%>%group_by(site)%>%summarise(root=sum(Total_surface_area_m2),.groups="drop"),by="site")%>%
  mutate(stem_r=stem/ground, root_r=root/ground, cls=c(CP40="Ghost",FLM30="Ghost",SRS5="Healthy",SRS6="Healthy")[site])
clsr <- sr%>%group_by(cls)%>%summarise(stem_r=mean(stem_r),root_r=mean(root_r),.groups="drop")
interm <- c(stem_r=mean(clsr$stem_r), root_r=mean(clsr$root_r))
ratio <- function(class,comp){
  r<-if(class%in%clsr$cls) clsr[[paste0(comp,"_r")]][clsr$cls==class] else interm[[paste0(comp,"_r")]]; as.numeric(r)}

# class inundation fraction (soil fraction) per campaign
frac_soil <- function(class,camp){
  if(class=="Ghost")  return(ifelse(camp=="Mar 2022",1,0))          # exposed 2022, flooded after
  if(class=="Healthy")return(0.5)                                    # tidal 50/50
  ff<-mean(df$water_depth[df$class==class & df$campaign==camp]>0, na.rm=TRUE)  # measured
  1-ifelse(is.finite(ff),ff,0.5)
}
mult <- function(class,camp,comp){
  if(comp=="soil")  return(frac_soil(class,camp))
  if(comp=="water") return(1-frac_soil(class,camp))
  if(comp=="cwd")   return(0.005)
  ratio(class,comp)                                                  # root, stem
}

set.seed(42); B<-2000
grid <- df %>% filter(!is.na(class),!is.na(campaign),!is.na(CH4_best.flux),component%in%comps) %>%
  distinct(class,campaign)
res <- list(); stk <- list()
for(i in seq_len(nrow(grid))){
  cl<-grid$class[i]; cp<-grid$campaign[i]
  boot<-numeric(B); point<-0
  for(co in comps){
    v<-df$CH4_best.flux[df$class==cl & df$campaign==cp & df$component==co & !is.na(df$CH4_best.flux)]
    m<-mult(cl,cp,co); if(!is.finite(m)) m<-0
    if(length(v)==0){next}
    point<-point + mean(v)*m
    stk[[paste(cl,cp,co)]]<-data.frame(class=cl,campaign=cp,component=co,contrib=mean(v)*m)
    bs<-if(length(v)>1) replicate(B, mean(sample(v,replace=TRUE))*m) else rep(v*m,B)
    boot<-boot+bs
  }
  res[[i]]<-data.frame(class=cl,campaign=cp,total=point,lo=quantile(boot,.025,na.rm=TRUE),hi=quantile(boot,.975,na.rm=TRUE))
}
tot<-bind_rows(res); stack<-bind_rows(stk)

# CARAFE class bars
td<-read.csv("output/carafe_topdown/delaria_endmembers_campaign.csv")%>%filter(gas=="CH4")%>%
  mutate(cls=recode(class,ghost_forest="Ghost",mangrove_forest="Healthy"))%>%filter(cls%in%c("Ghost","Healthy"))
cara<-bind_rows(
  td%>%filter(campaign=="Apr 2022")%>%mutate(campaign="Mar 2022"),
  td%>%filter(campaign=="Oct 2022"),
  td%>%filter(campaign%in%c("Feb 2023","Apr 2023"))%>%group_by(cls)%>%summarise(flux=mean(flux),se=sqrt(mean(se^2)),.groups="drop")%>%mutate(campaign="Mar 2023"))%>%
  transmute(class=cls,campaign,flux,se)

clord<-c("Ghost","Regen","Scrub","Healthy"); camps<-c("Mar 2022","Oct 2022","Mar 2023")
xB<--0.22; xC<-0.22
fx<-function(cl) as.numeric(factor(cl,levels=clord))
stack$x<-fx(stack$class)+xB; tot$x<-fx(tot$class)+xB
cara$x<-fx(cara$class)+xC
for(dd in c("stack","tot","cara")){ d<-get(dd); d$campaign<-factor(d$campaign,levels=camps); d$class<-factor(d$class,levels=clord); assign(dd,d) }
stack$component<-factor(stack$component,levels=comps)
pal<-c(soil="#8B4513",water="#4682B4",root="#D2691E",stem="#228B22",cwd="#E0C48A",
       "CARAFE (top-down)"="grey55")
lev<-c("soil","water","root","stem","cwd","CARAFE (top-down)")
stack$fillk<-factor(as.character(stack$component),levels=lev)
cara$fillk<-factor("CARAFE (top-down)",levels=lev)

p<-ggplot()+
  geom_col(data=stack,aes(x,contrib,fill=fillk),width=.4,color="grey25",linewidth=.2)+
  geom_errorbar(data=tot,aes(x,ymin=lo,ymax=hi),width=.14,linewidth=.5)+
  geom_col(data=cara,aes(x,flux,fill=fillk),width=.4,color="black",linewidth=.25)+
  geom_errorbar(data=cara,aes(x,ymin=flux-se,ymax=flux+se),width=.14,linewidth=.5)+
  facet_wrap(~campaign,nrow=1)+
  scale_x_continuous(breaks=1:4,labels=clord)+
  scale_fill_manual(values=pal,name=NULL,breaks=lev)+
  labs(x=NULL,y=expression("CH"[4]*" (nmol "*m^-2*" ground "*s^-1*")"),
       title="Bottom-up (colored stack, +/- bootstrap 95% CI) vs CARAFE (grey, +/- SE) by class x campaign")+
  theme_bw(base_size=11)+theme(legend.position="bottom",panel.grid.minor=element_blank(),
       strip.text=element_text(face="bold"),plot.title=element_text(size=9.5),
       axis.text.x=element_text(angle=45,hjust=1))
ggsave("output/figures/other/class_closure_ci.png",p,width=12,height=5.5,dpi=160)
cat("written output/figures/other/class_closure_ci.png\n\n")
print(tot%>%transmute(class,campaign,bottom_up=round(total,1),lo=round(lo,1),hi=round(hi,1))%>%
  left_join(cara%>%transmute(class,campaign,CARAFE=round(flux,1),se=round(se,1)),by=c("class","campaign"))%>%
  arrange(campaign,class)%>%as.data.frame())
