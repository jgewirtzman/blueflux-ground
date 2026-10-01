# =============================================================================
# Healthy-class closure under different soil:water inundation assumptions.
# The intact (SRS5/SRS6) sites are tidal; wet-season water is thinly sampled and
# bottom-up falls below CARAFE. Here we test frac_soil = 0% / 50% / 100% (i.e.
# emitting ground = all water / half / all exposed soil) per campaign vs CARAFE,
# to ask whether any inundation assumption closes the wet-season gap.
# =============================================================================
suppressMessages({library(dplyr); library(tidyr); library(ggplot2)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

conv_note <- "nmol m-2 ground s-1"
df <- read.csv("output/data_products/combined_gas_flux_dataset.csv")
df$campaign <- with(df, ifelse(year==2022&month==10,"Oct 2022",
                        ifelse(year==2023&month==3,"Mar 2023",
                        ifelse(year==2022&month==3,"Mar 2022",NA))))
H <- c("SRS5","SRS6")

# TLS ratios (m2/m2 ground) per healthy site
tls<-read.csv("data/tls/all_sites_summary.csv"); tstat<-read.csv("data/tls/tree_stats_per_site.csv")
sr <- tstat %>% transmute(site,ground=non_tree_m2) %>%
  left_join(tls%>%filter(segment_class%in%c("trunk","branch"))%>%group_by(site)%>%summarise(stem=sum(Total_surface_area_m2),.groups="drop"),by="site")%>%
  left_join(tls%>%filter(segment_class=="root")%>%group_by(site)%>%summarise(root=sum(Total_surface_area_m2),.groups="drop"),by="site")%>%
  mutate(stem_r=stem/ground, root_r=root/ground)

# per-site areal rates per campaign
rate <- df %>% filter(plot%in%H, component%in%c("soil","water","root","stem"), !is.na(campaign), !is.na(CH4_best.flux)) %>%
  group_by(site=plot, campaign, component) %>% summarise(areal=mean(CH4_best.flux), .groups="drop")
get<-function(s,cp,co){v<-rate$areal[rate$site==s&rate$campaign==cp&rate$component==co];if(length(v))v else NA}

camps <- c("Oct 2022","Mar 2023")   # both healthy sites present; Mar 2022 only SRS6
fracs <- c(0,0.5,1)
rows <- list()
for(cp in camps){ for(fs in fracs){
  per_site <- sapply(H, function(s){
    soil<-get(s,cp,"soil"); water<-get(s,cp,"water"); root<-get(s,cp,"root"); stem<-get(s,cp,"stem")
    rr<-sr$root_r[sr$site==s]; sre<-sr$stem_r[sr$site==s]
    # gap-fill missing water/soil within healthy class from the other site
    if(is.na(water)) water<-get(setdiff(H,s),cp,"water"); if(is.na(soil)) soil<-get(setdiff(H,s),cp,"soil")
    c(soil=ifelse(is.na(soil),0,soil)*fs, water=ifelse(is.na(water),0,water)*(1-fs),
      root=ifelse(is.na(root),0,root)*rr, stem=ifelse(is.na(stem),0,stem)*sre)
  })
  rows[[paste(cp,fs)]] <- data.frame(campaign=cp, frac_soil=fs, t(rowMeans(per_site)))
}}
sc <- bind_rows(rows) %>%
  pivot_longer(c(soil,water,root,stem), names_to="component", values_to="contrib") %>%
  mutate(scenario=factor(paste0(frac_soil*100,"% soil"), levels=c("0% soil","50% soil","100% soil")),
         campaign=factor(campaign,levels=camps), component=factor(component,levels=c("soil","water","root","stem")))
tot <- sc %>% group_by(campaign,scenario) %>% summarise(total=sum(contrib),.groups="drop")

# CARAFE healthy (mangrove_forest)
td<-read.csv("output/carafe_topdown/delaria_endmembers_campaign.csv")%>%filter(gas=="CH4",class=="mangrove_forest")
cara<-bind_rows(td%>%filter(campaign=="Oct 2022"),
  td%>%filter(campaign%in%c("Feb 2023","Apr 2023"))%>%summarise(flux=mean(flux),se=sqrt(mean(se^2)))%>%mutate(campaign="Mar 2023"))%>%
  mutate(campaign=factor(campaign,levels=camps))

pal<-c(soil="#8B4513",water="#4682B4",root="#D2691E",stem="#228B22")
p<-ggplot()+
  geom_col(data=sc,aes(scenario,contrib,fill=component),width=.7,color="grey30",linewidth=.25)+
  geom_hline(data=cara,aes(yintercept=flux),linetype="dashed",color="black")+
  geom_hline(data=cara,aes(yintercept=flux-se),linetype="dotted",color="grey40")+
  geom_hline(data=cara,aes(yintercept=flux+se),linetype="dotted",color="grey40")+
  geom_text(data=tot,aes(scenario,total,label=round(total,1)),vjust=-.4,size=3)+
  facet_wrap(~campaign,nrow=1)+
  scale_fill_manual(values=pal,name="component")+
  labs(x="healthy inundation assumption", y=expression("CH"[4]*" (nmol "*m^-2*" ground "*s^-1*")"),
       title="Healthy-class bottom-up under 0/50/100% soil vs CARAFE mangrove-forest flux (dashed = mean, dotted = +/-SE)")+
  theme_bw(base_size=11)+theme(legend.position="bottom",panel.grid.minor=element_blank(),
       strip.text=element_text(face="bold"),plot.title=element_text(size=9.5))
ggsave("output/figures/other/healthy_soil_scenarios.png",p,width=9,height=5,dpi=160)
cat("written output/figures/other/healthy_soil_scenarios.png\n\n")
print(tot %>% left_join(cara%>%transmute(campaign,CARAFE=flux,se),by="campaign") %>% as.data.frame())
