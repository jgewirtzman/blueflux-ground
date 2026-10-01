# =============================================================================
# Per-site x season component comparison vs CARAFE (exploratory, SI-style).
# For each site x campaign: stacked bottom-up component contributions
# (nmol m-2 ground s-1), gap-filled/unmeasured components flagged, and the
# CARAFE class end-member overlaid for the core Everglades sites it covers.
# Core sites (CP40,FLM30,SRS5,SRS6) use their own TLS surface areas; context
# sites (MI,RB10,SE1) and regen (BL60) lack TLS and use class-assigned
# structure (MI->ghost, RB10->healthy, BL60 & SE1->intermediate), flagged with
# a hatched border. CARAFE is shown only for the core sites (its footprints do
# not cover MI/RB10/SE1).
# =============================================================================
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(patchwork)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

pal   <- c(soil="#8B4513", water="#4682B4", root="#D2691E", stem="#228B22", cwd="#808080")
comps <- c("soil","water","root","stem","cwd")
sites_order <- c("CP40","FLM30","MI","BL60","SE1","SRS5","SRS6","RB10")
class_of  <- c(CP40="ghost",FLM30="ghost",MI="ghost",BL60="regen",SE1="scrub",
               SRS5="healthy",SRS6="healthy",RB10="healthy")
struct_of <- c(CP40="CP40",FLM30="FLM30",MI="ghost",BL60="interm",SE1="interm",
               SRS5="SRS5",SRS6="SRS6",RB10="healthy")   # which TLS ratio to use
core      <- c("CP40","FLM30","SRS5","SRS6")

# ---- raw areal rates (nmol m-2 surface s-1) per site x campaign x component --
df <- read.csv("output/data_products/combined_gas_flux_dataset.csv")
df$campaign <- with(df, ifelse(year==2022&month==10,"Oct 2022",
                        ifelse(year==2023&month==3,"Mar 2023",
                        ifelse(year==2022&month==3,"Mar 2022",NA))))
boot <- function(x,R=2000){x<-x[!is.na(x)]; if(!length(x)) return(c(m=NA,n=0))
  c(m=mean(x), n=length(x))}
rate <- df %>% filter(plot %in% sites_order, component %in% comps, !is.na(campaign), !is.na(CH4_best.flux)) %>%
  group_by(site=plot, campaign, component) %>% summarise(areal=mean(CH4_best.flux), n=n(), .groups="drop")

# ---- TLS ratios (m2 component / m2 ground) per site + class means ------------
tls  <- read.csv("data/tls/all_sites_summary.csv"); tstat<- read.csv("data/tls/tree_stats_per_site.csv")
sr <- tstat %>% transmute(site, ground=non_tree_m2) %>%
  left_join(tls %>% filter(segment_class %in% c("trunk","branch")) %>% group_by(site) %>%
              summarise(stem=sum(Total_surface_area_m2,na.rm=TRUE),.groups="drop"), by="site") %>%
  left_join(tls %>% filter(segment_class=="root") %>% group_by(site) %>%
              summarise(root=sum(Total_surface_area_m2,na.rm=TRUE),.groups="drop"), by="site") %>%
  mutate(stem_r=stem/ground, root_r=root/ground, cls=c(CP40="ghost",FLM30="ghost",SRS5="healthy",SRS6="healthy")[site])
cls_r <- sr %>% group_by(cls) %>% summarise(stem_r=mean(stem_r), root_r=mean(root_r), .groups="drop")
interm <- c(stem_r=mean(cls_r$stem_r), root_r=mean(cls_r$root_r))
get_ratio <- function(key, comp){
  v <- if (key %in% sr$site) sr[[paste0(comp,"_r")]][sr$site==key]
       else if (key=="ghost")   cls_r[[paste0(comp,"_r")]][cls_r$cls=="ghost"]
       else if (key=="healthy") cls_r[[paste0(comp,"_r")]][cls_r$cls=="healthy"]
       else interm[[paste0(comp,"_r")]]
  as.numeric(v)
}
# ---- inundation fraction: tidal 50/50; ghost-core campaign logic; else measured
frac_flood_site <- df %>% filter(plot %in% sites_order) %>% group_by(site=plot,campaign) %>%
  summarise(ff=mean(water_depth>0,na.rm=TRUE), .groups="drop")
frac <- function(site,camp){
  if (site %in% c("SRS5","SRS6")) return(c(soil=.5,water=.5))
  if (site %in% c("CP40","FLM30")) return(if(camp=="Mar 2022") c(soil=1,water=0) else c(soil=0,water=1))
  ff <- frac_flood_site$ff[frac_flood_site$site==site & frac_flood_site$campaign==camp]
  ff <- if(length(ff)&&is.finite(ff)) ff else 0.5
  c(soil=1-ff, water=ff)
}
# ---- ground contributions ---------------------------------------------------
scaled_site <- rate %>% rowwise() %>% mutate(
  mult = switch(component, soil=frac(site,campaign)["soil"], water=frac(site,campaign)["water"],
                root=get_ratio(struct_of[site],"root"), stem=get_ratio(struct_of[site],"stem"), cwd=0.005),
  contrib = areal*mult,
  assumed = !(site %in% core)) %>% ungroup()

# ---- AVERAGE within landcover class per campaign ---------------------------
class_lab <- c(CP40="Ghost",FLM30="Ghost",MI="Ghost",BL60="Regen",SE1="Scrub",
               SRS5="Healthy",SRS6="Healthy",RB10="Healthy")
class_order <- c("Ghost","Regen","Scrub","Healthy")
carafe_cls  <- c("Ghost"="ghost","Healthy"="healthy")     # only core classes get CARAFE
scaled <- scaled_site %>% mutate(klass=class_lab[site]) %>%
  group_by(klass, campaign, component) %>%
  summarise(contrib=mean(contrib), assumed=any(assumed), .groups="drop") %>%
  mutate(campaign=factor(campaign,levels=c("Mar 2022","Oct 2022","Mar 2023")),
         component=factor(component,levels=comps),
         site=factor(klass,levels=class_order))

# ---- coverage grid (class level: sum n over sites in class) -----------------
cov_n <- rate %>% mutate(klass=class_lab[site]) %>%
  group_by(klass,campaign,component) %>% summarise(n=sum(n),.groups="drop")
visited <- rate %>% mutate(klass=class_lab[site]) %>% distinct(klass,campaign) %>% mutate(k=paste(klass,campaign)) %>% pull(k)
cov <- expand_grid(klass=class_order, campaign=c("Mar 2022","Oct 2022","Mar 2023"), component=comps) %>%
  left_join(cov_n, by=c("klass","campaign","component")) %>%
  mutate(n=ifelse(is.na(n),0,n),
         status=ifelse(n>0,"measured", ifelse(!paste(klass,campaign) %in% visited,"class not visited","not measured")),
         campaign=factor(campaign,levels=c("Mar 2022","Oct 2022","Mar 2023")),
         component=factor(component,levels=rev(comps)), site=factor(klass,levels=class_order),
         lab=ifelse(n>0,n,""))

# ---- CARAFE (core sites only) ----------------------------------------------
td <- read.csv("output/carafe_topdown/delaria_endmembers_campaign.csv") %>% filter(gas=="CH4")
cara <- bind_rows(
  td %>% filter(campaign=="Apr 2022") %>% mutate(campaign="Mar 2022"),
  td %>% filter(campaign=="Oct 2022"),
  td %>% filter(campaign %in% c("Feb 2023","Apr 2023")) %>% group_by(class) %>%
    summarise(flux=mean(flux),se=sqrt(mean(se^2)),.groups="drop") %>% mutate(campaign="Mar 2023")) %>%
  mutate(cls=recode(class,ghost_forest="ghost",mangrove_forest="healthy")) %>% filter(cls %in% c("ghost","healthy"))
cara_pts <- data.frame(klass=names(carafe_cls), cls=unname(carafe_cls)) %>% left_join(cara,by="cls") %>%
  mutate(campaign=factor(campaign,levels=c("Mar 2022","Oct 2022","Mar 2023")), site=factor(klass,levels=class_order))

# ---- plot -------------------------------------------------------------------
pA <- ggplot() +
  geom_col(data=scaled, aes(site, contrib, fill=component, linetype=assumed), color="grey25", width=.72, linewidth=.25) +
  geom_point(data=cara_pts, aes(site, flux), shape=23, size=2.6, fill="white", stroke=.8) +
  geom_errorbar(data=cara_pts, aes(site, ymin=flux-se, ymax=flux+se), width=.22, linewidth=.4) +
  facet_wrap(~campaign, nrow=1) +
  scale_fill_manual(values=pal, name="component") +
  scale_linetype_manual(values=c(`FALSE`="blank",`TRUE`="22"), name="structure", labels=c("measured TLS","assumed (no TLS)")) +
  labs(x=NULL, y=expression("CH"[4]*" (nmol "*m^-2*" ground "*s^-1*")"),
       title="Class-mean bottom-up components (bars; dashed = assumed structure) vs CARAFE class flux (diamonds +/- SE)") +
  theme_bw(base_size=11)+theme(legend.position="bottom",panel.grid.minor=element_blank(),
       strip.text=element_text(face="bold"),plot.title=element_text(size=9.5),
       axis.text.x=element_text(angle=45,hjust=1))

pB <- ggplot(cov, aes(site, component, fill=status)) +
  geom_tile(color="white",linewidth=.5)+geom_text(aes(label=lab),size=2.7)+
  facet_wrap(~campaign,nrow=1)+
  scale_fill_manual(values=c("measured"="#4daf4a","not measured"="#e41a1c","class not visited"="grey88"),name=NULL)+
  labs(x=NULL,y=NULL,title="Component coverage (cell = total n chambers in class; red = component not measured that visit)")+
  theme_bw(base_size=11)+theme(legend.position="bottom",panel.grid=element_blank(),
       strip.text=element_text(face="bold"),plot.title=element_text(size=9.5),
       axis.text.x=element_text(angle=45,hjust=1))

fig <- pA/pB + plot_layout(heights=c(1,0.85))
dir.create("output/figures/other",recursive=TRUE,showWarnings=FALSE)
ggsave("output/figures/other/site_closure_comparison.png", fig, width=12.5, height=9, dpi=160)
cat("written output/figures/other/site_closure_comparison.png\n")
write.csv(scaled %>% transmute(class=klass,campaign,component,contrib,assumed),
          "output/upscaling/supp_site_component_contributions.csv", row.names=FALSE)
print(scaled %>% group_by(class=klass,campaign) %>%
      summarise(bottom_up=round(sum(contrib,na.rm=TRUE),1),.groups="drop") %>%
      left_join(cara_pts %>% distinct(class=klass,campaign,carafe=flux), by=c("class","campaign")) %>%
      as.data.frame())
