# =============================================================================
# Fig. S13 | Porewater total alkalinity vs salinity (October 2025) with a
#   conservative-mixing reference: (a) vs salinity, (b) measured vs predicted, (c) ratio (Florida endmembers: S = 0, TA 3000 uM;
#   S = 35, TA 2400 uM; heuristic, not calibrated). FLM30 not sampled.
# Same data, filters and endmembers as the legacy panels pub_SI_ta_vs_dic /
# pub_SI_ta_vs_salinity in publication_figures_soilprofile.R (porewater only,
# surface water excluded). Values shown in mM (uM / 1000).
# Encoding: fill colour = forest class, shape = site (as Fig. 4).
# Writes output/figures/other/si_S12_carbonate.{png,pdf}.
# =============================================================================
suppressMessages({library(dplyr); library(ggplot2); library(patchwork)})
invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/08_figures/palette.R")
source("code/00_lib/porewater_dic.R")
site_cls <- c(SRS5 = "intact", SRS6 = "intact", BL60 = "regenerating", CP40 = "ghost", FLM30 = "ghost")
site_shape <- c(SRS5 = 21, SRS6 = 24, BL60 = 22, CP40 = 23, FLM30 = 25)

pw <- read.csv("output/data_products/porewater_all_parameters.csv", check.names = FALSE) %>%
  filter(Depth_cm != "Surface", Site %in% c("SRS5", "SRS6", "BL60", "CP40")) %>%
  add_dic() %>%
  mutate(class = factor(site_cls[Site], names(pal_class)),
         Site = factor(Site, intersect(names(site_shape), unique(Site))),
         TA = Alkalinity_uM / 1000, DIC = DIC_uM / 1000)

# ---- (a) alkalinity vs salinity; (b) measured vs mixing-predicted (1:1); (c) enrichment over mixing ----
# Conservative mixing: freshwater 3,000 uM at S = 0, seawater 2,400 uM at S = 35 (heuristic end-members).
# (A TA vs DIC panel is not shown: DIC is calculated from pH and TA, so it is not independent.)
d <- pw %>% filter(!is.na(PSU), !is.na(TA)) %>%
  mutate(TAmix = (3000 + (2400 - 3000) * PSU / 35) / 1000, ratio = TA / TAmix)
site_shape <- site_shape[levels(d$Site)]
pt <- function(p) p + geom_point(aes(shape=Site, fill=class), colour="white", size=2.3, stroke=0.35) +
  scale_fill_manual(values=pal_class, name="forest class", guide=guide_legend(override.aes=list(shape=21,size=2.4,colour="white"))) +
  scale_shape_manual(values=site_shape, name="site", guide=guide_legend(override.aes=list(fill="grey35",colour="white",size=2.2)))
# A: current
mix <- data.frame(PSU=seq(0,65,length.out=100)) %>% mutate(TA=(3000+(2400-3000)*PSU/35)/1000)
pA <- pt(ggplot(d, aes(PSU, TA)) + geom_line(data=mix, linetype="dashed", colour="grey45") +
  annotate("text",x=64,y=2.3,label="conservative mixing",hjust=1,vjust=-0.7,size=2.2,colour="grey35")) +
  scale_y_continuous(limits=c(0,36),expand=c(0,0)) + scale_x_continuous(limits=c(0,65),expand=c(0,0)) +
  labs(x="Salinity (PSU)", y="Total alkalinity (mM)", tag="a") + theme_fig()
# B: measured vs predicted, 1:1
pB <- pt(ggplot(d, aes(TAmix, TA)) + geom_abline(linetype="dashed", colour="grey45") +
  annotate("text",x=30,y=30,label="1:1 (conservative mixing)",hjust=1,vjust=-0.6,size=2.2,colour="grey35",angle=45)) +
  coord_equal(xlim=c(0,36), ylim=c(0,36), expand=FALSE) +
  labs(x="Alkalinity predicted by mixing (mM)", y="Measured alkalinity (mM)", tag="b") + theme_fig()
# C: ratio by site
pC <- pt(ggplot(d, aes(ratio, Site)) + geom_vline(xintercept=1, linetype="dashed", colour="grey45") +
  annotate("text",x=1.3,y=0.6,label="conservative\nmixing",vjust=0,hjust=0,size=2.2,colour="grey35",lineheight=0.9)) +
  scale_x_continuous(limits=c(0,16), breaks=c(1,5,10,15), labels=function(x) paste0(x,"×"), expand=c(0,0)) +
  scale_y_discrete(limits=rev, expand=expansion(add=c(0.9,0.5))) +
  labs(x="Measured ÷ mixing-predicted alkalinity", y=NULL, tag="c") + theme_fig() + theme(legend.position="none")
th <- theme()
fig <- (pA + th + theme(legend.position="none")) | (pB + th + theme(legend.position="none")) | (pC + th)
fig <- fig + plot_layout(guides="collect") & theme(legend.position="bottom")
dir.create("output/figures/other", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/other/si_S12_carbonate.png", fig, width = 7.2, height = 3.0, dpi = 300, bg = "white")
ggsave("output/figures/other/si_S12_carbonate.pdf", fig, width = 7.2, height = 3.0, device = cairo_pdf)
