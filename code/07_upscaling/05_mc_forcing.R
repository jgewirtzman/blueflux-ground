# =============================================================================
# Monte Carlo uncertainty for the bottom-up CO2 (NEE) budget and the combined
# CO2+CH4 net radiative forcing. Propagates: chamber respiration variability
# (bootstrap of CO2 flux densities), the literature leaf term (Rd25, LAI via
# Beer's-law effective LAI), tower GPP, and the CH4 budget MC total.
# =============================================================================
suppressMessages({library(dplyr);library(tidyr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
set.seed(42)
N <- 5000
GWP100 <- 27.9; GWP20 <- 81.2
umol_to_g_yr <- 44e-6*3.156e7          # umol CO2 m-2 s-1 -> g CO2 m-2 yr-1
mgd_to_gyr   <- 365/1000               # mg CH4 m-2 d-1 -> g CH4 m-2 yr-1
CAMP <- c("Oct 2022","Mar 2023")

# --- leaf-term parameters (GPP_literature_outputs) ---
K_EXT<-0.5; effLAI<-function(L)(1-exp(-K_EXT*L))/K_EXT
# per-campaign daytime f_T multiplier (from the CO2 script's leaf_term)
gpp_raw<-read.csv("output/gpp/US-Skr_GPP_halfhourly_Mar2022_Oct2022_Mar2023.csv")
gpp_raw$campaign<-with(gpp_raw,ifelse(year==2022&month==10,"Oct 2022",ifelse(year==2023&month==3,"Mar 2023",NA)))
f_T<-function(T)exp(0.1012*(T-25)-0.0005*(T^2-25^2))
lt<-gpp_raw %>% filter(campaign %in% CAMP) %>%
  mutate(sw=ifelse(!is.na(SW_IN)&SW_IN>-900,SW_IN,SW_IN_model),is_day=ifelse(!is.na(sw)&sw>5,1,0),
         TA=ifelse(!is.na(TA)&TA>-900,TA,TA_model)) %>%
  group_by(campaign) %>%
  summarise(fT_day=mean(f_T(TA)*(1-0.30*is_day),na.rm=TRUE),
            GPP_mean=mean(GPP,na.rm=TRUE),
            GPP_unc=mean(GPP_sd,na.rm=TRUE),   # tower bootstrap GPP uncertainty (partitioning)
            .groups="drop")

# --- chamber CO2 flux densities -> bootstrap relative SE per component/class/campaign ---
fx<-read.csv("output/data_products/combined_gas_flux_dataset.csv") %>%
  filter(plot %in% c("CP40","FLM30","SRS5","SRS6"), month_year %in% c("2022-10","2023-03"),
         !is.na(CO2_best.flux)) %>%
  mutate(campaign=ifelse(month_year=="2022-10","Oct 2022","Mar 2023"),
         disturbance_level=ifelse(plot %in% c("CP40","FLM30"),"ghost","healthy"),
         component=ifelse(component=="pneumatophore","soil",component)) %>%
  filter(component %in% c("soil","water","root","stem","cwd"))
relse<-fx %>% group_by(campaign,disturbance_level,component) %>%
  summarise(m=mean(CO2_best.flux), s=sd(CO2_best.flux)/sqrt(n()), n=n(),.groups="drop") %>%
  mutate(rel_se=ifelse(abs(m)>1e-6 & n>2, pmin(abs(s/m),1.0), 0.5))   # cap/floor

# --- areal component point values (umol m-2 ground s-1) ---
comp<-read.csv("output/upscaling/summary_CO2_by_component.csv") %>%
  group_by(campaign,disturbance_level) %>%
  summarise(across(c(soil,water,root,stem,cwd,leaf),~mean(.x)),.groups="drop") %>%
  filter(campaign %in% CAMP)

# --- CH4 budget MC total (mg CH4 m-2 d-1) -> class mean + sd ---
ch4mc<-read.csv("output/upscaling/mc_component_uncertainty.csv") %>% filter(component=="total") %>%
  group_by(site,campaign,disturbance_level) %>% summarise(m=mean(mc_mean),se=mean(mc_se),.groups="drop") %>%
  group_by(campaign,disturbance_level) %>% summarise(ch4_m=mean(m),ch4_se=mean(se),.groups="drop") %>%
  filter(campaign %in% CAMP)

# --- Monte Carlo ---
res<-list()
for(i in 1:nrow(comp)){
  r<-comp[i,]; cap<-r$campaign; dl<-r$disturbance_level
  ltc<-lt[lt$campaign==cap,]
  draws<-replicate(N,{
    # respiration components: normal(mean, rel_se*mean)
    rsum<-0
    for(cc in c("soil","water","root","stem","cwd")){
      v<-r[[cc]]; rs<-relse$rel_se[relse$campaign==cap & relse$disturbance_level==dl & relse$component==cc]
      if(length(rs)==0) rs<-0.5
      rsum<-rsum+rnorm(1,v,abs(v)*rs)
    }
    # leaf: healthy only; Rd25 ~ U(1.28,1.62), LAI ~ N(2.3,0.3) truncated
    leaf<-if(dl=="healthy"){
      Rd<-runif(1,1.28,1.62); LAI<-max(1.5,rnorm(1,2.3,0.3)); Rd*ltc$fT_day*effLAI(LAI)
    } else 0
    # GPP: healthy from tower; ghost 0
    G<-if(dl=="healthy") rnorm(1,ltc$GPP_mean,ltc$GPP_unc) else 0
    nee<-rsum+leaf-G
    nee
  })
  ch<-ch4mc[ch4mc$campaign==cap & ch4mc$disturbance_level==dl,]
  ch4_draw<-rnorm(N,ch$ch4_m,ch$ch4_se)
  co2_g<-draws*umol_to_g_yr
  ch4_g<-ch4_draw*mgd_to_gyr
  net100<-co2_g+ch4_g*GWP100; net20<-co2_g+ch4_g*GWP20
  q<-function(x)quantile(x,c(.025,.5,.975),na.rm=TRUE)
  res[[i]]<-data.frame(campaign=cap,class=dl,
    nee_med=q(draws)[2],nee_lo=q(draws)[1],nee_hi=q(draws)[3],
    net100_med=q(net100)[2],net100_lo=q(net100)[1],net100_hi=q(net100)[3],
    net20_med=q(net20)[2],net20_lo=q(net20)[1],net20_hi=q(net20)[3])
}
out<-bind_rows(res)
write.csv(out,"output/upscaling/mc_CO2_forcing.csv",row.names=FALSE)
cat("=== CO2 NEE (umol m-2 s-1) with MC 95% CI ===\n")
out %>% transmute(campaign,class,NEE=sprintf("%.2f [%.2f, %.2f]",nee_med,nee_lo,nee_hi)) %>% as.data.frame() %>% print()
cat("\n=== Net forcing GWP100 (g CO2-eq m-2 yr-1) with MC 95% CI ===\n")
out %>% transmute(campaign,class,net100=sprintf("%.0f [%.0f, %.0f]",net100_med,net100_lo,net100_hi)) %>% as.data.frame() %>% print()

# class-level annual forcing (campaign-averaged) with CI
cls<-out %>% group_by(class) %>%
  summarise(net100_med=mean(net100_med),
            net100_lo=mean(net100_lo),net100_hi=mean(net100_hi),
            net20_med=mean(net20_med),net20_lo=mean(net20_lo),net20_hi=mean(net20_hi),.groups="drop")
cat("\n=== Class annual net forcing (campaign-avg) ===\n")
cls %>% mutate(GWP100=sprintf("%.0f [%.0f, %.0f]",net100_med,net100_lo,net100_hi),
               GWP20 =sprintf("%.0f [%.0f, %.0f]",net20_med,net20_lo,net20_hi)) %>%
  select(class,GWP100,GWP20) %>% as.data.frame() %>% print()
write.csv(cls,"output/upscaling/mc_net_forcing_by_class.csv",row.names=FALSE)
