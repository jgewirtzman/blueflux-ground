# =============================================================================
# Water CH4 flux by measurement position, across all campaigns.
# Shows every water chamber's flux, labeled by recorded collar_location and
# flagged in-plot vs off-plot/river, to assess whether open-water measurements
# read artificially low.
# =============================================================================
suppressMessages({library(dplyr); library(ggplot2)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

df <- read.csv("output/data_products/combined_gas_flux_dataset.csv")
df$camp <- with(df, ifelse(year==2022&month==10,"Oct 2022",
                    ifelse(year==2023&month==3,"Mar 2023",
                    ifelse(year==2022&month==3,"Mar 2022",NA))))
source("code/00_lib/water_position.R")   # channel/open-water placements; "outside plot" at ghost sites is still over the floor
w <- df %>% filter(component=="water", !is.na(CH4_best.flux), !is.na(camp)) %>%
  mutate(position = water_position(collar_location),
         class = recode(plot, CP40="ghost",FLM30="ghost",MI="ghost",
                        BL60="regenerating",SE1="scrub",SRS5="intact",SRS6="intact",RB10="intact"),
         site = factor(plot, levels=c("CP40","FLM30","BL60","SE1","SRS5","SRS6")),
         camp = factor(camp, levels=c("Mar 2022","Oct 2022","Mar 2023")),
         position = factor(position, c("above forest floor", "channel / open water")))

source("code/08_figures/palette.R")   # house palette + theme_fig()
pos_cols <- c("above forest floor" = "#2C7BB6", "channel / open water" = "#C2513A")
site_cls <- c(CP40 = "ghost", FLM30 = "ghost", BL60 = "regenerating", SE1 = "scrub", SRS5 = "intact", SRS6 = "intact")
w <- w %>% mutate(site_lab = factor(paste0(site, "\n", site_cls[as.character(site)]),
                                    paste0(levels(site), "\n", site_cls[levels(site)])))
# dissolved-gas fluxes, F = k600 (Sc/600)^-0.5 (Cw - Ceq), k600 = median of site medians with the range of
# site medians as whiskers (03_fit/02_water_flux_from_dissolved.R): our plot surface-water samples (forest
# floor) and the BlueFlux tidal-river survey (Vaughn & Raymond 2024, ORNL DAAC 2333): river stations SRS 5 /
# SRS 6 (channel) and the SRS6 tidal creek (forest floor), on campaign dates
ks <- read.csv("output/flux/03_fit/water_k_by_site.csv")
k_all <- ks$k600[ks$level == "all"]; k_rng <- range(ks$k600[ks$level == "site"])
K0 <- function(T_C, S) { T <- T_C + 273.15; exp(-67.1962 + 99.1624 * (100 / T) + 27.9015 * log(T / 100) +
  S * (-0.072909 + 0.041674 * (T / 100) - 0.0064603 * (T / 100)^2)) / 22.4136 }
sc <- function(T_C) 1909.4 - 120.78 * T_C + 4.1555 * T_C^2 - 0.080578 * T_C^3 + 0.00065777 * T_C^4
Fk <- function(k, dC, T_C) k * (sc(T_C) / 600)^-0.5 / 3600 / 100 * dC * 1e3
camp_of <- function(d) ifelse(format(d, "%Y-%m") == "2022-10", "Oct 2022", ifelse(format(d, "%Y-%m") == "2023-03", "Mar 2023", NA))
cal <- read.csv("output/flux/03_fit/water_k_calibration.csv")
TS <- cal %>% filter(grepl("^own GC", source)) %>% distinct(plot, campaign, T_C, S)
gc <- read.csv("data/environmental/dissolved_gas/dissolved_gas_all_observations.csv") %>% filter(sample_type == "surface_water", source == "GC") %>%
  mutate(camp = ifelse(grepl("2022", season), "Oct 2022", "Mar 2023")) %>% group_by(site, camp) %>%
  filter(CH4_uM >= 0.3 * median(CH4_uM)) %>% summarise(Cw = mean(CH4_uM) * 1e3, .groups = "drop") %>%
  left_join(TS, by = c(site = "plot", camp = "campaign")) %>%
  mutate(T_C = coalesce(T_C, ifelse(camp == "Oct 2022", 28, 26)), S = coalesce(S, 15), position = "above forest floor", src = "plot sample")
st <- c("SRS 5" = "SRS5", "SRS 6" = "SRS6", "SRS 6 Tidal Creek" = "SRS6", "SRS 6 Tidal Creek 2" = "SRS6", "SRS 6 Tidal Creek 3" = "SRS6")
tr <- read.csv("data/environmental/aquatic/ORNL_DAAC_2333_BLUEFLUX_Transect_Shark_Haney_Rivers_TarponBay.csv", fileEncoding = "UTF-8-BOM", na.strings = "-9999") %>%
  filter(site %in% names(st), is.finite(pCH4)) %>%
  transmute(stn = site, site = unname(st[site]), camp = camp_of(as.Date(date)), S = salinity, T_C = temp, pCH4,
            position = ifelse(grepl("Creek", stn), "above forest floor", "channel / open water"), src = "river survey") %>%
  filter(!is.na(camp)) %>% group_by(site, camp) %>%
  mutate(S = coalesce(S, mean(S, na.rm = TRUE)), T_C = coalesce(T_C, mean(T_C, na.rm = TRUE))) %>% ungroup() %>%
  mutate(Cw = pCH4 * 1e-6 * K0(T_C, S) * 1e9)
dis <- bind_rows(gc %>% select(site, camp, Cw, T_C, S, position, src), tr %>% select(site, camp, Cw, T_C, S, position, src)) %>%
  filter(site %in% levels(w$site)) %>%
  mutate(dC = Cw - 1.95e-6 * K0(T_C, S) * 1e9, F = Fk(k_all, dC, T_C), lo = Fk(k_rng[1], dC, T_C), hi = Fk(k_rng[2], dC, T_C),
         site = factor(site, levels(w$site)), camp = factor(camp, levels(w$camp)),
         site_lab = factor(paste0(site, "\n", site_cls[as.character(site)]), paste0(levels(w$site), "\n", site_cls[levels(w$site)])),
         position = factor(position, levels(w$position)))
write.csv(dis, "output/upscaling/supp_water_dissolved_flux.csv", row.names = FALSE)
# arithmetic mean per site x campaign x position (computed on the raw scale), drawn as a bar
mn <- w %>% group_by(camp, site_lab, position) %>% summarise(m = mean(CH4_best.flux), .groups = "drop")
dg <- position_dodge(width = 0.6)
p <- ggplot(w, aes(site_lab, CH4_best.flux, group = position)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_point(aes(fill = position), shape = 21, colour = "white", stroke = 0.25, size = 1.8, alpha = .9,
             position = position_jitterdodge(jitter.width = 0.12, jitter.height = 0, dodge.width = 0.6, seed = 1)) +
  geom_errorbar(data = mn, aes(x = site_lab, ymin = m, ymax = m, colour = position, group = position), inherit.aes = FALSE,
                width = 0.25, linewidth = 0.7, position = dg, show.legend = FALSE) +
  geom_linerange(data = dis, aes(x = site_lab, ymin = lo, ymax = hi, colour = position, group = position), inherit.aes = FALSE,
                 linewidth = 0.35, alpha = 0.6, position = position_dodge(width = 0.6), show.legend = FALSE) +
  geom_point(data = dis, aes(x = site_lab, y = F, colour = position, shape = src, group = position), inherit.aes = FALSE,
             fill = "white", size = 2, stroke = 0.6, position = position_dodge(width = 0.6)) +
  scale_shape_manual(values = c("plot sample" = 23, "river survey" = 22), name = expression("dissolved CH"[4]*" × k"[600])) +
  scale_colour_manual(values = pos_cols, guide = "none") +
  facet_grid(~camp, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = pos_cols, name = "position (chambers, filled; bars, means)") +
  scale_y_continuous(trans = "asinh", breaks = c(0, 1, 2, 5, 10, 20, 50, 100)) +
  labs(x = NULL, y = expression("Water-surface CH"[4]*" (nmol m"^-2*" s"^-1*")"), tag = "a") +
  guides(fill = guide_legend(override.aes = list(size = 2.2))) +
  theme_fig(base_size = 8) +
  theme(strip.text = element_text(hjust = 0.5), legend.title = element_text(face = "bold", size = 7),
        legend.text = element_text(size = 7), panel.grid.major.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.x = element_text(lineheight = 0.9))
dir.create("output/figures/other", recursive=TRUE, showWarnings=FALSE)
ggsave("output/figures/other/water_positions_by_campaign.png", p + theme(legend.box = "vertical", legend.spacing.y = unit(0, "pt")), width=7.2, height=3.6, dpi=300, bg="white")
cat("written output/figures/other/water_positions_by_campaign.png\n\n")

# full labeled table
tab <- w %>% transmute(plot, class, campaign=camp, position,
                       location=ifelse(is.na(collar_location),"",collar_location),
                       water_depth_cm=water_depth, CH4=round(CH4_best.flux,2)) %>%
  arrange(campaign, plot, position)
write.csv(tab, "output/upscaling/supp_water_positions.csv", row.names=FALSE)
cat("=== water flux by position summary (site x campaign) ===\n")
print(w %>% group_by(site, camp, position) %>%
      summarise(n=n(), CH4=round(mean(CH4_best.flux),2), .groups="drop") %>% as.data.frame())
