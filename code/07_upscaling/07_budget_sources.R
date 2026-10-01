# =============================================================================
# assemble_budget_sources.R
# -----------------------------------------------------------------------------
# Multi-source carbon-budget estimates: for EACH budget term, collect every
# independent estimate we have (our chambers, tower, CARAFE top-down, and the
# literature), all reconciled to g C m-2 yr-1, so they can be shown as grouped
# bars (one bar per source) with component stacks and uncertainty.
#
# Outputs:
#   output/upscaling/budget_sources_totals.csv     term x source x class totals (+CI)
#   output/upscaling/budget_sources_components.csv  component breakdown for stacks
#
# CURRENCY g C m-2 yr-1.  SIGN + = C loss/source, - = C gain/uptake/retained.
# =============================================================================
suppressMessages({library(dplyr); library(tidyr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

# --- conversions: any carbon flux (mol C basis) -> g C m-2 yr-1 --------------
UMOL_C <- 12.011e-6 * 3.156e7      # umol/m2/s -> g C/m2/yr  (== 379.1)
NMOL_C <- UMOL_C / 1000            # nmol/m2/s -> g C/m2/yr
CO2_C  <- 12.011 / 44.009          # g CO2 -> g C
CH4_C  <- 12.011 / 16.043          # g CH4 -> g C
MGD_C  <- 365/1000 * CH4_C         # mg CH4/m2/d -> g C/m2/yr
CAMP   <- c("Oct 2022", "Mar 2023")

recl  <- function(x) recode(x, healthy = "Healthy", ghost = "Ghost")
# mean + SE across site x campaign replicates (uncertainty of the class mean)
mse <- function(v) { v <- v[!is.na(v)]; c(m = mean(v), se = if (length(v) > 1) sd(v)/sqrt(length(v)) else NA) }
# CARAFE aligned to OUR campaigns: Oct 2022 (direct wet) + Mar-2023 analog =
# mean(Feb 2023, Apr 2023). Returns the two campaign-analog values.
# PLACEHOLDER: a peak-summer CARAFE season is forthcoming; the current CARAFE
# record is dry-season-weighted and will be updated (would raise wet-season
# CO2 uptake and CH4). Bars using it are flagged with "*".
carafe_camp <- function(df, col) {
  oct <- df[[col]][df$campaign == "Oct 2022"]
  mar <- mean(df[[col]][df$campaign %in% c("Feb 2023", "Apr 2023")], na.rm = TRUE)
  c(oct, mar)[!is.na(c(oct, mar))]
}

totals <- list(); comps <- list()
add_t <- function(term, source, class, m, lo = NA, hi = NA)
  totals[[length(totals)+1]] <<- tibble(term, source, class, value = m, lo, hi)
add_c <- function(term, source, class, component, value)
  comps[[length(comps)+1]] <<- tibble(term, source, class, component, value)

# =============================================================================
# NEE — three independent estimates: our chambers, US-SKR tower, CARAFE
# =============================================================================
co2t <- read.csv("output/upscaling/plot_level_CO2_totals.csv") %>% filter(campaign %in% CAMP)
for (cl in c("healthy","ghost")) {
  s <- co2t %>% filter(disturbance_level == cl)
  r <- mse(s$NEE_bottomup_g * CO2_C)                      # ours (bottom-up chambers)
  add_t("NEE", "Chambers (this study)", recl(cl), r["m"], r["m"]-r["se"], r["m"]+r["se"])
}
# tower (US-SKR, over the SRS-6 healthy mangrove only). Barr 2010 is the SAME
# tower ~18 yr earlier (2004) — shown as a second era, not a different site.
tw <- mse(unique(co2t$NEE_tower[co2t$disturbance_level=="healthy"]) * UMOL_C)  # per-campaign values
add_t("NEE", "US-SKR tower (2022-23)", "Healthy", tw["m"], tw["m"]-abs(tw["se"]), tw["m"]+abs(tw["se"]))
add_t("NEE", "US-SKR tower (Barr 2004)", "Healthy", -1170, -1297, -1043)   # Barr 2010 NEP 1170+/-127
# CARAFE top-down (Delaria et al.): mangrove_forest -> Healthy, ghost_forest -> Ghost
ca <- read.csv("data/carafe_topdown/delaria_CO2_daily_converted.csv")
for (klass in c("mangrove_forest","ghost_forest")) {
  s <- ca %>% filter(class == klass)
  cl <- ifelse(klass == "mangrove_forest", "Healthy", "Ghost")
  v  <- carafe_camp(s, "daily") * UMOL_C          # campaign-aligned (Oct22 + Mar23-analog)
  add_t("NEE", "CARAFE top-down*", cl, mean(v), min(v), max(v))
}

# =============================================================================
# GPP — tower (US-SKR); CARAFE gives NEE not GPP (ghost GPP ~ 0)
# =============================================================================
gp <- mse(unique(co2t$GPP_tower[co2t$disturbance_level=="healthy"]) * UMOL_C)  # + into ecosystem -> store negative
add_t("GPP", "US-SKR tower (2022-23)", "Healthy", -gp["m"], -gp["m"]-gp["se"], -gp["m"]+gp["se"])
add_t("GPP", "US-SKR tower (Barr 2004)", "Healthy", -2270, NA, NA)  # Barr 2010 EC (=NEP+R_E, Fig 11)
add_t("GPP", "CARAFE (top-down)", "Ghost", 0, NA, NA)   # dead canopy, GPP ~ 0

# =============================================================================
# Reco (CO2) — our chambers with component breakdown; Troxler = literature slot
# =============================================================================
comp <- read.csv("output/upscaling/summary_CO2_by_component.csv") %>% filter(campaign %in% CAMP)
# Our chamber Reco = BELOW-CANOPY components (chamber-measured) PLUS a canopy
# leaf-respiration term estimated from LITERATURE (Rd25 x LAI) — NOT a chamber
# flux. Treat them separately: below-canopy is the apples-to-apples comparison
# with Troxler's below-canopy chamber Reco; the leaf term is a literature add-on.
BELOW <- c("soil","root","stem","water","cwd")          # chamber-measured below-canopy
for (cl in c("healthy","ghost")) {
  s <- comp %>% filter(disturbance_level == cl); C <- recl(cl)
  # total ER = below-canopy chambers + literature canopy leaf Rs
  tt <- mse(s$Reco * UMOL_C)
  add_t("Reco", "This study: total ER", C, tt["m"], tt["m"]-tt["se"], tt["m"]+tt["se"])
  for (cc in BELOW) add_c("Reco", "This study: total ER", C, cc, mean(s[[cc]], na.rm=TRUE) * UMOL_C)
  add_c("Reco", "This study: total ER", C, "leaf", mean(s$leaf, na.rm=TRUE) * UMOL_C)  # literature Rs
  # below-canopy only (chamber) — compare to Troxler
  bc <- mse(s$Reco_noleaf * UMOL_C)
  add_t("Reco", "This study: below-canopy", C, bc["m"], bc["m"]-bc["se"], bc["m"]+bc["se"])
  for (cc in BELOW) add_c("Reco", "This study: below-canopy", C, cc, mean(s[[cc]], na.rm=TRUE) * UMOL_C)
}
# --- independent Reco (Healthy) ---------------------------------------------
# Troxler et al. 2015 (Ag.For.Met. 213:273, SRS-4/5/6): BELOW-CANOPY chamber ER
# = heterotrophic 351.5 + autotrophic 140.6 + non-separable 223.3 ~ 715 g C/m2/yr.
# Their TOTAL ER (below-canopy + dark-leaf from Barr) ~ 1570. Barr et al. 2010
# (JGR-BG) eddy-covariance TOTAL ER for SRS-6 ~ 1100 g C/m2/yr.
add_t("Reco", "Troxler 2015: below-canopy",   "Healthy", 715,  NA, NA)
add_t("Reco", "US-SKR tower (Barr 2004): ER", "Healthy", 1100, NA, NA)  # same tower, 2004
add_t("Reco", "Troxler 2015: total ER",       "Healthy", 1570, NA, NA)

# =============================================================================
# CH4 emission — our chambers (components) and CARAFE top-down
# =============================================================================
ch <- read.csv("output/upscaling/summary_CH4_by_component.csv") %>%
  filter(scenario == "exponential", campaign %in% CAMP)
CH4C <- c("stem","soil","root","water")
for (cl in c("healthy","ghost")) {
  s <- ch %>% filter(disturbance_level == cl)
  tot <- mse(s$total * MGD_C)
  add_t("CH4", "Chambers (this study)", recl(cl), tot["m"], tot["m"]-abs(tot["se"]), tot["m"]+abs(tot["se"]))
  for (cc in CH4C) add_c("CH4", "Chambers (this study)", recl(cl), cc, mean(s[[cc]], na.rm=TRUE) * MGD_C)
}
cch <- read.csv("data/carafe_topdown/delaria_endmembers_campaign.csv") %>% filter(gas == "CH4")
for (klass in c("mangrove_forest","ghost_forest")) {
  s <- cch %>% filter(class == klass)
  cl <- ifelse(klass == "mangrove_forest", "Healthy", "Ghost")
  v  <- carafe_camp(s, "flux") * NMOL_C            # campaign-aligned (Oct22 + Mar23-analog)
  add_t("CH4", "CARAFE top-down*", cl, mean(v), min(v), max(v))
}

# =============================================================================
# Lateral export — multiple literature estimates per term (Healthy only)
# =============================================================================
# All Reithmaier 2020 lateral fluxes are normalized to the 15.9 km2 tidally
# inundated MANGROVE area (per Ho 2017) — SAME basis as our per-ground budget,
# so directly usable. His DIC spans Eulerian 622 (mangrove-dominated lower
# estuary; he argues this is most representative) to Lagrangian 92 (whole
# estuary) — a METHOD range, not a normalization artifact.
# "TAlk" is a SUBSET of DIC (durable millennial-sink fraction) — do NOT sum with DIC.
# "POC He 2014" = water-column tidal/POM POC, distinct from Zhao cyclone litter-POC.
lat <- tibble::tribble(
  ~term,   ~source,                        ~class,    ~value, ~lo,  ~hi,
  "DIC",   "Zhao 2021 (forest)",           "Healthy",  145,   61,   229,
  "DIC",   "Ho 2017 (forest,min)",         "Healthy",   86,   75,   97,
  "DIC",   "Reithmaier 2020 (Lagrangian)", "Healthy",   92,   NA,   NA,
  "DIC",   "Reithmaier 2020 (Eulerian)",   "Healthy",  622,  311, 1244,
  "TAlk",  "Reithmaier 2020 (DIC subset)", "Healthy",  425,  210,  846,
  "DOC",   "Romigh 2006 (SRS-6)",          "Healthy",   56,   NA,   NA,
  "DOC",   "Ho 2017 (forest)",             "Healthy",    9,    8,   10,
  "DOC",   "Reithmaier 2020 (Eulerian)",   "Healthy",  171,   88,  346,
  "POC",   "Zhao 2021 (litter)",           "Healthy",  144,   71,  205,
  "POC",   "He 2014 (tidal/POM)",          "Healthy",  3.5,   2,    5,
  "CH4aq", "Yau 2024 (analog)",            "Healthy",  0.35, 0.22, 0.48
)
for (i in seq_len(nrow(lat))) with(lat[i,], add_t(paste0("Lateral ", term), source, class, value, lo, hi))
# CO2 evasion from the estuary/tidal channels (Reithmaier 2020, 92 mmol/m2/d per
# river surface = 403 g C/m2/yr). This is the RIVER air-water CO2 flux — a
# pathway distinct from our forest water-chamber CO2 (already in Reco). Shown for
# completeness; NOT summed into NECB (forest-vs-channel double-count unresolved).
add_t("CO2 evasion", "Reithmaier 2020 (estuary)", "Healthy", 403, NA, NA)

# =============================================================================
# Storage — burial and biomass (multiple estimates; stored as retained C, +)
# =============================================================================
sto <- tibble::tribble(
  ~term,       ~source,                 ~class,    ~value, ~lo, ~hi,
  "Burial",    "Zhao 2021 / Breithaupt","Healthy",  123,   69,  157,
  "dBiomass",  "Castaneda 2013 (woodNPP)","Healthy",200,   65,  247,
  "dBiomass",  "Chen & Twilley 1999",   "Healthy",  480,  480, 540
)
for (i in seq_len(nrow(sto))) with(sto[i,], add_t(term, source, class, value, lo, hi))

# =============================================================================
# NPP allocation / detrital production (INTERNAL fluxes — context only, NOT
# additive to NECB: this carbon leaves via Reco / burial / lateral POC already
# counted elsewhere. Shown to close the NPP-allocation picture.)
# =============================================================================
npp <- tibble::tribble(
  ~term,        ~source,                  ~class,    ~value, ~lo, ~hi,
  "Litterfall", "Castaneda 2013",         "Healthy",  389,   345, 456,  # SRS-6 456/5 345/4 365 (CF0.45)
  "Root NPP",   "Castaneda 2011",         "Healthy",  289,   NA,  NA,   # SRS-5 6.43 Mg/ha/yr
  # CWD production: NO FCE-specific survey exists (Krauss 2005 / Mugi 2022 review).
  # Anchored on mortality x AGB (~1-2% x 100-162 Mg/ha, Castaneda 2013 / Rivera-
  # Monroy 2019) and the global dead-wood-production review (Mugi 2022, upper).
  "CWD prod.",  "Mortality x AGB (FCE)",  "Healthy",  150,   100, 300,
  "CWD prod.",  "Mugi 2022 (global)",     "Healthy",  296,   146, 536
)
for (i in seq_len(nrow(npp))) with(npp[i,], if (!is.na(value)) add_t(term, source, class, value, lo, hi))

# =============================================================================
# WRITE + REPORT
# =============================================================================
term_lv <- c("GPP","NEE","Reco","CH4","Lateral DIC","Lateral TAlk","Lateral DOC","Lateral POC",
             "Lateral CH4aq","CO2 evasion","Burial","dBiomass","Litterfall","Root NPP","CWD prod.")
totals_df <- bind_rows(totals) %>%
  mutate(term = factor(term, levels = term_lv), class = factor(class, levels = c("Healthy","Ghost")))
comps_df  <- bind_rows(comps) %>%
  mutate(term = factor(term, levels = term_lv), class = factor(class, levels = c("Healthy","Ghost")))

dir.create("output/upscaling", showWarnings = FALSE, recursive = TRUE)
write.csv(totals_df, "output/upscaling/budget_sources_totals.csv", row.names = FALSE)
write.csv(comps_df,  "output/upscaling/budget_sources_components.csv", row.names = FALSE)

cat("=== Multi-source budget totals (g C m-2 yr-1; + loss / - gain) ===\n")
totals_df %>% arrange(class, term) %>%
  mutate(value = round(value), lo = round(lo), hi = round(hi)) %>%
  as.data.frame() %>% print(row.names = FALSE)
cat("\nWritten: budget_sources_totals.csv, budget_sources_components.csv\n")
