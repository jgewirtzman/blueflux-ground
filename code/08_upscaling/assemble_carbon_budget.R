# =============================================================================
# assemble_carbon_budget.R
# -----------------------------------------------------------------------------
# Full ecosystem CARBON (CO2 + CH4) mass balance per disturbance class.
#
# This step widens the vertical GHG budget (chambers x TLS + tower GPP) into a
# Net Ecosystem Carbon Balance (NECB) by adding the lateral-export and burial
# terms that close a coastal-wetland carbon budget. Measured terms are wired to
# the manuscript's official numbers (output/upscaling/*). Lateral and burial
# terms are LITERATURE values the user supplies below; they default to NA so the
# budget is never populated with fabricated numbers.
#
# CURRENCY: g C m-2 yr-1  (both gases converted to carbon mass).
# SIGN:  + = C LEAVES the ecosystem  (respired, emitted, laterally exported)
#        - = C ENTERS / is RETAINED  (photosynthetic uptake, burial, biomass gain)
#
#   NECB = -( Reco - GPP + CH4_vert + Lateral )        [ + => ecosystem gaining C ]
#   Closure check:   NECB   vs   (Burial + dBiomass)   measured independently.
#   Residual = NECB - (Burial + dBiomass); nonzero => an unaccounted pathway.
#
# Outputs:
#   output/upscaling/carbon_budget_full.csv     (tidy: one row per class x term)
#   output/upscaling/carbon_budget_summary.csv  (wide: NECB + closure per class)
# =============================================================================
suppressMessages({library(dplyr); library(tidyr)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

# --- unit conversions --------------------------------------------------------
CO2_to_C <- 12.011 / 44.009   # g CO2 -> g C
CH4_to_C <- 12.011 / 16.043   # g CH4 -> g C
CAMP     <- c("Oct 2022", "Mar 2023")   # campaigns behind the annual class means

# =============================================================================
# (1) LITERATURE / EXTERNAL TERMS  — EDIT THESE
# -----------------------------------------------------------------------------
# Fill in per-class values (g C m-2 yr-1) as you pull them from the literature.
# Keep NA for anything not yet sourced. `cite` is carried through to the output
# so every external number stays traceable. Positive = export/loss (lateral),
# positive = accumulation/gain (burial, biomass) — see `role` column below.
#
# Candidate starting points for Everglades / mangrove systems (VERIFY & cite —
# do not use without checking the source and matching units to g C m-2 yr-1):
#   Lateral DIC export : Ho et al. 2017; Maher et al. 2013; Rosentreter 2018
#   Lateral DOC export : Romigh/Twilley; Regier/Rivera-Monroy (FCE LTER)
#   Lateral POC export : Bouillon et al. 2008 (global mangrove synthesis)
#   Dissolved CH4 export: Rosentreter et al. 2018; Call et al. 2015
#   Soil C burial      : Breithaupt et al. 2012 (global); Breithaupt 2020 /
#                        Marchio et al. 2016 (Everglades-specific)
#   dBiomass (woody)   : from your own allometry (DBH x TLS) or FCE LTER growth/
#                        mortality; healthy = accrual (-), ghost = loss (+)
# =============================================================================
lit <- tibble::tribble(
  ~class,     ~lateral_DIC, ~lateral_DOC, ~lateral_POC, ~lateral_CH4, ~burial, ~dBiomass,
  "Healthy",  NA_real_,     NA_real_,     NA_real_,     NA_real_,     NA_real_, NA_real_,
  "Ghost",    NA_real_,     NA_real_,     NA_real_,     NA_real_,     NA_real_, NA_real_
)
lit_cite <- c(
  lateral_DIC = "TODO", lateral_DOC = "TODO", lateral_POC = "TODO",
  lateral_CH4 = "TODO", burial = "TODO", dBiomass = "TODO"
)

# =============================================================================
# (2) MEASURED VERTICAL TERMS  — from the pipeline's official outputs
# =============================================================================
# Net vertical CO2 (NEE) and CH4 emission: the annual, class-level numbers that
# feed net radiative forcing. co2_g_yr = g CO2 m-2 yr-1 (+ source); likewise CH4.
nf <- read.csv("output/upscaling/net_forcing_by_class.csv") %>%
  transmute(class    = recode(disturbance_level, healthy = "Healthy", ghost = "Ghost"),
            NEE_C    = co2_g_yr * CO2_to_C,     # net vertical CO2 as carbon (+ source)
            CH4vert_C = ch4_g_yr * CH4_to_C)    # vertical CH4 emission as carbon (+ source)

# GPP / Reco split for display, anchored so (Reco - GPP) == NEE exactly.
co2t <- read.csv("output/upscaling/plot_level_CO2_totals.csv") %>%
  filter(campaign %in% CAMP) %>%
  group_by(disturbance_level, campaign) %>%                       # site-mean
  summarise(Reco_g = mean(Reco_g), NEE_g = mean(NEE_bottomup_g), .groups = "drop") %>%
  mutate(GPP_g = Reco_g - NEE_g) %>%                              # NEE = Reco - GPP
  group_by(disturbance_level) %>%                                 # campaign-mean
  summarise(GPP_g = mean(GPP_g), .groups = "drop") %>%
  transmute(class = recode(disturbance_level, healthy = "Healthy", ghost = "Ghost"),
            GPP_C = GPP_g * CO2_to_C)                             # magnitude of uptake (>=0)

meas <- nf %>% left_join(co2t, by = "class") %>%
  mutate(GPP_C  = coalesce(GPP_C, 0),
         Reco_C = NEE_C + GPP_C)                                  # so Reco - GPP == NEE

# =============================================================================
# (3) ASSEMBLE TIDY BUDGET (one row per class x term)
# =============================================================================
# role:  source_flux  (+ = C to atmosphere/laterally out)
#        uptake_flux  (photosynthetic C in; stored as NEGATIVE)
#        storage_accum(+ = C retained in ecosystem: burial, biomass gain)
mk <- function(cl) {
  m <- meas[meas$class == cl, ]
  L <- lit[lit$class == cl, ]
  tibble::tribble(
    ~term,              ~category,  ~role,          ~value,          ~source,      ~citation,
    "GPP (uptake)",     "vertical", "uptake_flux",  -m$GPP_C,        "measured",   "tower + chambers",
    "Reco (CO2)",       "vertical", "source_flux",   m$Reco_C,       "measured",   "chambers x TLS",
    "CH4 emission",     "vertical", "source_flux",   m$CH4vert_C,    "measured",   "chambers x TLS",
    "Lateral DIC",      "lateral",  "source_flux",   L$lateral_DIC,  "literature", lit_cite[["lateral_DIC"]],
    "Lateral DOC",      "lateral",  "source_flux",   L$lateral_DOC,  "literature", lit_cite[["lateral_DOC"]],
    "Lateral POC",      "lateral",  "source_flux",   L$lateral_POC,  "literature", lit_cite[["lateral_POC"]],
    "Lateral CH4 (aq)", "lateral",  "source_flux",   L$lateral_CH4,  "literature", lit_cite[["lateral_CH4"]],
    "Soil C burial",    "storage",  "storage_accum", L$burial,       "literature", lit_cite[["burial"]],
    "dBiomass C",       "storage",  "storage_accum", L$dBiomass,     "literature", lit_cite[["dBiomass"]]
  ) %>% mutate(class = cl, pending = is.na(value), .before = 1)
}
budget <- bind_rows(lapply(c("Healthy", "Ghost"), mk))

# =============================================================================
# (4) NECB + CLOSURE (per class)
# =============================================================================
# NECB = -(sum of all source/uptake fluxes). Computed two ways so the reader
# sees how much of the balance still rests on unfilled literature terms.
summ <- budget %>%
  group_by(class) %>%
  summarise(
    flux_measured  = sum(value[role %in% c("source_flux","uptake_flux") & source == "measured"], na.rm = TRUE),
    flux_lateral   = sum(value[category == "lateral"], na.rm = TRUE),
    lateral_pending = any(pending[category == "lateral"]),
    NECB_vertical_only = -flux_measured,                         # measured vertical NECB
    NECB_full          = -(flux_measured + flux_lateral),        # incl. lateral (NA->0)
    burial_accum   = sum(value[term == "Soil C burial"], na.rm = TRUE),
    dbiomass_accum = sum(value[term == "dBiomass C"],   na.rm = TRUE),
    storage_pending = any(pending[category == "storage"]),
    .groups = "drop") %>%
  mutate(storage_indep  = burial_accum + dbiomass_accum,
         closure_resid  = NECB_full - storage_indep)             # ~0 when fully sourced & closed

# =============================================================================
# (5) WRITE + REPORT
# =============================================================================
dir.create("output/upscaling", showWarnings = FALSE, recursive = TRUE)
write.csv(budget, "output/upscaling/carbon_budget_full.csv",    row.names = FALSE)
write.csv(summ,   "output/upscaling/carbon_budget_summary.csv", row.names = FALSE)

cat("=== Full carbon budget (g C m-2 yr-1; + = C loss, - = C gain) ===\n")
budget %>% mutate(value = ifelse(pending, NA, round(value, 1))) %>%
  select(class, term, category, value, source) %>% as.data.frame() %>% print(row.names = FALSE)
cat("\n=== NECB + closure (g C m-2 yr-1) ===\n")
summ %>% transmute(class,
                   NECB_vertical = round(NECB_vertical_only, 1),
                   NECB_full     = round(NECB_full, 1),
                   burial_biomass = ifelse(storage_pending, NA, round(storage_indep, 1)),
                   closure_resid = ifelse(storage_pending | lateral_pending, NA, round(closure_resid, 1))) %>%
  as.data.frame() %>% print(row.names = FALSE)

pend <- budget %>% filter(pending) %>% distinct(term) %>% pull(term)
if (length(pend)) cat("\nLITERATURE TERMS STILL PENDING (edit section 1):\n  - ",
                      paste(pend, collapse = "\n  - "), "\n", sep = "")
cat("\nWritten: carbon_budget_full.csv, carbon_budget_summary.csv\n")
