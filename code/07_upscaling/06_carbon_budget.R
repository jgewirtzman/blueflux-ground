# =============================================================================
# assemble_carbon_budget.R
# -----------------------------------------------------------------------------
# Full ecosystem CARBON (CO2 + CH4) mass balance per disturbance class.
#
# Widens the vertical GHG budget (chambers x TLS + tower GPP) into a Net
# Ecosystem Carbon Balance (NECB) by adding the lateral-export and burial terms
# that close a coastal-wetland carbon budget. Measured terms are wired to the
# manuscript's official numbers (output/upscaling/*). Lateral / burial / biomass
# terms are LITERATURE values (section 1) with central + lo/hi + citation.
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
# (1) LITERATURE / EXTERNAL TERMS  — EDIT THESE  (g C m-2 yr-1; + = loss/export,
#     except burial/dBiomass where + = accumulation retained in ecosystem)
# -----------------------------------------------------------------------------
# Values below are for HEALTHY FCE tall riverine mangroves (Shark River / SRS).
# Where possible they are anchored to Zhao et al. 2021 (Sci. Reports), which
# reports DIC, litter-POC AND burial for the SAME SRS-4/5/6 sites in one
# internally-consistent, area-normalized framework — avoiding the forest-polygon
# vs tidal-contributing-area vs whole-estuary mismatch that inflates single-term
# estimates elsewhere. GHOST-forest values are left NA: no verified ghost-specific
# lateral/burial value exists, and healthy Shark River outwelling should NOT be
# assumed to apply to the ghost stands (different hydrologic setting).
#
# DOUBLE-COUNTING NOTE (DIC): our WATER chambers already measure in-situ air-water
# CO2 evasion. The lateral DIC term below is the tidally EXPORTED (non-degassed,
# to-ocean) DIC pool — complementary to, not overlapping with, the chamber
# evasion. Do NOT additionally add a literature air-water CO2 evasion term.
# (Ho 2017: of DIC entering the rivers, 42-48% degasses [= our water chambers],
#  the rest is exported to the ocean [= this term].) Ho's SF6 evasion was revised
#  DOWN from 171-232 (Ho 2014) to 99-105 mmol/m2/d (Ho 2016/17), now concordant
#  with Reithmaier's 92 -- so no factor-of-2 evasion discrepancy.
# NORMALIZATION (RESOLVED, read Reithmaier 2020 methods): Reithmaier's lateral
#  fluxes ARE normalized to the 15.9 km2 tidally-inundated MANGROVE area (per Ho
#  2017) = SAME basis as our per-ground budget. So DIC 622 IS directly usable;
#  the 6-8x gap vs Ho is METHOD (Eulerian/radon-222 vs Ho's flooding-fraction-
#  limited longitudinal flux, an explicit lower bound), not denominator.
# DECISION: adopt Reithmaier EULERIAN as central for the dissolved terms (he
#  argues it best represents the mangrove-dominated area). This closes the budget
#  non-circularly (basis resolved from methods; residual independently pointed
#  here first). CI spans his reported method range. Conservative (Zhao/Ho) values
#  retained as the low-sensitivity scenario (section 4b).
# see [[carbon-budget-lit-values]] for full provenance / verification votes.
lit <- tibble::tribble(
  ~class,    ~term,              ~value, ~lo,   ~hi,   ~citation,
  # --- HEALTHY (FCE tall riverine mangrove; Shark River / SRS) ---------------
  "Healthy", "Lateral DIC",       622,   311,  1244,   "Reithmaier 2020 Eulerian (142 mmol/m2/d, 15.9km2 mangrove area; range 71-284). Low bound: Zhao 2021 145 [61-229], Ho 2017 ~86 (flooding-limited lower bound)",
  "Healthy", "Lateral DOC",       171,   88,    346,   "Reithmaier 2020 Eulerian (39 mmol/m2/d; range 20-79). Low bound: Romigh 2006 (56, SRS-6), Ho 2017 (8-10)",
  "Healthy", "Lateral POC",       144,   71,    205,   "Zhao et al. 2021 (cyclone litter-POC, SRS-4/5/6 71-205; water-column POC ~0)",
  "Healthy", "Lateral CH4 (aq)",    0.35, 0.22,  0.48, "Yau et al. 2024 (analog, non-FCE; porewater CH4 strongly oxidized before export)",
  "Healthy", "Soil C burial",     123,   69,    157,   "Zhao et al. 2021 / Breithaupt et al. (SRS-4/5/6 69-157; whole-estuary ~123)",
  "Healthy", "dBiomass C",        200,   65,    500,   "Castaneda-Moya et al. 2013 wood NPP, repeat census (SRS-6=197, SRS-4=161, SRS-5=65 gC/m2/yr @ CF0.45); Chen & Twilley 1999 high end ~480-540. Aboveground wood increment; coarse-root adds ~30-50%",
  # --- GHOST (dieback / relict; ghost-specific values unquantified) ----------
  "Ghost",   "Lateral DIC",        NA,    NA,    NA,    "no ghost-specific value",
  "Ghost",   "Lateral DOC",        NA,    NA,    NA,    "no ghost-specific value",
  "Ghost",   "Lateral POC",        NA,    NA,    NA,    "no ghost-specific value",
  "Ghost",   "Lateral CH4 (aq)",   NA,    NA,    NA,    "no ghost-specific value",
  "Ghost",   "Soil C burial",      NA,    NA,    NA,    "no ghost-specific value (relict burial may continue)",
  "Ghost",   "dBiomass C",         NA,    NA,    NA,    "biomass LOSS; hurricane necromass ~2000-2300 gC/m2 stock (Lagomasino/Irma), but decay flux is largely ALREADY in measured CWD + dead-stem Reco -> only the lateral-POC fraction would be additive"
) %>%
  mutate(category = ifelse(term %in% c("Soil C burial", "dBiomass C"), "storage", "lateral"),
         role     = ifelse(term %in% c("Soil C burial", "dBiomass C"), "storage_accum", "source_flux"),
         source   = "literature")

# =============================================================================
# (2) MEASURED VERTICAL TERMS  — from the pipeline's official outputs
# =============================================================================
# Net vertical CO2 (NEE) and CH4 emission: the annual, class-level numbers that
# feed net radiative forcing. co2_g_yr = g CO2 m-2 yr-1 (+ source); likewise CH4.
nf <- read.csv("output/upscaling/net_forcing_by_class.csv") %>%
  transmute(class     = recode(disturbance_level, healthy = "Healthy", ghost = "Ghost"),
            NEE_C     = co2_g_yr * CO2_to_C,    # net vertical CO2 as carbon (+ source)
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

# measured terms as a tidy long table (no CI plumbed for chamber terms here)
meas_long <- meas %>%
  transmute(class,
            `GPP (uptake)` = -GPP_C, `Reco (CO2)` = Reco_C, `CH4 emission` = CH4vert_C) %>%
  pivot_longer(-class, names_to = "term", values_to = "value") %>%
  mutate(category = "vertical",
         role     = ifelse(term == "GPP (uptake)", "uptake_flux", "source_flux"),
         source   = "measured",
         citation = ifelse(term == "GPP (uptake)", "tower + chambers", "chambers x TLS"),
         lo = NA_real_, hi = NA_real_)

# =============================================================================
# (3) ASSEMBLE TIDY BUDGET (one row per class x term)
# =============================================================================
term_lv <- c("GPP (uptake)", "Reco (CO2)", "CH4 emission",
             "Lateral DIC", "Lateral DOC", "Lateral POC", "Lateral CH4 (aq)",
             "Soil C burial", "dBiomass C")
budget <- bind_rows(meas_long, lit) %>%
  mutate(pending = is.na(value),
         term    = factor(term, levels = term_lv),
         class   = factor(class, levels = c("Healthy", "Ghost"))) %>%
  arrange(class, term) %>%
  select(class, term, category, role, value, ci_lo = lo, ci_hi = hi, source, citation, pending)

# =============================================================================
# (4) NECB + CLOSURE (per class), with CI propagated from the literature ranges
# =============================================================================
# NECB = -(sum of all source/uptake fluxes). Lateral CI propagated as an
# envelope (sum of los / sum of his) so the reader sees how much of the balance
# still rests on the uncertain literature terms.
summ <- budget %>%
  group_by(class) %>%
  summarise(
    flux_measured   = sum(value[role %in% c("source_flux","uptake_flux") & source == "measured"], na.rm = TRUE),
    flux_lateral    = sum(value[category == "lateral"], na.rm = TRUE),
    flux_lateral_lo = sum(ci_lo[category == "lateral"], na.rm = TRUE),
    flux_lateral_hi = sum(ci_hi[category == "lateral"], na.rm = TRUE),
    lateral_pending = any(pending[category == "lateral"]),
    burial_accum    = sum(value[term == "Soil C burial"], na.rm = TRUE),
    burial_lo       = sum(ci_lo[term == "Soil C burial"], na.rm = TRUE),
    burial_hi       = sum(ci_hi[term == "Soil C burial"], na.rm = TRUE),
    dbiomass_accum  = sum(value[term == "dBiomass C"], na.rm = TRUE),
    storage_pending = any(pending[category == "storage"]),
    .groups = "drop") %>%
  mutate(NECB_vertical_only = -flux_measured,                          # measured vertical NECB
         NECB_full          = -(flux_measured + flux_lateral),         # incl. lateral (NA->0)
         NECB_full_lo       = -(flux_measured + flux_lateral_hi),      # more export => lower NECB
         NECB_full_hi       = -(flux_measured + flux_lateral_lo),
         storage_indep      = burial_accum + dbiomass_accum,
         closure_resid      = NECB_full - storage_indep)               # ~0 when closed

# =============================================================================
# (4b) LATERAL-EXPORT SENSITIVITY: how closure depends on the DIC method
# -----------------------------------------------------------------------------
# Central (section 1) now adopts Reithmaier EULERIAN dissolved fluxes -> budget
# closes. The 'low' scenario keeps the conservative forest-area lower bounds
# (Zhao DIC 145, Romigh DOC 56) to show that closure hinges on the DIC method
# (Eulerian vs Lagrangian/longitudinal). Both are on the SAME mangrove-area basis.
lat_scen <- tibble::tribble(
  ~scenario,                 ~lateral_total,
  "low (Zhao/Ho conserv.)",  145 + 56 + 144 + 0.35,   # conservative lower bounds
  "central (Reith. Euler.)", 622 + 171 + 144 + 0.35    # adopted central -> closes
)
scen <- summ %>% filter(class == "Healthy") %>%
  transmute(flux_measured, storage_indep) %>%
  tidyr::crossing(lat_scen) %>%
  transmute(class = "Healthy", scenario, lateral_total = round(lateral_total, 0),
            NECB_full     = round(-(flux_measured + lateral_total), 0),
            storage_indep = round(storage_indep, 0),
            closure_resid = round(-(flux_measured + lateral_total) - storage_indep, 0))

# =============================================================================
# (4c) RADIATIVE-FORCING FRAMINGS: what the CO2 term is compared with
# -----------------------------------------------------------------------------
# vertical         : on-site exchange only, CO2 = NEE (the net-forcing figure).
# necb_all_export  : all laterally exported C eventually returns to the air as
#                    CO2 (upper bound), so CO2 = -NECB_full; exported dissolved
#                    CH4 is emitted downstream as CH4.
# necb_alk_retained: as above, but exported alkalinity (Reithmaier 2020 TAlk,
#                    a subset of DIC, 425 [210-846]) stays in the ocean as
#                    bicarbonate, so CO2 = -(NECB_full + TAlk).
# storage          : independent storage only, CO2 = -(burial + wood increment).
# Healthy only: ghost lateral terms are not available (pending), so the ghost
# class is reported on the vertical framing alone.
# GWP: 27.9 (100 yr), 81.2 (20 yr); g CO2-eq m-2 yr-1; + = warming.
C_to_CO2 <- 44.01 / 12.011; C_to_CH4 <- 16.04 / 12.011
TALK <- c(central = 425, lo = 210, hi = 846)
h <- summ %>% filter(class == "Healthy")
ch4_v <- meas$CH4vert_C[meas$class == "Healthy"] * C_to_CH4                     # g CH4
ch4_l <- lit$value[lit$class == "Healthy" & lit$term == "Lateral CH4 (aq)"] * C_to_CH4
frame <- function(name, co2_C, ch4_g, co2_C_lo = NA, co2_C_hi = NA) tibble(
  class = "Healthy", framing = name, CO2_gCO2 = co2_C * C_to_CO2, CH4_g = ch4_g,
  net100 = co2_C * C_to_CO2 + ch4_g * 27.9, net20 = co2_C * C_to_CO2 + ch4_g * 81.2,
  net100_lo = co2_C_lo * C_to_CO2 + ch4_g * 27.9, net100_hi = co2_C_hi * C_to_CO2 + ch4_g * 27.9,
  ch4_pct100 = 100 * ch4_g * 27.9 / (abs(co2_C * C_to_CO2) + ch4_g * 27.9),
  ch4_pct20  = 100 * ch4_g * 81.2 / (abs(co2_C * C_to_CO2) + ch4_g * 81.2))
framings <- bind_rows(
  frame("vertical", h$flux_measured - meas$CH4vert_C[meas$class == "Healthy"], ch4_v),
  frame("necb_all_export", -h$NECB_full - meas$CH4vert_C[meas$class == "Healthy"] - lit$value[lit$class == "Healthy" & lit$term == "Lateral CH4 (aq)"],
        ch4_v + ch4_l, -h$NECB_full_hi, -h$NECB_full_lo),
  frame("necb_alk_retained", -(h$NECB_full + TALK[["central"]]) - meas$CH4vert_C[meas$class == "Healthy"],
        ch4_v + ch4_l, -(h$NECB_full_hi + TALK[["hi"]]), -(h$NECB_full_lo + TALK[["lo"]])),
  frame("storage", -h$storage_indep, ch4_v),
  { g <- meas %>% filter(class == "Ghost")
    frame("vertical", g$NEE_C, g$CH4vert_C * C_to_CH4) %>% mutate(class = "Ghost") })

# =============================================================================
# (5) WRITE + REPORT
# =============================================================================
dir.create("output/upscaling", showWarnings = FALSE, recursive = TRUE)
write.csv(budget, "output/upscaling/carbon_budget_full.csv",      row.names = FALSE)
write.csv(summ,   "output/upscaling/carbon_budget_summary.csv",   row.names = FALSE)
write.csv(scen,   "output/upscaling/carbon_budget_scenarios.csv", row.names = FALSE)
write.csv(framings, "output/upscaling/forcing_framings.csv", row.names = FALSE)
cat("\nForcing framings (g CO2-eq m-2 yr-1):\n"); print(as.data.frame(framings %>% mutate(across(where(is.numeric), ~ round(.x, 1)))))

fmt <- function(x) ifelse(is.na(x), NA, ifelse(abs(x) < 10, sprintf("%.2f", x), sprintf("%.0f", x)))
cat("=== Full carbon budget (g C m-2 yr-1; + = C loss, - = C gain) ===\n")
budget %>%
  mutate(CI = ifelse(is.na(ci_lo), "", paste0(" [", fmt(ci_lo), ", ", fmt(ci_hi), "]"))) %>%
  transmute(class, term, category,
            value = ifelse(pending, NA, paste0(fmt(value), CI)), source) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n=== NECB + closure (g C m-2 yr-1; + = ecosystem gaining C) ===\n")
summ %>% transmute(class,
                   NECB_vertical = round(NECB_vertical_only, 1),
                   NECB_full     = ifelse(lateral_pending, NA,
                                    sprintf("%.0f [%.0f, %.0f]", NECB_full, NECB_full_lo, NECB_full_hi)),
                   burial_biomass = ifelse(storage_pending, NA, round(storage_indep, 1)),
                   closure_resid  = ifelse(storage_pending | lateral_pending, NA, round(closure_resid, 1))) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n=== Healthy lateral-export sensitivity (closure_resid ~0 => budget closes) ===\n")
scen %>% as.data.frame() %>% print(row.names = FALSE)

pend <- budget %>% filter(pending) %>% distinct(class, term)
if (nrow(pend)) {
  cat("\nSTILL PENDING (edit section 1):\n")
  pend %>% as.data.frame() %>% print(row.names = FALSE)
}
cat("\nWritten: carbon_budget_full.csv, carbon_budget_summary.csv, carbon_budget_scenarios.csv\n")
