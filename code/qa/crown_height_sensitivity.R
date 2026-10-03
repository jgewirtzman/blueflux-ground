# =============================================================================
# QA / sensitivity: R. mangle root-crown datum for 2022 stem heights
# (00_lib/rhizophora_crown.R). Refits the stem CH4 models of
# 06_analysis/02_manuscript_results.R with heights from each crown option
# (field = default, tls, none = recorded heights taken as labelled) and reports
# the statistics quoted in the text. Also lists the crown-referenced records.
# Writes output/qa/crown_height_sensitivity.csv and
# output/qa/crown_height_records.csv.
# =============================================================================
suppressMessages({library(dplyr); library(lme4); library(lmerTest); library(emmeans)})
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
source("code/00_lib/rhizophora_crown.R")
d0 <- read.csv("output/data_products/combined_gas_flux_dataset.csv") %>%
  filter(is.na(CO2_best.flux) | CO2_best.flux >= -10) %>%
  mutate(season_agg = factor(case_when(season == "wet" ~ "Wet", season == "dry" ~ "Dry"), c("Wet", "Dry")),
         species = ifelse(species %in% "COPE", "COER", species))
nominal <- c(-50, -25, 0, 25, 50, 100, 150, 170)
heights <- function(mode) {
  Sys.setenv(HEIGHT_CROWN = mode); cr <- crown_heights(d0); Sys.unsetenv("HEIGHT_CROWN")
  depth0 <- ifelse(!is.na(d0$water_depth) & d0$water_depth > 0, d0$water_depth, 0)
  fc <- mode != "none" & d0$species %in% "RHMA" & d0$month_year %in% c("2022-03", "2022-10") &
    d0$component %in% c("stem", "root") & !is.na(d0$height) & d0$height %in% nominal & d0$plot %in% names(cr)
  hs <- ifelse(fc, pmax(0, cr[d0$plot] + d0$height), ifelse(d0$above %in% "water", d0$height + depth0, pmax(d0$height, 0)))
  list(h = hs - depth0, crown = cr, from_crown = fc)
}
cat_h <- function(h) factor(case_when(h < 50 ~ "0-50 cm", h < 100 ~ "50-100 cm", h < 150 ~ "100-150 cm", TRUE ~ ">150 cm"),
                            c("0-50 cm", "50-100 cm", "100-150 cm", ">150 cm"))
pc <- function(pr, a, b) { s <- summary(pr); i <- which(s$contrast %in% c(paste(a, "-", b), paste(b, "-", a))); s$p.value[i] }
fit <- function(mode) {
  H <- heights(mode); d <- d0 %>% mutate(hc = H$h)
  s <- d %>% filter(component == "stem", plot %in% c("BL60", "SRS5", "SRS6"), !is.na(species), !species %in% c("UNKN", ""),
                    !is.na(CH4_best.flux), !is.na(hc), hc >= 0) %>% mutate(y = asinh(CH4_best.flux), plot = factor(plot))
  s <- s %>% filter(species %in% names(which(table(species) >= 5))) %>% mutate(species = factor(species))
  m1 <- lmer(y ~ species + hc + season_agg + (1 | plot), data = s)
  a1 <- anova(m1); pr1 <- pairs(emmeans(m1, ~ species))
  em <- summary(emmeans(m1, ~ species, at = list(hc = c(0, 100), season_agg = "Wet"), by = "hc"))
  rh <- sinh(em$emmean[em$species == "RHMA"])
  s2 <- s %>% mutate(hcat = cat_h(hc)) %>% filter(hcat != ">150 cm") %>% mutate(hcat = droplevels(hcat))
  m2 <- lmer(y ~ species * hcat + season_agg + (1 | plot), data = s2); a2 <- anova(m2)
  ad <- d %>% filter(component == "stem", plot %in% c("BL60", "CP40", "FLM30", "SRS5", "SRS6"), !is.na(species),
                     !species %in% c("UNKN", ""), !is.na(CH4_best.flux), !is.na(hc), hc >= 0, !is.na(status), status != "CWD") %>%
    mutate(alive = ifelse(tolower(status) == "alive", "alive", "dead"), hcat = cat_h(hc), y = asinh(CH4_best.flux), plot = factor(plot)) %>%
    filter(hcat != ">150 cm", species %in% c("AVGE", "COER", "LARA", "RHMA")) %>%
    mutate(sp = factor(ifelse(species %in% c("AVGE", "RHMA"), paste0(species, "_", alive), species)), hcat = droplevels(hcat))
  m3 <- lmer(y ~ sp + hcat + season_agg + (1 | plot), data = ad); pr3 <- pairs(emmeans(m3, ~ sp))
  data.frame(crown_mode = mode, crown_cm = paste(names(H$crown), round(H$crown), collapse = "; "),
             n_from_crown = sum(H$from_crown), n_model1 = nrow(s), n_model2 = nrow(s2),
             p_species = a1["species", "Pr(>F)"], height_coef = fixef(m1)[["hc"]], p_height = a1["hc", "Pr(>F)"],
             p_season = a1["season_agg", "Pr(>F)"],
             p_LARA_RHMA = pc(pr1, "LARA", "RHMA"), p_COER_LARA = pc(pr1, "COER", "LARA"),
             LARA_emm = sinh(summary(emmeans(m1, ~ species))$emmean[summary(emmeans(m1, ~ species))$species == "LARA"]),
             RHMA_emm = sinh(summary(emmeans(m1, ~ species))$emmean[summary(emmeans(m1, ~ species))$species == "RHMA"]),
             RHMA_wet_0cm = rh[1], RHMA_wet_100cm = rh[2],
             p_species_x_height = a2["species:hcat", "Pr(>F)"],
             p_AVGE_alive_dead = pc(pr3, "AVGE_alive", "AVGE_dead"), p_RHMA_alive_dead = pc(pr3, "RHMA_alive", "RHMA_dead"))
}
out <- bind_rows(lapply(c("field", "tls", "none"), fit))
write.csv(out, "output/qa/crown_height_sensitivity.csv", row.names = FALSE)
print(t(out %>% mutate(across(where(is.double), ~ signif(.x, 3)))))
H <- heights("field")
write.csv(d0[H$from_crown, c("flux_id", "plot", "month_year", "component", "status", "height", "above", "water_depth")] %>%
            mutate(crown_cm = round(H$crown[plot]), height_above_sediment_cm = pmax(0, crown_cm + height)),
          "output/qa/crown_height_records.csv", row.names = FALSE)
