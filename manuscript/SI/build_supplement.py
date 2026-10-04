#!/usr/bin/env python3
"""Build the Science-format supplement (Markdown + Word) from supplement_Science.md.

Fills {{FIG Sn}}, {{TABLE Sn}}, {{DATA S2}} and {{REFS}} from the workflow outputs:
figures from output/figures, tables from the CSVs written by the R pipeline (and the
existing sensitivity / literature tables), references from manuscript/references.md.
Writes manuscript/SI/supplement_Science_built.md and supplement_Science.docx.

usage: python3 manuscript/SI/build_supplement.py
"""
import csv, os, re, subprocess, sys
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), ".."))
from refstyle import science

root = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
P = lambda *a: os.path.join(root, *a)
src = open(P("manuscript/SI/supplement_Science.md"), encoding="utf-8").read()

def rows(path):
    with open(P(path), encoding="utf-8") as f:
        return list(csv.DictReader(f))

def fmt(x, d=1):
    try:
        v = float(x)
    except (TypeError, ValueError):
        return "" if x in (None, "NA") else str(x)
    if v != v:
        return ""
    if abs(v) >= 1000:
        return f"{v:,.0f}".replace(",", " ")
    return f"{v:.{d}f}"

def md_table(header, body):
    out = ["| " + " | ".join(header) + " |", "|" + "---|" * len(header)]
    out += ["| " + " | ".join(str(c) for c in r) + " |" for r in body]
    return "\n".join(out)

def ci(m, lo, hi, d=1):
    if fmt(lo, d) == "":
        return fmt(m, d)
    return f"{fmt(m, d)} ({fmt(lo, d)} to {fmt(hi, d)})"

# ------------------------------------------------------------------ figures
FIGS = {
 "S1": (["output/figures/other/pub_SI_chamber_photos.png"],
        "Chamber types: (a) soil cylinder, (b) soil cylinder enclosing pneumatophores, (c) soil collar and dome, (d) floating water chamber, (e) prop root, (f) stem, (g) woody debris and (h) leaf."),
 "S2": (["output/figures/other/si_satellite_scenes.png"],
        "Sentinel-2 true-colour scenes of each site (rows; class colours) in every dry season (January–April) 2016–2025 (columns); Hurricane Irma (September 2017) falls between the 2017 and 2018 columns. 600 × 600 m around the plot for each year, the scene with the most cloud-free, valid pixels over the chip (reflectance stretched 0–0.15; Microsoft Planetary Computer)."),
 "S3": (["output/figures/other/si_ndvi.png"],
        "Site greenness through time. (a) Dry-season (January–April) Landsat NDVI at each site, 1995–2025: median of cloud-free Landsat 5, 7, 8 and 9 surface-reflectance scenes (up to eight per year) over a 90 × 90 m window at the plot (centred 100 m inland at the river-edge SRS5 and SRS6 plots); dashed lines, Hurricanes Wilma (October 2005), Irma (September 2017) and Ian (September 2022). (b) Wet- versus dry-season greenness after Hurricane Irma: Sentinel-2 NDVI (3 × 3 pixel, 30 m window) in the wet (August–November) and dry (January–April) seasons of 2018–2025, median of up to three clear scenes per season; grey lines join the two seasons of a year; diamonds and bars, mean and range. At the river-edge SRS5 and SRS6 plots the window is centred 100 m inland. Negative values at the ghost plots in the wet season reflect standing water."),
 "S4": (["output/figures/other/sampling_design.png", "output/figures/other/water_positions_by_campaign.png"],
        "Sampling design relative to tide and standing water. (a) Chamber measurements by site and campaign against water level above the soil (FCE LTER loggers; the SRS5 October 2022 record has a gap before 19 October); diamonds at −2 cm mark chambers without a recorded water depth; triangles mark dissolved-gas samples. (b) Water-surface CH4 flux by campaign and site, by position (above the flooded forest floor, or in a channel or open water at the tidal sites; the ghost and regenerating sites have no channels): chamber fluxes (filled circles; bars, arithmetic means) and fluxes estimated from dissolved CH4 (open symbols; F = k600 (Sc/600)^−0.5 (Cw − Ceq) with k600 = 1.29 cm h−1, whiskers 0.49–6.8, the range of site medians; M8): our plot surface-water samples (diamonds) and the BlueFlux tidal-river survey (squares; river stations and the SRS6 tidal creek; Vaughn and Raymond 2024). At SRS5 and SRS6 in March 2023, channel and floor chamber fluxes did not differ (Wilcoxon p = 0.29 and 1). The dissolved estimates span the chamber fluxes except at CP40 in October 2022, where the plot sample was depleted in CH4 (25 nM) while the chambers recorded ebullition."),
 "S5": (["output/figures/other/pub_component_by_plot_campaign_combined_condensed_boot.png"],
        "CH4 (a) and CO2 (b) flux by component for every plot and campaign, including context sites. Points are individual measurements; filled circles are arithmetic means, with bootstrap 95% CIs where n > 3; inverse-hyperbolic-sine axes."),
 "S6": (["output/figures/other/pub_SI_ebullition_partition.png"],
        "Ebullition. (a) Diffusive and detected ebullitive water-surface CH4 by site and season, with an upper bound on undetected ebullition (pale; excess of a whole-placement fit over diffusive + detected ebullitive flux, LGR placements); labels give n and the ebullitive share (detected–upper bound); bars, means ± SE of the total (shown where n > 3); points, individual measurements; each panel has its own linear y axis. (b) Example placements: a single bubble mid-placement (CP40, October 2022), two bubbles (CP40, March 2023), and long bubble-free placements in ghost (FLM30, 31 min) and intact forest (SRS6, 14 min), showing detected bubble steps (shaded), the de-ebulliated series and the diffusive linear fit (M7)."),
 "S7": (["output/figures/other/pub_SI_pneumatophore_density.png"],
        "Soil CH4 (a) and CO2 (b) flux against pneumatophore density per collar, by site (dry-season visits; shape, visit). Lines, least-squares fits ± 95% CI; ρ, Spearman rank correlation with p and n. Marco Island collars had no pneumatophores and are not shown."),
 "S8": (["output/figures/other/pub_stem_height_composite_combined.png"],
        "Stem detail at SRS5, SRS6, BL60, CP40 and FLM30. (a, b) Stem CH4 and CO2 by height class and forest class: overlapping class densities (inverse-hyperbolic-sine scale) for each height band, with each class's measurements in its own row beneath and arithmetic means (diamonds) for groups with n > 3. (c, d) CH4 and CO2 estimated marginal means (95% CI), averaged over height, by species and live/dead status from linear mixed-effects models with height class and season as fixed effects and site as a random intercept (M20); n, closures per group. Dead stems come mostly from the ghost sites, so status is partly confounded with forest class."),
 "S9": (["output/figures/other/stem_extrap_clean.png"],
        "Stem height extrapolation (text S1). (a) Measured stem CH4 (points; height above water or soil) and the stem profile under each of six extrapolation rules (lines; TLS height above ground), by site and campaign; the default rule is exponential decay to zero; shaded, above the 1.5 m chamber limit; square-root height axis; inverse-hyperbolic-sine flux axes that differ by site. (b) Stem CH4 per unit ground area (absolute tree flux) and (c) total plot CH4 under each rule, with non-stem components held constant and high tide at tidal sites; x axes differ by site."),
 "S10": (["output/figures/other/SA_by_segment_height_fixedY.png"],
        "Laser-scanned woody surface per unit ground area (m2 m−2 per 0.5 m height bin) by segment class (prop root, stem, branch) for the four scanned plots; labels give total woody surface per unit ground area and the share above the 1.5 m chamber height limit (dashed line)."),
 "S11": (["output/figures/other/si_tide_states.png"],
        "Stand CH4 of the intact forest at high and low tide by component (SRS5, SRS6; October 2022 and March 2023; exponential stem rule). Diamonds and bars, stand total and Monte Carlo 95% interval; dashed line, the tide-weighted value used in the budgets (flooded share of the floor in the campaign month; M11); inverse-hyperbolic-sine axis."),
 "S12": (["output/figures/other/si_flood_fraction.png"],
         "Flooded share of the intact forest floor at the tidal sites (M11; table S13). (a) Hourly water level (FCE LTER logger datum) at SRS5 and SRS6 in the campaign months; points, logger level at each chamber water-depth reading (filled, standing water; open, none); dashed line and band, fitted mean floor height ± 1 SD. (b) Floor height relative to the logger datum, fitted as a normal distribution by censored maximum likelihood from the depth readings (green; rug, readings with standing water), against the distribution of hourly water level in each campaign month; floor below the water is flooded. (c) Campaign-month flooded share: area-weighted mean of the hourly flooded share (central; bars, floor mean ± 1.96 SE), the all-or-nothing switch (whole floor flooded when the offset-corrected logger level is above zero) and the long-term area-weighted value (2010 onward, dashed)."),
 "S13": (["output/figures/other/si_waterline.png"],
         "Water level against the woody surfaces at the four laser-scanned sites (M10, M11; vertical axis to scale, cm above the plot's mean floor; horizontal positions, prop-root reach and pneumatophores illustrative). Lines, water level by campaign (dark, wet season, October 2022; light, dry season, March 2023): at the tidal intact sites (a, b), mean high (dashed) and low (solid) water of the campaign month from the FCE LTER loggers; at the ghost sites (c, d), mean recorded depth. Floor relief follows the fitted floor-height distribution (tidal sites) or the recorded depths (ghost sites); stems stand on the mean floor (tidal) or at the mean floor height of the stem chambers (ghost); stem and prop-root diameters, root heights and stem number per 7 m are from TLS; downed wood has the median measured diameter and lies on the mean floor. Subtitles give the share of each surface above water used in the budgets (mean of the two campaigns; lowest 0.5 m of stem), the flooded share of the floor, and the median annual maximum water level (2001–2024). Hourly extremes (99th and 1st percentiles) in the campaign months were +20 and −2 cm (October) and +12 and −28 cm (March) at SRS5, and +43 and −3 cm and +33 and −26 cm at SRS6."),
 "S14": (["output/figures/other/si_k600_compare.png"],
         "Gas-transfer velocity (k600) used for the water-surface fluxes estimated from dissolved gases (M8), against published values. Orange, Everglades measurements and the Everglades-fitted relation; grey, other systems; circles, measurements; triangles, equations. Diamond, median of the site medians of chamber-implied k600 (1.29 cm h−1; dashed line), the value used; bar, range of the site medians; ticks, the site medians. Measured: sheltered Everglades wetland water (Happell et al. 1995; Variano et al. 2009; Ho et al. 2018, as compiled there), Shark River channel tracer releases (Ho et al. 2014, 2016) and the river and estuary compilation of Raymond and Cole (2001). Equations, evaluated for forest-floor water (no current, 0.1 m depth) at the 1.2–2.4 m s−1 winds of the campaigns (point at 1.8): Ho et al. (2016) and Borges et al. (2004) without their current terms, Rosentreter et al. (2017; CH4), Wanninkhof (2014), Cole and Caraco (1998) and the three wind fits of Raymond and Cole (2001). The stream relations of Raymond et al. (2012) require channel slope and velocity and are not applicable to standing water."),
 "S15": (["output/figures/other/si_sensitivity_switch.png"],
         "Sensitivity of the intact-to-ghost switch (GWP20) to analytical choices, one at a time (text S9; table S10). Vertical line, central case (6,369 g CO2-eq m−2 yr−1); points left of it lower the switch, right of it raise it. Analytical choices keep the switch between ~5,600 and ~8,000 (GWP100: ~5,000–7,300); only carbon-balance framings that count lateral export or storage alone lower it further."),
 "S16": (["output/figures/other/si_S10_mc_uncertainty.png"],
         "Component CH4 per unit ground area by site and campaign: Monte Carlo mean and 95% interval (joint flux-rate and surface-area uncertainty; tide states weighted by the flooded share of the floor), with each component's share of the variance of stand CH4 at right (text S4); inverse-hyperbolic-sine axis; components that are always zero omitted."),
 "S17": (["output/figures/other/ed_porewater_rounds.png"],
         "Porewater by site, sampling round (October 2022, March 2023, October 2025) and depth: (a) dissolved CH4 and (b) salinity (site × round × depth means; porewater only); (c) salinity against dissolved CH4 by site (including surface water; shape, round; log(1 + x) axis). Site names coloured by forest class. No porewater CH4 was analysed for SRS5 and SRS6 in March 2023."),
 "S18": (["output/figures/other/si_S12_carbonate.png"],
         "Porewater alkalinity against conservative mixing (October 2025; 0–90 cm, surface water excluded; text S7). (a) Total alkalinity against salinity, with the mixing line (dashed; end-members salinity 0, 3,000 µM and salinity 35, 2,400 µM). (b) Measured against mixing-predicted alkalinity; dashed, 1:1. (c) Measured ÷ mixing-predicted alkalinity by site. Fill, forest class; shape, site. FLM30 was not sampled for alkalinity."),
 "S19": (["output/figures/other/si_campaign_context.png"],
         "Campaign context (M2). Monthly z-scores of SRS6 water level (FCE LTER, 2001–2024), air temperature and midday net CO2 uptake (US-Skr tower, 2004–2011 and 2018–2023) and tower CH4 flux (2018–2023; FCH4 + storage, quality flag ≤ 1, u* > 0.2), each standardised over all of its year-months. Lines, median for each calendar month; bands, interquartile range across years (16–22 years per month for water level, 2–6 for tower CH4); points, the campaign months (October 2022, March 2023) with ±1 SD of daily means within the month; shading, campaign months. The tower CH4 record carries a negative dry-season offset that the z-score removes; only its seasonal pattern is shown."),
 "S20": ([], "_[PLACEHOLDER — sediment metagenome detail: taxonomy by site and depth, marker-gene abundance per gram sediment and per gram organic carbon, DNA yield (Peccia laboratory).]_"),
}
def fig_block(k):
    files, cap = FIGS[k]
    imgs = "\n\n".join(f"![](<{f}>){{width=6.5in}}" for f in files if os.path.exists(P(f)))
    missing = [f for f in files if not os.path.exists(P(f))]
    note = f" _[missing: {', '.join(missing)}]_" if missing else ""
    return f"{imgs}\n\n**Fig. {k}.** {cap}{note}\n"

# ------------------------------------------------------------------ tables
cls = {"healthy": "intact", "regenerating": "regenerating", "ghost": "ghost", "scrub": "scrub"}
site_class = {"SRS5": "intact (core)", "SRS6": "intact (core)", "BL60": "regenerating (core)", "CP40": "ghost (core)",
              "FLM30": "ghost (core)", "MI": "ghost (context)", "RB10": "intact (context)", "SE1": "scrub (context)"}

def t_S1():
    rs = rows("output/analysis/si/si_site_table.csv")
    sites = list(dict.fromkeys(r["site"] for r in rs))
    attrs = list(dict.fromkeys(r["row"] for r in rs))
    val = {(r["row"], r["site"]): r["value"] for r in rs}
    sp = lambda s: ", ".join(f"*{x.strip()}*" for x in s.split(","))
    b = [[a] + [sp(val.get((a, s), "")) if a == "Dominant species" else val.get((a, s), "–") for s in sites] for a in attrs]
    return ("**Table S1. Study sites.** Core sites (SRS5–FLM30) and context sites (RB10, SE1, MI). Campaigns: month and year "
            "(number of chamber closures). Stand structure from terrestrial laser scanning of the four scanned plots (M10); "
            "DBH and height include standing dead trunks at the ghost sites; woody surface per m² of ground. "
            "FCE LTER: trees > 2.5 cm DBH in two 20 × 20 m plots, 2023 survey (data S2). "
            "Water level: at the tidal sites, mean high / low water of the campaign month above the plot's mean floor "
            "(FCE LTER loggers; M11), with the flooded share of the floor in hours; elsewhere, mean (maximum) water depth "
            "recorded at chamber positions. Porewater salinity: mean of the porewater samples (M16).\n\n"
            + md_table([""] + sites, b))

def t_S2():
    b = [[r["category"], r["n"], r["components"]] for r in rows("output/analysis/si/si_exclusions.csv")]
    return ("**Table S2. Measurement exclusions by criterion** (M6). Duplicate records are not independent closures; "
            "no flux was removed for being small, negative or below detection.\n\n" + md_table(["Criterion", "n", "By component"], b))

def t_S3():
    rr = rows("output/analysis/si/si_sample_sizes.csv")
    comps = [c for c in ["stem", "root", "soil", "water", "cwd", "leaves"] if c in rr[0]]
    b = [[r["plot"], r["campaign"]] + [r[c] for c in comps] + [r["total"]] for r in rr]
    return ("**Table S3. Analysed fluxes by site, campaign and component.** The intact water-surface term in October 2022 (SRS5, SRS6), "
            "where no floating chamber was deployed, was estimated from dissolved CH4 and CO2 (M8).\n\n" + md_table(["Site", "Campaign"] + comps + ["Total"], b))

def t_S4():
    rr = rows("output/analysis/si/si_component_rates.csv")
    b = [[r["component"], r["class"], r["season"], r["n_CH4"], ci(r["CH4_mean"], r["CH4_lo"], r["CH4_hi"], 2),
          ci(r["CO2_mean"], r["CO2_lo"], r["CO2_hi"], 2)] for r in rr]
    return ("**Table S4. Component flux rates by class and season** (bootstrap mean and 95% CI; CH4 nmol m−2 s−1, CO2 "
            "µmol m−2 s−1 per m2 of enclosed surface; intervals for n ≥ 3).\n\n" +
            md_table(["Component", "Class", "Season", "n", "CH4", "CO2"], b))

def t_S5():
    comp = rows("output/analysis/woody_height_model_comparison.csv")
    fx = rows("output/analysis/woody_height_model_fixed.csv")
    sp = rows("output/analysis/woody_height_model_species_check.csv")
    a = md_table(["Height form", "AIC", "edf", "ΔAIC"], [[r["height_form"], fmt(r["AIC"]), fmt(r["edf"]), fmt(r["dAIC"])] for r in comp])
    b = md_table(["Term", "Estimate", "SE", "p"], [[r["term"], fmt(r["estimate"], 3), fmt(r["se"], 3), fmt(r["p"], 3)] for r in fx])
    keys = list(sp[0].keys())
    c = md_table(keys, [[fmt(r[k], 3) if k != keys[0] else r[k] for k in keys] for r in sp])
    return ("**Table S5. Woody CH4 height model** (M20). (A) Height forms compared by AIC. (B) Parametric terms of the selected "
            "model (asinh scale; terms ending '.1' are on the scale part). (C) Species added to the selected model (reference *R. mangle*).\n\n"
            f"(A)\n\n{a}\n\n(B)\n\n{b}\n\n(C)\n\n{c}")

def t_S6():
    rr = rows("data/carafe_topdown/delaria_endmembers_campaign.csv")
    b = [[r["gas"], r["campaign"], r["class"].replace("_", " "), fmt(r["flux"]), fmt(r["se"]), r["units"], r["source"]] for r in rr]
    adj = rows("output/upscaling/supp_carafe_inundation_adjudication.csv")
    a2 = md_table(list(adj[0].keys()), [[fmt(v, 1) if i else v for i, v in enumerate(r.values())] for r in adj])
    return ("**Table S6. Airborne end-member fluxes** (two-class disaggregation, Delaria et al. 2024) and the airborne check of the "
            "ghost-forest inundation representation (text S2). _[July 2024 pending; confirm uncertainty definition.]_\n\n"
            + md_table(["Gas", "Deployment", "Class", "Flux", "SE", "Units", "Source"], b) + "\n\n" + a2)

def t_S7():
    rr = [r for r in rows("output/upscaling/summary_CH4_by_component.csv") if r["scenario"] == "exponential"]
    a = md_table(["Site", "Campaign", "Class", "Stem", "Root", "Soil", "Water", "Downed wood", "Total"],
                 [[r["site"], r["campaign"], cls.get(r["disturbance_level"], r["disturbance_level"])] +
                  [fmt(r[c], 2) for c in ["stem", "root", "soil", "water", "cwd", "total"]] for r in rr])
    co = rows("output/upscaling/plot_level_CO2_totals.csv")
    b = md_table(["Site", "Campaign", "Stem", "Root", "Soil", "Water", "Downed wood", "Leaf", "Respiration", "GPP", "NEE"],
                 [[r["site"], r["campaign"]] + [fmt(r[c], 2) for c in ["stem", "root", "soil", "water", "cwd", "leaf", "Reco", "GPP_used", "NEE_bottomup"]] for r in co])
    fr = rows("output/upscaling/flux_rates_with_gapfills.csv")
    c = md_table(["Site", "Campaign", "Component", "CH4 rate (95% CI)", "n", "Source"],
                 [[r["site"], r["campaign"], r["component"], ci(r["flux_rate"], r["ci_lo"], r["ci_hi"], 2), r["n_obs"], r["fill_source"]] for r in fr])
    return ("**Table S7. Stand budgets by site and campaign.** (A) CH4 by component, tide-weighted (mg CH4 m−2 ground d−1). "
            "(B) CO2 by component (µmol m−2 ground s−1; NEE = respiration − GPP). (C) Component CH4 rates used and their source "
            "(nmol m−2 s−1; 'gap' marks a rate taken from another site or campaign, M12).\n\n(A)\n\n" + a + "\n\n(B)\n\n" + b + "\n\n(C)\n\n" + c)

def t_S8():
    nf = {r["disturbance_level"]: r for r in rows("output/upscaling/net_forcing_by_class.csv")}
    mc = {r["class"]: r for r in rows("output/upscaling/mc_net_forcing_by_class.csv")}
    b = []
    for k, lab in [("healthy", "intact"), ("ghost", "ghost")]:
        n, m = nf[k], mc[k]
        b.append([lab, fmt(n["ch4_g_yr"], 2), fmt(n["co2_g_yr"], 0), fmt(n["net20"], 0), f"{fmt(m['net20_lo'],0)} to {fmt(m['net20_hi'],0)}",
                  fmt(n["net100"], 0), f"{fmt(m['net100_lo'],0)} to {fmt(m['net100_hi'],0)}", fmt(n["net_gwpstar"], 0),
                  f"{n['ch4_pct20']}%"])
    return ("**Table S8. Annual budgets and net forcing by class** (g m−2 yr−1; forcing in g CO2-eq m−2 yr−1; Monte Carlo 95% intervals).\n\n"
            + md_table(["Class", "CH4", "Net CO2", "Net GWP20", "95% interval", "Net GWP100", "95% interval", "Net GWP*", "CH4 share (GWP20)"], b))

def from_ncc_si(label):
    s = open(P("manuscript/SI/manuscript_NCC_SI.md"), encoding="utf-8").read()
    i = s.index(f"**Table {label}.")
    j = s.index("\n\n", s.index("\n|", i) + 2)
    return re.sub(r"\s*(?:Source|Sources?):\s*`?output/[^\s`]+`?\.?", "", s[i:j])   # drop pipeline file paths

def t_S9():
    return from_ncc_si("S9").replace("(S.M10)", "(M13)").replace("(S.T3)", "(text S3)").replace("(S.M17)", "(M18)") \
        .replace("(S.M16)", "(M22)").replace("S.M15", "M11").replace("(S.T2", "(text S2").replace("(S.T5)", "(text S5)")

def t_S10():
    return from_ncc_si("S10").replace("(S.T9)", "(text S9)")

def t_S11():
    rr = rows("output/data_products/porewater_N_fce_context.csv")
    return ("**Table S11. Porewater inorganic nitrogen: this study against FCE LTER monitoring at SRS5 and SRS6** (µmol L−1).\n\n" +
            md_table(list(rr[0].keys()), [[fmt(v, 2) if i > 1 else v for i, v in enumerate(r.values())] for r in rr]))

def t_S12():
    rr = rows("output/upscaling/supp_context_site_areal_rates.csv")
    return ("**Table S12. Component CH4 rates at all sites, campaigns pooled** (nmol m−2 s−1 per m2 of surface; 95% CI; context sites "
            "MI, RB10, SE1 alongside the core sites; text S8).\n\n" +
            md_table(["Site", "Component", "CH4 (95% CI)", "n"], [[r["plot"], r["component"], ci(r["nmol_m2_s"], r["lo"], r["hi"], 2), r["n"]] for r in rr]))

def t_S13():
    rr = rows("output/upscaling/flood_fraction.csv")
    return ("**Table S13. Flooded share of the intact forest floor** (M11): campaign-month mean of the hourly flooded share (±1.96 SE of the floor mean), "
            "the all-or-nothing switch and the long-term value, with the fitted floor-height distribution (cm relative to the logger datum).\n\n" +
            md_table(["Site", "Campaign", "Flooded share", "Switch", "2010–2023", "Floor µ (SE)", "Floor σ", "n readings (wet)"],
                     [[r["site"], r["campaign"], ci(r["frac_flooded"], r["frac_flooded_lo"], r["frac_flooded_hi"], 2), fmt(r["frac_flooded_switch"], 2),
                       fmt(r["frac_flooded_longterm"], 2), f"{fmt(r['floor_mu_cm'])} ({fmt(r['floor_mu_se'])})", fmt(r["floor_sd_cm"]),
                       f"{r['n_floor']} ({r['n_floor_wet']})"] for r in rr]))

def t_S14():
    rr = rows("output/upscaling/regional_ghost_forcing.csv")
    a = md_table(["Territory", "Area (km2)", "Switch GWP20 (Tg CO2-eq yr−1)", "GWP100", "GWP*", "Induced CH4 (Gg yr−1)"],
                 [[r["country"], fmt(r["area_km2"], 2), ci(r["switch_gwp20_Tg"], r["switch_gwp20_lo_Tg"], r["switch_gwp20_hi_Tg"], 3),
                   fmt(r["switch_gwp100_Tg"], 3), fmt(r["switch_gwpstar_Tg"], 3), ci(r["ch4_induced_Gg"], r["ch4_induced_lo_Gg"], r["ch4_induced_hi_Gg"], 3)] for r in rr])
    st = rows("output/upscaling/regional_ghost_forcing_by_storm.csv")
    b = md_table(["Storm", "Area (km2)", "Switch GWP20 (Tg)", "GWP100 (Tg)", "Induced CH4 (Gg)"],
                 [[r["storm"], fmt(r["area_km2"], 1), ci(r["switch_gwp20_Tg"], r["switch_gwp20_lo_Tg"], r["switch_gwp20_hi_Tg"], 2),
                   fmt(r["switch_gwp100_Tg"], 2), ci(r["ch4_induced_Gg"], r["ch4_induced_lo_Gg"], r["ch4_induced_hi_Gg"], 2)] for r in st])
    return ("**Table S14. Regional scaling of the switch over 2017 hurricane dieback** (M19). (A) By territory. (B) By storm, with the Florida bound.\n\n"
            "(A)\n\n" + a + "\n\n(B)\n\n" + b)

def t_S15():
    rr = rows("manuscript/literature/value_catalog.csv")
    return ("**Table S15. Literature values for canopy leaf respiration and leaf area** (M13; full provenance in the repository).\n\n" +
            md_table(["ID", "Quantity", "Value", "Units", "Species", "Site", "Method", "Source"],
                     [[r["id"], r["quantity"], r["value_as_published"], r["value_units"], r["species"], r["site"], r["method"], r["source_short"]] for r in rr]))

TABLES = {f"S{i}": globals()[f"t_S{i}"] for i in range(1, 16)}

# ------------------------------------------------------------------ data S2 and references
def data_s2():
    out = []
    for line in open(P("manuscript/SI/data_sources.md"), encoding="utf-8"):
        if " | " in line and not line.startswith("#"):
            k, cit, use = [x.strip() for x in line.split(" | ")]
            out.append([cit, use])
    return md_table(["Dataset", "Use"], out)

refs = {}
for line in open(P("manuscript/references.md"), encoding="utf-8"):
    if " | " in line and not line.startswith("#"):
        k, v = line.split(" | ", 1); refs[k.strip()] = v.strip()
SI_REFS = ["yau2024", "pacheco2024", "cabezas2018", "martin2020", "sotomayor1994", "salasrabaza2023", "allen2018", "atkin2014", "barr2009", "bouillon2007", "bunting2022", "cahoon2003", "castanedamoya2013",
           "carafe_instrument", "delaria2024", "forster2021", "griffiths2021", "hannun2020", "heskel2016", "ho2017", "ho2014", "ho2016", "ho2018", "raymondcole2001", "borges2004", "colecaraco1998", "rosentreter2017", "huntingford2017", "jeffrey2019", "lovelock2011", "martinez2021", "radabaugh2020", "romerouribe2022", "yu2023",
           "hutchinson1981", "hutjes2010", "kljun2015", "krauss2005", "lagomasino2021", "lin2024", "osland2020",
           "pedersen2010", "poulter2023", "powell_tls", "reed2025", "reithmaier2020", "gofluxref", "sippo2020", "smith2021",
           "stegehuis2026", "sturchio2022", "taillie2020", "troxler2015", "wanninkhof2014", "weiss1974", "wood2023",
           "yamamoto1976", "yong2024", "zhao2021", "zhu2024"]
def first_author(v):
    return re.sub(r"[^A-Za-z]", "", v.split(",")[0]).lower()
ref_lines = [science(r) for r in sorted((refs.get(k, f"[{k} — to add]") for k in SI_REFS), key=first_author)]
missing = [k for k in SI_REFS if k not in refs]

md = src
for k in FIGS:
    md = md.replace("{{FIG " + k + "}}", fig_block(k))
for k, f in TABLES.items():
    md = md.replace("{{TABLE " + k + "}}", f())
md = md.replace("{{DATA S2}}", data_s2()).replace("{{REFS}}", "\n".join(f"- {r}" for r in ref_lines))

out_md = P("manuscript/SI/supplement_Science_built.md")
open(out_md, "w", encoding="utf-8").write(md)
subprocess.run(["pandoc", out_md, "-o", P("manuscript/SI/supplement_Science.docx"), "--resource-path", root], check=True, cwd=root)
words = len(re.sub(r"\|.*\|", "", md).split())
print(f"supplement built: ~{words} words outside tables; {len(FIGS)} figures; {len(TABLES)} tables; references missing: {missing}")
