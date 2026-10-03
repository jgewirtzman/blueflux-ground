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
 "S2": (["output/figures/other/sampling_design.png", "output/figures/other/water_positions_by_campaign.png"],
        "Sampling design relative to tide and standing water. (a) Chamber measurements by site and campaign against water level above the soil (FCE LTER loggers; the SRS5 October 2022 record has a gap before 19 October); diamonds at −2 cm mark chambers without a recorded water depth; triangles mark dissolved-gas samples. (b) Water-surface CH4 flux by campaign and site, by chamber position (above the flooded forest floor, or in a channel or open water at the tidal sites; the ghost and regenerating sites have no channels); bars, arithmetic means. At the two tidal sites where both positions were sampled (SRS5, SRS6; March 2023), flux did not differ between them (Wilcoxon p = 0.29 and 1), so channel and open-water placements neither missed nor inflated water hotspots."),
 "S3": (["output/figures/other/pub_component_by_plot_campaign_combined_condensed_boot.png"],
        "CH4 (a) and CO2 (b) flux by component for every plot and campaign, including context sites. Points are individual measurements; filled circles are arithmetic means, with bootstrap 95% CIs where n > 3; inverse-hyperbolic-sine axes."),
 "S4": (["output/figures/other/pub_SI_ebullition_partition.png"],
        "Ebullition. (a) Diffusive and detected ebullitive water-surface CH4 by site and season, with an upper bound on undetected ebullition (pale; excess of a whole-placement fit over diffusive + detected ebullitive flux, LGR placements); labels give n and the ebullitive share (detected–upper bound); bars, means ± SE of the total (shown where n > 3); points, individual measurements; each panel has its own linear y axis. (b) Example placements: a single bubble mid-placement (CP40, October 2022), two bubbles (CP40, March 2023), and long bubble-free placements in ghost (FLM30, 31 min) and intact forest (SRS6, 14 min), showing detected bubble steps (shaded), the de-ebulliated series and the diffusive linear fit (M7)."),
 "S5": (["output/figures/other/pub_SI_pneumatophore_density.png"],
        "Soil CH4 (a) and CO2 (b) flux against pneumatophore density per collar, by site (dry-season visits; shape, visit). Lines, least-squares fits ± 95% CI; ρ, Spearman rank correlation with p and n. Marco Island collars had no pneumatophores and are not shown."),
 "S6": (["output/figures/other/pub_stem_height_composite_combined.png"],
        "Stem detail at SRS5, SRS6, BL60, CP40 and FLM30. (a, b) Stem CH4 and CO2 by height class and forest class: overlapping class densities (inverse-hyperbolic-sine scale) for each height band, with each class's measurements in its own row beneath and arithmetic means (diamonds) for groups with n > 3. (c, d) CH4 and CO2 estimated marginal means (95% CI), averaged over height, by species and live/dead status from linear mixed-effects models with height class and season as fixed effects and site as a random intercept (M20); n, closures per group. Dead stems come mostly from the ghost sites, so status is partly confounded with forest class."),
 "S7": (["output/figures/other/stem_extrap_clean.png"],
        "Stem height extrapolation (text S1). (a) Measured stem CH4 (points; height above water or soil) and the stem profile under each of six extrapolation rules (lines; TLS height above ground), by site and campaign; the default rule is exponential decay to zero; shaded, above the 1.5 m chamber limit; square-root height axis; inverse-hyperbolic-sine flux axes that differ by site. (b) Stem CH4 per unit ground area (absolute tree flux) and (c) total plot CH4 under each rule, with non-stem components held constant and high tide at tidal sites; x axes differ by site."),
 "S8": (["output/figures/other/SA_by_segment_height_fixedY.png"],
        "Laser-scanned woody surface per unit ground area (m2 m−2 per 0.5 m height bin) by segment class (prop root, stem, branch) for the four scanned plots; labels give total woody surface per unit ground area and the share above the 1.5 m chamber height limit (dashed line)."),
 "S9": (["output/figures/other/si_tide_states.png"],
        "Stand CH4 of the intact forest at high and low tide by component (SRS5, SRS6; October 2022 and March 2023; exponential stem rule). Diamonds and bars, stand total and Monte Carlo 95% interval; dashed line, the tide-weighted value used in the budgets (flooded share of the floor in the campaign month; M11); inverse-hyperbolic-sine axis."),
 "S10": (["output/figures/other/si_sensitivity_switch.png"],
         "Sensitivity of the intact-to-ghost switch (GWP20) to analytical choices, one at a time (text S9; table S10). Vertical line, central case (6,409 g CO2-eq m−2 yr−1); points left of it lower the switch, right of it raise it. Analytical choices keep the switch between ~5,600 and ~8,000 (GWP100: ~5,100–7,300); only carbon-balance framings that count lateral export or storage alone lower it further."),
 "S11": (["output/figures/other/si_S10_mc_uncertainty.png"],
         "Monte Carlo standard error of each CH4 component (joint flux-rate and surface-area uncertainty) by site and campaign, at high tide for tidal sites; components with zero uncertainty omitted (text S4); log axis."),
 "S12": (["output/figures/other/ed_porewater_rounds.png"],
         "Porewater dissolved CH4 (A) and salinity (B) by site, sampling round (October 2022, March 2023, October 2025) and depth, including FLM30."),
 "S13": (["output/figures/other/si_S12_carbonate.png"],
         "Porewater carbonate chemistry (October 2025; 0–90 cm, surface water excluded). (a) Total alkalinity against DIC calculated from pH and alkalinity, with the 1:1 line (dashed) and least-squares fits by site. (b) Total alkalinity against salinity, with class regressions (95% CI), Pearson r and n by class, and a conservative-mixing reference (dashed; end-members salinity 0, 3,000 µM and salinity 35, 2,400 µM; text S7). Fill, forest class; shape, site."),
 "S14": (["output/figures/other/pub_SI_salinity_vs_ch4_bysite.png"],
         "Porewater salinity against dissolved CH4 by site (site × round × depth means, including surface water) for the three sampling rounds (October 2022, March 2023, October 2025). Fill, forest class; shape, round; CH4 on a log(1 + x) axis."),
 "S15": (["output/figures/other/fig_carbon_budget.png"],
         "Carbon and methane budgets. (A) Carbon budget schematic (as Fig. 5A) for intact and ghost forest (g C m−2 yr−1; arrow width proportional to the square root of the flux, one scale for both). Solid arrows are measured here: GPP (tower) and CO2 and CH4 from each surface (chambers × scanned area), shown as mean ± 1 SE across the four plot × campaign estimates. Translucent, dashed arrows are literature values with their published ranges: litterfall, root production, wood mortality, wood increment, burial and lateral export (DIC, DOC, POC, dissolved CH4), for intact forest only. The closure residual (net exchange minus lateral export, burial and wood increment; M18, text S10) is 423 g C m−2 yr−1, with a range of −182 to 595 when the half-ranges of its terms are combined in quadrature. The imbalance is therefore within the uncertainty of lateral export. (B) Methane budget by pathway (g CH4 m−2 yr−1; diamonds, bottom-up total with Monte Carlo 95% interval), lateral dissolved CH4 export (intact), and the airborne mean of four 2022–2023 deployments (triangles, ±1.96 SE; daytime)."),
 "S16": (["output/gpp/plots/US-Skr_GPP_mean_diurnal_cycle.png"],
         "US-Skr tower: mean diurnal cycle of partitioned GPP during the campaign months (M14). Lines, monthly mean half-hourly GPP; ribbons, mean bootstrap 95% interval (partitioning-parameter uncertainty); local standard time."),
 "S17": (["output/figures/other/site_closure_comparison.png"],
         "Class-mean bottom-up CH4 by component and campaign compared with the airborne estimate. (a) Stacked component contributions (nmol m−2 ground s−1); dashed outlines mark classes with assumed structure (no TLS: regenerating, scrub and context sites); diamonds, airborne class flux ± SE (intact and ghost only). (b) Component coverage: chambers per class and campaign; white cells with 0, component not measured on that visit; pale cells, class not visited."),
 "S18": ([], "_[PLACEHOLDER — sediment metagenome detail: taxonomy by site and depth, marker-gene abundance per gram sediment and per gram organic carbon, DNA yield (Peccia laboratory).]_"),
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
    b = [[r["site_id"], r["site_name"], site_class.get(r["site_id"], ""), fmt(r["latitude"], 5), fmt(r["longitude"], 5),
          r["dominant_species"]] for r in rows("data/sites/site_metadata.csv")]
    return "**Table S1. Study sites.**\n\n" + md_table(["Site", "Name", "Class", "Latitude", "Longitude", "Dominant species"], b)

def t_S2():
    b = [[r["category"], r["n"], r["components"]] for r in rows("output/analysis/si/si_exclusions.csv")]
    return ("**Table S2. Measurement exclusions by criterion** (M6). Duplicate records are not independent closures; "
            "no flux was removed for being small, negative or below detection.\n\n" + md_table(["Criterion", "n", "By component"], b))

def t_S3():
    rr = rows("output/analysis/si/si_sample_sizes.csv")
    comps = [c for c in ["stem", "root", "soil", "water", "cwd", "leaves"] if c in rr[0]]
    b = [[r["plot"], r["campaign"]] + [r[c] for c in comps] + [r["total"]] for r in rr]
    return "**Table S3. Analysed fluxes by site, campaign and component.**\n\n" + md_table(["Site", "Campaign"] + comps + ["Total"], b)

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
SI_REFS = ["pacheco2024", "cabezas2018", "martin2020", "sotomayor1994", "salasrabaza2023", "allen2018", "atkin2014", "barr2009", "bouillon2007", "bunting2022", "cahoon2003", "castanedamoya2013",
           "carafe_instrument", "delaria2024", "forster2021", "griffiths2021", "hannun2020", "heskel2016", "ho2017", "huntingford2017", "jeffrey2019", "lovelock2011", "martinez2021", "radabaugh2020", "romerouribe2022", "yu2023",
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
