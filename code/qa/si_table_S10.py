# Regenerate SI Table S10 (one-at-a-time sensitivity) in manuscript/SI/manuscript_NCC_SI.md
# from output/qa/sensitivity_summary.csv (code/qa/sensitivity_summary.R). Run from the repo root.
import csv
S = "manuscript/SI/manuscript_NCC_SI.md"
rows=list(csv.DictReader(open("output/qa/sensitivity_summary.csv")))
lab={"none (1.00)":"none (Q10 = 1)","literature (2.00)":"literature (Q10 = 2)","stem_chambers (4.33)":"our stem chambers (Q10 = 4.33)",
 "negligible (0.01 m3/ha)":"none","krauss_lo (13 m3/ha)":"Krauss low (13 m³ ha⁻¹)","krauss_eyewall (132 m3/ha)":"Krauss eyewall (132 m³ ha⁻¹)","krauss_hi (181 m3/ha)":"Krauss high (181 m³ ha⁻¹)",
 "necb_alk_retained":"NECB, exported alkalinity retained","necb_all_export":"NECB, all export returned to the air","storage":"storage only (burial + wood)","central case":"—"}
fl={"equal_split":"equal 50/50 split","switch_campaign":"all-or-nothing switch, campaign months","switch_longterm":"all-or-nothing switch, 2010–2023",
    "area_campaign_lo":"area-weighted, floor mean −1.96 SE","area_campaign_hi":"area-weighted, floor mean +1.96 SE","area_longterm":"area-weighted, 2010–2023"}
def num(x):
    try: return f"{float(x):,.0f}".replace("-","−")
    except: return x.replace("-","−") if x else "–"
def n2(x):
    try: return f"{float(x):.2f}"
    except: return "–"
t=[]
for r in rows:
    st=lab.get(r["setting"],r["setting"])
    for k,v in fl.items():
        if st.startswith(k): st=v+st[len(k):]
    ch=r["choice"].replace("->","→")
    if ch=="central": ch="**Central**"
    sw="–" if r["switch_net20"] in ("","NA") else f"{num(r['switch_net20'])} [{num(r['switch_net100'])}]"
    t.append(f"| {ch} | {st} | {n2(r['intact_CH4_g'])} | {num(r['intact_NEE_gC']) if r['intact_NEE_gC'] not in ('','NA') else '–'} | {num(r['intact_net20'])} [{num(r['intact_net100'])}] | {num(r['ghost_net20'])} [{num(r['ghost_net100'])}] | {sw} |")
s=open(S).read()
h="| Choice | Setting | Intact CH4 (g m⁻² yr⁻¹) | Intact NEE (g C) | Intact forcing, GWP20 [GWP100] | Ghost forcing, GWP20 [GWP100] | Switch, GWP20 [GWP100] |\n|---|---|---|---|---|---|---|\n"
i=s.index(h)+len(h); j=s.index("\n\n",i)
s=s[:i]+"\n".join(t)+s[j:]
open(S,"w").write(s)
print("ok")
