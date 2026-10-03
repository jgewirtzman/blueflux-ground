#!/usr/bin/env python3
"""Build a Word version of a manuscript draft with figures inline.

Each figure (output/figures/main/FigN_*.png) is inserted, with its legend as the
caption, after the main-text section that first cites it; the separate
"Figure legends" section is then dropped. Uses pandoc.

usage: python3 manuscript/build_docx.py manuscript/drafts/manuscript_Science_draft.md
"""
import glob, os, re, subprocess, sys, tempfile

root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
src = sys.argv[1] if len(sys.argv) > 1 else "manuscript/drafts/manuscript_Science_draft.md"
src = os.path.join(root, src) if not os.path.isabs(src) else src
out = os.path.splitext(src)[0] + ".docx"
text = open(src, encoding="utf-8").read()

# legends: "**Fig. N. Title.** body" paragraphs in the Figure legends section
leg_start = text.index("## Figure legends")
leg_end = text.index("\n---", leg_start)
legends = {}
for m in re.finditer(r"^\*\*Fig\. (\d)\.(.*?)$", text[leg_start:leg_end], re.M):
    legends[int(m.group(1))] = "**Fig. " + m.group(1) + "." + m.group(2)
body = text[:leg_start] + text[leg_end:]

def fig_block(n):
    png = sorted(glob.glob(os.path.join(root, f"output/figures/main/Fig{n}_*.png")))[0]
    return f"\n\n![](<{os.path.relpath(png, root)}>){{width=6.5in}}\n\n{legends[n]}\n\n"

# insert each figure after the section that cites it most often (first such section on ties)
parts = re.split(r"(?=\n### |\n## )", body)
placed = set()
for n in sorted(legends):
    counts = [len(re.findall(rf"Fig\. {n}(?![0-9])", p)) for p in parts]
    if max(counts) == 0:
        continue
    i = counts.index(max(counts))
    parts[i] = parts[i].rstrip() + fig_block(n)
    placed.add(n)
for n in sorted(set(legends) - placed):       # any uncited figure goes before Supplementary materials
    body_end = parts[-1]
    parts[-1] = fig_block(n) + body_end
md = "".join(parts)

with tempfile.NamedTemporaryFile("w", suffix=".md", delete=False, encoding="utf-8") as f:
    f.write(md)
subprocess.run(["pandoc", f.name, "-o", out, "--resource-path", root], check=True)
print("wrote", out, "| figures placed:", sorted(placed))
