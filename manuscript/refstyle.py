"""Render a reference-library entry (Nature-style text in references.md) in Science style.

Science style: initials before surnames ("P. Bunting, A. Rosenqvist"), all authors up to
five and "et al." beyond, title in sentence form, journal in italics, volume in bold,
pages and year; no DOI. Entries that do not parse as journal articles (reports, books,
datasets) keep their text with the authors converted and the URL/DOI removed.
"""
import re

_AUTH = re.compile(r"(?P<sur>[^,&]+?), (?P<ini>(?:[A-Z][a-zà-ÿ]?\.(?:[ -]?[A-Z][a-zà-ÿ]?\.)*))")
_DOI = re.compile(r"\s*(?:https?://\S+|doi:\s*\S+)\s*$")
_JOUR = re.compile(r"^(?P<title>.+?\.)\s+(?P<jour>[^.].*?)\s+(?P<vol>\d+[A-Za-z]?),\s+(?P<pages>[^ ]+)\s+\((?P<year>\d{4})\)\.?\s*(?P<tail>.*)$")


def _split_authors(s):
    """Return (list of 'I. Surname', et_al flag, remainder) or None."""
    names, pos, et_al = [], 0, False
    while True:
        m = _AUTH.match(s, pos)
        if not m:
            return None
        names.append(f"{m.group('ini')} {m.group('sur').strip()}")
        pos = m.end()
        if s.startswith(" et al.", pos):
            et_al, pos = True, pos + len(" et al.")
            break
        if s.startswith(", ", pos) and _AUTH.match(s, pos + 2):
            pos += 2
            continue
        if s.startswith(" & ", pos):
            pos += 3
            continue
        break
    rest = s[pos:].lstrip(" ,")
    return names, et_al, rest


def science(entry):
    entry = re.sub(r"\[[^\]]*in preparation[^\]]*\]", "manuscript in preparation.", entry.strip())
    entry = re.sub(r"\s*\[[^\]]*\]", "", entry)                # editorial notes such as [CHECK ...]
    entry = _DOI.sub("", entry)
    sp = _split_authors(entry)
    if sp is None:
        return entry
    names, et_al, rest = sp
    if et_al or len(names) > 5:
        auth = f"{names[0]} et al."
    else:
        auth = ", ".join(names)
    m = _JOUR.match(rest)
    if m and not m.group("tail"):
        return (f"{auth}, {m.group('title')} *{m.group('jour')}* **{m.group('vol')}**, "
                f"{m.group('pages')} ({m.group('year')}).")
    return f"{auth}, {rest}"


if __name__ == "__main__":
    import sys
    for line in open(sys.argv[1], encoding="utf-8"):
        if " | " in line and not line.startswith("#"):
            k, v = line.split(" | ", 1)
            print(k, "->", science(v))
