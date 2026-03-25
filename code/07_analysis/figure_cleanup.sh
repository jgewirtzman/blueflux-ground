#!/bin/bash
# Figure cleanup: move non-retained figures to archive/
# Run from repo root: bash code/07_analysis/figure_cleanup.sh

FIGDIR="output/figures"
ARCHDIR="$FIGDIR/archive"
mkdir -p "$ARCHDIR"

# Main text figures
MAIN=(
  "pub_map_photo_composite"                                # Fig 1
  "pub_component_by_plot_campaign_combined_condensed_boot"  # Fig 2
  "pub_stem_height_composite_combined"                      # Fig 3
  "pub_pca_porewater_full_composite"                        # Fig 6
)

# SI figures
SI=(
  "pub_SI_ebullition_partition"        # Fig S1
  "pub_SI_pneumatophore_density"       # Fig S2
)

KEEP=("${MAIN[@]}" "${SI[@]}")

moved=0
kept=0
for f in "$FIGDIR"/*.pdf "$FIGDIR"/*.png; do
  [ -f "$f" ] || continue
  basename=$(basename "$f")
  # Strip extension to get stem
  stem="${basename%.*}"

  match=false
  for k in "${KEEP[@]}"; do
    if [ "$stem" = "$k" ]; then
      match=true
      break
    fi
  done

  if [ "$match" = "false" ]; then
    mv "$f" "$ARCHDIR/"
    echo "  -> archive: $basename"
    moved=$((moved + 1))
  else
    kept=$((kept + 1))
  fi
done

echo ""
echo "Kept: $kept files"
echo "Archived: $moved files"
echo ""
echo "Remaining:"
ls "$FIGDIR"/*.pdf "$FIGDIR"/*.png 2>/dev/null | sed 's|.*/||'
