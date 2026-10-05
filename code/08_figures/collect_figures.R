# =============================================================================
# Collect curated display items: copy the freshly generated figure for each
# main-text / SI display item into output/figures/main and output/figures/SI.
# Run after the figure scripts (last step of run_all.R). Sources that have not
# been regenerated are reported and the existing curated copy is left in place.
# =============================================================================
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

fig <- "output/figures"
display_items <- c(
  # curated file                        = generated source                              (script)
  "main/Fig1_system.png"                = "other/fig1_system.png",                        # fig1_system.R
  "main/Fig2_rates.png"                 = "other/fig2_rates.png",                         # fig2_rates.R
  "main/Fig3_stands.png"                = "other/fig3_stands.png",                        # fig3_stands.R
  "main/Fig4_geochemistry.png"          = "other/fig4_geochem.png",                       # fig4_geochem.R
  "main/Fig5_climate.png"               = "other/fig5_climate.png",                       # fig5_climate.R (+ fig_carbon_budget.R)
  "SI/FigureS1_satellite_scenes.png"       = "other/si_satellite_scenes.png",                  # si_satellite_scenes.R
  "SI/FigureS2_ndvi.png"                   = "other/si_ndvi.png",                              # si_ndvi.R
  "SI/FigureS3_campaign_context.png"       = "other/si_campaign_context.png",                  # si_campaign_context.R
  "SI/FigureS4_sampling_design.png"        = "other/sampling_design.png",                      # figS_sampling_design.R
  "SI/FigureS5_chambers.png"               = "other/pub_SI_chamber_photos.png",                # figS3_chamber_photos.R
  "SI/FigureS6_component_fluxes.png"       = "other/pub_component_by_plot_campaign_combined_condensed_boot.png",# fig2_component_boot.R
  "SI/FigureS7_pneumatophore.png"          = "other/pub_SI_pneumatophore_density.png",         # figS2_pneumatophore.R
  "SI/FigureS8_ebullition.png"             = "other/pub_SI_ebullition_partition.png",          # figS1_ebullition.R
  "SI/FigureS9_dissolved_k600.png"         = "other/si_grp_dissolved.png",                     # si_compose.py (water positions, k600)
  "SI/FigureS10_stem_detail.png"           = "other/pub_stem_height_composite_combined.png",   # fig3_stem_height.R
  "SI/FigureS11_SA_by_height.png"          = "other/SA_by_segment_height_fixedY.png",          # plot_SA_height_fixedY.R
  "SI/FigureS12_waterline.png"             = "other/si_waterline.png",                         # si_waterline.R
  "SI/FigureS13_tide.png"                  = "other/si_grp_tide.png",                          # si_compose.py (flood fraction, tide states)
  "SI/FigureS14_height_extrapolation.png"  = "other/stem_extrap_clean.png",                    # plot_extrap_clean.R
  "SI/FigureS15_MC_uncertainty.png"        = "other/si_S10_mc_uncertainty.png",                # si_upscaling_figs.R
  # S16 airborne flight tracks: placeholder
  "SI/FigureS17_porewater.png"             = "other/si_grp_porewater.png",                     # si_compose.py (rounds, carbonate)
  # S18 metagenomes: placeholder
  "SI/FigureS19_sensitivity_switch.png"    = "other/si_sensitivity_switch.png",                # si_upscaling_figs.R
  "SI/FigureS20_switch_methods.png"        = "other/si_switch_methods.png"                     # si_switch_methods.R
)
# S16 (flight tracks) and S18 (metagenome detail) are placeholders until those data arrive.
# Clear stale curated SI copies so the folder mirrors the current numbering.
unlink(Sys.glob(file.path(fig, "SI", "*.png")))

dir.create(file.path(fig, "main"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(fig, "SI"),   recursive = TRUE, showWarnings = FALSE)

for (dest in names(display_items)) {
  src <- file.path(fig, display_items[[dest]])
  if (file.exists(src)) {
    file.copy(src, file.path(fig, dest), overwrite = TRUE)
    cat(sprintf("  %-36s <- %s\n", dest, display_items[[dest]]))
  } else {
    cat(sprintf("  %-36s MISSING source %s (kept existing copy)\n", dest, display_items[[dest]]))
  }
}
