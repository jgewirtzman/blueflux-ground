# =============================================================================
# Collect curated display items: copy the freshly generated figure for each
# main-text / SI display item into output/figures/main and output/figures/SI.
# Run after the figure scripts (last step of run_all.R). Sources that have not
# been regenerated are reported and the existing curated copy is left in place.
# Fig 1 (map/photo composite) is built manually and is not collected here.
# =============================================================================
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

fig <- "output/figures"
display_items <- c(
  # curated file                      = generated source                              (script)
  "main/Fig2_rates.png"                = "other/fig2_rates.png",                         # fig2_rates.R (Fig 2a-c)
  "main/Fig3_stands.png"               = "other/fig3_stands.png",                        # fig3_stands.R
  "main/Fig4_geochemistry.png"         = "other/fig4_geochem.png",                       # fig4_geochem.R
  "main/Fig5_closure_forcing.png"      = "presentation/FigClosureForcing.png",           # plot_closure.R [to rebuild]
  "SI/ED8_porewater_rounds.png"       = "other/ed_porewater_rounds.png",                # ed_porewater_rounds.R
  "SI/ED2_tree_detail_old_Fig3.png"    = "other/pub_stem_height_composite_combined.png", # fig3_stem_height.R (old Fig 3; ED2 source)
  "SI/FigS1_ebullition.png"           = "other/pub_SI_ebullition_partition.png",        # figS1_ebullition.R
  "SI/FigS2_pneumatophore.png"        = "other/pub_SI_pneumatophore_density.png",       # figS2_pneumatophore.R
  "SI/FigS3_chambers.png"             = "other/pub_SI_chamber_photos.png",              # figS3_chamber_photos.R
  "SI/FigS4_perplot_fluxes.png"       = "other/pub_component_by_plot_campaign_combined_condensed_boot.png", # fig2_component_boot.R
  "SI/FigS5_height_extrapolation.png" = "other/stem_extrap_clean.png",                  # plot_extrap_clean.R
  "SI/FigS6_extrap_sensitivity.png"   = "other/height_extrap_sensitivity_total.png", # upscale_methane_to_plots.R
  "SI/FigS7_tide_scenarios.png"       = "other/scenario_comparison.png", # upscale_methane_to_plots.R
  "SI/FigS8_SA_by_height.png"         = "other/SA_by_segment_height_fixedY.png",        # plot_SA_height_fixedY.R
  "SI/FigS9_MC_uncertainty.png"       = "other/pub_uncertainty_decomp.png", # upscale_methane_to_plots.R
  "SI/FigS10_tower_GPP.png"           = "../gpp/plots/US-Skr_GPP_mean_diurnal_cycle.png", # plot_us_skr_gpp.R
  "SI/FigS11_porewater_depth.png"     = "other/pub_SI_depth_profiles_lines.png",        # site_characterization_figures.R
  "SI/FigS12_TA_DIC.png"              = "other/pub_SI_ta_vs_dic.png",                   # fig6_porewater_pca.R
  "SI/FigS13_TA_SO4_deficit.png"      = "other/pub_SI_TA_vs_SO4_deficit.png",           # fig6_porewater_pca.R
  "SI/FigS14_salinity_CH4_bysite.png" = "other/pub_SI_salinity_vs_ch4_bysite.png"       # site_characterization_figures.R
)

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
