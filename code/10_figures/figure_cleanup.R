# Figure cleanup: move non-retained figures to archive/
# Can be sourced at the end of any figure-generating script,
# or run standalone: Rscript code/10_figures/figure_cleanup.R

figdir <- "output/figures"
archdir <- file.path(figdir, "archive")
if (!dir.exists(archdir)) dir.create(archdir, recursive = TRUE)

keep_stems <- c(
  # Main text
  "pub_map_photo_composite",                                # Fig 1
  "pub_component_by_plot_campaign_combined_condensed_boot",  # Fig 2
  "pub_stem_height_composite_combined",                      # Fig 3
  "pub_pca_porewater_full_composite",                        # Fig 6
  # SI
  "pub_SI_ebullition_partition",                              # Fig S1
  "pub_SI_pneumatophore_density",                             # Fig S2
  "pub_SI_chamber_photos"                                     # Fig S3
)

all_files <- list.files(figdir, pattern = "\\.(pdf|png)$", full.names = FALSE)
moved <- 0
for (f in all_files) {
  stem <- sub("\\.(pdf|png)$", "", f)
  if (!stem %in% keep_stems) {
    file.rename(file.path(figdir, f), file.path(archdir, f))
    moved <- moved + 1
  }
}
kept <- length(all_files) - moved
cat(sprintf("Figure cleanup: kept %d, archived %d\n", kept, moved))
