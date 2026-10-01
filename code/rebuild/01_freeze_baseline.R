# =============================================================================
# Freeze the legacy flux workflow's outputs as the comparison baseline for the
# scripted rebuild (handoff work plan, step 1).
#
# Copies the current combined dataset and key downstream tables into
# output/rebuild/baseline/ and writes MANIFEST.csv (file, md5, rows, git
# commit). Later rebuild steps compare against these copies, so they must not
# be regenerated once frozen: the script refuses to overwrite an existing
# baseline unless FREEZE_OVERWRITE=1 is set.
#
# Run from the project root after the legacy pipeline (run_all.R) has written
# the outputs at the commit being frozen.
# =============================================================================
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

out_dir <- "output/rebuild/baseline"
files <- c(
  # measurement-level
  "output/data_products/combined_gas_flux_dataset.csv",
  "output/data_products/combined_gas_flux_dataset_archival.csv",
  "output/data_products/tree_stem_fluxes.csv",
  "output/data_products/soil_water_surface_fluxes.csv",
  "output/ebullition/reprocessed_negative_fluxes.csv",
  "output/ebullition/corrected_time_windows.csv",
  "output/ebullition/partitioned_fluxes.csv",
  "output/ebullition/placements_summary.csv",
  "output/ebullition/site_season_ebullition.csv",
  # summary statistics
  "output/data_products/flux_statistics_table.csv",
  "manuscript/text/manuscript_results.txt",
  # upscaling and budgets
  "output/upscaling/plot_level_CH4_totals.csv",
  "output/upscaling/plot_level_CO2_totals.csv",
  "output/upscaling/height_extrap_sensitivity.csv",
  "output/upscaling/mc_component_uncertainty.csv",
  "output/upscaling/mc_CO2_forcing.csv",
  "output/upscaling/mc_net_forcing_by_class.csv",
  "output/upscaling/net_forcing_by_class.csv",
  "output/upscaling/budget_decomposition.csv",
  "output/upscaling/carbon_budget_summary.csv",
  "output/upscaling/carbon_budget_full.csv"
)

missing <- files[!file.exists(files)]
if (length(missing)) stop("Missing baseline inputs:\n  ", paste(missing, collapse = "\n  "))

if (dir.exists(out_dir) && length(list.files(out_dir)) &&
    Sys.getenv("FREEZE_OVERWRITE") != "1") {
  stop(out_dir, " already holds a frozen baseline; set FREEZE_OVERWRITE=1 to replace it.")
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

commit <- tryCatch(system("git rev-parse HEAD", intern = TRUE), error = function(e) NA_character_)
dirty  <- tryCatch(length(system("git status --porcelain --untracked-files=no -- output manuscript/text", intern = TRUE)) > 0,
                   error = function(e) NA)

manifest <- do.call(rbind, lapply(files, function(f) {
  dest <- file.path(out_dir, gsub("/", "__", f))
  stopifnot(file.copy(f, dest, overwrite = TRUE))
  n_rows <- if (grepl("\\.csv$", f)) nrow(read.csv(f, check.names = FALSE)) else NA_integer_
  data.frame(source = f, baseline_file = basename(dest), md5 = unname(tools::md5sum(dest)),
             rows = n_rows, stringsAsFactors = FALSE)
}))
manifest$git_commit <- commit
manifest$outputs_modified_vs_commit <- dirty
manifest$frozen_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
write.csv(manifest, file.path(out_dir, "MANIFEST.csv"), row.names = FALSE)

cat("Froze", nrow(manifest), "files into", out_dir, "at commit", commit,
    if (isTRUE(dirty)) "(WARNING: outputs differ from the commit)" else "", "\n")
