#!/usr/bin/env Rscript
# =============================================================================
# run_all.R -- BlueFlux ground pipeline, raw analyzer files to figures.
#
#   Rscript run_all.R                     # everything, in order
#   Rscript run_all.R --from 05_dataset   # from a stage (or a step, e.g. 07_upscaling/02)
#   Rscript run_all.R --only 08_figures   # one stage (or one step)
#   Rscript run_all.R --list              # print the steps
#   Rscript run_all.R --qa                # also run the QA comparisons (need legacy files)
#
# Every step runs in its own R process (Rscript <script>) from the project
# root, so packages loaded by one step cannot mask functions in another; its
# output goes to output/logs/<stage>__<script>.log. The run stops at the first
# failing step.
#
# Stages 01-04 read the raw analyzer files (data/analyzer/, gitignored) and the
# US-Skr tower file (data/tower/AMF_US-Skr_BASE_HH_2-5.csv, gitignored). From
# stage 05 on, only tracked files are needed.
# =============================================================================

steps <- c(
  # 01 metadata: field sheets + dimension tables + curated corrections -> one auxfile
  "code/01_metadata/00_index_raw_files.R",
  "code/01_metadata/01_build_auxfile.R",
  # 02 windows: clock offsets and the fit window of every closure
  "code/02_windows/01_rise_detection.R",
  "code/02_windows/02_windows.R",
  # 03 fit: goFlux + fluxqc per gas; water flux from dissolved CH4 where unmeasured
  "code/03_fit/01_fit_fluxes.R",
  "code/03_fit/02_water_flux_from_dissolved.R",
  # 04 ebullition: floating-chamber placements, diffusive / ebullitive CH4 (goFlux fork, vendored)
  "code/04_ebullition/01_placements.R",
  "code/04_ebullition/02_partition.R",
  # 05 dataset: compiled datasets, written once
  "code/05_dataset/01_compile_datasets.R",
  "code/05_dataset/02_data_products.R",
  "code/05_dataset/03_data_dictionary.R",
  "code/05_dataset/04_porewater_nitrogen.R",
  "code/05_dataset/05_porewater_N_fce_context.R",
  # 06 analysis: statistics and manuscript numbers
  "code/06_analysis/01_summary_table.R",
  "code/06_analysis/02_manuscript_results.R",
  # 07 upscaling: tower GPP, plot budgets, forcing, carbon budget
  "code/07_upscaling/01_tower_gpp.R",
  "code/07_upscaling/01b_flood_fraction.R",
  "code/07_upscaling/01c_tls_datum.R",
  "code/07_upscaling/02_upscale_methane.R",
  "code/07_upscaling/03_upscale_co2.R",
  "code/07_upscaling/04_net_forcing.R",
  "code/07_upscaling/05_mc_forcing.R",
  "code/07_upscaling/06_carbon_budget.R",
  "code/07_upscaling/07_budget_sources.R",
  "code/07_upscaling/08_supplementary_analyses.R",
  "code/07_upscaling/09_regional_scaling.R",
  # 08 figures: display items, then copy into output/figures/main and SI
  "code/08_figures/fig2_component_boot.R",          # Fig 2, Fig S4
  "code/08_figures/fig3_stem_height.R",             # Fig 3
  "code/08_figures/plot_budget_figs.R",             # Fig 4
  "code/08_figures/fig6_porewater_pca.R",           # Fig 5, Figs S12-S13
  "code/08_figures/plot_closure.R",                 # Fig 6
  "code/08_figures/plot_carbon_budget.R",           # Fig 7
  "code/08_figures/plot_budget_multisource.R",      # Fig 8
  "code/08_figures/plot_budget_flow.R",             # Fig 9
  "code/08_figures/plot_budget_waterfall.R",        # Fig 10
  "code/08_figures/figS1_ebullition.R",             # Fig S1
  "code/08_figures/figS2_pneumatophore.R",          # Fig S2
  "code/08_figures/figS3_chamber_photos.R",         # Fig S3
  "code/08_figures/plot_extrap_clean.R",            # Fig S5
  "code/08_figures/plot_SA_height_fixedY.R",        # Fig S8
  "code/08_figures/plot_us_skr_gpp.R",              # Fig S10
  "code/08_figures/site_characterization_figures.R",# Figs S11, S14
  "code/08_figures/plot_site_closure.R",            # supplementary per-site closure
  "code/08_figures/plot_water_positions.R",         # supplementary water positions
  "code/08_figures/figS_sampling_design.R",
  "code/08_figures/fig_carafe_endmembers.R",            # airborne intact vs ghost (draft panel)
  "code/08_figures/fig_metagenome_placeholder.R",       # PLACEHOLDER metagenome figure        # sampling design vs tide (candidate SI figure)
  "code/08_figures/collect_figures.R",
  "code/09_archive/build_ornl_daac_package.R"     # DRAFT ORNL DAAC package (measured fluxes)
)
# Fig 1 (map + photo composite, code/08_figures/publication_map_composite.R) is
# assembled interactively and is not run here.

qa_steps <- c(   # legacy comparisons; need output/qa/baseline and, for some, intermediate/
  "code/qa/compare_auxfile_vs_legacy.R",
  "code/qa/compare_fit_vs_legacy.R",
  "code/qa/compare_dataset_vs_legacy.R",
  "code/qa/audit_saved_traces.R",
  "code/qa/air_temperature_options.R",
  "code/qa/window_overlaps.R",
  "code/qa/long_deployments.R",
  "code/qa/placements_review.R",
  "code/qa/compare_ebullition_vs_legacy.R",
  "code/qa/review_traces.R",
  "code/qa/picarro_update_cadence.R",
  "code/qa/sigma_pooling_check.R",
  "code/qa/trace_co2_shift.R",
  "code/qa/net_forcing_attribution.R",
  "code/qa/scripted_windows_review.R",
  "code/qa/report_old_new.R",
  "code/qa/budget_scenarios.R",
  "code/qa/flight_window_comparison.R",
  "code/qa/flooding_scenarios.R",
  "code/qa/ghost_exposed_floor.R",
  "code/qa/sensitivity_summary.R",
  "code/qa/chamber_heights_review.R"
)

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name) { i <- match(name, args); if (is.na(i)) NULL else args[i + 1] }
if (requireNamespace("here", quietly = TRUE)) setwd(here::here())
stage_of <- function(s) basename(dirname(s))
label_of <- function(s) paste0(stage_of(s), "/", sub("\\.R$", "", basename(s)))
matches  <- function(s, key) startsWith(label_of(s), key) | stage_of(s) == key

run <- steps
if (!is.null(opt("--from"))) { k <- which(matches(steps, opt("--from")))[1]
  if (is.na(k)) stop("no step matches --from ", opt("--from")); run <- steps[k:length(steps)] }
if (!is.null(opt("--only"))) { run <- steps[matches(steps, opt("--only"))]
  if (!length(run)) stop("no step matches --only ", opt("--only")) }
if ("--qa" %in% args) run <- c(run, qa_steps)
if ("--list" %in% args) { cat(sprintf("%2d  %s\n", seq_along(steps), label_of(steps)), sep = "")
  cat("QA (--qa):\n"); cat(sprintf("    %s\n", label_of(qa_steps)), sep = ""); quit(save = "no") }

needs_raw <- stage_of(run) %in% c("01_metadata", "02_windows", "03_fit", "04_ebullition")
if (any(needs_raw) && !dir.exists("data/analyzer"))
  stop("Stages 01-04 need the raw analyzer files in data/analyzer/ (gitignored). ",
       "Add them, or start from a later stage: Rscript run_all.R --from 05_dataset")
if (any(stage_of(run) %in% c("01_metadata", "07_upscaling")) && !file.exists("data/tower/AMF_US-Skr_BASE_HH_2-5.csv"))
  stop("Stages 01 and 07 need data/tower/AMF_US-Skr_BASE_HH_2-5.csv (gitignored).")

dir.create("output/logs", recursive = TRUE, showWarnings = FALSE)
cat("=== BlueFlux ground pipeline:", length(run), "steps |", format(Sys.time()), "===\n")
for (s in run) {
  log <- file.path("output/logs", paste0(stage_of(s), "__", sub("\\.R$", ".log", basename(s))))
  t0 <- Sys.time()
  if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
  rc <- system2("Rscript", s, stdout = log, stderr = log)
  if (file.exists("Rplots.pdf")) {   # a plot drawn without an open device: not an output
    file.remove("Rplots.pdf"); cat(sprintf("  (note: %s drew to the default device; Rplots.pdf removed)\n", label_of(s))) }
  cat(sprintf("%-45s %s  %5.0f s\n", label_of(s), if (rc == 0) "ok    " else "FAILED",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  if (rc != 0) { cat("\n--- last lines of", log, "---\n"); cat(tail(readLines(log), 20), sep = "\n")
    stop("step failed: ", s) }
}
cat("=== done ===\n")
