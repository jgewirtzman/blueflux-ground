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
# Inputs. The flux stages (03 onward) start from the clean inputs:
#   data/inputs/closures.csv, unlogged_placements.csv   one corrected row per closure
#   data/deposit/chamber_fluxes/analyzer_records/*.csv   analyzer records (deposited; gitignored)
# Stages 01-02 (internal, code/hygiene/) build data/inputs from the field sheets, scanned data sheets and
# curated corrections (data/field_notes, data/flux_metadata) and the vendor analyzer files
# (data/analyzer); they run only where those sources are present. Stage 07 also needs the
# US-Skr tower file (data/tower/AMF_US-Skr_BASE_HH_2-5.csv, gitignored).
# =============================================================================

steps <- c(
  # 01 metadata: field sheets + dimension tables + curated corrections -> one auxfile
  "code/hygiene/01_metadata/00_index_raw_files.R",          # vendor analyzer files only (skipped without data/analyzer)
  "code/hygiene/01_metadata/00b_export_analyzer_csv.R",     # vendor files -> clean daily CSVs (deposit; skipped without data/analyzer)
  "code/hygiene/01_metadata/01_build_auxfile.R",
  # 02 windows: clock offsets and the fit window of every closure
  "code/hygiene/02_windows/01_rise_detection.R",
  "code/hygiene/02_windows/02_windows.R",
  "code/hygiene/02_windows/03_export_inputs.R",             # -> data/inputs/closures.csv, unlogged_placements.csv
  # 03 fit: goFlux + fluxqc per gas; water flux from dissolved CH4 where unmeasured
  "code/03_fit/01_fit_fluxes.R",
  "code/05_dataset/00_porewater_2025.R",            # Oct 2025 porewater tables from lab files
  "code/05_dataset/00_dissolved_gas.R",             # dissolved CH4/CO2 (GC, Picarro) and site salinity table
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
  "code/08_figures/site_characterization_figures.R",# salinity-CH4 table for Fig 4B, Fig S17c (before 02_manuscript_results)
  "code/06_analysis/02_manuscript_results.R",
  "code/06_analysis/03_woody_height_model.R",
  "code/06_analysis/04_porewater_carbonate.R",
  "code/06_analysis/05_si_tables.R",
  # 07 upscaling: tower GPP, plot budgets, forcing, carbon budget
  "code/07_upscaling/01_tower_gpp.R",
  "code/07_upscaling/01b_flood_fraction.R",
  "code/07_upscaling/01c_tls_datum.R",
  "code/07_upscaling/01d_mangrove_extent.R",      # GMW 2016 extent for the Fig 1 map
  "code/07_upscaling/02_upscale_methane.R",
  "code/07_upscaling/03_upscale_co2.R",
  "code/07_upscaling/04_net_forcing.R",
  "code/07_upscaling/05_mc_forcing.R",
  "code/07_upscaling/06_carbon_budget.R",
  "code/07_upscaling/07_budget_sources.R",
  "code/07_upscaling/08_supplementary_analyses.R",
  "code/07_upscaling/09_regional_scaling.R",
  "code/07_upscaling/10_airborne_switch.R",          # switch by method (chamber, airborne, regional model)
  "code/06_analysis/06_site_table.R",                # Table S1 (needs flood_fraction.csv)
  "code/06_analysis/07_site_greenness_modis.R",     # site greenness (MODIS; cached download)
  "code/06_analysis/08_site_greenness_s2.R",        # site greenness (Sentinel-2; cached)
  "code/06_analysis/09_site_ndvi_history.R",        # Landsat NDVI 1995-2025 (cached); Fig 1c, Fig S3
  "code/06_analysis/10_ndvi_grain_srs.R",           # river-edge SRS5/SRS6: inland NDVI window (cached); Fig 1c
  "code/06_analysis/11_ndvi_seasonal_s2.R",         # wet vs dry Sentinel-2 NDVI 2018-2025 (cached); Fig S3b
  # 08 figures: display items, then copy into output/figures/main and SI
  "code/08_figures/fig2_component_boot.R",          # Fig S5
  "code/08_figures/fig3_stem_height.R",             # Fig S8
  "code/08_figures/fig_carbon_budget.R",           # Fig 3d schematic (rds)
  "code/08_figures/tls_render.py",                  # Fig 2c laser-scan panel (python3; from committed slab subsets)
  "code/08_figures/fig3_stands.R",                  # Fig 3; saves component shares (rds) for Fig 2
  "code/08_figures/fig2_rates.R",                   # Fig 2
  "code/08_figures/fig4_geochem.R",                 # Fig 4
  "code/08_figures/ed_porewater_rounds.R",          # Fig S17
  "code/08_figures/fig5_climate.R",                 # Fig 5
  "code/08_figures/fig1_system.R",                  # Fig 1 (map, photos, NDVI trajectories, schematic)
  "code/08_figures/plot_budget_figs.R",             # Fig 4
  "code/08_figures/fig6_porewater_pca.R",           # Fig 5, Figs S12-S13
  "code/08_figures/plot_closure.R",                 # Fig 6
  "code/08_figures/plot_carbon_budget.R",           # Fig 7
  "code/08_figures/plot_budget_multisource.R",      # Fig 8
  "code/08_figures/plot_budget_flow.R",             # Fig 9
  "code/08_figures/plot_budget_waterfall.R",        # Fig 10
  "code/08_figures/figS1_ebullition.R",             # Fig S6
  "code/08_figures/figS2_pneumatophore.R",          # Fig S7
  "code/08_figures/figS3_chamber_photos.R",         # Fig S1
  "code/08_figures/plot_extrap_clean.R",            # Fig S9
  "code/08_figures/si_flood_fraction.R",             # Fig S12
  "code/08_figures/si_exposure.R",                  # exposure shares (si_exposure_values.csv) for Fig S13
  "code/08_figures/si_ndvi.R",                       # Fig S3 (combines NDVI panels)
  "code/08_figures/si_satellite_scenes.R",          # Fig S2 (cached Sentinel-2 chips)
  "code/08_figures/si_waterline.R",                 # Fig S13
  "code/08_figures/si_k600_compare.R",              # Fig S14
  "code/08_figures/si_upscaling_figs.R",             # Figs S11, S15, S16 (from upscaling CSVs)
  "code/08_figures/si_porewater_carbonate.R",        # Fig S18
  "code/08_figures/plot_SA_height_fixedY.R",        # Fig S10
  "code/08_figures/plot_us_skr_gpp.R",              # tower GPP diagnostics
  "code/08_figures/si_switch_methods.R",            # Fig S21
  "code/08_figures/si_campaign_context.R",          # Fig S19
  "code/08_figures/plot_site_closure.R",            # exploratory site closure (not in SI)
  "code/08_figures/plot_water_positions.R",         # Fig S4b
  "code/08_figures/figS_sampling_design.R",         # Fig S4a
  "code/08_figures/fig_carafe_endmembers.R",            # airborne intact vs ghost (draft panel)
  "code/08_figures/fig_metagenome_placeholder.R",       # PLACEHOLDER metagenome figure        # sampling design vs tide (candidate SI figure)
  "code/08_figures/si_compose.py",                   # stacks SI figure parts (python3)
  "code/08_figures/collect_figures.R",
  "code/09_archive/build_ornl_daac_package.R",    # DRAFT ORNL DAAC package 1: chamber fluxes -> data/deposit/chamber_fluxes
  "code/09_archive/build_porewater_package.R"     # DRAFT ORNL DAAC package 2: porewater -> data/deposit/porewater_biogeochemistry
)

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
  "code/qa/chamber_heights_review.R",
  "code/qa/stem_height_by_class.R"
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

# Internal stages 01-02 (field-sheet reconciliation -> data/inputs) run only where their sources
# are present; the vendor-file steps also need data/analyzer. Without them the run starts from
# data/inputs and the deposited analyzer CSVs.
clean_raw <- "data/deposit/chamber_fluxes/analyzer_records"
internal <- stage_of(run) %in% c("01_metadata", "02_windows")
if (!dir.exists("data/field_notes") || !dir.exists("data/analyzer")) {
  if (any(internal)) cat("(internal stages 01-02 skipped: data/field_notes or data/analyzer absent; using data/inputs)\n")
  run <- run[!internal] }
if (any(stage_of(run) %in% c("03_fit", "04_ebullition")) && !length(list.files(clean_raw, pattern = "csv$")))
  stop("Stages 03-04 need the analyzer records in ", clean_raw, " (ORNL DAAC deposit). ",
       "Or start later: Rscript run_all.R --from 05_dataset")
if (any(stage_of(run) %in% c("01_metadata", "07_upscaling")) && !file.exists("data/tower/AMF_US-Skr_BASE_HH_2-5.csv"))
  stop("Stages 01 and 07 need data/tower/AMF_US-Skr_BASE_HH_2-5.csv (gitignored).")

dir.create("output/logs", recursive = TRUE, showWarnings = FALSE)
cat("=== BlueFlux ground pipeline:", length(run), "steps |", format(Sys.time()), "===\n")
for (s in run) {
  log <- file.path("output/logs", paste0(stage_of(s), "__", sub("\\.R$", ".log", basename(s))))
  t0 <- Sys.time()
  if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
  rc <- system2(if (grepl("\\.py$", s)) "python3" else "Rscript", s, stdout = log, stderr = log)
  if (file.exists("Rplots.pdf")) {   # a plot drawn without an open device: not an output
    file.remove("Rplots.pdf"); cat(sprintf("  (note: %s drew to the default device; Rplots.pdf removed)\n", label_of(s))) }
  cat(sprintf("%-45s %s  %5.0f s\n", label_of(s), if (rc == 0) "ok    " else "FAILED",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  if (rc != 0) { cat("\n--- last lines of", log, "---\n"); cat(tail(readLines(log), 20), sep = "\n")
    stop("step failed: ", s) }
}
cat("=== done ===\n")
