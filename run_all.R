#!/usr/bin/env Rscript
# =============================================================================
# run_all.R — Master pipeline
# =============================================================================
# Runs the analysis workflow from preprocessing through figures. Paths are
# relative to the project root; each script self-anchors with here::here(),
# so run from anywhere:
#
#   Rscript run_all.R        # run all steps
#   Rscript run_all.R 12     # run from step 12 onward
#
# TLS surface-area and porewater data are bundled in data/tls/ and
# data/porewater/. Override the location with env vars if needed:
#   BLUEFLUX_TLS_DIR       (default data/tls)
#   BLUEFLUX_MICROBES_DIR  (default data/porewater)
#
# Steps needing interactive RStudio (goFlux click.peak2, time-window picking)
# are flagged as MANUAL and not executed here.
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
start_step <- if (length(args) > 0) as.integer(args[1]) else 1L

run_step <- function(step_num, description, script_path) {
  if (step_num < start_step) {
    cat(sprintf("[%02d] SKIP: %s\n", step_num, description)); return(invisible(NULL))
  }
  cat(sprintf("\n========================================\n"))
  cat(sprintf("[%02d] %s\n     %s\n", step_num, description, script_path))
  cat(sprintf("========================================\n"))
  t0 <- Sys.time()
  tryCatch(source(script_path, local = new.env(parent = globalenv())),
    error = function(e) { cat(sprintf("ERROR in step %02d: %s\n", step_num, conditionMessage(e))); stop(e) })
  cat(sprintf("[%02d] DONE (%.1f s)\n", step_num, round(difftime(Sys.time(), t0, units = "secs"), 1)))
}

cat("=== BlueFlux Ground Analysis Pipeline ===\n")
cat("Starting from step:", start_step, " |  Time:", format(Sys.time()), "\n")

# ---- 02 PREPROCESS ----------------------------------------------------------
run_step(1, "Assign chamber volumes and surface areas",   "code/02_preprocess/assign_tree_vol_area.R")
run_step(2, "Convert to goFlux auxfiles",                 "code/02_preprocess/convert_to_auxfile.R")

# ---- 03 FLUX CALCULATION (interactive) --------------------------------------
if (start_step <= 3) {
  cat("\n[03] MANUAL: goFlux processing in RStudio if auxfiles changed.\n")
  cat("     code/03_flux_calculation/*.R  ;  rescue: code/04_qc_rescue/*.R\n")
}

# ---- 04 QC / RESCUE ---------------------------------------------------------
run_step(4, "Build missing auxfiles for rescued measurements", "code/04_qc_rescue/build_missing_auxfiles.R")

# ---- 05 INTEGRATION ---------------------------------------------------------
run_step(5, "Assemble clean combined dataset",            "code/05_integration/assemble_clean_dataset.R")
run_step(6, "Apply chamber volume corrections",           "code/05_integration/apply_chamber_corrections.R")

# ---- 06 EBULLITION ----------------------------------------------------------
run_step(7,  "Detect ebullition in raw traces",           "code/06_ebullition/detect_ebullition.R")
run_step(8,  "Reprocess water traces (partitioned)",      "code/06_ebullition/goflux_reprocess_ebullition.R")
run_step(9,  "Integrate ebullition into dataset",         "code/06_ebullition/integrate_ebullition.R")
run_step(10, "Apply negative-flux corrections",           "code/06_ebullition/apply_negative_flux_corrections.R")

# ---- 07 ANALYSIS ------------------------------------------------------------
run_step(11, "Summary statistics table",                  "code/07_analysis/summary_table.R")
run_step(12, "Compute manuscript results/numbers",        "code/07_analysis/manuscript_results.R")

# ---- 09 TOWER GPP (upstream input to CO2 upscaling) -------------------------
run_step(13, "Partition US-Skr tower GPP",                "code/09_tower_gpp/us_skr_gapfill_gpp.R")

# ---- 08 UPSCALING (chambers x TLS -> stand budgets) -------------------------
# Requires BLUEFLUX_TLS_DIR (surface-area products).
run_step(14, "Upscale methane to plot budgets",          "code/08_upscaling/upscale_methane_to_plots.R")
run_step(15, "Upscale CO2 / NEE to plot budgets",        "code/08_upscaling/upscale_co2_to_plots.R")
run_step(16, "Monte Carlo net forcing",                  "code/08_upscaling/mc_co2_forcing.R")

# ---- 10 FIGURES (main -> figures/main, SI -> figures/SI, else figures/other)
if (start_step <= 17) cat("\n[17] MANUAL: Fig 1 map/photo composite — code/10_figures/publication_map_composite.R\n")
run_step(18, "Fig 2: Component flux rates",              "code/10_figures/fig2_component_boot.R")
run_step(19, "Fig 3: Stem height x species",            "code/10_figures/fig3_stem_height.R")
run_step(20, "Fig 4: Bottom-up budgets",               "code/10_figures/plot_budget_figs.R")
run_step(21, "Fig 5: Porewater PCA / regime shift",    "code/10_figures/fig6_porewater_pca.R")
run_step(22, "Fig 6: Closure + net forcing",           "code/10_figures/plot_closure.R")
run_step(23, "Fig S1: Ebullition partitioning",        "code/10_figures/figS1_ebullition.R")
run_step(24, "Fig S2: Pneumatophore density vs flux",  "code/10_figures/figS2_pneumatophore.R")
run_step(25, "Fig S3: Chamber photographs",            "code/10_figures/figS3_chamber_photos.R")

# ---- DONE -------------------------------------------------------------------
cat("\n========================================\n=== Pipeline complete ===\n")
cat("Time:", format(Sys.time()), "\n")
cat("Dataset:  output/data_products/combined_gas_flux_dataset.csv\n")
cat("Budgets:  output/upscaling/  |  Forcing: output/upscaling/net_forcing_by_class.csv\n")
cat("Figures:  output/figures/{main,SI,presentation,other}\n")
cat("Results:  manuscript/text/manuscript_results.txt\n")
cat("========================================\n")
