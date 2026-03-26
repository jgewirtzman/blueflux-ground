#!/usr/bin/env Rscript
# =============================================================================
# run_all.R — Master pipeline script
# =============================================================================
# Runs the complete analysis workflow from preprocessing through figures.
#
# Usage:
#   Rscript run_all.R        # runs all steps
#   Rscript run_all.R 5      # runs from step 5 onward
#
# Steps requiring interactive RStudio (goFlux click.peak2, interactive time
# picker) are noted but not executed. Re-run those manually if auxfiles or
# time windows change, then re-run this script from the appropriate step.
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
start_step <- if (length(args) > 0) as.integer(args[1]) else 1L

run_step <- function(step_num, description, script_path) {
  if (step_num < start_step) {
    cat(sprintf("[%02d] SKIP: %s\n", step_num, description))
    return(invisible(NULL))
  }
  cat(sprintf("\n========================================\n"))
  cat(sprintf("[%02d] %s\n", step_num, description))
  cat(sprintf("     %s\n", script_path))
  cat(sprintf("========================================\n"))
  t0 <- Sys.time()
  tryCatch(
    source(script_path, local = new.env(parent = globalenv())),
    error = function(e) {
      cat(sprintf("ERROR in step %02d: %s\n", step_num, conditionMessage(e)))
      stop(e)
    }
  )
  elapsed <- round(difftime(Sys.time(), t0, units = "secs"), 1)
  cat(sprintf("[%02d] DONE (%.1f s)\n", step_num, elapsed))
}

cat("=== BlueFlux Ground Analysis Pipeline ===\n")
cat("Starting from step:", start_step, "\n")
cat("Time:", format(Sys.time()), "\n\n")

# ---- PREPROCESSING ----------------------------------------------------------
run_step(1, "Assign chamber volumes and surface areas",
         "code/02_preprocess/assign_tree_vol_area.R")

run_step(2, "Convert to goFlux auxfiles",
         "code/02_preprocess/convert_to_auxfile.R")

# ---- FLUX CALCULATION (interactive) -----------------------------------------
if (start_step <= 3 && start_step >= 3) {
  cat("\n========================================\n")
  cat("[03] MANUAL: goFlux processing\n")
  cat("     Run in RStudio if auxfiles changed.\n")
  cat("     Scripts: code/03_flux_calculation/*.R\n")
  cat("     Rescue:  code/04_qc_rescue/*.R\n")
  cat("========================================\n")
}

# ---- QC / RESCUE -------------------------------------------------------------
run_step(4, "Build missing auxfiles for rescued measurements",
         "code/04_qc_rescue/build_missing_auxfiles.R")

# ---- INTEGRATION -------------------------------------------------------------
run_step(5, "Assemble clean combined dataset",
         "code/05_integration/assemble_clean_dataset.R")

run_step(6, "Apply HA/HB chamber volume corrections",
         "code/05_integration/apply_chamber_corrections.R")

# ---- EBULLITION --------------------------------------------------------------
run_step(7, "Detect ebullition in raw analyzer traces",
         "code/06_ebullition/detect_ebullition.R")

run_step(8, "Reprocess water traces through goFlux (partitioned)",
         "code/06_ebullition/goflux_reprocess_ebullition.R")

run_step(9, "Integrate ebullition water traces into combined dataset",
         "code/06_ebullition/integrate_ebullition.R")

run_step(10, "Apply negative flux corrections and remove artifacts",
          "code/06_ebullition/apply_negative_flux_corrections.R")

# ---- ANALYSIS ----------------------------------------------------------------
run_step(11, "Generate summary statistics table",
          "code/07_analysis/summary_table.R")

run_step(12, "Compute manuscript results and numbers",
          "code/07_analysis/manuscript_results.R")

# ---- FIGURES (one per display item) ------------------------------------------
# Fig 1: Map + photos (run manually — requires spatial data + photos)
if (start_step <= 13 && start_step >= 13) {
  cat("\n[13] MANUAL: Fig 1 map/photo composite\n")
  cat("     code/07_analysis/publication_map_composite.R\n")
}

run_step(14, "Fig 2: Component flux rates (bootstrapped)",
          "code/07_analysis/fig2_component_boot.R")

run_step(15, "Fig 3: Stem height x species composite",
          "code/07_analysis/fig3_stem_height.R")

# Fig 4: TLS surface areas (PLACEHOLDER — awaiting data from Lizzy)
# Fig 5: Budget closure (PLACEHOLDER — awaiting TLS + CARAFE)

run_step(16, "Fig 6: Porewater PCA + depth profiles composite",
          "code/07_analysis/fig6_porewater_pca.R")

run_step(17, "Fig S1: Ebullition partitioning",
          "code/07_analysis/figS1_ebullition.R")

run_step(18, "Fig S2: Pneumatophore density vs flux",
          "code/07_analysis/figS2_pneumatophore.R")

run_step(19, "Fig S3: Chamber type photographs",
          "code/07_analysis/figS3_chamber_photos.R")

# ---- CLEANUP -----------------------------------------------------------------
run_step(20, "Figure cleanup (archive non-main figures)",
          "code/07_analysis/figure_cleanup.R")

# ---- DONE --------------------------------------------------------------------
cat("\n========================================\n")
cat("=== Pipeline complete ===\n")
cat("Time:", format(Sys.time()), "\n")
cat("Final dataset: output/combined_gas_flux_dataset.csv\n")
cat("Figures:       output/figures/\n")
cat("Results:       output/manuscript_results.txt\n")
cat("========================================\n")
