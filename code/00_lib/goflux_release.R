# =============================================================================
# goFlux fork release used for chamber flux fitting (stage 03).
#
# goFlux (Rheault et al. 2024) version 0.5.0.9002 with additions
# (Gewirtzman 2026, doi:10.5281/zenodo.23256675): GitHub release v0.5.0.9002
# of jgewirtzman/goFlux, commit 006f625. It replaces fluxqc (retired):
# process.fluxes(), flux.class(), qc.flags(), co2.tracer(), find.rise(),
# windows.from.table() and write.outputs() come from it.
#
# Installed into a project library, .Rlib/goflux-0.5.0.9002 (gitignored), so
# the goFlux in the user library is left as it is. Call goflux_release()
# before library(goFlux); each pipeline step runs in its own R process.
# Stage 04 still uses the vendored aqua fork (goflux_fork.R).
# =============================================================================
GOFLUX_REL_REF <- "jgewirtzman/goFlux@v0.5.0.9002"
GOFLUX_REL_SHA <- "006f625f9aaa8e42ab7be6afd27a00adce3da990"
GOFLUX_REL_LIB <- ".Rlib/goflux-0.5.0.9002"

goflux_release <- function() {
  desc <- file.path(GOFLUX_REL_LIB, "goFlux", "DESCRIPTION")
  if (!file.exists(desc) || !identical(unname(read.dcf(desc, "RemoteSha")[1, 1]), GOFLUX_REL_SHA)) {
    dir.create(GOFLUX_REL_LIB, recursive = TRUE, showWarnings = FALSE)
    message("Installing ", GOFLUX_REL_REF, " into ", GOFLUX_REL_LIB)
    remotes::install_github(GOFLUX_REL_REF, lib = GOFLUX_REL_LIB, dependencies = FALSE,
                            upgrade = "never", quiet = TRUE)
  }
  .libPaths(c(normalizePath(GOFLUX_REL_LIB), .libPaths()))
  invisible(GOFLUX_REL_LIB)
}
