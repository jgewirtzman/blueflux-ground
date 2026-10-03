# =============================================================================
# goFlux fork used for floating-chamber ebullition (stage 04).
#
# goAquaFlux(diffusion.window = "deebulliated") exists only on the goFlux fork
# branch feat/aqua-diffusive-deebulliated (commit 2ed7224, not on any remote).
# Its package source is vendored here, pinned:
#   vendor/goFlux_0.4.0_aqua-deebulliated_2ed7224.tar.gz
#   (git archive 2ed7224 DESCRIPTION NAMESPACE LICENSE R man data)
# and installed into a project library, .Rlib/goflux-aqua-2ed7224 (gitignored),
# so the released goFlux 0.4.0 in the user library is left as it is. Stage 04
# runs goAquaFlux in a separate R process with that library first on the path
# (callr), so the two goFlux versions never meet in one session.
# =============================================================================
GOFLUX_FORK_TARBALL <- "vendor/goFlux_0.4.0_aqua-deebulliated_2ed7224.tar.gz"
GOFLUX_FORK_LIB     <- ".Rlib/goflux-aqua-2ed7224"

goflux_fork_lib <- function() {
  stamp <- file.path(GOFLUX_FORK_LIB, "goFlux", "DESCRIPTION")
  if (!file.exists(stamp)) {
    dir.create(GOFLUX_FORK_LIB, recursive = TRUE, showWarnings = FALSE)
    message("Installing the goFlux fork (", GOFLUX_FORK_TARBALL, ") into ", GOFLUX_FORK_LIB)
    rc <- system2(file.path(R.home("bin"), "R"),
                  c("CMD", "INSTALL", "--no-test-load", "-l", shQuote(GOFLUX_FORK_LIB), shQuote(GOFLUX_FORK_TARBALL)),
                  stdout = TRUE, stderr = TRUE)
    if (!file.exists(stamp)) stop("goFlux fork install failed:\n", paste(tail(rc, 20), collapse = "\n"))
  }
  normalizePath(GOFLUX_FORK_LIB)
}
