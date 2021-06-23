# Global reference to scipy module
scipy <- NULL

.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)

  # Initialize scipy module .onLoad
  scipy <<- reticulate::import("scipy", delay_load = TRUE)
}
