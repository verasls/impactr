scipy <- NULL

.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)
  scipy <<- reticulate::import("scipy", delay_load = TRUE)
}
