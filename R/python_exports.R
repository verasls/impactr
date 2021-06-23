# Global reference to scipy module
scipy <- NULL

.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)

  # Initialize scipy module .onLoad
  scipy <<- reticulate::import("scipy", delay_load = TRUE)
}

#' Install SciPy
#'
#' Install the SciPy Python package into a virtual environment.
#'
#' @param method Installation method. By default, "auto" automatically finds a
#' method that will work in the local environment.
#' @param conda The path to a conda executable. Use "auto" to allow reticulate
#' to automatically find an appropriate conda binary.
#'
#' @seealso \code{\link[reticulate:py_install]{py_install()}}
#'
#' @export
install_scipy <- function(method = "auto", conda = "auto") {
  reticulate::py_install("scipy", method = method, conda = conda)
}
