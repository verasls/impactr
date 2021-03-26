#' Filter a signal
#'
#' Function imported from python. Apply a butterworth digital filter to a
#' signal. For more information run
#' \code{\link[reticulate:py_help]{reticulate::py_help(filter_signal)}}.
#'
#' @param ... Arguments to be passed to the python function:
#'   * x: A numeric vector.
#'   * n: The order of the filter.
#'   * wn: The critical frequency or frequencies.
#'   * type: The type of the filter.
#'
#' @return A numeric vector with the filtered signal.
#'
#' @export
filter_signal <- NULL

# Global reference to scipy module
scipy <- NULL

.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)

  # Define functions from python script at package load time
  module_path <- system.file("python", package = pkgname)
  filter <- reticulate::import_from_path("filter", module_path)
  filter_signal <<- filter$filter_signal

  # Initialize scipy module .onLoad
  scipy <<- reticulate::import("scipy", delay_load = TRUE)
}
