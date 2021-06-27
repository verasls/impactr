#' Get path to example data
#'
#' \code{impactr} comes with some example ActiGraph accelerometer raw data
#' files in its `inst/extdata` directory. This function make them easy to
#' access.
#'
#' @param file A character string with the file name. If \code{NULL}, the
#'   example files will be listed.
#'
#' @return If \code{file = NULL}, it returns the file names of the example
#'   data files, else it returns the path to the example data.
#'
#' @export
#'
#' @examples
#' impactr_example()
#' impactr_example("hip-raw.csv")
impactr_example <- function(file = NULL) {
  if (is.null(file)) {
    dir(system.file("extdata", package = "impactr"))
  } else {
    system.file("extdata", file, package = "impactr", mustWork = TRUE)
  }
}
