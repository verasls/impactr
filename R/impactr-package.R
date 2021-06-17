#' @useDynLib impactr, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("impactr", libpath)
}
