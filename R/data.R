#' Install accdata package
#'
#' A helper function to install the \code{accdata} package from a
#' \href{https://lveras.com/drat/}{\code{drat}} repository. The \code{accdata}
#' package contains datasets that can be used to test the functionalities
#' from \code{impactr}. Note that \code{accdata} is a large package
#' (approximately 80 MB) and could take a while to download and install.
#'
#' @export
install_accdata <- function() {
  utils::install.packages("accdata", repos = "lveras.com/drat", type = "source")
}

#' Import datasets from accdata package
#'
#' A helper function to import datasets from the \code{accdata} package.
#'
#' @param data A character string indicating which data to load. The currently
#'   available datasets are "daily_acc_3d" and "daily_acc_7d".
#'
#' @details To import these datasets you need to install the \code{accdata}
#'   package. It can be installed by running \code{install_accdata()}. The
#'   datasets documentation can be accessed by \code{?accdata::`dataset_name`}
#'   (e.g., \code{?accdata::daily_acc_3d}.
#'
#' @return An object of class \code{impactr_data}.
#'
#' @export
#'
#' @examples
#' # Ensure that {accdata} package is available before running the example.
#' # If it is not, run install_accdata() to install the required package.
#' if (requireNamespace("accdata", quietly = TRUE)) {
#'   data <- import_dataset("daily_acc_3d")
#'   data
#' }
import_dataset <- function(data) {

  if (!requireNamespace("accdata", quietly = TRUE)) {
    msg <- glue::glue(
      "To use this function you must install the \"accdata\" package. \\
      You can do it by running `install_accdata()`."
    )
    rlang::abort(msg)
  }

  valid_data <- c("daily_acc_3d", "daily_acc_7d")

  if (data == "daily_acc_3d") {
    accdata::daily_acc_3d
  } else if (data == "daily_acc_7d") {
    accdata::daily_acc_7d
  } else {
    lvmisc::abort_argument_value("data", valid_data)
  }

}
