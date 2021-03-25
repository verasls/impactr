#' Filter the acceleration signal
#'
#' Filter the acceleration signal using a butterworth digital filter.
#'
#' @param data An \code{impactr_data} object, as obtained with
#'   \link[=read_acc]{read_acc()}.
#' @param order The order of the filter. Defaults to 4.
#' @param cutoff The filter cut-off frequency in Hz. Defaults to 20.
#' @param type The type of filter. Defaults to "lowpass".
#'
#' @return An object of class \code{impactr_data}.
#'
#' @details The default values of the filter parameters are matching the filter
#'   used in the paper by Veras et al. that developed the mechanical loading
#'   prediction equations (see References).
#'
#' @references \itemize{
#'   \item Veras L, Diniz-Sousa F, Boppre G, Devezas V, Santos-Sousa H, Preto J,
#'   Machado L, Vilas- Boas JP, Oliveira J, Fonseca H. Accelerometer-based
#'   prediction of skeletal mechanical loading during walking in normal weight
#'   to severely obese subjects. Osteoporosis International. 2020. 31(7):1239-
#'   1250. \doi{https://doi.org/10.1007/s00198-020-05295-2}.
#'  }
#'
#' @export
#'
#' @examples
#' data <- read_acc(impactr_example("hip-imu.csv"))
#' filter_acc(data)
filter_acc <- function(data, order = 4, cutoff = 20, type = "lowpass") {
  check_args_filter_acc(order, cutoff, type)
  fnyq <- attributes(data)$samp_freq / 2
  wn <- cutoff / fnyq
  data$acc_X <- filter_signal(data$acc_X, order, wn, type)
  data$acc_Y <- filter_signal(data$acc_Y, order, wn, type)
  data$acc_Z <- filter_signal(data$acc_Z, order, wn, type)
  data
}

#' Filter a signal
#'
#' Apply a butterworth digital filter to a signal.
#'
#' @param signal A numeric vector.
#' @param n The order of the filter.
#' @param wn The critical frequency or frequencies.
#' @param type The type of the filter.
#'
#' @return A numeric vector with the filtered signal.
#'
#' @export
filter_signal <- function(signal, n, wn, type) {
  ba <- scipy$signal$butter(n, wn, type)
  scipy$signal$filtfilt(ba[[1]], ba[[2]], signal)
}

check_args_filter_acc <- function(order, cutoff, type) {
  if (!is.numeric(order)) {
    lvmisc::abort_argument_type("order", must = "be numeric", not = order)
  }
  if (!is.numeric(cutoff)) {
    lvmisc::abort_argument_type("cutoff", must = "be numeric", not = cutoff)
  }
  if (!is.character(type)) {
    lvmisc::abort_argument_type("type", must = "be character", not = type)
  }
}
