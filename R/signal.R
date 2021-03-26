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

  filter_type <- get_filter_type(order, cutoff, type)
  attributes(data)$filter_type <- filter_type
  data
}

get_filter_type <- function(order, cutoff, type) {
  order <- toOrdinal::toOrdinal(order)
  if (grepl("lowpass", type)) {
    type <- "low-pass"
  }
  cutoff <- paste0(cutoff, "Hz")

  unclass(glue::glue("Butterworth ({order}-ord, {type}, {cutoff})"))
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

#' Find peaks in a signal
#'
#' @export
find_peaks <- function(data, vector, min_height = 1.3, min_dist = 0.4) {

  min_dist <- attributes(data)$samp_freq * min_dist

  if (grepl("resultant", vector, ignore.case = TRUE)) {
    acc_resultant <- as.numeric(data[["acc_R"]])
    p_resultant <- scipy$signal$find_peaks(
      acc_resultant, height = min_height, distance = min_dist
    )
    peaks <- list(
      resultant = list(
        height = as.numeric(p_resultant[[2]][[1]]),
        idx = as.numeric(p_resultant[[1]] + 1)
      )
    )
  } else if (grepl("vertical", vector, ignore.case = TRUE)) {
    acc_vertical <- as.numeric(data[["acc_Y"]]) * - 1
    p_vertical <- scipy$signal$find_peaks(
      acc_vertical, height = min_height, distance = min_dist
    )
    peaks <- list(
      vertical = list(
        height = as.numeric(p_vertical[[2]][[1]]),
        idx = as.numeric(p_vertical[[1]] + 1)
      )
    )
  } else if (grepl("both", vector, ignore.case = TRUE)) {
    acc_resultant <- as.numeric(data[["acc_R"]])
    acc_vertical <- as.numeric(data[["acc_Y"]]) * - 1
    p_resultant <- scipy$signal$find_peaks(
      acc_resultant, height = min_height, distance = min_dist
    )
    p_vertical <- scipy$signal$find_peaks(
      acc_vertical, height = min_height, distance = min_dist
    )
    peaks <- list(
      resultant = list(
        height = as.numeric(p_resultant[[2]][[1]]),
        idx = as.numeric(p_resultant[[1]] + 1)
      ),
      vertical = list(
        height = as.numeric(p_vertical[[2]][[1]]),
        idx = as.numeric(p_vertical[[1]] + 1)
      )
    )
  }

  attributes(data)$peaks <- peaks
  row.names(data) <- NULL
  data
}
