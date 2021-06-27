#' Filter the acceleration signal
#'
#' Filter the acceleration signal using a butterworth digital filter.
#'
#' @param data An \code{impactr_data} object, as obtained with
#'   \link[=read_acc]{read_acc()}.
#' @param order The order of the filter. Defaults to 4.
#' @param cutoff The filter cut-off frequency in Hz. Defaults to 20. For
#'   low- and high-pass filters, must be a scalar. For band-pass and band-
#'   stop, a vector of length two.
#' @param type The type of filter. Defaults to "lowpass". Can be "lowpass",
#'   "highpass", "bandpass" or "bandstop".
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
#' data <- read_acc(impactr_example("hip-raw.csv"))
#' filter_acc(data)
filter_acc <- function(data, order = 4, cutoff = 20, type = "lowpass") {
  check_args_filter_acc(order, cutoff, type)
  filter_type <- get_filter_type(order, cutoff, type)
  attributes(data)$filter_type <- filter_type

  if (type == "lowpass") {
    type <- "low"
  } else if (type == "highpass") {
    type <- "high"
  } else if (type == "bandpass") {
    type <- "pass"
  } else if (type == "bandstop") {
    type <- "stop"
  }

  fnyq <- attributes(data)$samp_freq / 2
  w <- cutoff / fnyq

  data$acc_X <- filter_signal(data$acc_X, order, w, type)
  data$acc_Y <- filter_signal(data$acc_Y, order, w, type)
  data$acc_Z <- filter_signal(data$acc_Z, order, w, type)
  data
}

filter_signal <- function(signal, n, w, type) {
  ba <- signal::butter(n, w, type)
  signal::filtfilt(ba, signal)
}

get_filter_type <- function(order, cutoff, type) {
  order <- toOrdinal::toOrdinal(order)
  if (type == "lowpass") {
    type <- "low-pass"
  } else if (type == "highpass") {
    type <- "high-pass"
  } else if (type == "bandpass") {
    type <- "band-pass"
  } else if (type == "bandstop") {
    type <- "band-stop"
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
  valid_type <- c("lowpass", "highpass", "bandpass", "bandstop")
  if (type %!in% valid_type) {
    lvmisc::abort_argument_value("type", valid_type)
  }
  if (
    type %in% c("lowpass", "highpass") &
    length(cutoff) != 1
  ) {
    lvmisc::abort_argument_length(
      "cutoff", must = "have length 1", not = cutoff
    )
  } else if (
    type %in% c("bandpass", "bandstop") &
    length(cutoff) != 2
  ) {
    lvmisc::abort_argument_length(
      "cutoff", must = "have length 2", not = cutoff
    )
  }
}

#' Find peaks in a signal
#'
#' Find peaks in the acceleration signal.
#'
#' @param data An \code{impactr_data} object, as obtained with
#'   \link[=read_acc]{read_acc()}.
#' @param vector A character string indicating in which acceleration vector to
#'   find the peaks. Can be "resultant", "vertical" or "all".
#' @param min_height The minimum height of the peaks (in \emph{g}).
#' @param min_dist The minimum horizontal distance between peaks (in seconds).
#'
#' @return An object of class \code{impactr_peaks} with the peaks magnitude
#'   stored in the columns.
#'
#' @details The default values of the filter parameters are matching the filter
#'   used in the paper by Veras et al. that developed the mechanical loading
#'   prediction equations (see References).
#'   When the \code{vector} parameter is set to "all", there may contain
#'   \code{NA} values in the \code{resultant_peak_acc} and/or
#'   \code{vertical_peak_acc} at the timestamps in which a peak value for that
#'   vector could not be identified.
#'
#' @details The default values of \code{min_height} and \code{min_dist} are
#'   matching the criteria used in the paper by Veras et al. that developed the
#'   mechanical loading prediction equations (see References)
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
#' data <- read_acc(impactr_example("hip-raw.csv"))
#' data <- use_resultant(data)
#' find_peaks(data, vector = "resultant")
find_peaks <- function(data, vector, min_height = 1.3, min_dist = 0.4) {
  check_args_find_peaks(data, vector, min_height, min_dist)
  min_dist <- attributes(data)$samp_freq * min_dist

  if (vector != "all") {
    if (vector == "vertical") {
      acc <- as.numeric(data[["acc_Y"]]) * - 1
      var_name <- "vertical_peak_acc"
    } else if (vector == "resultant") {
      acc <- as.numeric(data[["acc_R"]])
      var_name <- "resultant_peak_acc"
    }
    peaks <- pracma::findpeaks(
      acc, minpeakheight = min_height, minpeakdistance = min_dist
    )
    peaks <- as.data.frame(peaks)
    peaks <- peaks[order(peaks[, 2]), ]
    peaks <- list(
      height = peaks[, 1],
      idx = peaks[, 2]
    )

    impactr_peaks <- tibble::tibble(
      timestamp = data$timestamp[peaks$idx],
      peak_acc = peaks$height
    )
    impactr_peaks <- new_impactr_peaks(
      impactr_peaks,
      start_date_time = attributes(data)$start_date_time,
      samp_freq = attributes(data)$samp_freq,
      acc_placement = attributes(data)$acc_placement,
      subj_body_mass = attributes(data)$subj_body_mass,
      filter_type = attributes(data)$filter_type,
      peaks_idx = peaks$idx,
      acc_signal = acc
    )
    names(impactr_peaks)[2] <- var_name
    return(impactr_peaks)
  } else {
    acc_vertical <- as.numeric(data[["acc_Y"]]) * - 1
    acc_resultant <- as.numeric(data[["acc_R"]])
    p_vertical <- pracma::findpeaks(
      acc_vertical, minpeakheight = min_height, minpeakdistance = min_dist
    )
    p_resultant <- pracma::findpeaks(
      acc_resultant, minpeakheight = min_height, minpeakdistance = min_dist
    )
    p_vertical <- as.data.frame(p_vertical)
    p_vertical <- p_vertical[order(p_vertical[, 2]), ]
    p_resultant <- as.data.frame(p_resultant)
    p_resultant <- p_resultant[order(p_resultant[, 2]), ]
    peaks <- list(
      vertical = list(
        height = p_vertical[, 1],
        idx = p_vertical[, 2]
      ),
      resultant = list(
        height = p_resultant[, 1],
        idx = p_resultant[, 2]
      )
    )
    total_peaks_idx <- sort(union(peaks$vertical$idx, peaks$resultant$idx))
    impactr_peaks <- tibble::tibble(
      timestamp = data$timestamp[total_peaks_idx],
      vertical_peak_acc = purrr::map_dbl(
        seq_along(total_peaks_idx),
        ~ get_peaks_vector(total_peaks_idx, peaks, "vertical", .x)
      ),
      resultant_peak_acc = purrr::map_dbl(
        seq_along(total_peaks_idx),
        ~ get_peaks_vector(total_peaks_idx, peaks, "resultant", .x)
      )
    )
    impactr_peaks <- new_impactr_peaks(
      impactr_peaks,
      start_date_time = attributes(data)$start_date_time,
      samp_freq = attributes(data)$samp_freq,
      acc_placement = attributes(data)$acc_placement,
      subj_body_mass = attributes(data)$subj_body_mass,
      filter_type = attributes(data)$filter_type,
      peaks_idx = list(
        vertical = peaks$vertical$idx,
        resultant = peaks$resultant$idx
      ),
      acc_signal = list(
        vertical = acc_vertical,
        resultant = acc_resultant
      )
    )
    return(impactr_peaks)
  }
}

get_peaks_vector <- function(total_peaks_idx, peaks, vector, i) {
  ifelse(
    total_peaks_idx[i] %in% peaks[[vector]]$idx,
    peaks[[vector]]$height[which(peaks[[vector]]$idx == total_peaks_idx[i])],
    NA
  )
}

#' @importFrom lvmisc %!in%
check_args_find_peaks <- function(data, vector, min_height, min_dist) {
  if (!is.character(vector)) {
    lvmisc::abort_argument_type("vector", must = "be character", not = vector)
  }
  if (!is.numeric(min_height)) {
    lvmisc::abort_argument_type(
      "min_height", must = "be numeric", not = min_height
    )
  }
  if (!is.numeric(min_dist)) {
    lvmisc::abort_argument_type(
      "min_dist", must = "be numeric", not = min_dist
    )
  }
  valid_values <- c("resultant", "vertical", "all")
  if (vector %!in% valid_values) {
    lvmisc::abort_argument_value("vector", valid_values)
  }
  if (vector %in% c("resultant", "all")) {
    if ("acc_R" %!in% names(data)) {
      rlang::abort(
        glue::glue(
          "The column with the resultant acceleration is not \\
          present in `data`. Please, compute it with `use_resultant()`."
        )
      )
    }
  }
}
