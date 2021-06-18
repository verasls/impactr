#' Predict mechanical loading
#'
#' Predict either ground reaction force or loding rate, or both, based on
#' accelerometer data.
#'
#' @param data An \code{impactr_data} object, as obtained with
#'   \link[=read_acc]{read_acc()}.
#' @param outcome A character string. Can be either "grf" (for ground reaction
#'   force), or "lr" (for loading rate) or "both" (for both mechanical loading
#'   variables).
#' @param vector A character string indicating in which acceleration vector to
#'   find the peaks. Can be "resultant" or "vertical".
#' @param equation A character string indicating which equation to use to make
#'   the predictions. The only value supported, currently, is "walking/running".
#'
#' @return An object of class \code{impactr_peaks} with the ground reaction
#'   force and/or loading rate peaks magnitude stored in the columns.
#'
#' @export
#'
#' @examples
#' data <- read_acc(impactr_example("hip-imu.csv")) |>
#'   specify_parameters(acc_placement = "hip", subj_body_mass = 78) |>
#'   use_resultant() |>
#'   find_peaks("resultant") |>
#'   predict_loading(
#'     outcome = "grf",
#'     vector = "resultant",
#'     equation = "walking/running"
#'   )
#' head(data)
predict_loading <- function(data, outcome, vector, equation) {
  if (grepl("\\bgrf\\b", outcome, ignore.case = TRUE)) {
    predict_grf(data, vector, equation)
  } else if (grepl("\\blr\\b", outcome, ignore.case = TRUE)) {
    predict_lr(data, vector, equation)
  } else if (grepl("\\bboth\\b", outcome, ignore.case = TRUE)) {
    impactr_peaks <- predict_grf(data, vector, equation)
    lr <- predict_lr(data, vector, equation)
    var_name <- names(lr)[3]
    impactr_peaks[var_name] <- lr[var_name]
    impactr_peaks
  }
}

predict_grf <- function(data, vector, equation) {
  coeff <- get_grf_coefficients(
    attributes(data)$acc_placement, vector, equation
  )
  body_mass <- attributes(data)$subj_body_mass
  peaks <- data[[paste0(vector, "_peak_acc")]]
  data[[paste0(vector, "_peak_grf")]] <- compute_loading(
    coeff, peaks, body_mass
  )
  data
}

predict_lr <- function(data, vector, equation) {
  samp_freq <- attributes(data)$samp_freq
  coeff <- get_lr_coefficients(attributes(data)$acc_placement, vector, equation)
  body_mass <- attributes(data)$subj_body_mass
  peaks_idx <- attributes(data)$peaks_idx
  if (!grepl("\\bboth\\b", vector)) {
    acc_vector <- attributes(data)$acc_signal
  }

  start_idx <- get_curve_start(acc_vector, peaks_idx)
  peaks <- compute_peak_acc_rate(acc_vector, start_idx, peaks_idx, samp_freq)
  data[[paste0(vector, "_peak_lr")]] <- compute_loading(
    coeff, peaks, body_mass
  )
  data
}

get_grf_coefficients <- function(acc_placement, vector, equation) {
  if (equation == "walking/running") {
    if (acc_placement == "ankle" & vector == "resultant") {
      list(b0 = 1026.046, b1 = - 153.073, b2 = 6.641, b3 = 2.097)
    } else if (acc_placement == "back" & vector == "resultant") {
      list(b0 = 795.173, b1 = - 258.882, b2 = 5.951, b3 = 4.903)
    } else if (acc_placement == "hip" & vector == "resultant") {
      list(b0 = 844.500, b1 = - 264.692, b2 = 4.677, b3 = 5.118)
    } else if (acc_placement == "ankle" & vector == "vertical") {
      list(b0 = 1014.045, b1 = - 226.690, b2 = 4.854, b3 = 3.562)
    } else if (acc_placement == "back" & vector == "vertical") {
      list(b0 = 890.428, b1 = - 339.790, b2 = 4.533, b3 = 6.131)
    } else if (acc_placement == "hip" & vector == "vertical") {
      list(b0 = 908.037, b1 = - 322.843, b2 = 4.546, b3 = 5.691)
     }
  }
}

get_lr_coefficients <- function(acc_placement, vector, equation) {
  if (equation == "walking/running") {
    if (acc_placement == "ankle" & vector == "resultant") {
      list(b0 = 6534.981, b1 = - 15.738, b2 = - 76.433, b3 = 4.258)
    } else if (acc_placement == "back" & vector == "resultant") {
      list(b0 = 6155.636, b1 = - 81.779, b2 = - 5.500, b3 = 5.179)
    } else if (acc_placement == "hip" & vector == "resultant") {
      list(b0 = 4431.800, b1 = - 33.175, b2 = 12.632, b3 = 4.014)
    } else if (acc_placement == "ankle" & vector == "vertical") {
      list(b0 = 5124.478, b1 = - 47.525, b2 = 8.344, b3 = 2.588)
    } else if (acc_placement == "back" & vector == "vertical") {
      list(b0 = 6605.822, b1 = - 112.779, b2 = - 3.767, b3 = 5.061)
    } else if (acc_placement == "hip" & vector == "vertical") {
      list(b0 = 5343.980, b1 = - 89.984, b2 = 3.49, b3 = 4.808)
    }
  }
}
