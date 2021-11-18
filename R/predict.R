#' Predict mechanical loading
#'
#' Predict either ground reaction force or loading rate, or both, based on
#' accelerometer data.
#'
#' @param data An \code{impactr_data} object, as obtained with
#'   \link[=read_acc]{read_acc()}.
#' @param outcome A character string. Can be either "grf" (for ground reaction
#'   force), or "lr" (for loading rate) or "all" (for both mechanical loading
#'   variables).
#' @param vector A character string indicating in which acceleration vector to
#'   find the peaks. Can be "resultant", "vertical" or "all".
#' @param model A character string indicating which model to use to make
#'   the predictions. The values currently supported are "walking",
#'   "walking/running" and "jumping".
#'
#' @return An object of class \code{impactr_peaks} with the ground reaction
#'   force and/or loading rate peaks magnitude stored in the columns.
#'
#' @export
#'
#' @examples
#' data <- read_acc(impactr_example("hip-raw.csv"))
#' data <- specify_parameters(data, acc_placement = "hip", subj_body_mass = 78)
#' data <- find_peaks(data, vector = "vertical")
#' predict_loading(
#'   data,
#'   outcome = "grf",
#'   vector = "vertical",
#'   model = "walking/running"
#' )
predict_loading <- function(data, outcome, vector, model) {
  check_args_compute_loading(data, outcome, vector, model)
  if (outcome == "grf") {
    data <- predict_grf(data, vector, model)
  } else if (outcome == "lr") {
    data <- predict_lr(data, vector, model)
  } else if (outcome == "all") {
    impactr_peaks <- predict_grf(data, vector, model)
    lr <- predict_lr(data, vector, model)
    impactr_peaks <- tibble::add_column(
      impactr_peaks,
      lr[, grep("_lr$", names(lr))]
    )
    data <- impactr_peaks
  }
  check_output(data, vector)
}

predict_grf <- function(data, vector, model) {
  body_mass <- attributes(data)$subj_body_mass
  if (vector != "all") {
    coeff <- get_grf_coefficients(
      attributes(data)$acc_placement, vector, model
    )
    peaks <- data[[paste0(vector, "_peak_acc")]]
    data[[paste0(vector, "_peak_grf")]] <- compute_loading(
      coeff, peaks, body_mass
    )
  } else {
    coeff <- list(
      vertical = get_grf_coefficients(
        attributes(data)$acc_placement, "vertical", model
      ),
      resultant = get_grf_coefficients(
        attributes(data)$acc_placement, "resultant", model
      )
    )
    peaks <- list(
      vertical = data[["vertical_peak_acc"]],
      resultant = data[["resultant_peak_acc"]]
    )
    data[["vertical_peak_grf"]] <- compute_loading(
      coeff$vertical, peaks$vertical, body_mass
    )
    data[["resultant_peak_grf"]] <- compute_loading(
      coeff$resultant, peaks$resultant, body_mass
    )
  }
  data
}

predict_lr <- function(data, vector, model) {
  samp_freq <- attributes(data)$samp_freq
  body_mass <- attributes(data)$subj_body_mass
  if (vector != "all") {
    coeff <- get_lr_coefficients(
      attributes(data)$acc_placement, vector, model
    )
    start_idx <- if (is.list(attributes(data)$curve_start)) {
      attributes(data)$curve_start[[vector]]
    } else {
      attributes(data)$curve_start
    }
    peaks_idx <- if (is.list(attributes(data)$peaks_idx)) {
      attributes(data)$peaks_idx[[vector]]
    } else {
      attributes(data)$peaks_idx
    }
    acc_vector <- if (is.list(attributes(data)$acc_signal)) {
      attributes(data)$acc_signal[[vector]]
    } else {
      attributes(data)$acc_signal
    }

    peaks <- compute_peak_acc_rate(acc_vector, start_idx, peaks_idx, samp_freq)

    data[[paste0(vector, "_peak_lr")]] <- vector(
      "numeric", length(data[[paste0(vector, "_peak_acc")]])
    )
    NA_idx <- which(is.na(data[[paste0(vector, "_peak_acc")]]))
    nonNA_idx <- which(!is.na(data[[paste0(vector, "_peak_acc")]]))
    data[[paste0(vector, "_peak_lr")]][NA_idx] <- NA
    data[[paste0(vector, "_peak_lr")]][nonNA_idx] <- compute_loading(
      coeff, peaks, body_mass
    )
  } else {
    coeff <- list(
      vertical = get_lr_coefficients(
        attributes(data)$acc_placement, "vertical", model
      ),
      resultant = get_lr_coefficients(
        attributes(data)$acc_placement, "resultant", model
      )
    )
    peaks_idx <- list(
      vertical = attributes(data)$peaks_idx[["vertical"]],
      resultant = attributes(data)$peaks_idx[["resultant"]]
    )
    acc_vector <- list(
      vertical = attributes(data)$acc_signal[["vertical"]],
      resultant = attributes(data)$acc_signal[["resultant"]]
    )

    start_idx <- list(
      vertical = attributes(data)$curve_start$vertical,
      resultant = attributes(data)$curve_start$resultant
    )
    peaks <- list(
      vertical = compute_peak_acc_rate(
        acc_vector$vertical, start_idx$vertical, peaks_idx$vertical, samp_freq
      ),
      resultant = compute_peak_acc_rate(
        acc_vector$resultant, start_idx$resultant,
        peaks_idx$resultant, samp_freq
      )
    )

  data[["vertical_peak_lr"]] <- vector(
    "numeric", length(data[["vertical_peak_acc"]])
  )
  data[["vertical_peak_lr"]][which(is.na(data[["vertical_peak_acc"]]))] <- NA
  data[["vertical_peak_lr"]][which(!is.na(data[["vertical_peak_acc"]]))] <-
    compute_loading(coeff$vertical, peaks$vertical, body_mass)
  data[["resultant_peak_lr"]] <- vector(
    "numeric", length(data[["resultant_peak_acc"]])
  )
  data[["resultant_peak_lr"]][which(is.na(data[["resultant_peak_acc"]]))] <- NA
  data[["resultant_peak_lr"]][which(!is.na(data[["resultant_peak_acc"]]))] <-
    compute_loading(coeff$resultant, peaks$resultant, body_mass)
  }
  data
}

get_grf_coefficients <- function(acc_placement, vector, model) {
  if (model == "walking/running") {
    if (acc_placement == "ankle" & vector == "resultant") {
      list(b0 = 1026.046, b1 = - 153.073, b2 = 6.641, b3 = 2.097, b4 = 0)
    } else if (acc_placement == "back" & vector == "resultant") {
      list(b0 = 795.173, b1 = - 258.882, b2 = 5.951, b3 = 4.903, b4 = 0)
    } else if (acc_placement == "hip" & vector == "resultant") {
      list(b0 = 844.500, b1 = - 264.692, b2 = 4.677, b3 = 5.118, b4 = 0)
    } else if (acc_placement == "ankle" & vector == "vertical") {
      list(b0 = 1014.045, b1 = - 226.690, b2 = 4.854, b3 = 3.562, b4 = 0)
    } else if (acc_placement == "back" & vector == "vertical") {
      list(b0 = 890.428, b1 = - 339.790, b2 = 4.533, b3 = 6.131, b4 = 0)
    } else if (acc_placement == "hip" & vector == "vertical") {
      list(b0 = 908.037, b1 = - 322.843, b2 = 4.546, b3 = 5.691, b4 = 0)
     }
  } else if (model == "walking") {
    if (acc_placement == "ankle") {
      rlang::abort(
        glue::glue(
          "The `ankle` accelerometer placement is not supported in this \\
          model. Please choose between `back` or `hip` or change the model."
        )
      )
    } else if (acc_placement == "back" & vector == "resultant") {
      list(
        b0 = - 698.7031, b1 = 1047.5129, b2 = - 345.2605,
        b3 = 3.8294, b4 = 6.0219
      )
    } else if (acc_placement == "hip" & vector == "resultant") {
      list(
        b0 = - 300.9909, b1 = 522.6850, b2 = - 171.5606,
        b3 = 3.9596, b4 = 5.3671
      )
    } else if (acc_placement == "back" & vector == "vertical") {
      list(
        b0 = - 776.8934, b1 = 1042.9052, b2 = - 336.2115,
        b3 = 6.2132, b4 = 5.0805
      )
    } else if (acc_placement == "hip" & vector == "vertical") {
      list(
        b0 = - 435.7365, b1 = 586.6627, b2 = - 188.9689,
        b3 = 5.8047, b4 = 4.9544
      )
    }
  } else if (model == "jumping") {
    if (acc_placement == "ankle" & vector == "resultant") {
      list(b0 = 1551.020, b1 = - 132.384, b2 = 7.927, b3 = 2.415, b4 = 0)
    } else if (acc_placement == "back" & vector == "resultant") {
      list(b0 = - 350.125, b1 = 152.952, b2 = 22.618, b3 = 0.654, b4 = 0)
    } else if (acc_placement == "hip" & vector == "resultant") {
      list(b0 = - 493.877, b1 = 188.759, b2 = 18.008, b3 = 1.279, b4 = 0)
    } else if (acc_placement == "ankle" & vector == "vertical") {
      list(b0 = 1662.525, b1 = - 196.301, b2 = 8.515, b3 = 3.169, b4 = 0)
    } else if (acc_placement == "back" & vector == "vertical") {
      list(b0 = - 287.919, b1 = 131.396, b2 = 24.338, b3 = 0.642, b4 = 0)
    } else if (acc_placement == "hip" & vector == "vertical") {
      list(b0 = - 786.169, b1 = 177.403, b2 = 23.953, b3 = 1.355, b4 = 0)
    }
  }
}

get_lr_coefficients <- function(acc_placement, vector, model) {
  if (model == "walking/running") {
    if (acc_placement == "ankle" & vector == "resultant") {
      list(b0 = 6534.981, b1 = - 15.738, b2 = - 76.433, b3 = 4.258, b4 = 0)
    } else if (acc_placement == "back" & vector == "resultant") {
      list(b0 = 6155.636, b1 = - 81.779, b2 = - 5.500, b3 = 5.179, b4 = 0)
    } else if (acc_placement == "hip" & vector == "resultant") {
      list(b0 = 4431.800, b1 = - 33.175, b2 = 12.632, b3 = 4.014, b4 = 0)
    } else if (acc_placement == "ankle" & vector == "vertical") {
      list(b0 = 5124.478, b1 = - 47.525, b2 = 8.344, b3 = 2.588, b4 = 0)
    } else if (acc_placement == "back" & vector == "vertical") {
      list(b0 = 6605.822, b1 = - 112.779, b2 = - 3.767, b3 = 5.061, b4 = 0)
    } else if (acc_placement == "hip" & vector == "vertical") {
      list(b0 = 5343.980, b1 = - 89.984, b2 = 3.49, b3 = 4.808, b4 = 0)
    }
  } else if (model == "walking") {
    if (acc_placement == "ankle") {
      rlang::abort(
        glue::glue(
          "The `ankle` accelerometer placement is not supported in this \\
          model. Please choose between `back` or `hip` or change the model."
        )
      )
    } else if (acc_placement == "back" & vector == "resultant") {
      list(
        b0 = - 287.0209, b1 = 572.7967, b2 = - 9.8958,
        b3 = 18.1178, b4 = 3.4078
      )
    } else if (acc_placement == "hip" & vector == "resultant") {
      list(
        b0 = - 3510.410, b1 = 514.898, b2 = - 8.639,
        b3 = 51.937, b4 = 2.929
      )
    } else if (acc_placement == "back" & vector == "vertical") {
      list(
        b0 = - 324.0761, b1 = 552.8242, b2 = - 11.9453,
        b3 = 18.1405, b4 = 3.9586
      )
    } else if (acc_placement == "hip" & vector == "vertical") {
      list(
        b0 = - 2687.8662, b1 = 407.8434, b2 = - 7.6603,
        b3 = 45.8905, b4 = 3.8995
      )
    }
  } else if (model == "jumping") {
    if (acc_placement == "ankle" & vector == "resultant") {
      list(b0 = 71932.438, b1 = - 218.268, b2 = 74.463, b3 = 3.474, b4 = 0)
    } else if (acc_placement == "back" & vector == "resultant") {
      list(b0 = - 1161.976, b1 = 22.804, b2 = 624.413, b3 = 2.135, b4 = 0)
    } else if (acc_placement == "hip" & vector == "resultant") {
      list(b0 = 5118.300, b1 = 33.054, b2 = 346.667, b3 = 2.835, b4 = 0)
    } else if (acc_placement == "ankle" & vector == "vertical") {
      list(b0 = 58864.225, b1 = - 194.575, b2 = 142.545, b3 = 3.733, b4 = 0)
    } else if (acc_placement == "back" & vector == "vertical") {
      list(b0 = 8303.550, b1 = - 19.708, b2 = 685.299, b3 = 1.900, b4 = 0)
    } else if (acc_placement == "hip" & vector == "vertical") {
      list(b0 = - 11471.926, b1 = 15.332, b2 = 691.269, b3 = 2.670, b4 = 0)
    }
  }
}

#' @importFrom lvmisc %!in%
check_data <- function(data, vector) {
  if (
    vector == "vertical" &
    "vertical_peak_acc" %!in% names(data)
  ) {
    rlang::abort(
      glue::glue(
        "Column `vertical_peak_acc` is missing from `data`. Please, \\
        run find_peaks() with vector argument set as `vertical` or `all`."
      )
    )
  } else if (
    vector == "resultant" &
    "resultant_peak_acc" %!in% names(data)
  ) {
    rlang::abort(
      glue::glue(
        "Column `resultant_peak_acc` is missing from `data`. Please, \\
        run find_peaks() with vector argument set as `resultant` or `all`."
      )
    )
  } else if (
    vector == "all" &
    "vertical_peak_acc" %!in% names(data)
  ) {
    rlang::abort(
      glue::glue(
        "Column `vertical_peak_acc` is missing from `data`. Please, \\
        run find_peaks() with vector argument set as `all`."
      )
    )
  } else if (
    vector == "all" &
    "resultant_peak_acc" %!in% names(data)
  ) {
    rlang::abort(
      glue::glue(
        "Column `resultant_peak_acc` is missing from `data`. Please, \\
        run find_peaks() with vector argument set as `all`."
      )
    )
  }
}

check_output <- function(data, vector) {
  if (
    vector == "vertical" &
    any(grepl("resultant_", names(data)))
  ) {
    data <- data[, -which(grepl("resultant_", names(data)))]
    rlang::warn(
      glue::glue(
        "Columns referring to the resultant vector were removed as \\
        predict_loading() vector argument was set to `vertical`."
      )
    )
  } else if (
    vector == "resultant" &
    any(grepl("vertical_", names(data)))
  ) {
    data <- data[, -which(grepl("vertical_", names(data)))]
    rlang::warn(
      glue::glue(
        "Columns referring to the vertical vector were removed as \\
        predict_loading() vector argument was set to `resultant`."
      )
    )
  } else {
    data <- data
  }
  data[rowSums(is.na(data[, -1])) != ncol(data[, -1]), ]
}

#' @importFrom lvmisc %!in%
check_args_compute_loading <- function(data, outcome, vector, model) {
  acc_placement <- attributes(data)$acc_placement
  if (is.na(acc_placement)) {
    rlang::abort(
      glue::glue(
        "No accelerometer placement was informed. Please, use \\
        specify_parameters() to define it."
      )
    )
  }

  subj_body_mass <- attributes(data)$subj_body_mass
  if (is.na(subj_body_mass)) {
    rlang::abort(
      glue::glue(
        "No subject body mass was informed. Please, use \\
        specify_parameters() to define it."
      )
    )
  }

  valid_outcome <- c("grf", "lr", "all")
  if (outcome %!in% valid_outcome) {
    lvmisc::abort_argument_value("outcome", valid_outcome)
  }

  valid_model <- c("walking/running", "walking", "jumping")
  if (model %!in% valid_model) {
    lvmisc::abort_argument_value("model", valid_model)
  }

  valid_vector <- c("vertical", "resultant", "all")
  if (vector %!in% valid_vector) {
    lvmisc::abort_argument_value("vector", valid_vector)
  }

  check_data(data, vector)
}
