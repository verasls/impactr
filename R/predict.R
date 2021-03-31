predict_loading <- function(data, outcome, vector, equation) {
  if (grepl("grf", outcome)) {
    predict_grf(data, vector, equation)
  } else if (grepl("lr", outcome)) {
    predict_lr(data, vector, equation)
  } else if (grepl("both", outcome)) {
    list(
      grf = predict_grf(data, vector, equation),
      lr = predict_lr(data, vector, equation)
    )
  }
}

predict_grf <- function(data, vector, equation) {
  coeff <- get_grf_coefficients(
    attributes(data)$acc_placement, vector, equation
  )
  body_mass <- attributes(data)$subj_body_mass
  peaks <- attributes(data)$peaks[[vector]]$height
  compute_loading(coeff, peaks, body_mass)
}

predict_lr <- function(data, vector, equation) {
  samp_freq <- attributes(data)$samp_freq
  coeff <- get_lr_coefficients(attributes(data)$acc_placement, vector, equation)
  body_mass <- attributes(data)$subj_body_mass
  peaks_idx <- attributes(data)$peaks[[vector]]$idx

  if (grepl("resultant", vector)) {
    acc_vector <- as.numeric(data[["acc_R"]])
  } else if (grepl("vertical", vector)) {
    acc_vector <- as.numeric(data[["acc_Y"]]) * -1
  }
  start_idx <- get_curve_start(acc_vector, peaks_idx)
  peaks <- compute_peak_acc_rate(acc_vector, start_idx, peaks_idx, samp_freq)
  compute_loading(coeff, peaks, body_mass)
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
