predict_grf <- function(data, vector) {
  coeff <- get_coefficients("grf", attributes(data)$acc_placement, vector)
  peaks <- attributes(data)$peaks[[vector]]$height
  body_mass <- attributes(data)$subj_body_mass
  compute_grf(coeff, peaks, body_mass)
}

get_coefficients <- function(outcome, placement, vector) {
  if (outcome == "grf") {
    if (placement == "hip" & vector == "resultant") {
      list(b0 = 844.500, b1 = - 264.692, b2 = 4.677, b3 = 5.118)
    }
  }
}
