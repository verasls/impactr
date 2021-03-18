#' Constructor for impactr_data object
#'
#' @param x A data frame.
#' @param start_date_time A scalar of class \code{POSIXct}.
#' @param samp_freq A numerical scalar
#' @param acc_placement A character scalar
#' @param subj_body_mass A numerical scalar
#' @param filter_type A character scalar
#'
#' @keywords internal
new_impactr_data <- function(x,
                             start_date_time,
                             samp_freq,
                             acc_placement,
                             subj_body_mass,
                             filter_type) {
  stopifnot(is.data.frame(x))
  n_row <- nrow(x)
  tibble::new_tibble(
    x,
    start_date_time = start_date_time,
    samp_freq = samp_freq,
    acc_placement = acc_placement,
    subj_body_mass = subj_body_mass,
    filter_type = filter_type,
    nrow = n_row,
    class = "impactr_data"
  )
}

#' @importFrom tibble tbl_sum
#' @importFrom pillar dim_desc
tbl_sum.impactr_data <- function(x) {
  cat("# Raw accelerometer data\n")
  c(
    "Start time" = as.character(attributes(x)$start_date_time),
    "Sampling frequency" = paste0(attributes(x)$samp_freq, "Hz"),
    "Accelerometer placement" = ifelse(
      is.na(attributes(x)$acc_placement),
      "Non-specified",
      attributes(x)$acc_placement
    ),
    "Subject body mass" = ifelse(
      is.na(attributes(x)$subj_body_mass),
      "Non-specified",
      as.character(attributes(x)$subj_body_mass)
    ),
    "Filter" = ifelse(
      is.na(attributes(x)$filter_type),
      "No filter applied",
      attributes(x)$filter_type
    ),
    "Data dimensions" = dim_desc(x)
  )
}
