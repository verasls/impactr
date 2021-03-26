#' Constructor for impactr_data object
#'
#' @param x A data frame.
#' @param start_date_time A scalar of class \code{POSIXct}.
#' @param samp_freq A numerical scalar
#' @param acc_placement A character scalar
#' @param subj_body_mass A numerical scalar
#' @param filter_type A character scalar
#'
#' @export
#' @keywords internal
new_impactr_data <- function(x,
                             start_date_time,
                             samp_freq,
                             acc_placement,
                             subj_body_mass,
                             filter_type,
                             peaks) {
  stopifnot(is.data.frame(x))
  n_row <- nrow(x)
  tibble::new_tibble(
    x,
    start_date_time = start_date_time,
    samp_freq = samp_freq,
    acc_placement = acc_placement,
    subj_body_mass = subj_body_mass,
    filter_type = filter_type,
    peaks = peaks,
    nrow = n_row,
    class = "impactr_data"
  )
}

#' @importFrom tibble tbl_sum
#' @importFrom pillar dim_desc
#' @export
#' @keywords internal
tbl_sum.impactr_data <- function(x) {
  c(
    "Start time" = as.character(attributes(x)$start_date_time),
    "Sampling frequency" = paste0(attributes(x)$samp_freq, "Hz"),
    "Accelerometer placement" = ifelse(
      is.na(attributes(x)$acc_placement),
      "Non-specified",
      stringr::str_to_sentence(attributes(x)$acc_placement)
    ),
    "Subject body mass" = ifelse(
      is.na(attributes(x)$subj_body_mass),
      "Non-specified",
      paste0(as.character(attributes(x)$subj_body_mass), "kg")
    ),
    "Filter" = ifelse(
      is.na(attributes(x)$filter_type),
      "No filter applied",
      attributes(x)$filter_type
    ),
    "Peaks" = ifelse(
      all(is.na(attributes(x)$peaks)),
      "Not detected",
      ifelse(
        length(attributes(x)$peaks) == 1,
        paste0(
          formatC(length(attributes(x)$peaks[[1]][["idx"]]), big.mark = ","),
          " peaks (", names(attributes(x)$peaks), ")"
        ),
        paste0(
          formatC(length(attributes(x)$peaks[[1]][["idx"]]), big.mark = ","),
          " peaks (", names(attributes(x)$peaks)[[1]], ") / ",
          formatC(length(attributes(x)$peaks[[2]][["idx"]]), big.mark = ","),
          " peaks (", names(attributes(x)$peaks)[[2]], ")"
        )
      )
    ),
    "Data dimensions" = dim_desc(x)
  )
}
