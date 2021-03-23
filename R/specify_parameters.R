#' Specify prediction model parameters
#'
#' Specify the accelerometer placement used and the subject body mass. These
#' data is needed in order to use the mechanical loading prediction models.
#'
#' @param data An \code{impactr_data} object, as obtained with
#'   \link[=read_acc]{read_acc()}.
#' @param acc_placement A character string indicating the accelerometer
#'   placement. Can be either "ankle", "back", or "hip".
#' @param subj_body_mass A double scalar indicating the subject body mass
#'   in kilograms.
#'
#' @return An object of class \code{impactr_data} with the specified parameters
#'   as attributes.
#'
#' @export
#'
#' @examples
#' data <- read_acc(impactr_example("hip-imu.csv"))
#' specify_parameters(data, acc_placement = "hip", subj_body_mass = 79.2)
specify_parameters <- function(data, acc_placement, subj_body_mass) {
  check_args_specify_parameters(acc_placement, subj_body_mass)

  acc_placement <- get_acc_placement(acc_placement)
  attributes(data)$acc_placement <- acc_placement
  attributes(data)$subj_body_mass <- subj_body_mass
  data
}

get_acc_placement <- function(acc_placement) {
  if (grepl("ankle", acc_placement, ignore.case = TRUE)) {
    "ankle"
  } else if (grepl("back", acc_placement, ignore.case = TRUE)) {
    "back"
  } else if (grepl("hip", acc_placement, ignore.case = TRUE)) {
    "hip"
  } else {
    acc_placement
  }
}

#' @importFrom lvmisc %!in%
check_args_specify_parameters <- function(acc_placement, subj_body_mass) {
  if (!is.character(acc_placement)) {
    lvmisc::abort_argument_type(
      "acc_placement", must = "be character", not = acc_placement
    )
  }
  if (!is.numeric(subj_body_mass)) {
    lvmisc::abort_argument_type(
      "subj_body_mass", must = "be numeric", not = subj_body_mass
    )
  }
  acc_placement <- get_acc_placement(acc_placement)
  valid_values <- c("ankle", "back", "hip")
  if (acc_placement %!in% valid_values) {
    lvmisc::abort_argument_value("acc_placement", valid_values)
  }
}
