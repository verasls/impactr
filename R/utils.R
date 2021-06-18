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
  row.names(data) <- NULL
  data
}

get_acc_placement <- function(acc_placement) {
  if (grepl("\\bankle\\b", acc_placement, ignore.case = TRUE)) {
    "ankle"
  } else if (grepl("\\bback\\b", acc_placement, ignore.case = TRUE)) {
    "back"
  } else if (grepl("\\bhip\\b", acc_placement, ignore.case = TRUE)) {
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

#' Define region of interest
#'
#' Define the region of interest for data analysis based on the accelerometer
#' data timestamp.
#'
#' @param data An \code{impactr_data} object, as obtained with
#'   \link[=read_acc]{read_acc()}.
#' @param start,end A character string with the start and end times of the
#'   region of interest in the "HH:MM:SS" format.
#'
#' @return An object of class \code{impactr_data}.
#'
#' @export
#'
#' @examples
#' data <- read_acc(impactr_example("hip-imu.csv"))
#' define_region(data, start = "15:45:00", end = "15:46:00")
define_region <- function(data, start, end) {
  check_args_define_region(data, start, end)

  samp_freq <- attributes(data)$samp_freq
  start_date_time <- attributes(data)$start_date_time
  start_date <- as.Date(start_date_time)
  tz <- format(start_date_time, format = "%Z")
  start_roi <- as.POSIXct(paste(start_date, start), tz = tz)
  end_roi <- as.POSIXct(paste(start_date, end), tz = tz)

  duration_start <- lubridate::interval(start_date_time, start_roi)
  duration_start <- lubridate::as.duration(duration_start)
  n_sec_start <- as.numeric(duration_start, "seconds")
  n_rows_start <- n_sec_start * samp_freq
  start_idx <- n_rows_start + 1

  duration_end <- lubridate::interval(start_roi, end_roi)
  duration_end <- lubridate::as.duration(duration_end)
  n_sec_end <- as.numeric(duration_end, "seconds")
  n_rows_end <- n_sec_end * samp_freq
  end_idx <- start_idx + n_rows_end - 1

  data[start_idx:end_idx, ]
}

check_args_define_region <- function(data, start, end) {
  if (!is.character(start)) {
    lvmisc::abort_argument_type("start", must = "be character", not = start)
  }
  if (!is.character(end)) {
    lvmisc::abort_argument_type("end", must = "be character", not = end)
  }
  if (!grepl("^(?:2[0-3]|[01][0-9]):[0-5][0-9]:[0-5][0-9]$", start)) {
    rlang::abort("`start` must be in the `HH:MM:SS` format.")
  }
  if (!grepl("^(?:2[0-3]|[01][0-9]):[0-5][0-9]:[0-5][0-9]$", end)) {
    rlang::abort("`end` must be in the `HH:MM:SS` format.")
  }

  start_date_time <- attributes(data)$start_date_time
  start_date <- as.Date(start_date_time)
  tz <- format(start_date_time, format = "%Z")
  start_roi <- as.POSIXct(paste(start_date, start), tz = tz)
  if (start_roi < start_date_time) {
    rlang::abort("`start` must not be before `data` Start time")
  }

  samp_freq <- attributes(data)$samp_freq
  end_date_time <- start_date_time + lubridate::seconds(nrow(data) / samp_freq)
  end_roi <- as.POSIXct(paste(start_date, end), tz = tz)
  if (end_roi > end_date_time) {
    rlang::abort("`end` must not be after the last `data` timestamp")
  }

  start_time_arg <- as.POSIXct(start, format = "%H:%M:%S")
  end_time_arg <- as.POSIXct(end, format = "%H:%M:%S")
  if (end_time_arg < start_time_arg) {
    rlang::abort("`end` must not be before `start`.")
  }
}

#' Use resultant vector
#'
#' Computes the acceleration resultant vector.
#'
#' @param data An \code{impactr_data} object, as obtained with
#'
#' @return An object of class \code{impactr_data} with the \code{acc_R} column
#'   containing the acceleration resultant vector.
#'
#' @export
#'
#' @examples
#' data <- read_acc(impactr_example("hip-imu.csv"))
#' use_resultant(data)
use_resultant <- function(data) {
  data$acc_R <- compute_resultant(data$acc_X, data$acc_Y, data$acc_Z)
  data
}
