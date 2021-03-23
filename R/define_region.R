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
#' define_region(data, start = "15:08:00", end = "15:45:00")
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
