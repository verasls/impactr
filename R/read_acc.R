#' Read raw accelerometer data
#'
#' Reads raw accelerometer data files into an \code{impactr_data} object.
#'
#' @param file Path to a raw accelerometer data file.
#'
#' @return An object of class \code{impactr_data}.
#'
#' @export
#'
#' @examples
#' read_acc(impactr_example("hip-raw.csv"))
read_acc <- function(file) {
  metadata <- get_metadata(file)
  x <- vroom::vroom(
    file, skip = 10,
    col_select = c(
      acc_X = "Accelerometer X",
      acc_Y = "Accelerometer Y",
      acc_Z = "Accelerometer Z"
    ),
    col_types = c(
      "Accelerometer X" = "d",
      "Accelerometer Y" = "d",
      "Accelerometer Z" = "d"
    )
  )
  x <- make_timestamp(x, metadata)

  new_impactr_data(
    x,
    start_date_time = metadata$start_date_time,
    samp_freq = metadata$samp_freq,
    acc_placement = NA,
    subj_body_mass = NA,
    filter_type = NA
  )
}

get_metadata <- function(file) {
  check_metadata(file)
  header <- read_header(file)
  samp_freq <- get_samp_freq(header)
  start_date_time <- get_start_date_time(header)
  list(
    samp_freq = samp_freq,
    start_date_time = start_date_time
  )
}

check_metadata <- function(file) {
  header <- read_header(file)
  if (!has_header(header)) {
    rlang::abort(
      glue::glue(
        "No header detected in the file `{file}`. \\
        Please, provide the entire file."
      )
    )
  }
  if (!is_actigraph(header)) {
    rlang::abort(
      glue::glue(
        "The file `{file}` is not from an ActiGraph accelerometer. \\
        `impactr` currently only supports ActiGraph accelerometer \\
        data files."
      )
    )
  }
  if (!is_raw_data(header)) {
    rlang::abort(
      glue::glue(
        "The file `{file}` is not a raw data file. `impactr` needs the raw \\
        data to work."
      )
    )
  }
}

has_header <- function(header) {
 if (any(grepl("\\bActiGraph\\b", header[1, ]))) {
    TRUE
 } else if (any(grepl("\\bGENEActiv\\b", header[1, ]))) {
    TRUE
 } else {
    FALSE
 }
}

is_actigraph <- function(header) {
  any(grepl("\\bActiGraph\\b", header[1, ]))
}

is_raw_data <- function(header) {
  epoch_row <- grep("Epoch", header[, 1])
  epoch <- substr(header[epoch_row, 1], 25, 32)
  epoch == "00:00:00"
}

get_samp_freq <- function(header) {
  header_vector <- unlist(strsplit(header[, 1], " "))
  samp_freq_row <- which(header_vector == "Hz") - 1
  as.numeric(header_vector[samp_freq_row])
}

get_start_date_time <- function(header) {
  start_date_row <- grep("start date", header[, 1], ignore.case = TRUE)
  start_date <- gsub("[[:alpha:]]", "", header[start_date_row, 1])

  start_time_row <- grep("start time", header[, 1], ignore.case = TRUE)
  start_time <- gsub("[[:alpha:]]", "", header[start_time_row, 1])

  start_date_time <- paste(start_date, start_time)

  parse_format <- get_parse_format(header)
  lubridate::parse_date_time(start_date_time, parse_format)
}

get_parse_format <- function(header) {
  header_vector <- unlist(strsplit(header[, 1], " "))
  date_format_row <- which(header_vector == "format") + 1
  date_format <- header_vector[date_format_row]

  if (grepl("-", date_format)) {
    sep_char <- "-"
  } else if (grepl("/", date_format)) {
    sep_char <- "/"
  }
  date_format_vector <- unlist(strsplit(date_format, sep_char))

  date_format <- unclass(
    glue::glue_collapse(purrr::map_chr(date_format_vector, sub_format), "/")
  )
  time_format <- "%H:%M:%S"
  paste0(date_format, time_format)
}

sub_format <- function(date_format) {
  if (grepl("d", date_format, ignore.case = TRUE)) {
    "%d"
  } else if (grepl("m", date_format, ignore.case = TRUE)) {
    "%m"
  } else if (grep("y", date_format, ignore.case = TRUE)) {
    "%Y"
  }
}

make_timestamp <- function(x, metadata) {
  start <- metadata$start_date_time
  n_secs <- nrow(x) / metadata$samp_freq
  period <- 1 / metadata$samp_freq
  end <- start + as.difftime(n_secs, units = "secs")

  timestamp <- utils::head(seq(from = start, to = end, by = period), - 1)
  cbind(timestamp, x)
}

read_header <- function(file) {
  utils::read.csv(file, nrows = 10, header = FALSE)
}
