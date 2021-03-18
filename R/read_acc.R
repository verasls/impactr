read_acc <- function(file) {
  metadata <- get_metadata(file)
  x <- vroom::vroom(
    file, skip = 10,
    col_select = c(
      timestamp = "Timestamp",
      acc_X = "Accelerometer X",
      acc_Y = "Accelerometer Y",
      acc_Z = "Accelerometer Z"
    ),
    col_types = c(
      "Timestamp" = "T",
      "Accelerometer X" = "d",
      "Accelerometer Y" = "d",
      "Accelerometer Z" = "d"
    )
  )
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
  if (ncol(header) == 1) {
    TRUE
  } else if (all(is.na(header[, 2:ncol(header)]))) {
    TRUE
  }
}

is_actigraph <- function(header) {
  grepl("ActiGraph", header[1, 1])
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
  lubridate::parse_date_time(start_date_time, "%m/%d/%Y%H:%M:%S")
}

read_header <- function(file) {
  utils::read.csv(file, nrows = 10, header = FALSE)
}
