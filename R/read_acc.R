get_metadata <- function(file) {
  header <- read_header(file)
  samp_freq <- get_samp_freq(header)
  start_date_time <- get_start_date_time(header)
  list(
    samp_freq = samp_freq,
    start_date_time = start_date_time
  )
}

get_samp_freq <- function(header) {
  samp_freq <- unlist(strsplit(header[, 1], " "))
  row_num <- which(samp_freq == "Hz") - 1
  as.numeric(samp_freq[row_num])
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
  read.csv(file, nrows = 10, header = FALSE)
}
