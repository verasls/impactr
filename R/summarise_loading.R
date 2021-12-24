summarise_acc_peaks <- function(data,
                                vector,
                                ranges = c(1, 2, 3, 4, 5)) {

  data$date <- as.Date(data$timestamp)
  h <- 3600 * attributes(data)$samp_freq

  if (vector %in% c("vertical", "resultant")) {
    variable <- paste0(vector, "_peak_acc")
    peaks <- data[[variable]]
  }

  date <- unique(data$date)
  weekday <- weekdays(date)
  measurement_day <- seq_len(length(weekday))
  n_peaks <- purrr::map_dbl(
    date, ~ length(which(data$date == .x & !is.na(peaks)))
  )

  data.frame(
    filename = attributes(data)$filename,
    date, weekday, measurement_day,
    variable, n_peaks
  )

}
