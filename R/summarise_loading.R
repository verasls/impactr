summarise_acc_peaks <- function(data,
                                vector,
                                ranges = c(1, 2, 3, 4, 5, Inf)) {

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

  summary <- data.frame(
    filename = attributes(data)$filename,
    date, weekday, measurement_day,
    variable, n_peaks
  )

  if (is.null(ranges)) {

    return(summary)

  } else {

    flag <- "none"
    if (ranges[1] != 1) {
      ranges <- c(1, ranges)
      flag <- "1added"
    }
    if (!is.infinite(ranges[length(ranges)])) {
      ranges <- c(ranges, Inf)
    }

    dates <- rep(date, each = length(ranges) - 1)
    min <- rep(ranges[-length(ranges)], length(date))
    max <- rep(ranges[-1], length(date))

    p <- purrr::map_dbl(
      seq_len(length(dates)),
      ~ summarise_by_range(data, dates[.x], min[.x], max[.x])
    )
    p <- as.data.frame(
      matrix(p, nrow = length(date), ncol = length(ranges) - 1, byrow = TRUE)
    )

    min <- min[1:(length(ranges) - 1)]
    max <- max[1:(length(ranges) - 1)]

    p_colnames <- purrr::map_chr(
      seq_len(length(ranges) - 1),
      ~ make_colnames("acc", min[.x], max[.x], flag)
    )

    colnames(p) <- p_colnames

    summary <- cbind(summary, p)

    return(summary)

  }

}

summarise_by_range <- function(data, date, min, max) {
  length(
    which(
      data$date == date &
      data$resultant_peak_acc >= min &
      data$resultant_peak_acc < max
    )
  )
}

make_colnames <- function(outcome, min, max, flag) {
  if (outcome == "acc") {
    unit <- "g"
  }

  if (min == 1 & flag == "1added") {
    paste0("n_peaks_up_to_", max, "_", unit)
  } else if (is.infinite(max)) {
    paste0("n_peaks_above_", min, "_", unit)
  } else {
    paste0("n_peaks_", min, "_to_", max, "_", unit)
  }
}
