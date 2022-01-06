summarise_peaks <- function(data, variable, vector, ranges = NULL) {

  data$date <- as.Date(data$timestamp)
  date <- unique(data$date)

  if (variable[1] == "all") {
    variable <- paste0("_peak_", c("acc", "grf", "lr"))
  } else {
    variable <- paste0("_peak_", variable)
  }

  if (vector == "all") {
    variable_v <- paste0("vertical", variable)
    variable_r <- paste0("resultant", variable)
    variable <- c(variable_v, variable_r)
    summary <- purrr::map(variable, ~ summarise_aux(data, date, .x))
  } else {
    if (length(variable) > 1) {
      variable <- paste0(vector, variable)
      summary <- purrr::map(variable, ~ summarise_aux(data, date, .x))
    } else {
      variable <- paste0(vector, variable)
      summary <- summarise_aux(data, date, variable)
    }
  }
  element_names <- get_element_names(summary)
  summary <- rlang::set_names(summary, element_names)

  if (is.null(ranges)) {
    return(summary)
  } else {
    if (vector == "all") {
      purrr::map2(
        variable, summary,
        ~ summarise_by_range(data, date, .x, ranges, .y)
      )
    } else {
      summarise_by_range(data, date, variable, ranges, summary)
    }
  }

}

summarise_aux <- function(data, date, variable) {

  weekday <- weekdays(date)
  measurement_day <- seq_len(length(weekday))

  peaks_per_day <- purrr::map(
    date,
    ~ data[
      which(data[["date"]] == .x & !is.na(data[[variable]])),
      variable, drop = TRUE
    ]
  )
  n_peaks <- purrr::map_dbl(peaks_per_day, length)
  min_peaks <- round(purrr::map_dbl(peaks_per_day, min), 2)
  max_peaks <- round(purrr::map_dbl(peaks_per_day, max), 2)
  mean_peaks <- round(purrr::map_dbl(peaks_per_day, mean), 2)
  sd_peaks <- round(purrr::map_dbl(peaks_per_day, stats::sd), 2)

  summary <- data.frame(
    filename = attributes(data)$filename,
    date, weekday, measurement_day,
    variable, n_peaks,
    min_peaks, max_peaks, mean_peaks, sd_peaks
  )

}

summarise_by_range <- function(data, date, variable, ranges, summary) {

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
      ~ summarise_by_range_aux(data, variable, dates[.x], min[.x], max[.x])
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

summarise_by_range_aux <- function(data, variable, date, min, max) {

  length(
    which(
      data[["date"]] == date &
      data[[variable]] >= min &
      data[[variable]] < max
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

get_element_names <- function(summary) {

  n <- purrr::map_chr(summary, ~ .x[1, "variable"])
  n <- stringr::str_replace_all(n, "_", " ")
  n <- paste0(toupper(substr(n, 1, 1)), substr(n, 2, nchar(n)))
  l <- purrr::map_dbl(stringr::str_locate_all(n, " "), ~ .x[2, "start"])
  paste0(substr(n, 1, l), toupper(substr(n, l, nchar(n))))

}
