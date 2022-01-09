summarise_loading <- function(data,
                              variable,
                              vector,
                              daily_average = TRUE,
                              ranges_acc = NULL,
                              ranges_grf = NULL,
                              ranges_lr = NULL,
                              save_summary = FALSE) {

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
    summary <- purrr::map(variable, ~ summarise_loading_aux(data, date, .x))
  } else {
    if (length(variable) > 1) {
      variable <- paste0(vector, variable)
      summary <- purrr::map(variable, ~ summarise_loading_aux(data, date, .x))
    } else {
      variable <- paste0(vector, variable)
      summary <- summarise_loading_aux(data, date, variable)
    }
  }
  if (!is.data.frame(summary)) {
    element_names <- get_element_names(summary)
    summary <- rlang::set_names(summary, element_names)
  }

  if (!is.null(ranges_acc)) {
    summary <- summarise_by_range(
      data, date, variable, "acc", ranges_acc, summary
    )
  }
  if (!is.null(ranges_grf)) {
    summary <- summarise_by_range(
      data, date, variable, "grf", ranges_grf, summary
    )
  }
  if (!is.null(ranges_lr)) {
    summary <- summarise_by_range(
      data, date, variable, "lr", ranges_lr, summary
    )
  }

  if (is.data.frame(summary)) {
    summary <- tibble::as_tibble(summary)
  } else {
    summary <- purrr::map(summary, tibble::as_tibble)
  }

  if (isTRUE(daily_average)) {
    if (is.data.frame(summary)) {
      average_summary <- get_daily_average(summary)
    } else {
      average_summary <- purrr::map(summary, get_daily_average)
    }
    summary <- list(
      `Summary per day` = summary, `Daily average` = average_summary
    )
  }

  if (is.character(save_summary)) {
    filename <- get_filename(summary, save_summary)

    if (is.data.frame(summary)) {
      save_loading_summary(data, summary, filename)
    } else {
      if (length(summary) != length(filename)) {
        summary <- c(summary[["Summary per day"]], summary[["Daily average"]])
      }
      purrr::walk2(summary, filename, ~ save_loading_summary(data, .x, .y))
    }

  }

  summary

}

summarise_loading_aux <- function(data, date, variable) {

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

summarise_by_range <- function(data,
                               date,
                               variable,
                               variable_chr,
                               ranges,
                               summary) {

  if (is.data.frame(summary)) {
    summary <- summarise_by_range_aux(data, date, variable, ranges, summary)
  } else {
    i <- which(grepl(variable_chr, variable))
    summary[i] <- purrr::map2(
      variable[which(grepl(variable_chr, variable))], summary[i],
      ~ summarise_by_range_aux(data, date, .x, ranges, .y)
    )
  }
  summary

}

summarise_by_range_aux <- function(data, date, variable, ranges, summary) {

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
      ~ get_range_summary(data, variable, dates[.x], min[.x], max[.x])
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

get_range_summary <- function(data, variable, date, min, max) {

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
  paste0(substr(n, 1, l), toupper(substr(n, l + 1, nchar(n))))

}

get_daily_average <- function(summary) {

  num_vars <- names(summary)[6:ncol(summary)]
  num_vals <- round(
    purrr::map_dbl(num_vars, ~ mean(summary[[.x]], na.rm = TRUE)), 2
  )
  vals <- c(
    summary[[1, 1]], summary[[1, 5]], num_vals
  )
  daily_average <- data.frame(t(vals))
  daily_average <- tibble::as_tibble(daily_average)
  colnames(daily_average) <- names(summary)[c(1, 5:ncol(summary))]
  daily_average

}

get_filename <- function(summary, save_summary) {

  if (!grepl("/$", save_summary)) {
    save_summary <- paste0(save_summary, "/")
  }

  if (is.data.frame(summary)) {
    filename <- paste0(
      save_summary,
      summary[1, "variable", drop = TRUE], ".csv"
    )
  } else if ("Summary per day" %in% names(summary)) {
    per_day <- summary[["Summary per day"]]
    average <- summary[["Daily average"]]
    if (is.data.frame(per_day) & is.data.frame(average)) {
      per_day <- per_day[1, "variable", drop = TRUE]
      average <- average[1, "variable", drop = TRUE]
      filename <- paste0(
        save_summary,
        c(per_day, paste0("daily_average_", average)), ".csv"
      )
    } else {
      per_day <- tolower(stringr::str_replace_all(names(per_day), " ", "_"))
      average <- tolower(stringr::str_replace_all(names(average), " ", "_"))
      filename <- paste0(
        save_summary,
        c(per_day, paste0("daily_average_", average)), ".csv"
      )
    }
  } else {
    filename <- paste0(
      save_summary,
      tolower(stringr::str_replace_all(names(summary), " ", "_")), ".csv"
    )
  }
  filename

}

save_loading_summary <- function(data, summary, filename) {

  if (file.exists(filename)) {
    pre_summary <- utils::read.csv(filename)
    if (length(unique(pre_summary$filename)) == 1) {
      if (unique(pre_summary$filename) == attributes(data)$filename) {
        file.remove(filename)
      }
    }
  }
  use_colnames <- ifelse(file.exists(filename), FALSE, TRUE)
  suppressWarnings(
    utils::write.table(
      summary, file = filename, sep = ",", append = TRUE,
      row.names = FALSE, col.names = use_colnames
    )
  )

}
