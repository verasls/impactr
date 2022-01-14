#' Detect and remove accelerometer non-wear time
#'
#' Detects the accelerometer non-wear time based on an algorithm developed
#' by van Hees (see Details) and remove these periods from the raw data. This
#' function can also draw a plot to better visualize the detected non-wear
#' periods and generate a wear time daily summary.
#'
#' @param data An \code{impactr_data} object, as obtained with
#'   \link[=read_acc]{read_acc()}.
#' @param window1,window2 Windows size, in minutes, for the non-wear
#'   detection algorithm. Defaults to 60 and 15 minutes, respectively.
#'   Also, \code{window2} must be smaller than \code{window1}, and
#'   \code{window1} must be a multiple of \code{window2}.
#' @param threshold Number of axes that need to meet the non-wear criteria.
#'   Defaults to 2.
#' @param min_hour_crit The minimum number of hours marked as wear time
#'   in a day for it to be considered valid (see Data validation).
#'   Defaults to 0, meaning that every day is considered valid.
#' @param min_day_crit The minimum number of valid days for the data of
#'   a given subject to be considered valid (see Data validation).
#'   Defaults to 0, meaning that all data is valid.
#' @param plot A logical value indicating whether or not to display the
#'   plot to visualize the detected non-wear periods. Defaults to \code{FALSE}.
#'   Notice that the plot will only be displayed in your R session if you do
#'   not provide a path to save the plot (see the argument \code{save_plot}).
#' @param save_plot,save_summary Indicates whether of not to save the plot to
#'   visualize the detected non-wear periods to a pdf file and the wear time
#'   daily summary to a csv file, respectively. Defaults to \code{FALSE}.
#'   Provide a valid path to a file, ending with the ".pdf" extension for the
#'   plot or with the ".csv" extension to the summary, as a character string
#'   if you want the outputs to be saved.
#'
#' @return An object of class \code{impactr_data} and a plot if
#'   \code{plot = TRUE} and \code{save_plot = FALSE}.
#'
#' @section The non-wear detection algorithm:
#'
#'   The current version of this algorithm is described in a paper by van Hees
#'   et al (see References) and also in this
#'   \href{https://CRAN.R-project.org/package=GGIR/vignettes/GGIR.html#53_Non-wear_detection}{vignette} from package GGIR.
#'   Briefly, in a first stage it identifies non-wear time based on threshold
#'   values of standard deviation (0.013\emph{g}) and range (0.050\emph{g}) of
#'   raw acceleration from each axis. The classification is done per blocks of
#'   \code{window2} size (default 15 minutes) based on the characteristics of
#'   a larger \code{window1} (default 60 minutes) centred at the
#'   \code{window2}. In the second stage of the algorithm, the plausibility of
#'   wear periods in between non-wear periods is tested based on the duration
#'   and proportion of the duration relative to the surrounding non-wear
#'   periods.
#'
#' @section Data validation:
#'
#'   After the detection of non-wear periods through the algorithm, a data
#'   validation step is applied. For each measurement day to be considered
#'   valid, it has to present a minimum number of wear time hours determined
#'   by the \code{min_hour_crit} argument. If the number of wear time hours of
#'   a given day falls below the threshold, the whole day is considered invalid
#'   and is then removed from the subsequent analyses. The whole measurement
#'   is also classified as valid or invalid based on the number of valid days
#'   and a threshold given by \code{min_day_crit}. If the number of valid days
#'   is less than the value determined by the \code{min_day_crit} argument,
#'   the whole data is deleted and the \code{remove_nonwear()} function
#'   signals an error, stopping its execution. Nevertheless, this error does
#'   not prevent the plot to be displayed or saved, or the wear time daily
#'   summary to be saved, if the arguments are set to do so.
#'
#' @references \itemize{
#'   \item van Hees VT, Gorzelniak L, Dean León EC, Eder M, Pias M, Taherian S,
#'   Ekelund U, Renström F, Franks PW, Horsch A, Brage S. Separating movement
#'   and gravity components in an acceleration signal and implications for the
#'   assessment of human daily physical activity. PLoS One. 2013. Apr 23.
#'   \doi{https://doi.org/10.1371/journal.pone.0061691}.
#'  }
#'
#' @export
#'
#' @examples
#' # Ensure that {accdata} package is available before running the example.
#' # If it is not, run install_accdata() to install the required package.
#' if (requireNamespace("accdata", quietly = TRUE)) {
#'   data <- import_dataset("daily_acc_3d")
#'   remove_nonwear(data)
#' }
remove_nonwear <- function(data,
                           window1 = 60,
                           window2 = 15,
                           threshold = 2,
                           min_hour_crit = 0,
                           min_day_crit = 0,
                           plot = FALSE,
                           save_plot = FALSE,
                           save_summary = FALSE) {

  check_args_remove_nonwear(
    data, window1, window2, threshold,
    min_hour_crit, min_day_crit, plot, save_plot, save_summary
  )

  nonwear <- detect_nonwear(data, window1, window2, threshold)
  if (isTRUE(plot) | is.character(save_plot)) {
    plot_nonwear(
      data, window2, nonwear$stage1, nonwear$stage2, save_plot
    )
  }
  nonwear <- mark_nonwear(data, nonwear$stage1, nonwear$stage2, window2)
  nonwear <- summarise_nonwear(
    nonwear, min_hour_crit, min_day_crit, save_summary
  )
  if (is.null(nonwear)) {
    msg <- "Stopping as no valid data were provided."
    rlang::abort(msg)
  }
  delete_nonwear(nonwear)

}

#' Detection of accelerometer non-wear time
#'
#' Implementation of the van Hees algorithm of non-wear detection.
#'
#' This is an internal function, designed to be used inside the wrapper
#' function \link[=remove_nonwear]{remove_nonwear()} that integrates all
#' steps of detection, removal and inspection of accelerometer non-wear time.
#'
#' @param data An \code{impactr_data} object, as obtained with
#'   \link[=read_acc]{read_acc()}.
#' @param window1,window2 Windows size, in minutes, for the non-wear
#'   detection algorithm. \code{window2} must be smaller than \code{window1},
#'   and \code{window1} must be a multiple of \code{window2}.
#' @param threshold Number of axes that need to meet the non-wear criteria.
#'
#' @return A named list of length 2 (stage1 and stage2) containing the binary
#'   non-wear classification (0 is wear and 1 is non-wear) of each algorithm's
#'   stage per \code{window2} blocks.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link[=plot_nonwear]{plot_nonwear()}},
#'   \code{\link[=mark_nonwear]{mark_nonwear()}},
#'   \code{\link[=summarise_nonwear]{summarise_nonwear()}},
#'   \code{\link[=delete_nonwear]{delete_nonwear()}}
detect_nonwear <- function(data, window1, window2, threshold) {

  window1 <- window1 * 60 * attributes(data)$samp_freq
  window2 <- window2 * 60 * attributes(data)$samp_freq

  nonwear_stage1 <- nonwear_stage1(data, window1, window2, threshold)
  nonwear_stage2 <- nonwear_stage2(nonwear_stage1, window1, window2)

  return(list(stage1 = nonwear_stage1, stage2 = nonwear_stage2))

}

#' Plot the non-wear time detection
#'
#' Draws a plot of the resultant acceleration in epochs of \code{window2} size
#' marking the non-wear time detected by each stage of the non-wear detection
#' algorithm.
#'
#' This is an internal function, designed to be used inside the wrapper
#' function \link[=remove_nonwear]{remove_nonwear()} that integrates all
#' steps of detection, removal and inspection of accelerometer non-wear time.
#'
#' @param data An \code{impactr_data} object, as obtained with
#'   \link[=read_acc]{read_acc()}.
#' @param window2 Window 2 size, in minutes, for the non-wear
#'   detection algorithm. Must be the same value as used by the
#'   \code{detect_nonwear()} function.
#' @param nonwear_stage1,nonwear_stage2 A numeric vector containing the
#'   binary non-wear classification (0 is wear and 1 is non-wear) of each
#'   algorithm's stage per \code{window2} blocks as obtained with the
#'   \link[=detect_nonwear]{detect_nonwear()} function.
#' @param save_plot Indicates whether of not to save the plot to visualize the
#'   detected non-wear periods to a pdf file. Provide a valid path to a file
#'   ending with the ".pdf" extension as a character string if you want the
#'   plot to be saved.
#'
#' @return If \code{save_plot = FALSE} it returns the plot, otherwise it
#'   saves it.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link[=detect_nonwear]{detect_nonwear()}},
#'   \code{\link[=mark_nonwear]{mark_nonwear()}},
#'   \code{\link[=summarise_nonwear]{summarise_nonwear()}},
#'   \code{\link[=delete_nonwear]{delete_nonwear()}}
plot_nonwear <- function(data,
                         window2,
                         nonwear_stage1,
                         nonwear_stage2,
                         save_plot) {

  resultant <- block_average(data, window2)

  day <- round(1440 / window2)
  day_end <- round(length(resultant) / day)
  days_axis <- seq(0, day_end, by = 1 / day)
  days_axis <- days_axis[seq_len(length(resultant))]

  nonwear_stage1 <- c(0, nonwear_stage1, 0)
  start_i <- which(diff(nonwear_stage1) == 1) + 1
  end_i <- which(diff(nonwear_stage1) == - 1) + 1
  start <- days_axis[start_i]
  end <- days_axis[end_i]

  nonwear_stage2 <- c(0, nonwear_stage2, 0)
  start_i2 <- which(diff(nonwear_stage2) == 1) + 1
  end_i2 <- which(diff(nonwear_stage2) == - 1) + 1
  start2 <- days_axis[start_i2]
  end2 <- days_axis[end_i2]

  ymin <- min(resultant)
  ymax <- round(max(resultant) + (max(resultant) - 1) * 2, 1)

  if (is.character(save_plot)) {
    grDevices::pdf(save_plot, width = 7, height = 7)
  }
  graphics::par(mar = c(8, 5, 5, 5), xpd = TRUE)
  plot(
    days_axis, resultant, type = "l",
    xlim = c(0, day_end),
    ylim = c(1, ymax),
    xaxp = c(0, day_end, day_end),
    main = attributes(data)$filename,
    xlab = "Days",
    ylab = "Acceleration (g)"
  )
  if (length(start) > 0) {
    graphics::rect(
      xleft = start, ybottom = ymin, xright = end, ytop = ymax,
      col = grDevices::rgb(0.278, 0.518, 0.471, alpha = 0.4), lty = 0
    )
  }
  if (length(start2) > 0) {
    graphics::rect(
      xleft = start2, ybottom = ymin, xright = end2, ytop = ymax,
      col = grDevices::rgb(0.278, 0.518, 0.471, alpha = 0.8), lty = 0
    )
  }
  graphics::legend(
    "bottom",
    inset = c(0.0, -0.3),
    legend = c(
      "Detected non-wear time",
      "Artificial movement removed"
    ),
    fill = c(
      grDevices::rgb(0.278, 0.518, 0.471, alpha = 0.4),
      grDevices::rgb(0.278, 0.518, 0.471, alpha = 0.8)
    ),
    bty = "n",
    horiz = TRUE
  )
  if (is.character(save_plot)) {
    grDevices::dev.off()
  }

}

#' Mark accelerometer non-wear time
#'
#' Creates a new column, named `wear`, in the \code{impactr_data} object
#' provided by \code{data} indicating its classification of wear or non-wear
#' time.
#'
#' This is an internal function, designed to be used inside the wrapper
#' function \link[=remove_nonwear]{remove_nonwear()} that integrates all
#' steps of detection, removal and inspection of accelerometer non-wear time.
#'
#' @param data An \code{impactr_data} object, as obtained with
#'   \link[=read_acc]{read_acc()}.
#' @param nonwear_stage1,nonwear_stage2 A numeric vector containing the
#'   binary non-wear classification (0 is wear and 1 is non-wear) of each
#'   algorithm's stage per \code{window2} blocks as obtained with the
#'   \code{detect_nonwear()} function.
#' @param window2 Window 2 size, in minutes, for the non-wear
#'   detection algorithm. Must be the same value as used by the
#'   \code{detect_nonwear()} function.
#'
#' @return The \code{impactr_data} object provided by \code{data} with a new
#'   column named `wear` with the binary wear classification (0 is non-wear and
#'   1 is wear) per sample. Notice that this binary classification is regarding
#'   the wear time NOT non-wear.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link[=detect_nonwear]{detect_nonwear()}},
#'   \code{\link[=plot_nonwear]{plot_nonwear()}},
#'   \code{\link[=summarise_nonwear]{summarise_nonwear()}},
#'   \code{\link[=delete_nonwear]{delete_nonwear()}}
mark_nonwear <- function(data, nonwear_stage1, nonwear_stage2, window2) {

  window2 <- window2 * 60 * attributes(data)$samp_freq
  non_wear <- nonwear_stage1 + nonwear_stage2

  block_start <- seq(1, nrow(data), by = window2)
  block_end <- seq(window2, nrow(data), by = window2)
  if (nrow(data) - block_start[length(block_start)] < window2) {
    block_start <- block_start[1:(length(block_start) - 1)]
  }

  wear_start_i <- block_start[which(non_wear == 0)]
  wear_end_i <- block_end[which(non_wear == 0)]

  wear <- rep(0, nrow(data))
  for (i in seq_len(length(wear_start_i))) {
    wear[wear_start_i[i]:wear_end_i[i]] <- 1
  }
  data$wear <- wear
  data

}

#' Summarise accelerometer non-wear time
#'
#' Validates each measurement day based on the minimum number of wear hours
#' and validates the entire observation based on the minimum number of valid
#' days. It updates the values of the `wear` column (created by
#' \link[=mark_nonwear]{mark_nonwear()}) of the \code{impactr_data} object
#' provided by \code{data}, marking as non-wear time the days considered to
#' be invalid. It also creates a wear time daily summary that can be saved in
#' a csv file.
#'
#' This is an internal function, designed to be used inside the wrapper
#' function \link[=remove_nonwear]{remove_nonwear()} that integrates all
#' steps of detection, removal and inspection of accelerometer non-wear time.
#'
#' @param data An \code{impactr_data} object with the `wear` column, as
#'   obtained with \link[=mark_nonwear]{mark_nonwear()}.
#' @param min_hour_crit The minimum number of hours marked as wear time
#'   in a day for it to be considered valid.
#' @param min_day_crit The minimum number of valid days for the data of
#'   a given subject to be considered valid.
#' @param save_summary Indicates whether of not to save the wear time
#'   daily summary to a csv file. Provide a valid path to a file ending
#'   with the ".csv" extension as a character string if you want the
#'   summary to be saved.
#'
#' @return The \code{impactr_data} object provided by \code{data} with the
#'   `wear` column values updated based on the validations. If no data are
#'   classified as valid, it returns \code{NULL}. Also saves the summary
#'   in a csv file if \code{save_summary} is a path.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link[=detect_nonwear]{detect_nonwear()}},
#'   \code{\link[=plot_nonwear]{plot_nonwear()}},
#'   \code{\link[=mark_nonwear]{mark_nonwear()}},
#'   \code{\link[=delete_nonwear]{delete_nonwear()}}
summarise_nonwear <- function(data,
                              min_hour_crit,
                              min_day_crit,
                              save_summary) {

  data$date <- as.Date(data$timestamp)
  h <- 3600 * attributes(data)$samp_freq

  date <- unique(data$date)
  weekday <- weekdays(date)
  measurement_day <- seq_len(length(weekday))
  recorded_hours <- round(
    purrr::map_dbl(
      date, ~ length(which(data$date == .x))
    ) / h,
    1
  )
  valid_hours <- round(
    purrr::map_dbl(
      date, ~ length(which(data$date == .x & data$wear == 1))
    ) / h,
    1
  )
  valid_day <- rep("No", length(weekday))
  valid_day[which(valid_hours >= min_hour_crit)] <- "Yes"
  valid_observation <- ifelse(
    length(which(valid_day == "Yes")) >= min_day_crit, "Yes", "No"
  )

  nonwear_summary <- data.frame(
    filename = attributes(data)$filename,
    date, weekday, measurement_day,
    recorded_hours, valid_hours,
    min_hour_crit, min_day_crit,
    valid_day, valid_observation
  )
  if (is.character(save_summary)) {
    if (file.exists(save_summary)) {
      pre_summary <- utils::read.csv(save_summary)
      if (length(unique(pre_summary$filename)) == 1) {
        if (unique(pre_summary$filename) == attributes(data)$filename) {
          file.remove(save_summary)
        }
      }
    }
    use_colnames <- ifelse(file.exists(save_summary), FALSE, TRUE)
    suppressWarnings(
      utils::write.table(
        nonwear_summary, file = save_summary, sep = ",", append = TRUE,
        row.names = FALSE, col.names = use_colnames
      )
    )
  }

  if (valid_observation == "No") {
    msg <- glue::glue(
      "Data from file `{attributes(data)$filename}` is not valid as \\
      the number of valid days ({length(which(valid_day == \"Yes\"))}) is \\
      less than the criteria ({min_day_crit})."
    )
    rlang::inform(msg)
    return(invisible(NULL))
  }

  invalid_day <- date[which(valid_day == "No")]
  data$wear[which(data$date == invalid_day)] <- 0
  data[, -ncol(data)]

}

#' Delete accelerometer non-wear time
#'
#' Deletes all periods marked as non-wear.
#'
#' This is an internal function, designed to be used inside the wrapper
#' function \link[=remove_nonwear]{remove_nonwear()} that integrates all
#' steps of detection, removal and inspection of accelerometer non-wear time.
#'
#' @param data An \code{impactr_data} object with the `wear` column, as
#'   obtained with \link[=mark_nonwear]{mark_nonwear()}.
#'
#' @return The \code{impactr_data} object provided by \code{data} without the
#'   `wear` column and with the non-wear time removed.
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link[=detect_nonwear]{detect_nonwear()}},
#'   \code{\link[=plot_nonwear]{plot_nonwear()}},
#'   \code{\link[=mark_nonwear]{mark_nonwear()}},
#'   \code{\link[=summarise_nonwear]{summarise_nonwear()}}
delete_nonwear <- function(data) {

  remove <- which(data$wear == 0)
  data <- data[-remove, -ncol(data)]
  data

}

nonwear_stage1 <- function(data, window1, window2, threshold) {

  range_crit <- 0.05
  sd_crit <- 0.013

  n_blocks <- floor(nrow(data) / window2)
  crit <- ((window1 / window2) / 2) + 1
  nonwear_stage1 <- matrix(0, n_blocks, 3)

  for (i in 1:n_blocks) {
    if (i <= crit) {
      start <- 1
      end <- window1
    } else if (i > crit & i < (n_blocks - crit)) {
      start <- (((i - 1) * window2) + window2 / 2) - window1 / 2
      end <- (((i - 1) * window2) + window2 / 2) + window1 / 2
    } else if (i >= (n_blocks - crit)) {
      start <- (n_blocks - crit) * window2
      end <- n_blocks * window2
    }

    for (j in 1:3) {
      max_acc <- max(data[[j + 1]][(start + 1):end], na.rm = TRUE)
      min_acc <- min(data[[j + 1]][(start + 1):end], na.rm = TRUE)
      range_acc <- abs(max_acc - min_acc)
      sd_acc <- stats::sd(data[[j + 1]][(start + 1):end], na.rm = TRUE)

      if (is.numeric(range_acc) & is.numeric(sd_acc)) {
        if (range_acc < range_crit & sd_acc < sd_crit) {
          nonwear_stage1[i, j] <- 1
        }
      } else {
        nonwear_stage1[i, j] <- 1
      }
    }
  }

  nonwear_stage1 <- rowSums(nonwear_stage1)
  nonwear_stage1[which(nonwear_stage1 >= threshold)] <- 1
  nonwear_stage1

}

nonwear_stage2 <- function(nonwear_stage1, window1, window2) {

  h_crit_1 <- 1 / (window2 / window1)
  h_crit_3 <- 3 / (window2 / window1)
  h_crit_6 <- 6 / (window2 / window1)
  h_crit_24 <- 24 / (window2 / window1)

  non_wear_original <- nonwear_stage2 <- matrix(0, length(nonwear_stage1), 1)
  non_wear_idx <- which(nonwear_stage1 == 1)
  non_wear_original[non_wear_idx] <- 1
  non_wear_original <- c(0, non_wear_original, 0)
  nonwear_stage2 <- c(0, nonwear_stage2, 0)

  start_wear <- which(diff(non_wear_original) == 1) + 1
  start_non_wear <- which(diff(non_wear_original) == - 1) + 1


  if (length(start_wear) > 1) {
    length_wear <- matrix(0, length(start_wear) - 1, 1)
    length_non_wear_after <- matrix(0, length(start_wear) - 1, 1)
    length_non_wear_before <- matrix(0, length(start_wear) - 1, 1)
    surrounding_non_wear <- matrix(0, length(start_wear) - 1, 1)

    for (i in 1:(length(start_wear) - 1)) {
      length_wear[i] <- abs(start_wear[i + 1] - start_non_wear[i])
      length_non_wear_after[i] <- abs(start_non_wear[i + 1] - start_wear[i + 1])
      length_non_wear_before[i] <- abs(start_non_wear[i] - start_wear[i])
      surrounding_non_wear[i] <- length_non_wear_after[i] +
        length_non_wear_before[i]

      if (
        length_wear[i] < h_crit_6 &
        (length_wear[i] / surrounding_non_wear[i]) < 0.3
      ) {
        nonwear_stage2[start_non_wear[i]:start_wear[i + 1] - 1] <- 1
      }
      if (
        length_wear[i] < h_crit_3 &
        (length_wear[i] / surrounding_non_wear[i]) < 0.8
      ) {
        nonwear_stage2[start_non_wear[i]:start_wear[i + 1] - 1] <- 1
      }
      if (start_wear[i] > length(nonwear_stage1) - h_crit_24) {
        if (length_wear[i] < h_crit_3 & length_non_wear_before[i] > h_crit_1) {
          nonwear_stage2[start_non_wear[i]:start_wear[i + 1] - 1] <- 1
        }
      }
    }
  }

  if (length(start_wear) > 0) {
    if (start_wear[1] < h_crit_3 & start_wear[1] > 1) {
      nonwear_stage2[1:(start_wear[1] - 1)] <- 1
    }

    last_non_wear <- start_non_wear[length(start_non_wear)]
    if (
      last_non_wear > length(nonwear_stage2) - h_crit_3 &
      last_non_wear != length(nonwear_stage2)
    ) {
      nonwear_stage2[last_non_wear:length(nonwear_stage2)] <- 1
    }
  }

  non_wear_original <- non_wear_original[-c(1, length(non_wear_original))]
  nonwear_stage2 <- nonwear_stage2[-c(1, length(nonwear_stage2))]

  for (i in 1:2) {
    non_wear_original_b <- nonwear_stage2 + non_wear_original
    non_wear_original_b[which(non_wear_original_b > 1)] <- 1
    non_wear_original_b <- c(0, non_wear_original_b, 0)
    nonwear_stage2_b <- c(0, nonwear_stage2, 0)

    start_wear_b <- which(diff(non_wear_original_b) == 1) + 1
    start_non_wear_b <- which(diff(non_wear_original_b) == - 1) + 1

    if (length(start_wear_b) > 1) {
      length_wear_b <- matrix(0, length(start_wear_b) - 1, 1)
      length_non_wear_after_b <- matrix(0, length(start_wear_b) - 1, 1)
      length_non_wear_before_b <- matrix(0, length(start_wear_b) - 1, 1)
      surrounding_non_wear_b <- matrix(0, length(start_wear_b) - 1, 1)

      for (j in 1:(length(start_wear_b) - 1)) {
        length_wear_b[j] <- abs(start_wear_b[j + 1] - start_non_wear_b[j])
        length_non_wear_after_b[j] <- abs(
          start_non_wear_b[j + 1] - start_wear_b[j + 1]
        )
        length_non_wear_before_b[j] <- abs(
          start_non_wear_b[j] - start_wear_b[j]
        )
        surrounding_non_wear_b[j] <- length_non_wear_after_b[j] +
          length_non_wear_before_b[j]

        if (
          length_wear_b[j] < h_crit_6 &
          (length_wear_b[j] / surrounding_non_wear_b[j]) < 0.3
        ) {
          nonwear_stage2_b[start_non_wear_b[j]:start_wear_b[j + 1] - 1] <- 1
        }
        if (
          length_wear_b[j] < h_crit_3 &
          (length_wear_b[j] / surrounding_non_wear_b[j]) < 0.8
        ) {
          nonwear_stage2_b[start_non_wear_b[j]:start_wear_b[j + 1] - 1] <- 1
        }
        if (start_wear_b[j] > length(nonwear_stage1) - h_crit_24) {
          if (
            length_wear_b[j] < h_crit_3 & length_non_wear_before_b[j] > h_crit_1
          ) {
            nonwear_stage2_b[start_non_wear_b[j]:start_wear_b[j + 1] - 1] <- 1
          }
        }
      }
    }
    nonwear_stage2 <- nonwear_stage2_b[-c(1, length(nonwear_stage2_b))]
  }

  nonwear_stage2[which(nonwear_stage1 + nonwear_stage2 == 2)] <- 0
  nonwear_stage2

}

block_average <- function(data, window2) {

  if ("acc_R" %in% colnames(data)) {
    resultant <- data[["acc_R"]]
  } else {
    resultant <- compute_resultant(data$acc_X, data$acc_Y, data$acc_Z)
  }
  resultant[which(resultant < 1)] <- 1

  window2 <- window2 * 60 * attributes(data)$samp_freq

  resultant <- cumsum(c(0, resultant))
  select <- seq(1, length(resultant), by = window2)
  diff(resultant[round(select)] / unique(abs(diff(round(select)))))

}

#' @importFrom lvmisc %!in%
check_args_remove_nonwear <- function(data,
                                      window1,
                                      window2,
                                      threshold,
                                      min_hour_crit,
                                      min_day_crit,
                                      plot,
                                      save_plot,
                                      save_summary) {

  if (!is_impactr_data(data)) {
    lvmisc::abort_argument_class(
      "data", must = "be of class `impactr_data`", not = data
    )
  }

  if (!is.numeric(window1)) {
    lvmisc::abort_argument_type("window1", must = "be numeric", not = window1)
  }
  if (!is.numeric(window2)) {
    lvmisc::abort_argument_type("window2", must = "be numeric", not = window2)
  }
  if ((window1 / window2) %% 1 != 0) {
    rlang::abort("`window1` must be a multiple of `window2`.")
  }

  if (!is.numeric(min_day_crit)) {
    lvmisc::abort_argument_type(
      "min_day_crit", must = "be numeric", not = min_day_crit
    )
  }

  threshold_vals <- 1:3
  if (threshold %!in% threshold_vals) {
    lvmisc::abort_argument_value("threshold", threshold_vals)
  }

  if (min_hour_crit > 24 | min_hour_crit < 0) {
    rlang::abort("`min_hour_crit` must be between 0 and 24.")
  }

  if (!is.logical(plot)) {
    lvmisc::abort_argument_type("plot", must = "be logical", not = plot)
  }

  if (!isFALSE(save_plot) & !is.character(save_plot)) {
    rlang::abort(
      "`save_plot` must be `FALSE` or a character string indicating a path."
    )
  }
  if (is.character(save_plot) & !stringr::str_ends(save_plot, ".pdf")) {
    rlang::abort("`save_plot` must end in `.pdf`")
  }

  if (!isFALSE(save_summary) & !is.character(save_summary)) {
    rlang::abort(
      "`save_summary` must be `FALSE` or a character string indicating a path."
    )
  }
  if (is.character(save_summary) & !stringr::str_ends(save_summary, ".csv")) {
    rlang::abort("`save_summary` must end in `.csv`")
  }

}
