#' @export
analyse_loading <- function(data_path,
                            output_path,
                            acc_placement,
                            subj_body_mass,
                            remove_nonwear = TRUE,
                            window1 = 60,
                            window2 = 15,
                            threshold = 2,
                            min_hour_crit = 0,
                            min_day_crit = 0,
                            nonwear_plot = TRUE,
                            save_nonwear_plot = TRUE,
                            save_nonwear_summary = TRUE,
                            filter_acc = TRUE,
                            filter_order = 4,
                            filter_cutoff = 20,
                            filter_type = "lowpass",
                            use_resultant = TRUE,
                            find_peaks = TRUE,
                            min_peak_height = 1.3,
                            min_peak_dist = 0.4,
                            vector = "all",
                            predict_grf = FALSE,
                            predict_lr = FALSE,
                            model = NULL,
                            summarise_acc = TRUE,
                            summarise_grf = FALSE,
                            summarise_lr = FALSE,
                            ranges_acc = NULL,
                            ranges_grf = NULL,
                            ranges_lr = NULL,
                            daily_average = TRUE,
                            save_loading_summary = TRUE) {

  data_path <- list.files(data_path, full.names = TRUE)
  n_files <- seq_len(length(data_path))

  if (!grepl("/$", output_path)) {
    output_path <- paste0(output_path, "/")
  }

  cat("\nStarting analyses\n")
  cat("\nStep 1: Processing accelerometer data\n")
  if (length(data_path) == 1) {
    data <- process_acc_data(
      data_path, output_path,
      acc_placement, subj_body_mass,
      remove_nonwear, window1, window2, threshold,
      min_hour_crit, min_day_crit,
      nonwear_plot, save_nonwear_plot, save_nonwear_summary,
      filter_acc, filter_order, filter_cutoff, filter_type,
      use_resultant
    )
  } else {
    data <- purrr::map(
      n_files, function(.x) {
        if (.x != 1) cat("\n")
        msg <- paste0(
          "Processing file ", .x, " out of ", length(data_path),
          " (", basename(data_path[.x]), ")\n"
        )
        cat(msg)

        process_acc_data(
          data_path[.x], output_path,
          acc_placement[.x], subj_body_mass[.x],
          remove_nonwear, window1, window2, threshold,
          min_hour_crit, min_day_crit,
          nonwear_plot, save_nonwear_plot, save_nonwear_summary,
          filter_acc, filter_order, filter_cutoff, filter_type,
          use_resultant
        )
      }
    )
  }

  cat("\nStep 2: Estimating mechanical loading\n")
  if (isTRUE(find_peaks)) {
    if (length(data_path) == 1) {
      data <- estimate_loading(
        data, min_peak_height, min_peak_dist, vector,
        predict_grf, predict_lr, model
      )
    } else {
      data <- purrr::map(
        n_files, function(.x) {
          if (.x != 1) cat("\n")
          msg <- paste0(
            "Processing file ", .x, " out of ", length(data_path),
            " (", basename(data_path[.x]), ")\n"
          )
          cat(msg)

          data <- estimate_loading(
            data[[.x]], min_peak_height, min_peak_dist, vector,
            predict_grf, predict_lr, model
          )
        }
      )
    }
  } else {
    cat("Nothing to be done\n")
  }

  cat("\nStep 3: Summarise loading\n")
  if (isTRUE(summarise_acc) | isTRUE(summarise_grf) | isTRUE(summarise_lr)) {
    if (isTRUE(save_loading_summary)) {
      loading_summary_dir <- paste0(output_path, "loading_summary/summaries/")
      if (!dir.exists(loading_summary_dir)) {
        dir.create(loading_summary_dir, recursive = TRUE)
      }
    }

    variable <- variable_to_summarise(
      summarise_acc, summarise_grf, summarise_lr
    )
    if (length(data_path) == 1) {
      summarise_loading(
        data, variable, vector, daily_average,
        ranges_acc, ranges_grf, ranges_lr,
        loading_summary_dir
      )
    } else {
      purrr::map(
        n_files, function(.x) {
          if (.x != 1) cat("\n")
          msg <- paste0(
            "Processing file ", .x, " out of ", length(data_path),
            " (", basename(data_path[.x]), ")\n"
          )
          cat(msg)

          summarise_loading(
            data[[.x]], variable, vector, daily_average,
            ranges_acc, ranges_grf, ranges_lr,
            loading_summary_dir
          )
        }
      )
    }
  } else {
    cat("Nothing to be done\n")
  }

  cat("\nDone!\n")
  return(invisible(NULL))

}

process_acc_data <- function(data_path,
                             output_path,
                             acc_placement,
                             subj_body_mass,
                             remove_nonwear,
                             window1,
                             window2,
                             threshold,
                             min_hour_crit,
                             min_day_crit,
                             nonwear_plot,
                             save_nonwear_plot,
                             save_nonwear_summary,
                             filter_acc,
                             filter_order,
                             filter_cutoff,
                             filter_type,
                             use_resultant) {

  cat("  Reading accelerometer data file...")
  data <- read_acc(data_path)
  cat(" ok\n")
  data <- specify_parameters(data, acc_placement, subj_body_mass)

  if (isTRUE(remove_nonwear)) {
    filename <- attributes(data)$filename

    if (isTRUE(save_nonwear_plot)) {
      nonwear_plot_dir <- paste0(output_path, "nonwear/plots/")
      if (!dir.exists(nonwear_plot_dir)) {
        dir.create(nonwear_plot_dir, recursive = TRUE)
      }
      save_nonwear_plot <- paste0(
        nonwear_plot_dir,
        substr(filename, 1, nchar(filename) - 4), ".pdf"
      )
    }

    if (isTRUE(save_nonwear_summary)) {
      nonwear_summary_dir <- paste0(output_path, "nonwear/summaries/")
      if (!dir.exists(nonwear_summary_dir)) {
        dir.create(nonwear_summary_dir, recursive = TRUE)
      }
      save_nonwear_summary <- paste0(
        nonwear_summary_dir,
        substr(filename, 1, nchar(filename) - 4), ".csv"
      )
    }

    cat("  Removing accelerometer non-wear time...")
    data <- remove_nonwear(
      data, window1, window2, threshold,
      min_hour_crit, min_day_crit,
      nonwear_plot, save_nonwear_plot, save_nonwear_summary
    )
    cat(" ok\n")
  }

  if (isTRUE(filter_acc)) {
    cat("  Filtering accelerometer data...")
    data <- filter_acc(data, filter_order, filter_cutoff, filter_type)
    cat(" ok\n")
  }

  if (isTRUE(use_resultant)) {
    cat("  Computing the resultant vector...")
    data <- use_resultant(data)
    cat(" ok\n")
  }

  data

}

estimate_loading <- function(data,
                             min_peak_height,
                             min_peak_dist,
                             vector,
                             predict_grf,
                             predict_lr,
                             model) {

  cat("  Finding peaks in the acceleration signal...")
  peaks <- find_peaks(data, vector, min_peak_height, min_peak_dist)
  cat(" ok\n")

  if (isTRUE(predict_grf) & isFALSE(predict_lr)) {
    outcome <- "grf"
  } else if (isFALSE(predict_grf) & isTRUE(predict_lr)) {
    outcome <- "lr"
  } else if (isTRUE(predict_grf) & isTRUE(predict_lr)) {
    outcome <- "all"
  } else if (isFALSE(predict_grf) & isFALSE(predict_lr)) {
    outcome <- "none"
  }

  if (outcome != "none") {
    cat(" Computing mechanical loading outcomes")
    peaks <- predict_loading(peaks, outcome, vector, model)
    cat(" ok\n")
  }

  peaks

}

variable_to_summarise <- function(summarise_acc, summarise_grf, summarise_lr) {

  variable <- vector("character", 0)

  if (isTRUE(summarise_acc)) {
    variable <- c(variable, "acc")
  }
  if (isTRUE(summarise_grf)) {
    variable <- c(variable, "grf")
  }
  if (isTRUE(summarise_lr)) {
    variable <- c(variable, "lr")
  }
  if (length(variable) == 3) {
    variable <- "all"
  } else {
    variable <- variable
  }

  variable

}
