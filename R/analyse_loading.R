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
                            daily_average = TRUE) {

  data_path <- list.files(data_path, full.names = TRUE)
  n_files <- seq_len(length(data_path))

  if (!grepl("/$", output_path)) {
    output_path <- paste0(output_path, "/")
  }

  meta_step1_dir <- paste0(output_path, "meta/step1/")
  meta_step1_file <- list.files(meta_step1_dir, full.names = TRUE)
  meta_step1_filename <- substr(
    basename(meta_step1_file), 1, nchar(basename(meta_step1_file)) - 4
  )

  cat("\nStarting analyses\n")
  cat("\nStep 1: Processing accelerometer data\n")
  if (length(data_path) == 1) {
    data_filename <- substr(
      basename(data_path), 1, nchar(basename(data_path)) - 4
    )
    data_filename <- meta_step1_file[
      which(meta_step1_filename == data_filename)
    ]
    if (!file.exists(data_filename)) {
      tryCatch(
        error = function(cnd) {
          cat(paste0(" ", conditionMessage(cnd)))
        },
        process_acc_data(
          data_path, output_path,
          acc_placement, subj_body_mass,
          remove_nonwear, window1, window2, threshold,
          min_hour_crit, min_day_crit,
          nonwear_plot, save_nonwear_plot, save_nonwear_summary,
          filter_acc, filter_order, filter_cutoff, filter_type,
          use_resultant
        )
      )
      check_meta_step1(data_path, output_path)
    }
  } else {
    purrr::walk(
      n_files, function(.x) {
        if (.x != 1) cat("\n")
        msg <- paste0(
          "Processing file ", .x, " out of ", length(data_path),
          " (", basename(data_path[.x]), ")\n"
        )
        cat(msg)

        data_filename <- substr(
          basename(data_path[.x]), 1, nchar(basename(data_path[.x])) - 4
        )
        data_filename <- meta_step1_file[
          which(meta_step1_filename == data_filename)
        ]
        if (!file.exists(data_filename)) {
          tryCatch(
            error = function(cnd) {
              cat(paste0(" ", conditionMessage(cnd)))
            },
            process_acc_data(
              data_path[.x], output_path,
              acc_placement[.x], subj_body_mass[.x],
              remove_nonwear, window1, window2, threshold,
              min_hour_crit, min_day_crit,
              nonwear_plot, save_nonwear_plot, save_nonwear_summary,
              filter_acc, filter_order, filter_cutoff, filter_type,
              use_resultant
            )
          )
          check_meta_step1(data_path[.x], output_path)
        } else {
          cat("  File already processed, skipping...\n")
        }
      }
    )
  }

  meta_step2_dir <- paste0(output_path, "meta/step2/")
  meta_step2_file <- list.files(meta_step2_dir, full.names = TRUE)
  meta_step2_filename <- substr(
    basename(meta_step2_file), 1, nchar(basename(meta_step2_file)) - 4
  )

  cat("\nStep 2: Estimating mechanical loading\n")
  if (isTRUE(find_peaks)) {
    if (length(data_path) == 1) {
      peaks_filename <- substr(
        basename(data_path), 1, nchar(basename(data_path)) - 4
      )
      peaks_filename <- meta_step2_file[
        which(meta_step2_filename == peaks_filename)
      ]
      if (length(peaks_filename) == 0) {
        data_filename <- substr(
          basename(data_path), 1, nchar(basename(data_path)) - 4
        )
        load(meta_step1_file[which(meta_step1_filename == data_filename)])

        estimate_loading(
          data, output_path, min_peak_height, min_peak_dist, vector,
          predict_grf, predict_lr, model
        )
      }
    } else {
      purrr::map(
        n_files, function(.x) {
          if (.x != 1) cat("\n")
          msg <- paste0(
            "Processing file ", .x, " out of ", length(data_path),
            " (", basename(data_path[.x]), ")\n"
          )
          cat(msg)

          peaks_filename <- substr(
            basename(data_path[.x]), 1, nchar(basename(data_path[.x])) - 4
          )
          peaks_filename <- meta_step2_file[
            which(meta_step2_filename == peaks_filename)
          ]
          if (length(peaks_filename) == 0) {
            data_filename <- substr(
              basename(data_path[.x]), 1, nchar(basename(data_path[.x])) - 4
            )
            load(meta_step1_file[which(meta_step1_filename == data_filename)])

            if (!is.null(data)) {
              estimate_loading(
                data, output_path, min_peak_height, min_peak_dist, vector,
                predict_grf, predict_lr, model
              )
            } else {
              cat("  No valid data for this file\n")
            }
          } else {
            cat("  File already processed, skipping...\n")
          }
        }
      )
    }
  } else {
    cat("Nothing to be done\n")
  }

  cat("\nStep 3: Summarise loading\n")
  if (isTRUE(summarise_acc) | isTRUE(summarise_grf) | isTRUE(summarise_lr)) {
      loading_summary_dir <- paste0(output_path, "loading_summary/summaries/")
      if (!dir.exists(loading_summary_dir)) {
        dir.create(loading_summary_dir, recursive = TRUE)
      }

    variable <- variable_to_summarise(
      summarise_acc, summarise_grf, summarise_lr
    )
    if (length(data_path) == 1) {
      data_filename <- substr(
        basename(data_path), 1, nchar(basename(data_path)) - 4
      )
      peaks_file <- meta_step2_file[
        which(meta_step2_filename == data_filename)
      ]
      load(peaks_file)

      invisible(
        summarise_loading(
          peaks, variable, vector, daily_average,
          ranges_acc, ranges_grf, ranges_lr,
          loading_summary_dir
        )
      )
    } else {
      if (length(list.files(loading_summary_dir)) > 0) {
        invisible(
          file.remove(list.files(loading_summary_dir, full.names = TRUE))
        )
      }
      purrr::walk(
        n_files, function(.x) {
          if (.x != 1) cat("\n")
          msg <- paste0(
            "Processing file ", .x, " out of ", length(data_path),
            " (", basename(data_path[.x]), ")\n"
          )
          cat(msg)

          data_filename <- substr(
            basename(data_path[.x]), 1, nchar(basename(data_path[.x])) - 4
          )
          peaks_file <- meta_step2_file[
            which(meta_step2_filename == data_filename)
          ]
          if (length(peaks_file) > 0) {
            load(peaks_file)

            cat("  Saving summary files...")
            invisible(
              summarise_loading(
                peaks, variable, vector, daily_average,
                ranges_acc, ranges_grf, ranges_lr,
                loading_summary_dir
              )
            )
            cat(" ok\n")
          } else {
            cat("  No loading peaks found for this file, skipping...\n")
          }
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

  meta_dir <- paste0(output_path, "meta/step1/")
  if (!dir.exists(meta_dir)) {
    dir.create(meta_dir, recursive = TRUE)
  }
  meta_file <- paste0(
    meta_dir, substr(filename, 1, nchar(filename) - 4), ".rda"
  )
  save(data, file = meta_file)

  invisible(NULL)

}

estimate_loading <- function(data,
                             output_path,
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

  filename <- attributes(data)$filename
  meta_dir <- paste0(output_path, "meta/step2/")
  if (!dir.exists(meta_dir)) {
    dir.create(meta_dir, recursive = TRUE)
  }
  meta_file <- paste0(
    meta_dir, substr(filename, 1, nchar(filename) - 4), ".rda"
  )
  save(peaks, file = meta_file)

  invisible(NULL)

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

check_meta_step1 <- function(data_path, output_path) {

  meta_dir <- paste0(output_path, "meta/step1/")
  if (!dir.exists(meta_dir)) {
    dir.create(meta_dir, recursive = TRUE)
  }

  meta_file <- paste0(
    meta_dir,
    substr(basename(data_path), 1, nchar(basename(data_path)) - 4),
    ".rda"
  )

  if (!file.exists(meta_file)) {
    data <- NULL
    save(data, file = meta_file)
  }

}
