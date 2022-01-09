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
                            use_resultant = TRUE) {

  data_path <- list.files(data_path, full.names = TRUE)
  n_files <- seq_len(length(data_path))

  if (!grepl("/$", output_path)) {
    output_path <- paste0(output_path, "/")
  }

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

  cat("  Reading accelerometer data file\n")
  data <- read_acc(data_path)
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

    cat("  Removing accelerometer non-wear time\n")
    data <- remove_nonwear(
      data, window1, window2, threshold,
      min_hour_crit, min_day_crit,
      nonwear_plot, save_nonwear_plot, save_nonwear_summary
    )
  }

  if (isTRUE(filter_acc)) {
    cat("  Filtering accelerometer data\n")
    data <- filter_acc(data, filter_order, filter_cutoff, filter_type)
  }

  if (isTRUE(use_resultant)) {
    cat("  Computing the resultant vector\n\n")
    data <- use_resultant(data)
  }

  data

}
