has_accdata <- requireNamespace("accdata", quietly = TRUE)

if (has_accdata) {
  daily_acc_3d <- import_dataset("daily_acc_3d")
  data <- define_region(
    daily_acc_3d,
    start_time = "2016-01-20 00:01:00",
    end_time = "2016-01-20 23:59:59"
  )
}

test_that("error handling works", {
  if (!has_accdata) {
    skip("`accdata` not available")
  }

  expect_error(
    remove_nonwear(mtcars),
    class = "error_argument_class"
  )
  expect_error(
    remove_nonwear(data, window1 = "10"),
    class = "error_argument_type"
  )
  expect_error(
    remove_nonwear(data, window2 = TRUE),
    class = "error_argument_type"
  )
  expect_error(
    remove_nonwear(data, min_day_crit = "yes"),
    class = "error_argument_type"
  )
  expect_error(remove_nonwear(data, window1 = 60, window2 = 7))
  expect_error(remove_nonwear(data, threshold = 4))
  expect_error(remove_nonwear(data, min_hour_crit = 27))
  expect_error(remove_nonwear(data, plot = "yes"))
  expect_error(remove_nonwear(data, save_plot = 1))
  expect_error(remove_nonwear(data, save_summary = TRUE))
  expect_error(remove_nonwear(data, save_plot = "plot.tiff"))
  expect_error(remove_nonwear(data, save_summary = "summary.xlsx"))
  suppressMessages(
    expect_error(remove_nonwear(data, min_hour_crit = 15, min_day_crit = 3))
  )
})

test_that("no warnings are generated when trying to save a plot", {
  if (!has_accdata) {
    skip("`accdata` not available")
  }

  attributes(data)$filename <- "Sáv"
  plot_filename <- tempfile(fileext = ".pdf")
  expect_warning(
    remove_nonwear(data, save_plot = plot_filename),
    regexp = NA
  )
  if (file.exists(plot_filename)) file.remove(plot_filename)
})

test_that("non-wear detection works", {
  if (!has_accdata) {
    skip("`accdata` not available")
  }

  expect_snapshot(print(remove_nonwear(data), n = 1000))
})

test_that("plotting detected non-wear time works", {
  if (!has_accdata) {
    skip("`accdata` not available")
  }

  plot_file <- tempfile(fileext = ".pdf")
  summary_file <- tempfile(fileext = ".csv")
  out <- remove_nonwear(
    data, save_plot = plot_file, save_summary = summary_file
  )

  expect_true(file.exists(plot_file))
  expect_true(file.exists(summary_file))

  file.remove(plot_file)
  file.remove(summary_file)
})
