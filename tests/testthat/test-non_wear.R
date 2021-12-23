data <- define_region(
  daily_acc,
  start_time = "2017-01-03 00:01:00",
  end_time = "2017-01-03 23:59:59"
)

test_that("error handling works", {
  expect_error(
    remove_nonwear(mtcars),
    class = "error_argument_class"
  )
  expect_error(
    remove_nonwear(daily_acc, window1 = "10"),
    class = "error_argument_type"
  )
  expect_error(
    remove_nonwear(daily_acc, window2 = TRUE),
    class = "error_argument_type"
  )
  expect_error(
    remove_nonwear(daily_acc, min_day_crit = "yes"),
    class = "error_argument_type"
  )
  expect_error(remove_nonwear(daily_acc, window1 = 60, window2 = 7))
  expect_error(remove_nonwear(daily_acc, threshold = 4))
  expect_error(remove_nonwear(daily_acc, min_hour_crit = 27))
  expect_error(remove_nonwear(daily_acc, plot = "yes"))
  expect_error(remove_nonwear(daily_acc, save_plot = 1))
  expect_error(remove_nonwear(daily_acc, save_summary = TRUE))
  expect_error(remove_nonwear(daily_acc, save_plot = "plot.tiff"))
  expect_error(remove_nonwear(daily_acc, save_summary = "summary.xlsx"))
  suppressMessages(
    expect_error(remove_nonwear(data, min_hour_crit = 15, min_day_crit = 3))
  )
})

test_that("non-wear detection works", {
  expect_snapshot(print(remove_nonwear(data), n = 1000))
})

test_that("plotting detected non-wear time works", {
  plot_file <- tempfile(fileext = ".pdf")
  summary_file <- tempfile(fileext = ".csv")
  out <- remove_nonwear(
    daily_acc, save_plot = plot_file, save_summary = summary_file
  )

  expect_true(file.exists(plot_file))
  expect_true(file.exists(summary_file))

  file.remove(plot_file)
  file.remove(summary_file)
})
