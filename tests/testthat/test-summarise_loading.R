data <- define_region(
  daily_acc,
  start_time = "2017-01-03 18:00:00",
  end_time = "2017-01-03 20:00:00"
) |>
  use_resultant() |>
  find_peaks(vector = "all")

test_that("error handling works", {
  expect_error(
    summarise_loading(mtcars),
    class = "error_argument_class"
  )
  expect_error(
    summarise_loading(data, "acceleration", "vertical"),
    class = "error_argument_value"
  )
  expect_error(
    summarise_loading(data, "acc", "both"),
    class = "error_argument_value"
  )
  expect_error(
    summarise_loading(data, "acc", "vertical", daily_average = "yes"),
    class = "error_argument_type"
  )
  expect_error(
    summarise_loading(data, "acc", "vertical", ranges_acc = "1, 2")
  )
  expect_error(
    summarise_loading(data, "acc", "vertical", ranges_acc = -1:2)
  )
  expect_error(
    summarise_loading(data, "acc", "vertical", ranges_acc = c(3, 2, 1))
  )
  expect_error(
    summarise_loading(data, "acc", "vertical", ranges_grf = 1:10)
  )
  expect_error(
    summarise_loading(data, "acc", "vertical", save_summary = TRUE)
  )
  expect_error(
    summarise_loading(data, "grf", "resultant")
  )
})

test_that("summarising works", {
  expect_snapshot(summarise_loading(data, "acc", "all", ranges_acc = 1:5))
})

test_that("saving the summary works", {
  summary_dir <- tempdir()
  out <- summarise_loading(
    data, "acc", "all", ranges_acc = 1:5, save_summary = summary_dir
  )

  file1 <- paste0(summary_dir, "/vertical_peak_acc.csv")
  file2 <- paste0(summary_dir, "/resultant_peak_acc.csv")
  file3 <- paste0(summary_dir, "/daily_average_vertical_peak_acc.csv")
  file4 <- paste0(summary_dir, "/daily_average_resultant_peak_acc.csv")
  files <- c(file1, file2, file3, file4)

  purrr::walk(files, ~ expect_true(file.exists(.x)))
  purrr::walk(files, file.remove)
})
