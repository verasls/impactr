has_accdata <- requireNamespace("accdata", quietly = TRUE)

if (has_accdata) {
  daily_acc_3d <- import_dataset("daily_acc_3d")
  data <- define_region(
    daily_acc_3d,
    start_time = "2016-01-20 00:01:00",
    end_time = "2016-01-20 23:59:59"
  )
  data <- use_resultant(data)
  data <- find_peaks(data, "all")
}

test_that("error handling works", {
  if (!has_accdata) {
    skip("`accdata` not available")
  }

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
  if (!has_accdata) {
    skip("`accdata` not available")
  }

  expect_snapshot(summarise_loading(data, "acc", "all", ranges_acc = 1:5))
})

test_that("saving the summary works", {
  if (!has_accdata) {
    skip("`accdata` not available")
  }

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
