test_that("error handling works", {
  data <- read_acc(test_path("test-data-hip-imu.csv"))

  expect_error(
    find_peaks(data, TRUE),
    "`vector` must be character; not logical."
  )
  expect_error(
    find_peaks(data, "vertical", min_height = "12"),
    "`min_height` must be numeric; not character."
  )
  expect_error(
    find_peaks(data, "vertical", min_dist = "12"),
    "`min_dist` must be numeric; not character."
  )
  expect_error(
    find_peaks(data, "total"),
    "`vector` must be one of \"resultant\", \"vertical\" or \"both\"."
  )
})
