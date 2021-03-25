test_that("error handling works", {
  data <- read_acc(test_path("test-data-hip-imu.csv"))

  expect_error(
    filter_acc(data, order = "4"),
    "`order` must be numeric; not character.",
    class = "error_argument_type"
  )
  expect_error(
    filter_acc(data, cutoff = "4"),
    "`cutoff` must be numeric; not character.",
    class = "error_argument_type"
  )
  expect_error(
    filter_acc(data, type = 4),
    "`type` must be character; not double.",
    class = "error_argument_type"
  )
})
