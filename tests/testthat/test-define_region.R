test_that("error handling works", {
  data <- read_acc(test_path("test-data-hip-imu.csv"))

  expect_error(
    define_region(data, 15, "15:03:00"),
    "`start` must be character; not double.",
    class = "error_argument_type"
  )
  expect_error(
    define_region(data, "15:01:00", 15),
    "`end` must be character; not double.",
    class = "error_argument_type"
  )
  expect_error(
    define_region(data, "5:10:00", "15:15:00"),
    "`start` must be in the `HH:MM:SS` format."
  )
  expect_error(
    define_region(data, "15:10:00", "15:65:00"),
    "`end` must be in the `HH:MM:SS` format."
  )
  expect_error(
    define_region(data, "13:00:00", "16:00:00"),
    "`start` must not be before `data` Start time"
  )
  expect_error(
    define_region(data, "15:01:00", "16:00:00"),
    "`end` must not be after the last `data` timestamp"
  )
  expect_error(
    define_region(data, "15:01:00", "14:00:00"),
    "`end` must not be before `start`."
  )
})

test_that("define_region() works", {
  data <- read_acc(test_path("test-data-long.csv"))
  out <- define_region(data, "15:00:01", "15:00:03")

  expect_equal(nrow(out), 200)
})
