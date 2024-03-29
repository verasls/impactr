# specify_parameters() tests ----------------------------------------------

test_that("specify_parameters() error handling works", {
  data <- read_acc(test_path("test-data-hip-imu.csv"))

  expect_error(
    specify_parameters(data, 1, 80),
    "`acc_placement` must be character; not double",
    class = "error_argument_type"
  )
  expect_error(
    specify_parameters(data, "hip", "80"),
    "`subj_body_mass` must be numeric; not character",
    class = "error_argument_type"
  )
  expect_error(
    specify_parameters(data, "waist", 80),
    "`acc_placement` must be one of \"ankle\", \"back\" or \"hip\"",
    class = "error_argument_value"
  )
  expect_error(
    data |>
      specify_parameters("hip", 78) |>
      specify_parameters("hip", 78),
    glue::glue(
      "`Accelerometer placement` and `Subject body mass` attributes were \\
      updated by a second call of specify_parameters()"
    )
  )
})

test_that("attributes have the correct values", {
  data <- read_acc(test_path("test-data-hip-imu.csv"))
  out <- specify_parameters(data, "hip", 80)

  expect_equal(attributes(out)$acc_placement, "hip")
  expect_equal(attributes(out)$subj_body_mass, 80)
})

# define_region() tests ---------------------------------------------------

test_that("define_region() error handling works", {
  data <- read_acc(test_path("test-data-hip-imu.csv"))

  expect_error(
    define_region(data, 15, "2017-12-09 15:03:00"),
    "`start_time` must be character; not double.",
    class = "error_argument_type"
  )
  expect_error(
    define_region(data, "2017-12-09 15:01:00", 15),
    "`end_time` must be character; not double.",
    class = "error_argument_type"
  )
  expect_error(
    define_region(data, "2017-12-09 5:10:00", "2017-12-09 15:15:00"),
    "`start_time` must be in the `YYYY-MM-DD HH:MM:SS` format."
  )
  expect_error(
    define_region(data, "2017-12-09 15:10:00", "2017-12-09 15:65:00"),
    "`end_time` must be in the `YYYY-MM-DD HH:MM:SS` format."
  )
  expect_error(
    define_region(data, "2017-12-09 13:00:00", "2017-12-09 16:00:00"),
    "`start_time` must not be before `data` Start time"
  )
  expect_error(
    define_region(data, "2017-12-09 15:01:00", "2017-12-09 16:00:00"),
    "`end_time` must not be after the last `data` timestamp"
  )
  expect_error(
    define_region(data, "2017-12-09 15:01:00", "2017-12-09 14:00:00"),
    "`end_time` must not be before `start_time`."
  )
})

test_that("define_region() works", {
  data <- read_acc(test_path("test-data-long.csv"))
  out <- define_region(data, "2017-12-09 15:00:01", "2017-12-09 15:00:03")

  expect_equal(nrow(out), 200)
})
