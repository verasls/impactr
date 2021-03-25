test_that("error handling works", {
  # No header
  file <- test_path("test-data-no-header.csv")
  expect_error(
    read_acc(file),
    glue::glue(
      "No header detected in the file `{file}`. \\
      Please, provide the entire file."
    )
  )

  # Not an ActiGraph accelerometer file
  file <- test_path("test-data-no-actigraph.csv")
  expect_error(
    read_acc(file),
    glue::glue(
      "The file `{file}` is not from an ActiGraph accelerometer. \\
      `impactr` currently only supports ActiGraph accelerometer \\
      data files."
    )
  )

  # Not raw data
  file <- test_path("test-data-epoch.csv")
  expect_error(
    read_acc(file),
    glue::glue(
      "The file `{file}` is not a raw data file. `impactr` needs the raw \\
      data to work."
    )
  )
})

test_that("output object is of class `impactr_data`", {
  test_imu <- read_acc(test_path("test-data-hip-imu.csv"))
  test_raw <- read_acc(test_path("test-data-hip-raw.csv"))

  expect_s3_class(test_imu, "impactr_data")
  expect_s3_class(test_raw, "impactr_data")
})

test_that("output attributes are correct", {
  test_imu <- read_acc(test_path("test-data-hip-imu.csv"))
  test_raw <- read_acc(test_path("test-data-hip-raw.csv"))

  # Attributes number
  expect_equal(length(attributes(test_imu)), 8)
  expect_equal(length(attributes(test_raw)), 8)

  # Attributes names
  attr_names <- c(
    "names", "row.names", "start_date_time", "samp_freq",
    "acc_placement", "subj_body_mass", "filter_type", "class"
  )
  expect_equal(names(attributes(test_imu)), attr_names)
  expect_equal(names(attributes(test_raw)), attr_names)

  # Attributes class/type
  expect_s3_class(attributes(test_imu)$start_date_time, "POSIXct")
  expect_s3_class(attributes(test_raw)$start_date_time, "POSIXct")
  expect_type(attributes(test_imu)$samp_freq, "double")
  expect_type(attributes(test_raw)$samp_freq, "double")
  expect_type(attributes(test_imu)$acc_placement, "logical")
  expect_type(attributes(test_raw)$acc_placement, "logical")
  expect_true(is.na(attributes(test_imu)$subj_body_mass))
  expect_true(is.na(attributes(test_raw)$subj_body_mass))
  expect_true(is.na(attributes(test_imu)$filter_type))
  expect_true(is.na(attributes(test_raw)$filter_type))
})

test_that("output columns are correct", {
  test_imu <- read_acc(test_path("test-data-hip-imu.csv"))
  test_raw <- read_acc(test_path("test-data-hip-raw.csv"))

  # Column names
  col_names <- c("timestamp", "acc_X", "acc_Y", "acc_Z")
  expect_equal(names(test_imu), col_names)
  expect_equal(names(test_raw), col_names)

  # Columns class/type
  expect_s3_class(test_imu$timestamp, "POSIXct")
  expect_s3_class(test_raw$timestamp, "POSIXct")
  expect_type(test_imu$acc_X, "double")
  expect_type(test_raw$acc_X, "double")
  expect_type(test_imu$acc_Y, "double")
  expect_type(test_raw$acc_Y, "double")
  expect_type(test_imu$acc_Z, "double")
  expect_type(test_raw$acc_Z, "double")
})

test_that("read_acc() works with date format separated by `/`", {
  sep_dash <- read_acc(test_path("test-data-hip-imu.csv"))
  sep_slash <- read_acc(test_path("test-data-date-sep.csv"))
  expect_equal(sep_dash, sep_slash)
})
