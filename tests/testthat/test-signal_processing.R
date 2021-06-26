# filter_acc() tests ------------------------------------------------------

test_that("filter_acc() error handling works", {
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
    unclass(glue::glue(
      "`type` must be one of \"lowpass\", \"highpass\", \\
      \"bandpass\" or \"bandstop\"."
    )),
    class = "error_argument_value"
  )
})

test_that("default filtering works", {
  data <- data.frame(acc_X = 1:16, acc_Y = 17:32, acc_Z = 33:48)
  attributes(data)$samp_freq <- 100

  out <- filter_acc(data)[1:5, ]
  expect <- data.frame(
    acc_X = c(0.9165122, 1.9171296, 3.0198027, 4.0638896, 5.0100178),
    acc_Y = c(12.1254049, 17.8774066, 20.3506784, 20.5725831, 20.6257695),
    acc_Z = c(23.334298, 33.837684, 37.681554, 37.081277, 36.2415221)
  )

  expect_equal(out$acc_X, expect$acc_X)
  expect_equal(out$acc_Y, expect$acc_Y)
  expect_equal(out$acc_Z, expect$acc_Z)
})

# find_peaks() tests ------------------------------------------------------

test_that("find_peaks() error handling works", {
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
    "`vector` must be one of \"resultant\", \"vertical\" or \"all\"."
  )
  expect_error(find_peaks(data, "resultant"))
})
