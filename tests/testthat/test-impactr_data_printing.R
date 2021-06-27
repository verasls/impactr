test_that("printing from read_acc() works", {
  expect_snapshot(read_acc(test_path("test-data-hip-imu.csv")))
})

test_that("printing from specify_parameter() works", {
  data <- read_acc(test_path("test-data-hip-imu.csv"))
  expect_snapshot(specify_parameters(data, "hip", 80))
})

test_that("printing from filter_acc() works", {
  data <- read_acc(test_path("test-data-hip-imu.csv"))
  expect_snapshot(filter_acc(data))
})

test_that("printing from find_peaks() works", {
  data <- read_acc(impactr_example("hip-raw.csv"))
  data <- use_resultant(data)
  expect_snapshot(find_peaks(data, "vertical"))
  expect_snapshot(find_peaks(data, "resultant"))
  expect_snapshot(find_peaks(data, "all"))
})
