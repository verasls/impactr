test_that("error handling works", {
  skip_if_no_scipy()

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

test_that("default filtering works", {
  skip_if_no_scipy()

  data <- data.frame(acc_X = 1:16, acc_Y = 17:32, acc_Z = 33:48)
  attributes(data)$samp_freq <- 100

  out <- filter_acc(data)[1:5, ]
  expect <- data.frame(
    acc_X = c(1.000387653, 1.999833278, 2.999745298, 3.999947004, 5.000076399),
    acc_Y = c(17.000388, 17.999833, 18.999745, 19.999947, 21.000076),
    acc_Z = c(33.000388, 33.999833, 34.999745, 35.999947, 37.000076)
  )
  dim(expect$acc_X) <- 5
  dim(expect$acc_Y) <- 5
  dim(expect$acc_Z) <- 5

  expect_equal(out$acc_X, expect$acc_X)
  expect_equal(out$acc_Y, expect$acc_Y)
  expect_equal(out$acc_Z, expect$acc_Z)
})
