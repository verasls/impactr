test_that("error handling works", {
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
})

test_that("attributes have the correct values", {
  data <- read_acc(test_path("test-data-hip-imu.csv"))
  out <- specify_parameters(data, "hip", 80)

  expect_equal(attributes(out)$acc_placement, "hip")
  expect_equal(attributes(out)$subj_body_mass, 80)
})
