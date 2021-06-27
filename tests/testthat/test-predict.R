test_that("error handling works", {
  data <- read_acc(impactr_example("hip-raw.csv"))
  data <- specify_parameters(data, acc_placement = "hip", subj_body_mass = 78)
  data <- find_peaks(data, vector = "vertical")

  expect_error(
    predict_loading(
      data, outcome = "force", vector = "resultant", model = "walking/running"
    ),
    "`outcome` must be one of \"grf\", \"lr\" or \"all\"",
    class = "error_argument_value"
  )
  expect_error(
    predict_loading(
      data, outcome = "grf", vector = "both", model = "walking/running"
    ),
    "`vector` must be one of \"vertical\", \"resultant\" or \"all\"",
    class = "error_argument_value"
  )
  expect_error(
    predict_loading(
      data, outcome = "grf", vector = "resultant", model = 1
    ),
    "`model` must be one of \"walking/running\"",
    class = "error_argument_value"
  )
  expect_error(
    predict_loading(
      data, outcome = "grf", vector = "resultant", model = "walking/running"
    )
  )

  data <- read_acc(impactr_example("hip-raw.csv"))
  data <- specify_parameters(data, acc_placement = "hip", subj_body_mass = 78)
  data <- use_resultant(data)
  data <- find_peaks(data, vector = "all")
  expect_warning(
    predict_loading(
      data, outcome = "grf", vector = "vertical", model = "walking/running"
    )
  )
})

test_that("predict_loading() works", {
  data <- read_acc(impactr_example("hip-raw.csv"))
  data <- specify_parameters(data, acc_placement = "hip", subj_body_mass = 78)
  data <- use_resultant(data)
  data <- find_peaks(data, vector = "all")

  expect_s3_class(
    predict_loading(
      data, outcome = "all", vector = "all", model = "walking/running"
    ),
    "impactr_peaks"
  )
})
