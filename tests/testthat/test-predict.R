test_that("error handling works", {
  data <- read_acc(impactr_example("hip-raw.csv"))
  data <- specify_parameters(data, acc_placement = "hip", subj_body_mass = 78)
  data <- find_peaks(data, vector = "vertical")

  expect_error(
    predict_loading(
      data, outcome = "force", vector = "resultant", model = "walking/running"
    ),
    class = "error_argument_value"
  )
  expect_error(
    predict_loading(
      data, outcome = "grf", vector = "both", model = "walking/running"
    ),
    class = "error_argument_value"
  )
  expect_error(
    predict_loading(
      data, outcome = "grf", vector = "resultant", model = 1
    ),
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

  data <- read_acc(impactr_example("hip-raw.csv"))
  data <- specify_parameters(data, acc_placement = "ankle", subj_body_mass = 78)
  data <- find_peaks(data, vector = "vertical")
  expect_error(
    predict_loading(
      data, outcome = "grf", vector = "vertical", model = "walking"
    ),
    glue::glue(
      "The `ankle` accelerometer placement is not supported in this \\
      model. Please choose between `back` or `hip` or change the model."
    )
  )
})

test_that("predict_loading() works", {
  data <- read_acc(impactr_example("hip-raw.csv"))
  data <- specify_parameters(data, acc_placement = "hip", subj_body_mass = 78)
  data <- use_resultant(data)
  data <- find_peaks(data, vector = "all")

  expect_impactr_peaks <- function(data, outcome, vector, model, n_cols) {
    out <- suppressWarnings(predict_loading(data, outcome, vector, model))
    expect_s3_class(out, "impactr_peaks")
    expect_equal(length(out), n_cols)
  }

  expect_impactr_peaks(data, "grf", "vertical", "walking/running", 3)
  expect_impactr_peaks(data, "grf", "resultant", "walking/running", 3)
  expect_impactr_peaks(data, "grf", "all", "walking/running", 5)
  expect_impactr_peaks(data, "lr", "vertical", "walking/running", 3)
  expect_impactr_peaks(data, "lr", "resultant", "walking/running", 3)
  expect_impactr_peaks(data, "lr", "all", "walking/running", 5)
  expect_impactr_peaks(data, "all", "vertical", "walking/running", 4)
  expect_impactr_peaks(data, "all", "resultant", "walking/running", 4)
  expect_impactr_peaks(data, "all", "all", "walking/running", 7)
  expect_impactr_peaks(data, "grf", "vertical", "walking", 3)
  expect_impactr_peaks(data, "grf", "resultant", "walking", 3)
  expect_impactr_peaks(data, "grf", "all", "walking", 5)
  expect_impactr_peaks(data, "lr", "vertical", "walking", 3)
  expect_impactr_peaks(data, "lr", "resultant", "walking", 3)
  expect_impactr_peaks(data, "lr", "all", "walking", 5)
  expect_impactr_peaks(data, "all", "vertical", "walking", 4)
  expect_impactr_peaks(data, "all", "resultant", "walking", 4)
  expect_impactr_peaks(data, "all", "all", "walking", 7)
})
