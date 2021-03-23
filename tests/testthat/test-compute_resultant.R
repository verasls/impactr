test_that("resultant vector is computed correctly", {
  vector <- data.frame(
    x = c(0, 1, 5, -7),
    y = c(0, -1, 3, -9),
    z = c(0, 0, -8, 10)
  )
  out <- compute_resultant(vector$x, vector$y, vector$z)

  expect_equal(length(out), 4)
  expect_equal(out, c(0, 1.41421356, 9.89949494, 15.16575089))
})
