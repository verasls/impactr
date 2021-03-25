skip_if_no_scipy <- function() {
  have_scipy <- reticulate::py_module_available("scipy")
  if (!have_scipy)
    skip("scipy not available for testing")
}
