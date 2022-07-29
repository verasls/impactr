# impactr 0.4.1
* Fixed an issue with saving the non-wear plot with `remove_nonwear()` (#2).
* Change the name of the "valid_observation" to "valid_file" in the non-wear summary to better express its meaning.
* Limited the number of characters of the non-wear plot title to 50 characters, preventing the plot title to exceed the plot window limits. In case of large (n. char. > 50) titles, `remove_nonwear()` automatically crops it.
* Return `NA` in the summary variables from `summarise_loading()` whenever the number of detected peaks is 0.
* Change the coefficients of the prediction models for walking/running to match the final version of the published [paper](https://doi.org/10.1080/17461391.2022.2102437).

# impactr 0.4.0
* Added the function `remove_nonwear()` to detect and remove accelerometer non-wear time.
* Added the function `summarise_loading()`.
* Include an interface to access example datasets from the [{accdata}](https://github.com/verasls/accdata/) package. Run `?import_dataset` for help.
* Changed how resultant vector is computed to improve speed.
* `read_acc()` no longer displays a progress bar.

# impactr 0.3.0
* `pracma::findpeaks()` is now used to get the index of the curve start.
* Fixed a bug in which `predict_loading()` did not return the expected columns if `outcome` is set to "all".
* Added a new supported model: "jumping".

# impactr 0.2.0
* `define_region()` now works with multi-day data. See the updated documentation.
* `specify_parameters()` and `filter_acc()` throw errors when called more than once on the same data. This prevents attributes being accidentally added on top of existing ones.
* `predict()` throws an error when required attributes are missing.
* Fixed a test failure with {tibble} release 3.1.4 (#1).
