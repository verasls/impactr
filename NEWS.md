# impactr 0.2.0.9000
* `pracma::findpeaks()` is now used to get the index of the curve start.
* Fix a bug in which predict_loading() did not return the expected columns if `outcome` is set to "all".
* Add a new supported model: "jumping"

# impactr 0.2.0
* define_region() now works with multi-day data. See the updated documentation.
* specify_parameters() and filter_acc() throw errors when called more than once on the same data. This prevents attributes being accidentaly added on top of existing ones.
* predict() throws an error when required attributes are missing.
* Fix a test failure with {tibble} release 3.1.4 (#1).
