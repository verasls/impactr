# impactr 0.1.9000

* define_region() now works with multi-day data. See the updated documentation.
* specify_parameters() and filter_acc() throw errors when called more than once on the same data. This prevents attributes being accidentaly added on top of existing ones.
* Fix a test failure with {tibble} release 3.1.4 (#1).
