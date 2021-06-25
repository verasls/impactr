
<!-- README.md is generated from README.Rmd. Please edit that file -->

# impactr <a href='https://github.com/verasls/impactr'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/impactr)](https://CRAN.R-project.org/package=impactr)
[![R-CMD-check](https://github.com/verasls/impactr/workflows/R-CMD-check/badge.svg)](https://github.com/verasls/impactr/actions)
[![Codecov test
coverage](https://codecov.io/gh/verasls/impactr/branch/main/graph/badge.svg)](https://codecov.io/gh/verasls/impactr?branch=main)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

`impactr` is a package with functions to read, process and analyse raw
accelerometer data related to mechanical loading variables. You can
learn more about this package features and how to use in
`vignette("impactr")`.

## Installation

To install the latest stable version of impactr from
[CRAN](https://CRAN.R-project.org), run:

``` r
install.packages("impactr")
```

You can also install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("verasls/impactr")
```

Some key functions for signal processing depend on the Python module
SciPy. To install it, use the `install_scipy()` function:

``` r
library(impactr)
install_scipy()
```

## Usage

``` r
library(impactr)

read_acc(impactr_example("hip-imu.csv")) |>
 define_region(
    start_time = "15:45:00",
    end_time = "15:45:30"
  ) |>
  specify_parameters(
    acc_placement = "hip",
    subj_body_mass = 78
  ) |>
  filter_acc() |>
  use_resultant() |>
  find_peaks(vector = "resultant") |>
  predict_loading(
    outcome = "grf",
    vector = "resultant",
    model = "walking/running"
  )
```
