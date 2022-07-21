
<!-- README.md is generated from README.Rmd. Please edit that file -->

# impactr <a href='https://github.com/verasls/impactr'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5035522.svg)](https://doi.org/10.5281/zenodo.5035522)
[![CRAN
status](https://www.r-pkg.org/badges/version/impactr)](https://CRAN.R-project.org/package=impactr)
[![devel
Version](https://img.shields.io/badge/devel-0.3.0.9000-brightgreen.svg)](https://github.com/verasls/impactr/)
[![R-CMD-check](https://github.com/verasls/impactr/workflows/R-CMD-check/badge.svg)](https://github.com/verasls/impactr/actions)
[![Codecov test
coverage](https://codecov.io/gh/verasls/impactr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/verasls/impactr?branch=main)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
<!-- badges: end -->

`impactr` is a package with functions to read, process and analyse raw
accelerometer data related to mechanical loading variables. You can
learn more about this package features and how to use it in
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

## Usage

``` r
library(impactr)

read_acc(impactr_example("hip-raw.csv")) |>
 define_region(
    start_time = "2021-04-06 15:45:00",
    end_time = "2021-04-06 15:45:30"
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
#> # Start time:              2021-04-06 15:43:00
#> # Sampling frequency:      100Hz
#> # Accelerometer placement: Hip
#> # Subject body mass:       78kg
#> # Filter:                  Butterworth (4th-ord, low-pass, 20Hz)
#> # Data dimensions:         26 × 3
#>    timestamp           resultant_peak_acc resultant_peak_grf
#>    <dttm>                           <dbl>              <dbl>
#>  1 2021-04-06 15:45:00               1.32              1387.
#>  2 2021-04-06 15:45:01               1.36              1392.
#>  3 2021-04-06 15:45:04               1.30              1384.
#>  4 2021-04-06 15:45:04               2.32              1521.
#>  5 2021-04-06 15:45:05               1.50              1412.
#>  6 2021-04-06 15:45:06               1.68              1435.
#>  7 2021-04-06 15:45:06               1.51              1412.
#>  8 2021-04-06 15:45:07               1.96              1473.
#>  9 2021-04-06 15:45:08               1.37              1393.
#> 10 2021-04-06 15:45:08               1.86              1459.
#> # … with 16 more rows
```
