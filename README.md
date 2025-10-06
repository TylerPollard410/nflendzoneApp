
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nflendzoneApp

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- [![Codecov test coverage](https://codecov.io/gh/TylerPollard410/nflendzoneApp/graph/badge.svg)](https://app.codecov.io/gh/TylerPollard410/nflendzoneApp?branch=main) -->
<!-- [![R-CMD-check](https://github.com/TylerPollard410/nflendzoneApp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/TylerPollard410/nflendzoneApp/actions/workflows/R-CMD-check.yaml) -->
<!-- badges: end -->

## Installation

You can install the development version of `{nflendzoneApp}` from
[GitHub](https://github.com/TylerPollard410/nflendzoneApp):

``` r
# install.packages("pak")
pak::pak("TylerPollard410/nflendzoneApp")
```

## Run

You can launch the application by running:

``` r
nflendzoneApp::run_app()
```

## About

You are reading the doc about version : 0.1.0

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-10-06 15:55:11 EDT"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading nflendzoneApp
#> ── R CMD check results ──────────────────────────────── nflendzoneApp 0.1.0 ────
#> Duration: 42s
#> 
#> ❯ checking dependencies in R code ... NOTE
#>   Namespace in Imports field not imported from: ‘bs4Dash’
#>     All declared Imports should be used.
#> 
#> 0 errors ✔ | 0 warnings ✔ | 1 note ✖
```

``` r
covr::package_coverage()
#> nflendzoneApp Coverage: 95.32%
#> R/mod_team_rankings.R: 0.00%
#> R/run_app.R: 0.00%
#> R/app_config.R: 100.00%
#> R/app_ui.R: 100.00%
#> R/golem_utils_server.R: 100.00%
#> R/golem_utils_ui.R: 100.00%
#> R/mod_standings.R: 100.00%
```
