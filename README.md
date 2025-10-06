
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{nflendzoneApp}`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/TylerPollard410/nflendzoneApp/graph/badge.svg)](https://app.codecov.io/gh/TylerPollard410/nflendzoneApp)
<!-- badges: end -->

## Installation

You can install the development version of `{nflendzoneApp}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
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
#> [1] "2025-10-06 09:12:47 EDT"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading nflendzoneApp
#> ── R CMD check results ──────────────────────────────── nflendzoneApp 0.1.0 ────
#> Duration: 3.8s
#> 
#> ❯ checking package dependencies ... ERROR
#>   VignetteBuilder package not declared: ‘quarto’
#>   
#>   See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
#>   manual.
#> 
#> 1 error ✖ | 0 warnings ✔ | 0 notes ✔
#> Error: R CMD check found ERRORs
```

``` r
covr::package_coverage()
#> nflendzoneApp Coverage: 96.92%
#> R/run_app.R: 0.00%
#> R/app_config.R: 100.00%
#> R/app_ui.R: 100.00%
#> R/golem_utils_server.R: 100.00%
#> R/golem_utils_ui.R: 100.00%
#> R/mod_standings.R: 100.00%
```
