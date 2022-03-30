
<!-- README.md is generated from README.Rmd. Please edit that file -->

# integrateIt

<!-- badges: start -->
<!-- badges: end -->

The goal of integrateIt is to approximate numerical integrations using
the Trapezoid Rule and Simpson’s Rule.

## Installation

You can install the development version of integrateIt from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("johnsontr/AppliedStatisticalProgramming2022")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(integrateIt)
f <- function(x) {sin(x)}
Simpson(a = 0, b = 10, d = 5, f)
#> [1] 1.627079
Trapezoid(a = 0, b = 10, d = 5, f)
#> [1] 1.180854
```

The functions will output a numerical estimation for the definite
integral approximations.
