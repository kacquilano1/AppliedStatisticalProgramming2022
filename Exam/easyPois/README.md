
<!-- README.md is generated from README.Rmd. Please edit that file -->

# easyPois

<!-- badges: start -->
<!-- badges: end -->

The goal of easyPois is to …

## Installation

You can install the development version of easyPois from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("johnsontr/AppliedStatisticalProgramming2022")
```

## Example


This is a basic example which shows a Poisson estimate using the basic s.e. calculation, and using the bootstrap calculation

```{r example}
library(easyPois)
estimatePois(y = 1:10, SEtype = "bootstrap", B = 10)
estimatePois(y = 1:10, SEtype = "basic")
```

The function estimatePois will output numerical estimations for the Poisson distribution

This package works by taking in the following arguments:
y = A vector of data.
SEtype = The type of standard errors to be calculated: "basic" or "bootstrap".
B = The number of bootstrapped samples.

estimatePois uses these values to calculate the maximium likelihood (MLE) for lambda using the inputted data. It then uses that value to calculate the log likelihood for the data, conditioned on the lambda calculated as MLE. It calculated the standard errors for the data using either a basic calculation or bootstrapping the calculation, depending on the argument choice.

estimatePois returns an object of the class PoisMLE.
This object has the slots:
y = The original vector of data
MLE = The maximum likelihood estimator for lambda.
LL = The log likelihood for the observed data.
SE = The standard error.
SEtype = The type of standard errors calculated: "basic" or "bootstrap".

the log likelihood of the inputted data. It
