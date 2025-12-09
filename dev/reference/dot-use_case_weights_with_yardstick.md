# Determine if case weights should be passed on to yardstick

This S3 method defines the logic for deciding when a case weight vector
should be passed to yardstick metric functions and used to measure model
performance. The current logic is that frequency weights (i.e.
[`hardhat::frequency_weights()`](https://hardhat.tidymodels.org/reference/frequency_weights.html))
are the only situation where this should occur.

## Usage

``` r
.use_case_weights_with_yardstick(x)

# S3 method for class 'hardhat_importance_weights'
.use_case_weights_with_yardstick(x)

# S3 method for class 'hardhat_frequency_weights'
.use_case_weights_with_yardstick(x)
```

## Arguments

- x:

  A vector

## Value

A single `TRUE` or `FALSE`.

## Examples

``` r
library(parsnip)
library(dplyr)

frequency_weights(1:10) |>
  .use_case_weights_with_yardstick()
#> [1] TRUE

importance_weights(seq(1, 10, by = .1))|>
  .use_case_weights_with_yardstick()
#> [1] FALSE
```
