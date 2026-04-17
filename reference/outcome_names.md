# Determine names of the outcome data in a workflow

Determine names of the outcome data in a workflow

## Usage

``` r
outcome_names(x, ...)

# S3 method for class 'terms'
outcome_names(x, ...)

# S3 method for class 'formula'
outcome_names(x, ...)

# S3 method for class 'recipe'
outcome_names(x, ...)

# S3 method for class 'workflow'
outcome_names(x, ...)

# S3 method for class 'tune_results'
outcome_names(x, ...)

# S3 method for class 'workflow_variables'
outcome_names(x, data = NULL, ...)
```

## Arguments

- x:

  An object.

- ...:

  Not used.

## Value

A character string of variable names

## Examples

``` r
library(dplyr)
lm(cbind(mpg, wt) ~ ., data = mtcars) |>
  purrr::pluck(terms) |>
  outcome_names()
#> [1] "mpg" "wt" 
```
