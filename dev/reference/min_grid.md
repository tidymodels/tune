# Determine the minimum set of model fits

[`min_grid()`](https://generics.r-lib.org/reference/min_grid.html)
determines exactly what models should be fit in order to evaluate the
entire set of tuning parameter combinations. This is for internal use
only and the API may change in the near future.

## Usage

``` r
# S3 method for class 'model_spec'
min_grid(x, grid, ...)

fit_max_value(x, grid, ...)

# S3 method for class 'boost_tree'
min_grid(x, grid, ...)

# S3 method for class 'linear_reg'
min_grid(x, grid, ...)

# S3 method for class 'logistic_reg'
min_grid(x, grid, ...)

# S3 method for class 'mars'
min_grid(x, grid, ...)

# S3 method for class 'multinom_reg'
min_grid(x, grid, ...)

# S3 method for class 'proportional_hazards'
min_grid(x, grid, ...)

# S3 method for class 'nearest_neighbor'
min_grid(x, grid, ...)

# S3 method for class 'cubist_rules'
min_grid(x, grid, ...)

# S3 method for class 'C5_rules'
min_grid(x, grid, ...)

# S3 method for class 'rule_fit'
min_grid(x, grid, ...)

# S3 method for class 'pls'
min_grid(x, grid, ...)

# S3 method for class 'poisson_reg'
min_grid(x, grid, ...)
```

## Arguments

- x:

  A model specification.

- grid:

  A tibble with tuning parameter combinations.

- ...:

  Not currently used.

## Value

A tibble with the minimum tuning parameters to fit and an additional
list column with the parameter combinations used for prediction.

## Details

`fit_max_value()` can be used in other packages to implement a
[`min_grid()`](https://generics.r-lib.org/reference/min_grid.html)
method.

## Examples

``` r
library(dplyr)
library(dials)
library(parsnip)

## -----------------------------------------------------------------------------
## No ability to exploit submodels:

svm_spec <-
  svm_poly(cost = tune(), degree = tune()) |>
  set_engine("kernlab") |>
  set_mode("regression")

svm_grid <-
  svm_spec |>
  extract_parameter_set_dials() |>
  grid_regular(levels = 3)

min_grid(svm_spec, svm_grid)
#> # A tibble: 9 × 3
#>        cost degree .submodels
#>       <dbl>  <int> <list>    
#> 1  0.000977      1 <list [0]>
#> 2  0.177         1 <list [0]>
#> 3 32             1 <list [0]>
#> 4  0.000977      2 <list [0]>
#> 5  0.177         2 <list [0]>
#> 6 32             2 <list [0]>
#> 7  0.000977      3 <list [0]>
#> 8  0.177         3 <list [0]>
#> 9 32             3 <list [0]>

## -----------------------------------------------------------------------------
## Can use submodels

xgb_spec <-
  boost_tree(trees = tune(), min_n = tune()) |>
  set_engine("xgboost") |>
  set_mode("regression")

xgb_grid <-
  xgb_spec |>
  extract_parameter_set_dials() |>
  grid_regular(levels = 3)

min_grid(xgb_spec, xgb_grid)
#> # A tibble: 3 × 3
#>   trees min_n .submodels      
#>   <int> <int> <list>          
#> 1  2000     2 <named list [1]>
#> 2  2000    21 <named list [1]>
#> 3  2000    40 <named list [1]>
```
