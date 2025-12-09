# Remove some tuning parameter results

For objects produced by the `tune_*()` functions, there may only be a
subset of tuning parameter combinations of interest. For large data
sets, it might be helpful to be able to remove some results. This
function trims the `.metrics` column of unwanted results as well as
columns `.predictions` and `.extracts` (if they were requested).

## Usage

``` r
filter_parameters(x, ..., parameters = NULL)
```

## Arguments

- x:

  An object of class `tune_results` that has multiple tuning parameters.

- ...:

  Expressions that return a logical value, and are defined in terms of
  the tuning parameter values. If multiple expressions are included,
  they are combined with the `&` operator. Only rows for which all
  conditions evaluate to `TRUE` are kept.

- parameters:

  A tibble of tuning parameter values that can be used to filter the
  predicted values before processing. This tibble should only have
  columns for tuning parameter identifiers (e.g. `"my_param"` if
  `tune("my_param")` was used). There can be multiple rows and one or
  more columns. **If used, this parameter must be named.**

## Value

A version of `x` where the lists columns only retain the parameter
combinations in `parameters` or satisfied by the filtering logic.

## Details

Removing some parameter combinations might affect the results of
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
for the object.

## Examples

``` r
library(dplyr)
library(tibble)

# For grid search:
data("example_ames_knn")

## -----------------------------------------------------------------------------
# select all combinations using the 'rank' weighting scheme

ames_grid_search |>
  collect_metrics()
#> # A tibble: 20 × 11
#>        K weight_func  dist_power   lon   lat .metric .estimator   mean
#>    <int> <chr>             <dbl> <int> <int> <chr>   <chr>       <dbl>
#>  1    21 triweight         0.626     1     4 rmse    standard   0.0729
#>  2    21 triweight         0.626     1     4 rsq     standard   0.833 
#>  3     5 gaussian          0.411     2     7 rmse    standard   0.0742
#>  4     5 gaussian          0.411     2     7 rsq     standard   0.826 
#>  5    35 gaussian          1.29      3    13 rmse    standard   0.0791
#>  6    35 gaussian          1.29      3    13 rsq     standard   0.814 
#>  7    12 epanechnikov      1.53      4     7 rmse    standard   0.0762
#>  8    12 epanechnikov      1.53      4     7 rsq     standard   0.817 
#>  9    35 rank              1.32      8     1 rmse    standard   0.0791
#> 10    35 rank              1.32      8     1 rsq     standard   0.813 
#> 11     4 biweight          0.311     8     4 rmse    standard   0.0783
#> 12     4 biweight          0.311     8     4 rsq     standard   0.805 
#> 13    32 triangular        0.165     9    15 rmse    standard   0.0769
#> 14    32 triangular        0.165     9    15 rsq     standard   0.818 
#> 15    33 triweight         0.511    10     3 rmse    standard   0.0727
#> 16    33 triweight         0.511    10     3 rsq     standard   0.835 
#> 17     3 gaussian          1.86     10    15 rmse    standard   0.0834
#> 18     3 gaussian          1.86     10    15 rsq     standard   0.775 
#> 19    40 triangular        0.167    11     7 rmse    standard   0.0775
#> 20    40 triangular        0.167    11     7 rsq     standard   0.815 
#> # ℹ 3 more variables: n <int>, std_err <dbl>, .config <chr>

filter_parameters(ames_grid_search, weight_func == "rank") |>
  collect_metrics()
#> # A tibble: 2 × 11
#>       K weight_func dist_power   lon   lat .metric .estimator   mean
#>   <int> <chr>            <dbl> <int> <int> <chr>   <chr>       <dbl>
#> 1    35 rank              1.32     8     1 rmse    standard   0.0791
#> 2    35 rank              1.32     8     1 rsq     standard   0.813 
#> # ℹ 3 more variables: n <int>, std_err <dbl>, .config <chr>

rank_only <- tibble::tibble(weight_func = "rank")
filter_parameters(ames_grid_search, parameters = rank_only) |>
  collect_metrics()
#> # A tibble: 2 × 11
#>       K weight_func dist_power   lon   lat .metric .estimator   mean
#>   <int> <chr>            <dbl> <int> <int> <chr>   <chr>       <dbl>
#> 1    35 rank              1.32     8     1 rmse    standard   0.0791
#> 2    35 rank              1.32     8     1 rsq     standard   0.813 
#> # ℹ 3 more variables: n <int>, std_err <dbl>, .config <chr>

## -----------------------------------------------------------------------------
# Keep only the results from the numerically best combination

ames_iter_search |>
  collect_metrics()
#> # A tibble: 40 × 12
#>        K weight_func  dist_power   lon   lat .metric .estimator   mean
#>    <int> <chr>             <dbl> <int> <int> <chr>   <chr>       <dbl>
#>  1    21 triweight         0.626     1     4 rmse    standard   0.0729
#>  2    21 triweight         0.626     1     4 rsq     standard   0.833 
#>  3     5 gaussian          0.411     2     7 rmse    standard   0.0742
#>  4     5 gaussian          0.411     2     7 rsq     standard   0.826 
#>  5    35 gaussian          1.29      3    13 rmse    standard   0.0791
#>  6    35 gaussian          1.29      3    13 rsq     standard   0.814 
#>  7    12 epanechnikov      1.53      4     7 rmse    standard   0.0762
#>  8    12 epanechnikov      1.53      4     7 rsq     standard   0.817 
#>  9    35 rank              1.32      8     1 rmse    standard   0.0791
#> 10    35 rank              1.32      8     1 rsq     standard   0.813 
#> # ℹ 30 more rows
#> # ℹ 4 more variables: n <int>, std_err <dbl>, .config <chr>,
#> #   .iter <int>

best_param <- select_best(ames_iter_search, metric = "rmse")
ames_iter_search |>
  filter_parameters(parameters = best_param) |>
  collect_metrics()
#> # A tibble: 2 × 12
#>       K weight_func dist_power   lon   lat .metric .estimator   mean
#>   <int> <chr>            <dbl> <int> <int> <chr>   <chr>       <dbl>
#> 1    33 triweight        0.511    10     3 rmse    standard   0.0727
#> 2    33 triweight        0.511    10     3 rsq     standard   0.835 
#> # ℹ 4 more variables: n <int>, std_err <dbl>, .config <chr>,
#> #   .iter <int>
```
