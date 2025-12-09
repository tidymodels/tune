# Investigate best tuning parameters

`show_best()` displays the top sub-models and their performance
estimates.

`select_best()` finds the tuning parameter combination with the best
performance values.

`select_by_one_std_err()` uses the "one-standard error rule" (Breiman
\_el at, 1984) that selects the most simple model that is within one
standard error of the numerically optimal results.

`select_by_pct_loss()` selects the most simple model whose loss of
performance is within some acceptable limit.

## Usage

``` r
show_best(x, ...)

# Default S3 method
show_best(x, ...)

# S3 method for class 'tune_results'
show_best(
  x,
  ...,
  metric = NULL,
  eval_time = NULL,
  n = 5,
  call = rlang::current_env()
)

select_best(x, ...)

# Default S3 method
select_best(x, ...)

# S3 method for class 'tune_results'
select_best(x, ..., metric = NULL, eval_time = NULL)

select_by_pct_loss(x, ...)

# Default S3 method
select_by_pct_loss(x, ...)

# S3 method for class 'tune_results'
select_by_pct_loss(x, ..., metric = NULL, eval_time = NULL, limit = 2)

select_by_one_std_err(x, ...)

# Default S3 method
select_by_one_std_err(x, ...)

# S3 method for class 'tune_results'
select_by_one_std_err(x, ..., metric = NULL, eval_time = NULL)
```

## Arguments

- x:

  The results of
  [`tune_grid()`](https://tune.tidymodels.org/dev/reference/tune_grid.md)
  or
  [`tune_bayes()`](https://tune.tidymodels.org/dev/reference/tune_bayes.md).

- ...:

  For `select_by_one_std_err()` and `select_by_pct_loss()`, this
  argument is passed directly to
  [`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)
  so that the user can sort the models from *most simple to most
  complex*. That is, for a parameter `p`, pass the unquoted expression
  `p` if smaller values of `p` indicate a simpler model, or `desc(p)` if
  larger values indicate a simpler model. At least one term is required
  for these two functions. See the examples below.

- metric:

  A character value for the metric that will be used to sort the models.
  (See <https://yardstick.tidymodels.org/articles/metric-types.html> for
  more details). Not required if a single metric exists in `x`. If there
  are multiple metric and none are given, the first in the metric set is
  used (and a warning is issued).

- eval_time:

  A single numeric time point where dynamic event time metrics should be
  chosen (e.g., the time-dependent ROC curve, etc). The values should be
  consistent with the values used to create `x`. The `NULL` default will
  automatically use the first evaluation time used by `x`.

- n:

  An integer for the number of top results/rows to return.

- call:

  The call to be shown in errors and warnings.

- limit:

  The limit of loss of performance that is acceptable (in percent
  units). See details below.

## Value

A tibble with columns for the parameters. `show_best()` also includes
columns for performance metrics.

## Details

For percent loss, suppose the best model has an RMSE of 0.75 and a
simpler model has an RMSE of 1. The percent loss would be
`(1.00 - 0.75)/1.00 * 100`, or 25 percent. Note that loss will always be
non-negative.

## References

Breiman, Leo; Friedman, J. H.; Olshen, R. A.; Stone, C. J. (1984).
*Classification and Regression Trees.* Monterey, CA: Wadsworth.

## Examples

``` r
data("example_ames_knn")

show_best(ames_iter_search, metric = "rmse")
#> # A tibble: 5 × 12
#>       K weight_func dist_power   lon   lat .metric .estimator   mean
#>   <int> <chr>            <dbl> <int> <int> <chr>   <chr>       <dbl>
#> 1    33 triweight        0.511    10     3 rmse    standard   0.0727
#> 2    21 triweight        0.626     1     4 rmse    standard   0.0729
#> 3    50 triweight        0.597     6    11 rmse    standard   0.0733
#> 4    32 triweight        0.809    15    10 rmse    standard   0.0735
#> 5     6 inv              0.337     3     2 rmse    standard   0.0737
#> # ℹ 4 more variables: n <int>, std_err <dbl>, .config <chr>,
#> #   .iter <int>

select_best(ames_iter_search, metric = "rsq")
#> # A tibble: 1 × 6
#>       K weight_func dist_power   lon   lat .config          
#>   <int> <chr>            <dbl> <int> <int> <chr>            
#> 1    33 triweight        0.511    10     3 pre08_mod07_post0

# To find the least complex model within one std error of the numerically
# optimal model, the number of nearest neighbors are sorted from the largest
# number of neighbors (the least complex class boundary) to the smallest
# (corresponding to the most complex model).

select_by_one_std_err(ames_grid_search, metric = "rmse", desc(K))
#> # A tibble: 1 × 6
#>       K weight_func dist_power   lon   lat .config          
#>   <int> <chr>            <dbl> <int> <int> <chr>            
#> 1    33 triweight        0.511    10     3 pre08_mod07_post0

# Now find the least complex model that has no more than a 5% loss of RMSE:
select_by_pct_loss(
  ames_grid_search,
  metric = "rmse",
  limit = 5, desc(K)
)
#> # A tibble: 1 × 6
#>       K weight_func dist_power   lon   lat .config          
#>   <int> <chr>            <dbl> <int> <int> <chr>            
#> 1    33 triweight        0.511    10     3 pre08_mod07_post0
```
