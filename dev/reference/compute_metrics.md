# Calculate and format metrics from tuning functions

This function computes metrics from tuning results. The arguments and
output formats are closely related to those from
[`collect_metrics()`](https://tune.tidymodels.org/dev/reference/collect_predictions.md),
but this function additionally takes a `metrics` argument with a [metric
set](https://yardstick.tidymodels.org/reference/metric_set.html) for new
metrics to compute. This allows for computing new performance metrics
without requiring users to re-evaluate models against resamples.

Note that the [control
option](https://tune.tidymodels.org/dev/reference/control_grid.md)
`save_pred = TRUE` must have been supplied when generating `x`.

## Usage

``` r
compute_metrics(x, metrics, summarize, event_level, ...)

# Default S3 method
compute_metrics(x, metrics, summarize = TRUE, event_level = "first", ...)

# S3 method for class 'tune_results'
compute_metrics(x, metrics, ..., summarize = TRUE, event_level = "first")
```

## Arguments

- x:

  The results of a tuning function like
  [`tune_grid()`](https://tune.tidymodels.org/dev/reference/tune_grid.md)
  or
  [`fit_resamples()`](https://tune.tidymodels.org/dev/reference/fit_resamples.md),
  generated with the control option `save_pred = TRUE`.

- metrics:

  A [metric
  set](https://yardstick.tidymodels.org/reference/metric_set.html) of
  new metrics to compute. See the "Details" section below for more
  information.

- summarize:

  A single logical value indicating whether metrics should be summarized
  over resamples (`TRUE`) or return the values for each individual
  resample. See
  [`collect_metrics()`](https://tune.tidymodels.org/dev/reference/collect_predictions.md)
  for more details on how metrics are summarized.

- event_level:

  A single string containing either `"first"` or `"second"`. This
  argument is passed on to yardstick metric functions when any type of
  class prediction is made, and specifies which level of the outcome is
  considered the "event".

- ...:

  Not currently used.

## Value

A tibble. See
[`collect_metrics()`](https://tune.tidymodels.org/dev/reference/collect_predictions.md)
for more details on the return value.

## Details

Each metric in the set supplied to the `metrics` argument must have a
metric type (usually `"numeric"`, `"class"`, or `"prob"`) that matches
some metric evaluated when generating `x`. e.g. For example, if `x` was
generated with only hard `"class"` metrics, this function can't compute
metrics that take in class probabilities (`"prob"`.) By default, the
tuning functions used to generate `x` compute metrics of all needed
types.

## Examples

``` r
# load needed packages:
library(parsnip)
library(rsample)
library(yardstick)

# evaluate a linear regression against resamples.
# note that we pass `save_pred = TRUE`:
res <-
  fit_resamples(
    linear_reg(),
    mpg ~ cyl + hp,
    bootstraps(mtcars, 5),
    control = control_grid(save_pred = TRUE)
  )

# to return the metrics supplied to `fit_resamples()`:
collect_metrics(res)
#> # A tibble: 2 × 6
#>   .metric .estimator  mean     n std_err .config        
#>   <chr>   <chr>      <dbl> <int>   <dbl> <chr>          
#> 1 rmse    standard   3.37      5  0.206  pre0_mod0_post0
#> 2 rsq     standard   0.731     5  0.0199 pre0_mod0_post0

# to compute new metrics:
compute_metrics(res, metric_set(mae))
#> # A tibble: 1 × 6
#>   .metric .estimator  mean     n std_err .config        
#>   <chr>   <chr>      <dbl> <int>   <dbl> <chr>          
#> 1 mae     standard    2.72     5   0.251 pre0_mod0_post0

# if `metrics` is the same as that passed to `fit_resamples()`,
# then `collect_metrics()` and `compute_metrics()` give the same
# output, though `compute_metrics()` is quite a bit slower:
all.equal(
  collect_metrics(res),
  compute_metrics(res, metric_set(rmse, rsq))
)
#> [1] TRUE
```
