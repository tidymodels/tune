# Bootstrap confidence intervals for performance metrics

Using out-of-sample predictions, the bootstrap is used to create
percentile confidence intervals.

## Usage

``` r
# S3 method for class 'tune_results'
int_pctl(
  .data,
  metrics = NULL,
  eval_time = NULL,
  times = 1001,
  parameters = NULL,
  alpha = 0.05,
  allow_par = TRUE,
  event_level = "first",
  keep_replicates = FALSE,
  ...
)
```

## Arguments

- .data:

  A object with class `tune_results` where the `save_pred = TRUE` option
  was used in the control function.

- metrics:

  A
  [`yardstick::metric_set()`](https://yardstick.tidymodels.org/reference/metric_set.html).
  By default, it uses the same metrics as the original object.

- eval_time:

  A vector of evaluation times for censored regression models. `NULL` is
  appropriate otherwise. If `NULL` is used with censored models, a
  evaluation time is selected, and a warning is issued.

- times:

  The number of bootstrap samples.

- parameters:

  An optional tibble of tuning parameter values that can be used to
  filter the predicted values before processing. This tibble should only
  have columns for each tuning parameter identifier (e.g. `"my_param"`
  if `tune("my_param")` was used).

- alpha:

  Level of significance.

- allow_par:

  A logical to allow parallel processing (if a parallel backend is
  registered).

- event_level:

  A single string. Either `"first"` or `"second"` to specify which level
  of truth to consider as the "event".

- keep_replicates:

  A logic for saving the individual estimates from each bootstrap sample
  (as a list column called `.values`).

- ...:

  Not currently used.

## Value

A tibble of metrics with additional columns for `.lower` and `.upper`
(and potentially, `.values`).

## Details

For each model configuration (if any), this function takes bootstrap
samples of the out-of-sample predicted values. For each bootstrap
sample, the metrics are computed and these are used to compute
confidence intervals. See
[`rsample::int_pctl()`](https://rsample.tidymodels.org/reference/int_pctl.html)
and the references therein for more details.

Note that the `.estimate` column is likely to be different from the
results given by
[`collect_metrics()`](https://tune.tidymodels.org/dev/reference/collect_predictions.md)
since a different estimator is used. Since random numbers are used in
sampling, set the random number seed prior to running this function.

The number of bootstrap samples should be large to have reliable
intervals. The defaults reflect the fewest samples that should be used.

The computations for each configuration can be extensive. To increase
computational efficiency parallel processing can be used. The future
package is used here. To execute the resampling iterations in parallel,
specify a [plan](https://future.futureverse.org/reference/plan.html)
with future first. The `allow_par` argument can be used to avoid
parallelism.

Also, if a censored regression model used numerous evaluation times, the
computations can take a long time unless the times are filtered with the
`eval_time` argument.

## References

Davison, A., & Hinkley, D. (1997). *Bootstrap Methods and their
Application*. Cambridge: Cambridge University Press.
doi:10.1017/CBO9780511802843

## See also

[`rsample::int_pctl()`](https://rsample.tidymodels.org/reference/int_pctl.html)

## Examples

``` r
if (rlang::is_installed("modeldata")) {
  data(Sacramento, package = "modeldata")
  library(rsample)
  library(parsnip)

  set.seed(13)
  sac_rs <- vfold_cv(Sacramento)

  lm_res <-
    linear_reg() |>
    fit_resamples(
      log10(price) ~ beds + baths + sqft + type + latitude + longitude,
      resamples = sac_rs,
      control = control_resamples(save_pred = TRUE)
    )

  set.seed(31)
  int_pctl(lm_res)
}
#> # A tibble: 2 × 6
#>   .metric .estimator .lower .estimate .upper .config        
#>   <chr>   <chr>       <dbl>     <dbl>  <dbl> <chr>          
#> 1 rmse    bootstrap   0.141     0.151  0.160 pre0_mod0_post0
#> 2 rsq     bootstrap   0.521     0.565  0.607 pre0_mod0_post0
```
