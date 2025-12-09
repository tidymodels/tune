# Tools for selecting metrics and evaluation times

Tools for selecting metrics and evaluation times

## Usage

``` r
choose_metric(x, metric, ..., call = rlang::caller_env())

check_metric_in_tune_results(mtr_info, metric, ..., call = rlang::caller_env())

choose_eval_time(
  x,
  metric,
  ...,
  eval_time = NULL,
  quietly = FALSE,
  call = rlang::caller_env()
)

maybe_choose_eval_time(x, mtr_set, eval_time)

first_metric(mtr_set)

first_eval_time(
  mtr_set,
  ...,
  metric = NULL,
  eval_time = NULL,
  quietly = FALSE,
  call = rlang::caller_env()
)

.filter_perf_metrics(x, metric, eval_time)

check_metrics_arg(mtr_set, wflow, ..., call = rlang::caller_env())

check_eval_time_arg(eval_time, mtr_set, ..., call = rlang::caller_env())
```

## Arguments

- x:

  An object with class `tune_results`.

- metric:

  A character value for which metric is being used.

- ...:

  These dots are for future extensions and must be empty.

- call:

  The call to be displayed in warnings or errors.

- eval_time:

  An optional vector of times to compute dynamic and/or integrated
  metrics.

- quietly:

  Logical. Should warnings be muffled?

- mtr_set:

  A
  [`yardstick::metric_set()`](https://yardstick.tidymodels.org/reference/metric_set.html).

- wflow:

  A
  [`workflows::workflow()`](https://workflows.tidymodels.org/reference/workflow.html).

## Details

These are developer-facing functions used to compute and validate
choices for performance metrics. For survival analysis models, there are
similar functions for the evaluation time(s) required for dynamic and/or
integrated metrics.

`choose_metric()` is used with functions such as
[`show_best()`](https://tune.tidymodels.org/dev/reference/show_best.md)
or
[`select_best()`](https://tune.tidymodels.org/dev/reference/show_best.md)
where a single valid metric is required to rank models. If no value is
given by the user, the first metric value is used (with a warning).

For evaluation times, one is only required when the metric type is
dynamic (e.g.
[`yardstick::brier_survival()`](https://yardstick.tidymodels.org/reference/brier_survival.html)
or
[`yardstick::roc_auc_survival()`](https://yardstick.tidymodels.org/reference/roc_auc_survival.html)).
For these metrics, we require a single numeric value that was originally
given to the function used to produce `x` (such as
[`tune_grid()`](https://tune.tidymodels.org/dev/reference/tune_grid.md)).

If a time is required and none is given, the first value in the vector
originally given in the `eval_time` argument is used (with a warning).

`maybe_choose_eval_time()` is for cases where multiple evaluation times
are acceptable but you need to choose a good default. The "maybe" is
because the function that would use `maybe_choose_eval_time()` can
accept multiple metrics (like
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)).
