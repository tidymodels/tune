# Various accessor functions

These functions return different attributes from objects with class
`tune_result`.

## Usage

``` r
.get_tune_parameters(x)

.get_tune_parameter_names(x)

.get_extra_col_names(x)

.get_tune_metrics(x)

.get_tune_metric_names(x)

.get_tune_eval_times(x)

.get_tune_eval_time_target(x)

.get_tune_outcome_names(x)

.get_tune_workflow(x)

# S3 method for class 'tune_results'
.get_fingerprint(x, ...)
```

## Arguments

- x:

  An object of class `tune_result`.

## Value

- `.get_tune_parameters()` returns a `dials` `parameter` object or a
  tibble.

- `.get_tune_parameter_names()`, `.get_tune_metric_names()`, and
  `.get_tune_outcome_names()` return a character string.

- `.get_tune_metrics()` returns a metric set or NULL.

- `.get_tune_workflow()` returns the workflow used to fit the resamples
  (if `save_workflow` was set to `TRUE` during fitting) or NULL.
