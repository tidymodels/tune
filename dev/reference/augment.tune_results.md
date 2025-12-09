# Augment data with holdout predictions

For `tune` objects that use resampling, these
[`augment()`](https://generics.r-lib.org/reference/augment.html) methods
will add one or more columns for the hold-out predictions (i.e. from the
assessment set(s)).

## Usage

``` r
# S3 method for class 'tune_results'
augment(x, ..., parameters = NULL)

# S3 method for class 'resample_results'
augment(x, ...)

# S3 method for class 'last_fit'
augment(x, ...)
```

## Arguments

- x:

  An object resulting from one of the `tune_*()` functions,
  [`fit_resamples()`](https://tune.tidymodels.org/dev/reference/fit_resamples.md),
  or
  [`last_fit()`](https://tune.tidymodels.org/dev/reference/last_fit.md).
  The control specifications for these objects should have used the
  option `save_pred = TRUE`.

- ...:

  Not currently used.

- parameters:

  A data frame with a single row that indicates what tuning parameters
  should be used to generate the predictions (for `tune_*()` objects
  only). If `NULL`, `select_best(x)` will be used with the first metric
  and, if applicable, the first evaluation time point, used to create
  `x`.

## Value

A data frame with one or more additional columns for model predictions.

## Details

For some resampling methods where rows may be replicated in multiple
assessment sets, the prediction columns will be averages of the holdout
results. Also, for these methods, it is possible that all rows of the
original data do not have holdout predictions (like a single bootstrap
resample). In this case, all rows are return and a warning is issued.

For objects created by
[`last_fit()`](https://tune.tidymodels.org/dev/reference/last_fit.md),
the test set data and predictions are returned.

Unlike other
[`augment()`](https://generics.r-lib.org/reference/augment.html)
methods, the predicted values for regression models are in a column
called `.pred` instead of `.fitted` (to be consistent with other
tidymodels conventions).

For regression problems, an additional `.resid` column is added to the
results.
