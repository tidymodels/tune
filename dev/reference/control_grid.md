# Control aspects of the grid search process

Control aspects of the grid search process

## Usage

``` r
control_grid(
  verbose = FALSE,
  allow_par = TRUE,
  extract = NULL,
  save_pred = FALSE,
  pkgs = NULL,
  save_workflow = FALSE,
  event_level = "first",
  parallel_over = NULL,
  backend_options = NULL,
  workflow_size = 100
)

control_resamples(
  verbose = FALSE,
  allow_par = TRUE,
  extract = NULL,
  save_pred = FALSE,
  pkgs = NULL,
  save_workflow = FALSE,
  event_level = "first",
  parallel_over = NULL,
  backend_options = NULL,
  workflow_size = 100
)

new_backend_options(..., class = character())
```

## Arguments

- verbose:

  A logical for logging results (other than warnings and errors, which
  are always shown) as they are generated during training in a single R
  process. When using most parallel backends, this argument typically
  will not result in any logging. If using a dark IDE theme, some
  logging messages might be hard to see; try setting the
  `tidymodels.dark` option with `options(tidymodels.dark = TRUE)` to
  print lighter colors.

- allow_par:

  A logical to allow parallel processing (if a parallel backend is
  registered).

- extract:

  An optional function with at least one argument (or `NULL`) that can
  be used to retain arbitrary objects from the model fit object, recipe,
  or other elements of the workflow.

- save_pred:

  A logical for whether the out-of-sample predictions should be saved
  for each model *evaluated*.

- pkgs:

  An optional character string of R package names that should be loaded
  (by namespace) during parallel processing.

- save_workflow:

  A logical for whether the workflow should be appended to the output as
  an attribute.

- event_level:

  A single string containing either `"first"` or `"second"`. This
  argument is passed on to yardstick metric functions when any type of
  class prediction is made, and specifies which level of the outcome is
  considered the "event".

- parallel_over:

  A single string containing either `"resamples"` or `"everything"`
  describing how to use parallel processing. Alternatively, `NULL` is
  allowed, which chooses between `"resamples"` and `"everything"`
  automatically.

  If `"resamples"`, then tuning will be performed in parallel over
  resamples alone. Within each resample, the preprocessor (i.e. recipe
  or formula) is processed once, and is then reused across all models
  that need to be fit.

  If `"everything"`, then tuning will be performed in parallel at two
  levels. An outer parallel loop will iterate over resamples.
  Additionally, an inner parallel loop will iterate over all unique
  combinations of preprocessor and model tuning parameters for that
  specific resample. This will result in the preprocessor being
  re-processed multiple times, but can be faster if that processing is
  extremely fast.

  If `NULL`, chooses `"resamples"` if there are more than one resample,
  otherwise chooses `"everything"` to attempt to maximize core
  utilization.

  Note that switching between `parallel_over` strategies is not
  guaranteed to use the same random number generation schemes. However,
  re-tuning a model using the same `parallel_over` strategy is
  guaranteed to be reproducible between runs.

- backend_options:

  An object of class `"tune_backend_options"` as created by
  `tune::new_backend_options()`, used to pass arguments to specific
  tuning backend. Defaults to `NULL` for default backend options.

- workflow_size:

  A non-negative number (in MB) that is used as a threshold for a
  warning regarding the size of the workflow. Only used when
  `save_workflow = TRUE`.

## Details

For `extract`, this function can be used to output the model object, the
recipe (if used), or some components of either or both. When evaluated,
the function's sole argument has a fitted workflow If the formula method
is used, the recipe element will be `NULL`.

The results of the `extract` function are added to a list column in the
output called `.extracts`. Each element of this list is a tibble with
tuning parameter column and a list column (also called `.extracts`) that
contains the results of the function. If no extraction function is used,
there is no `.extracts` column in the resulting object. See
[`tune_bayes()`](https://tune.tidymodels.org/dev/reference/tune_bayes.md)
for more specific details.

Note that for
[`collect_predictions()`](https://tune.tidymodels.org/dev/reference/collect_predictions.md),
it is possible that each row of the original data point might be
represented multiple times per tuning parameter. For example, if the
bootstrap or repeated cross-validation are used, there will be multiple
rows since the sample data point has been evaluated multiple times. This
may cause issues when merging the predictions with the original data.

`control_resamples()` is an alias for `control_grid()` and is meant to
be used with
[`fit_resamples()`](https://tune.tidymodels.org/dev/reference/fit_resamples.md).

## Hyperparameters and extracted objects

When making use of submodels, tune can generate predictions and
calculate metrics for multiple model `.config`urations using only one
model fit. However, this means that if a function was supplied to a
control function's `extract` argument, tune can only execute that
extraction on the one model that was fitted. As a result, in the
[`collect_extracts()`](https://tune.tidymodels.org/dev/reference/collect_predictions.md)
output, tune opts to associate the extracted objects with the
hyperparameter combination used to fit that one model workflow, rather
than the hyperparameter combination of a submodel. In the output, this
appears like a hyperparameter entry is recycled across many `.config`
entries—this is intentional.

See <https://parsnip.tidymodels.org/articles/Submodels.html> to learn
more about submodels.
