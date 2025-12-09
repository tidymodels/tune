# Control aspects of the Bayesian search process

Control aspects of the Bayesian search process

## Usage

``` r
control_bayes(
  verbose = FALSE,
  verbose_iter = FALSE,
  no_improve = 10L,
  uncertain = Inf,
  seed = sample.int(10^5, 1),
  extract = NULL,
  save_pred = FALSE,
  time_limit = NA,
  pkgs = NULL,
  save_workflow = FALSE,
  save_gp_scoring = FALSE,
  event_level = "first",
  parallel_over = NULL,
  backend_options = NULL,
  allow_par = TRUE,
  workflow_size = 100
)
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

- verbose_iter:

  A logical for logging results of the Bayesian search process. Defaults
  to FALSE. If using a dark IDE theme, some logging messages might be
  hard to see; try setting the `tidymodels.dark` option with
  `options(tidymodels.dark = TRUE)` to print lighter colors.

- no_improve:

  The integer cutoff for the number of iterations without better
  results.

- uncertain:

  The number of iterations with no improvement before an uncertainty
  sample is created where a sample with high predicted variance is
  chosen (i.e., in a region that has not yet been explored). The
  iteration counter is reset after each uncertainty sample. For example,
  if `uncertain = 10`, this condition is triggered every 10 samples with
  no improvement.

- seed:

  An integer for controlling the random number stream. Tuning functions
  are sensitive to both the state of RNG set outside of tuning functions
  with [`set.seed()`](https://rdrr.io/r/base/Random.html) as well as the
  value set here. The value of the former determines RNG for the
  higher-level tuning process, like grid generation and setting the
  value of this argument if left as default. The value of this argument
  determines RNG state in workers for each iteration of model fitting,
  determined by the value of `parallel_over`.

- extract:

  An optional function with at least one argument (or `NULL`) that can
  be used to retain arbitrary objects from the model fit object, recipe,
  or other elements of the workflow.

- save_pred:

  A logical for whether the out-of-sample predictions should be saved
  for each model *evaluated*.

- time_limit:

  A number for the minimum number of *minutes* (elapsed) that the
  function should execute. The elapsed time is evaluated at internal
  checkpoints and, if over time, the results at that time are returned
  (with a warning). This means that the `time_limit` is not an exact
  limit, but a minimum time limit.

  Note that timing begins immediately on execution. Thus, if the
  `initial` argument to
  [`tune_bayes()`](https://tune.tidymodels.org/dev/reference/tune_bayes.md)
  is supplied as a number, the elapsed time will include the time needed
  to generate initialization results.

- pkgs:

  An optional character string of R package names that should be loaded
  (by namespace) during parallel processing.

- save_workflow:

  A logical for whether the workflow should be appended to the output as
  an attribute.

- save_gp_scoring:

  A logical to save the intermediate Gaussian process models for each
  iteration of the search. These are saved to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) with names
  `gp_candidates_{i}.RData` where `i` is the iteration. These results
  are deleted when the R session ends. This option is only useful for
  teaching purposes.

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
  [`tune::new_backend_options()`](https://tune.tidymodels.org/dev/reference/control_grid.md),
  used to pass arguments to specific tuning backend. Defaults to `NULL`
  for default backend options.

- allow_par:

  A logical to allow parallel processing (if a parallel backend is
  registered).

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

## Hyperparameters and extracted objects

When making use of submodels, tune can generate predictions and
calculate metrics for multiple model `.config`urations using only one
model fit. However, this means that if a function was supplied to a
[control
function's](https://tune.tidymodels.org/dev/reference/control_grid.md)
`extract` argument, tune can only execute that extraction on the one
model that was fitted. As a result, in the
[`collect_extracts()`](https://tune.tidymodels.org/dev/reference/collect_predictions.md)
output, tune opts to associate the extracted objects with the
hyperparameter combination used to fit that one model workflow, rather
than the hyperparameter combination of a submodel. In the output, this
appears like a hyperparameter entry is recycled across many `.config`
entries—this is intentional.

See <https://parsnip.tidymodels.org/articles/Submodels.html> to learn
more about submodels.
