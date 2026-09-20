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

## Value

An S3 object of class `control_grid` (also used for `control_bayes`,
`control_resamples`, `control_last_fit`) used to contain the control
settings in a grid search as a list.

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

## Examples

``` r

library(recipes)
library(rsample)
library(parsnip)
library(workflows)

set.seed(1234)

folds <- vfold_cv(mtcars, v = 5)

recipe <- recipe(mpg ~ ., data = mtcars) |>
  step_normalize(all_numeric_predictors())

# Model with tuning parameters, used for `tune_grid()` and `tune_bayes()`.
spec <- mlp(
  hidden_units = tune(),
  penalty = tune()
) |>
  set_engine("nnet") |>
  set_mode("regression")

wf <- workflow() |>
  add_model(spec) |>
  add_recipe(recipe)

# ---------------------------------------------------------------------------
# control_grid(): Modify tuning process in `tune_grid()`.
ctrl <- control_grid(
  # save out-of-sample predictions
  save_pred = TRUE,
  # print progress while tuning
  verbose = TRUE
)

tune_grid(
  wf,
  resamples = folds,
  grid = 5,
  control = ctrl
)
#> i Fold1: preprocessor 1/1
#> i Fold1: preprocessor 1/1 (prediction data)
#> i Fold1: preprocessor 1/1, model 1/5
#> i Fold1: preprocessor 1/1, model 1/5 (predictions)
#> i Fold1: preprocessor 1/1, model 2/5
#> i Fold1: preprocessor 1/1, model 2/5 (predictions)
#> i Fold1: preprocessor 1/1, model 3/5
#> i Fold1: preprocessor 1/1, model 3/5 (predictions)
#> i Fold1: preprocessor 1/1, model 4/5
#> i Fold1: preprocessor 1/1, model 4/5 (predictions)
#> i Fold1: preprocessor 1/1, model 5/5
#> i Fold1: preprocessor 1/1, model 5/5 (predictions)
#> i Fold2: preprocessor 1/1
#> i Fold2: preprocessor 1/1 (prediction data)
#> i Fold2: preprocessor 1/1, model 1/5
#> i Fold2: preprocessor 1/1, model 1/5 (predictions)
#> i Fold2: preprocessor 1/1, model 2/5
#> i Fold2: preprocessor 1/1, model 2/5 (predictions)
#> i Fold2: preprocessor 1/1, model 3/5
#> i Fold2: preprocessor 1/1, model 3/5 (predictions)
#> i Fold2: preprocessor 1/1, model 4/5
#> i Fold2: preprocessor 1/1, model 4/5 (predictions)
#> i Fold2: preprocessor 1/1, model 5/5
#> i Fold2: preprocessor 1/1, model 5/5 (predictions)
#> i Fold3: preprocessor 1/1
#> i Fold3: preprocessor 1/1 (prediction data)
#> i Fold3: preprocessor 1/1, model 1/5
#> i Fold3: preprocessor 1/1, model 1/5 (predictions)
#> i Fold3: preprocessor 1/1, model 2/5
#> i Fold3: preprocessor 1/1, model 2/5 (predictions)
#> i Fold3: preprocessor 1/1, model 3/5
#> i Fold3: preprocessor 1/1, model 3/5 (predictions)
#> i Fold3: preprocessor 1/1, model 4/5
#> i Fold3: preprocessor 1/1, model 4/5 (predictions)
#> i Fold3: preprocessor 1/1, model 5/5
#> i Fold3: preprocessor 1/1, model 5/5 (predictions)
#> i Fold4: preprocessor 1/1
#> i Fold4: preprocessor 1/1 (prediction data)
#> i Fold4: preprocessor 1/1, model 1/5
#> i Fold4: preprocessor 1/1, model 1/5 (predictions)
#> i Fold4: preprocessor 1/1, model 2/5
#> i Fold4: preprocessor 1/1, model 2/5 (predictions)
#> i Fold4: preprocessor 1/1, model 3/5
#> i Fold4: preprocessor 1/1, model 3/5 (predictions)
#> i Fold4: preprocessor 1/1, model 4/5
#> i Fold4: preprocessor 1/1, model 4/5 (predictions)
#> i Fold4: preprocessor 1/1, model 5/5
#> i Fold4: preprocessor 1/1, model 5/5 (predictions)
#> i Fold5: preprocessor 1/1
#> i Fold5: preprocessor 1/1 (prediction data)
#> i Fold5: preprocessor 1/1, model 1/5
#> i Fold5: preprocessor 1/1, model 1/5 (predictions)
#> i Fold5: preprocessor 1/1, model 2/5
#> i Fold5: preprocessor 1/1, model 2/5 (predictions)
#> i Fold5: preprocessor 1/1, model 3/5
#> i Fold5: preprocessor 1/1, model 3/5 (predictions)
#> i Fold5: preprocessor 1/1, model 4/5
#> i Fold5: preprocessor 1/1, model 4/5 (predictions)
#> i Fold5: preprocessor 1/1, model 5/5
#> i Fold5: preprocessor 1/1, model 5/5 (predictions)
#> # Tuning results
#> # 5-fold cross-validation 
#> # A tibble: 5 × 5
#>   splits         id    .metrics          .notes           .predictions
#>   <list>         <chr> <list>            <list>           <list>      
#> 1 <split [25/7]> Fold1 <tibble [10 × 6]> <tibble [0 × 4]> <tibble>    
#> 2 <split [25/7]> Fold2 <tibble [10 × 6]> <tibble [0 × 4]> <tibble>    
#> 3 <split [26/6]> Fold3 <tibble [10 × 6]> <tibble [0 × 4]> <tibble>    
#> 4 <split [26/6]> Fold4 <tibble [10 × 6]> <tibble [0 × 4]> <tibble>    
#> 5 <split [26/6]> Fold5 <tibble [10 × 6]> <tibble [0 × 4]> <tibble>    

# ---------------------------------------------------------------------------
# control_bayes(): Modify tuning process in `tune_bayes()`.
ctrl_bayes <- control_bayes(
  save_pred = TRUE,
  verbose = TRUE
)

set.seed(3246)
tune_bayes(
  wf,
  resamples = folds,
  initial = 5,
  iter = 10,
  control = ctrl_bayes
)
#> 
#> ❯  Generating a set of 5 initial parameter results
#> ✓ Initialization complete
#> 
#> i Estimating performance
#> i Fold1: preprocessor 1/1
#> i Fold1: preprocessor 1/1 (prediction data)
#> i Fold1: preprocessor 1/1, model 1/1
#> i Fold1: preprocessor 1/1, model 1/1 (predictions)
#> i Fold2: preprocessor 1/1
#> i Fold2: preprocessor 1/1 (prediction data)
#> i Fold2: preprocessor 1/1, model 1/1
#> i Fold2: preprocessor 1/1, model 1/1 (predictions)
#> i Fold3: preprocessor 1/1
#> i Fold3: preprocessor 1/1 (prediction data)
#> i Fold3: preprocessor 1/1, model 1/1
#> i Fold3: preprocessor 1/1, model 1/1 (predictions)
#> i Fold4: preprocessor 1/1
#> i Fold4: preprocessor 1/1 (prediction data)
#> i Fold4: preprocessor 1/1, model 1/1
#> i Fold4: preprocessor 1/1, model 1/1 (predictions)
#> i Fold5: preprocessor 1/1
#> i Fold5: preprocessor 1/1 (prediction data)
#> i Fold5: preprocessor 1/1, model 1/1
#> i Fold5: preprocessor 1/1, model 1/1 (predictions)
#> ✓ Estimating performance
#> i Estimating performance
#> i Fold1: preprocessor 1/1
#> i Fold1: preprocessor 1/1 (prediction data)
#> i Fold1: preprocessor 1/1, model 1/1
#> i Fold1: preprocessor 1/1, model 1/1 (predictions)
#> i Fold2: preprocessor 1/1
#> i Fold2: preprocessor 1/1 (prediction data)
#> i Fold2: preprocessor 1/1, model 1/1
#> i Fold2: preprocessor 1/1, model 1/1 (predictions)
#> i Fold3: preprocessor 1/1
#> i Fold3: preprocessor 1/1 (prediction data)
#> i Fold3: preprocessor 1/1, model 1/1
#> i Fold3: preprocessor 1/1, model 1/1 (predictions)
#> i Fold4: preprocessor 1/1
#> i Fold4: preprocessor 1/1 (prediction data)
#> i Fold4: preprocessor 1/1, model 1/1
#> i Fold4: preprocessor 1/1, model 1/1 (predictions)
#> i Fold5: preprocessor 1/1
#> i Fold5: preprocessor 1/1 (prediction data)
#> i Fold5: preprocessor 1/1, model 1/1
#> i Fold5: preprocessor 1/1, model 1/1 (predictions)
#> ✓ Estimating performance
#> i Estimating performance
#> i Fold1: preprocessor 1/1
#> i Fold1: preprocessor 1/1 (prediction data)
#> i Fold1: preprocessor 1/1, model 1/1
#> i Fold1: preprocessor 1/1, model 1/1 (predictions)
#> i Fold2: preprocessor 1/1
#> i Fold2: preprocessor 1/1 (prediction data)
#> i Fold2: preprocessor 1/1, model 1/1
#> i Fold2: preprocessor 1/1, model 1/1 (predictions)
#> i Fold3: preprocessor 1/1
#> i Fold3: preprocessor 1/1 (prediction data)
#> i Fold3: preprocessor 1/1, model 1/1
#> i Fold3: preprocessor 1/1, model 1/1 (predictions)
#> i Fold4: preprocessor 1/1
#> i Fold4: preprocessor 1/1 (prediction data)
#> i Fold4: preprocessor 1/1, model 1/1
#> i Fold4: preprocessor 1/1, model 1/1 (predictions)
#> i Fold5: preprocessor 1/1
#> i Fold5: preprocessor 1/1 (prediction data)
#> i Fold5: preprocessor 1/1, model 1/1
#> i Fold5: preprocessor 1/1, model 1/1 (predictions)
#> ✓ Estimating performance
#> i Estimating performance
#> i Fold1: preprocessor 1/1
#> i Fold1: preprocessor 1/1 (prediction data)
#> i Fold1: preprocessor 1/1, model 1/1
#> i Fold1: preprocessor 1/1, model 1/1 (predictions)
#> i Fold2: preprocessor 1/1
#> i Fold2: preprocessor 1/1 (prediction data)
#> i Fold2: preprocessor 1/1, model 1/1
#> i Fold2: preprocessor 1/1, model 1/1 (predictions)
#> i Fold3: preprocessor 1/1
#> i Fold3: preprocessor 1/1 (prediction data)
#> i Fold3: preprocessor 1/1, model 1/1
#> i Fold3: preprocessor 1/1, model 1/1 (predictions)
#> i Fold4: preprocessor 1/1
#> i Fold4: preprocessor 1/1 (prediction data)
#> i Fold4: preprocessor 1/1, model 1/1
#> i Fold4: preprocessor 1/1, model 1/1 (predictions)
#> i Fold5: preprocessor 1/1
#> i Fold5: preprocessor 1/1 (prediction data)
#> i Fold5: preprocessor 1/1, model 1/1
#> i Fold5: preprocessor 1/1, model 1/1 (predictions)
#> ✓ Estimating performance
#> i Estimating performance
#> i Fold1: preprocessor 1/1
#> i Fold1: preprocessor 1/1 (prediction data)
#> i Fold1: preprocessor 1/1, model 1/1
#> i Fold1: preprocessor 1/1, model 1/1 (predictions)
#> i Fold2: preprocessor 1/1
#> i Fold2: preprocessor 1/1 (prediction data)
#> i Fold2: preprocessor 1/1, model 1/1
#> i Fold2: preprocessor 1/1, model 1/1 (predictions)
#> i Fold3: preprocessor 1/1
#> i Fold3: preprocessor 1/1 (prediction data)
#> i Fold3: preprocessor 1/1, model 1/1
#> i Fold3: preprocessor 1/1, model 1/1 (predictions)
#> i Fold4: preprocessor 1/1
#> i Fold4: preprocessor 1/1 (prediction data)
#> i Fold4: preprocessor 1/1, model 1/1
#> i Fold4: preprocessor 1/1, model 1/1 (predictions)
#> i Fold5: preprocessor 1/1
#> i Fold5: preprocessor 1/1 (prediction data)
#> i Fold5: preprocessor 1/1, model 1/1
#> i Fold5: preprocessor 1/1, model 1/1 (predictions)
#> ✓ Estimating performance
#> i Estimating performance
#> i Fold1: preprocessor 1/1
#> i Fold1: preprocessor 1/1 (prediction data)
#> i Fold1: preprocessor 1/1, model 1/1
#> i Fold1: preprocessor 1/1, model 1/1 (predictions)
#> i Fold2: preprocessor 1/1
#> i Fold2: preprocessor 1/1 (prediction data)
#> i Fold2: preprocessor 1/1, model 1/1
#> i Fold2: preprocessor 1/1, model 1/1 (predictions)
#> i Fold3: preprocessor 1/1
#> i Fold3: preprocessor 1/1 (prediction data)
#> i Fold3: preprocessor 1/1, model 1/1
#> i Fold3: preprocessor 1/1, model 1/1 (predictions)
#> i Fold4: preprocessor 1/1
#> i Fold4: preprocessor 1/1 (prediction data)
#> i Fold4: preprocessor 1/1, model 1/1
#> i Fold4: preprocessor 1/1, model 1/1 (predictions)
#> i Fold5: preprocessor 1/1
#> i Fold5: preprocessor 1/1 (prediction data)
#> i Fold5: preprocessor 1/1, model 1/1
#> i Fold5: preprocessor 1/1, model 1/1 (predictions)
#> ✓ Estimating performance
#> i Estimating performance
#> i Fold1: preprocessor 1/1
#> i Fold1: preprocessor 1/1 (prediction data)
#> i Fold1: preprocessor 1/1, model 1/1
#> i Fold1: preprocessor 1/1, model 1/1 (predictions)
#> i Fold2: preprocessor 1/1
#> i Fold2: preprocessor 1/1 (prediction data)
#> i Fold2: preprocessor 1/1, model 1/1
#> i Fold2: preprocessor 1/1, model 1/1 (predictions)
#> i Fold3: preprocessor 1/1
#> i Fold3: preprocessor 1/1 (prediction data)
#> i Fold3: preprocessor 1/1, model 1/1
#> i Fold3: preprocessor 1/1, model 1/1 (predictions)
#> i Fold4: preprocessor 1/1
#> i Fold4: preprocessor 1/1 (prediction data)
#> i Fold4: preprocessor 1/1, model 1/1
#> i Fold4: preprocessor 1/1, model 1/1 (predictions)
#> i Fold5: preprocessor 1/1
#> i Fold5: preprocessor 1/1 (prediction data)
#> i Fold5: preprocessor 1/1, model 1/1
#> i Fold5: preprocessor 1/1, model 1/1 (predictions)
#> ✓ Estimating performance
#> i Estimating performance
#> i Fold1: preprocessor 1/1
#> i Fold1: preprocessor 1/1 (prediction data)
#> i Fold1: preprocessor 1/1, model 1/1
#> i Fold1: preprocessor 1/1, model 1/1 (predictions)
#> i Fold2: preprocessor 1/1
#> i Fold2: preprocessor 1/1 (prediction data)
#> i Fold2: preprocessor 1/1, model 1/1
#> i Fold2: preprocessor 1/1, model 1/1 (predictions)
#> i Fold3: preprocessor 1/1
#> i Fold3: preprocessor 1/1 (prediction data)
#> i Fold3: preprocessor 1/1, model 1/1
#> i Fold3: preprocessor 1/1, model 1/1 (predictions)
#> i Fold4: preprocessor 1/1
#> i Fold4: preprocessor 1/1 (prediction data)
#> i Fold4: preprocessor 1/1, model 1/1
#> i Fold4: preprocessor 1/1, model 1/1 (predictions)
#> i Fold5: preprocessor 1/1
#> i Fold5: preprocessor 1/1 (prediction data)
#> i Fold5: preprocessor 1/1, model 1/1
#> i Fold5: preprocessor 1/1, model 1/1 (predictions)
#> ✓ Estimating performance
#> i Estimating performance
#> i Fold1: preprocessor 1/1
#> i Fold1: preprocessor 1/1 (prediction data)
#> i Fold1: preprocessor 1/1, model 1/1
#> i Fold1: preprocessor 1/1, model 1/1 (predictions)
#> i Fold2: preprocessor 1/1
#> i Fold2: preprocessor 1/1 (prediction data)
#> i Fold2: preprocessor 1/1, model 1/1
#> i Fold2: preprocessor 1/1, model 1/1 (predictions)
#> i Fold3: preprocessor 1/1
#> i Fold3: preprocessor 1/1 (prediction data)
#> i Fold3: preprocessor 1/1, model 1/1
#> i Fold3: preprocessor 1/1, model 1/1 (predictions)
#> i Fold4: preprocessor 1/1
#> i Fold4: preprocessor 1/1 (prediction data)
#> i Fold4: preprocessor 1/1, model 1/1
#> i Fold4: preprocessor 1/1, model 1/1 (predictions)
#> i Fold5: preprocessor 1/1
#> i Fold5: preprocessor 1/1 (prediction data)
#> i Fold5: preprocessor 1/1, model 1/1
#> i Fold5: preprocessor 1/1, model 1/1 (predictions)
#> ✓ Estimating performance
#> i Estimating performance
#> i Fold1: preprocessor 1/1
#> i Fold1: preprocessor 1/1 (prediction data)
#> i Fold1: preprocessor 1/1, model 1/1
#> i Fold1: preprocessor 1/1, model 1/1 (predictions)
#> i Fold2: preprocessor 1/1
#> i Fold2: preprocessor 1/1 (prediction data)
#> i Fold2: preprocessor 1/1, model 1/1
#> i Fold2: preprocessor 1/1, model 1/1 (predictions)
#> i Fold3: preprocessor 1/1
#> i Fold3: preprocessor 1/1 (prediction data)
#> i Fold3: preprocessor 1/1, model 1/1
#> i Fold3: preprocessor 1/1, model 1/1 (predictions)
#> i Fold4: preprocessor 1/1
#> i Fold4: preprocessor 1/1 (prediction data)
#> i Fold4: preprocessor 1/1, model 1/1
#> i Fold4: preprocessor 1/1, model 1/1 (predictions)
#> i Fold5: preprocessor 1/1
#> i Fold5: preprocessor 1/1 (prediction data)
#> i Fold5: preprocessor 1/1, model 1/1
#> i Fold5: preprocessor 1/1, model 1/1 (predictions)
#> ✓ Estimating performance
#> # Tuning results
#> # 5-fold cross-validation 
#> # A tibble: 55 × 6
#>    splits         id    .metrics          .notes   .predictions .iter
#>    <list>         <chr> <list>            <list>   <list>       <int>
#>  1 <split [25/7]> Fold1 <tibble [10 × 6]> <tibble> <tibble>         0
#>  2 <split [25/7]> Fold2 <tibble [10 × 6]> <tibble> <tibble>         0
#>  3 <split [26/6]> Fold3 <tibble [10 × 6]> <tibble> <tibble>         0
#>  4 <split [26/6]> Fold4 <tibble [10 × 6]> <tibble> <tibble>         0
#>  5 <split [26/6]> Fold5 <tibble [10 × 6]> <tibble> <tibble>         0
#>  6 <split [25/7]> Fold1 <tibble [2 × 6]>  <tibble> <tibble>         1
#>  7 <split [25/7]> Fold2 <tibble [2 × 6]>  <tibble> <tibble>         1
#>  8 <split [26/6]> Fold3 <tibble [2 × 6]>  <tibble> <tibble>         1
#>  9 <split [26/6]> Fold4 <tibble [2 × 6]>  <tibble> <tibble>         1
#> 10 <split [26/6]> Fold5 <tibble [2 × 6]>  <tibble> <tibble>         1
#> # ℹ 45 more rows

# ---------------------------------------------------------------------------
# `fit_resamples()` and `last_fit()` can't use `tune()` placeholders, so for
# `control_resamples()` and `control_last_fit()` we use a model with fixed
# parameters.
# ---------------------------------------------------------------------------
# control_resamples(): Retain out-of-sample predictions and fitted workflows
# in the grid and each fold.
spec_fixed <- linear_reg() |>
  set_engine("lm")

wf_fixed <- workflow() |>
  add_model(spec_fixed) |>
  add_recipe(recipe)

keep_pred <- control_resamples(
  save_pred = TRUE,
  save_workflow = TRUE
)

fit_resamples(
  wf_fixed,
  resamples = folds,
  control = keep_pred
)
#> # Resampling results
#> # 5-fold cross-validation 
#> # A tibble: 5 × 5
#>   splits         id    .metrics         .notes           .predictions
#>   <list>         <chr> <list>           <list>           <list>      
#> 1 <split [25/7]> Fold1 <tibble [2 × 4]> <tibble [0 × 4]> <tibble>    
#> 2 <split [25/7]> Fold2 <tibble [2 × 4]> <tibble [0 × 4]> <tibble>    
#> 3 <split [26/6]> Fold3 <tibble [2 × 4]> <tibble [0 × 4]> <tibble>    
#> 4 <split [26/6]> Fold4 <tibble [2 × 4]> <tibble [0 × 4]> <tibble>    
#> 5 <split [26/6]> Fold5 <tibble [2 × 4]> <tibble [0 × 4]> <tibble>    

# ---------------------------------------------------------------------------
# control_last_fit(): Control last fit process by printing progress.
split <- initial_split(mtcars)

ctrl_last_fit <- control_last_fit(verbose = TRUE)

last_fit(
  wf_fixed,
  split = split,
  control = ctrl_last_fit
)
#> i train/test split: preprocessor 1/1
#> i train/test split: preprocessor 1/1 (prediction data)
#> i train/test split: preprocessor 1/1, model 1/1
#> i train/test split: preprocessor 1/1, model 1/1 (predictions)
#> i train/test split: preprocessor 1/1, model 1/1 (extracts)
#> # Resampling results
#> # Manual resampling 
#> # A tibble: 1 × 6
#>   splits         id           .metrics .notes   .predictions .workflow 
#>   <list>         <chr>        <list>   <list>   <list>       <list>    
#> 1 <split [24/8]> train/test … <tibble> <tibble> <tibble>     <workflow>
```
