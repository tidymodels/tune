# Fit multiple models via resampling

`fit_resamples()` computes a set of performance metrics across one or
more resamples. It does not perform any tuning (see
[`tune_grid()`](https://tune.tidymodels.org/dev/reference/tune_grid.md)
and
[`tune_bayes()`](https://tune.tidymodels.org/dev/reference/tune_bayes.md)
for that), and is instead used for fitting a single model+recipe or
model+formula combination across many resamples.

## Usage

``` r
fit_resamples(object, ...)

# S3 method for class 'model_spec'
fit_resamples(
  object,
  preprocessor,
  resamples,
  ...,
  metrics = NULL,
  eval_time = NULL,
  control = control_resamples()
)

# S3 method for class 'workflow'
fit_resamples(
  object,
  resamples,
  ...,
  metrics = NULL,
  eval_time = NULL,
  control = control_resamples()
)
```

## Arguments

- object:

  A `parsnip` model specification or an unfitted
  [workflow()](https://workflows.tidymodels.org/reference/workflow.html).
  No tuning parameters are allowed; if arguments have been marked with
  [tune()](https://hardhat.tidymodels.org/reference/tune.html), their
  values must be
  [finalized](https://tune.tidymodels.org/dev/reference/finalize_model.md).

- ...:

  Currently unused.

- preprocessor:

  A traditional model formula or a recipe created using
  [`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html).

- resamples:

  An `rset` resampling object created from an `rsample` function, such
  as
  [`rsample::vfold_cv()`](https://rsample.tidymodels.org/reference/vfold_cv.html).

- metrics:

  A
  [`yardstick::metric_set()`](https://yardstick.tidymodels.org/reference/metric_set.html),
  or `NULL` to compute a standard set of metrics.

- eval_time:

  A numeric vector of time points where dynamic event time metrics
  should be computed (e.g. the time-dependent ROC curve, etc). The
  values must be non-negative and should probably be no greater than the
  largest event time in the training set (See Details below).

- control:

  A
  [`control_resamples()`](https://tune.tidymodels.org/dev/reference/control_grid.md)
  object used to fine tune the resampling process.

## Case Weights

Some models can utilize case weights during training. tidymodels
currently supports two types of case weights: importance weights
(doubles) and frequency weights (integers). Frequency weights are used
during model fitting and evaluation, whereas importance weights are only
used during fitting.

To know if your model is capable of using case weights, create a model
spec and test it using
[`parsnip::case_weights_allowed()`](https://parsnip.tidymodels.org/reference/case_weights_allowed.html).

To use them, you will need a numeric column in your data set that has
been passed through either
[`hardhat:: importance_weights()`](https://hardhat.tidymodels.org/reference/importance_weights.html)
or
[`hardhat::frequency_weights()`](https://hardhat.tidymodels.org/reference/frequency_weights.html).

For functions such as `fit_resamples()` and the `tune_*()` functions,
the model must be contained inside of a
[`workflows::workflow()`](https://workflows.tidymodels.org/reference/workflow.html).
To declare that case weights are used, invoke
[`workflows::add_case_weights()`](https://workflows.tidymodels.org/reference/add_case_weights.html)
with the corresponding (unquoted) column name.

From there, the packages will appropriately handle the weights during
model fitting and (if appropriate) performance estimation.

## Censored Regression Models

Three types of metrics can be used to assess the quality of censored
regression models:

- static: the prediction is independent of time.

- dynamic: the prediction is a time-specific probability (e.g., survival
  probability) and is measured at one or more particular times.

- integrated: same as the dynamic metric but returns the integral of the
  different metrics from each time point.

Which metrics are chosen by the user affects how many evaluation times
should be specified. For example:

    # Needs no `eval_time` value
    metric_set(concordance_survival)

    # Needs at least one `eval_time`
    metric_set(brier_survival)
    metric_set(brier_survival, concordance_survival)

    # Needs at least two eval_time` values
    metric_set(brier_survival_integrated, concordance_survival)
    metric_set(brier_survival_integrated, concordance_survival)
    metric_set(brier_survival_integrated, concordance_survival, brier_survival)

Values of `eval_time` should be less than the largest observed event
time in the training data. For many non-parametric models, the results
beyond the largest time corresponding to an event are constant (or
`NA`).

## Performance Metrics

To use your own performance metrics, the
[`yardstick::metric_set()`](https://yardstick.tidymodels.org/reference/metric_set.html)
function can be used to pick what should be measured for each model. If
multiple metrics are desired, they can be bundled. For example, to
estimate the area under the ROC curve as well as the sensitivity and
specificity (under the typical probability cutoff of 0.50), the
`metrics` argument could be given:

      metrics = metric_set(roc_auc, sens, spec)

Each metric is calculated for each candidate model.

If no metric set is provided, one is created:

- For regression models, the root mean squared error and coefficient of
  determination are computed.

- For classification, the area under the ROC curve and overall accuracy
  are computed.

Note that the metrics also determine what type of predictions are
estimated during tuning. For example, in a classification problem, if
metrics are used that are all associated with hard class predictions,
the classification probabilities are not created.

The out-of-sample estimates of these metrics are contained in a list
column called `.metrics`. This tibble contains a row for each metric and
columns for the value, the estimator type, and so on.

[`collect_metrics()`](https://tune.tidymodels.org/dev/reference/collect_predictions.md)
can be used for these objects to collapse the results over the resampled
(to obtain the final resampling estimates per tuning parameter
combination).

## Obtaining Predictions

When `control_grid(save_pred = TRUE)`, the output tibble contains a list
column called `.predictions` that has the out-of-sample predictions for
each parameter combination in the grid and each fold (which can be very
large).

The elements of the tibble are tibbles with columns for the tuning
parameters, the row number from the original data object (`.row`), the
outcome data (with the same name(s) of the original data), and any
columns created by the predictions. For example, for simple regression
problems, this function generates a column called `.pred` and so on. As
noted above, the prediction columns that are returned are determined by
the type of metric(s) requested.

This list column can be `unnested` using
[`tidyr::unnest()`](https://tidyr.tidyverse.org/reference/unnest.html)
or using the convenience function
[`collect_predictions()`](https://tune.tidymodels.org/dev/reference/collect_predictions.md).

## Extracting Information

The `extract` control option will result in an additional function to be
returned called `.extracts`. This is a list column that has tibbles
containing the results of the user's function for each tuning parameter
combination. This can enable returning each model and/or recipe object
that is created during resampling. Note that this could result in a
large return object, depending on what is returned.

The control function contains an option (`extract`) that can be used to
retain any model or recipe that was created within the resamples. This
argument should be a function with a single argument. The value of the
argument that is given to the function in each resample is a workflow
object (see
[`workflows::workflow()`](https://workflows.tidymodels.org/reference/workflow.html)
for more information). Several helper functions can be used to easily
pull out the preprocessing and/or model information from the workflow,
such as
[`extract_preprocessor()`](https://workflows.tidymodels.org/reference/extract-workflow.html)
and
[`extract_fit_parsnip()`](https://workflows.tidymodels.org/reference/extract-workflow.html).

As an example, if there is interest in getting each parsnip model fit
back, one could use:

      extract = function (x) extract_fit_parsnip(x)

Note that the function given to the `extract` argument is evaluated on
every model that is *fit* (as opposed to every model that is
*evaluated*). As noted above, in some cases, model predictions can be
derived for sub-models so that, in these cases, not every row in the
tuning parameter grid has a separate R object associated with it.

Finally, it is a good idea to include calls to
[`require()`](https://rdrr.io/r/base/library.html) for packages that are
used in the function. This helps prevent failures when using parallel
processing.

## See also

[`control_resamples()`](https://tune.tidymodels.org/dev/reference/control_grid.md),
[`collect_predictions()`](https://tune.tidymodels.org/dev/reference/collect_predictions.md),
[`collect_metrics()`](https://tune.tidymodels.org/dev/reference/collect_predictions.md)

## Examples

``` r
library(recipes)
library(rsample)
library(parsnip)
library(workflows)

set.seed(6735)
folds <- vfold_cv(mtcars, v = 5)

spline_rec <- recipe(mpg ~ ., data = mtcars) |>
  step_spline_natural(disp) |>
  step_spline_natural(wt)

lin_mod <- linear_reg() |>
  set_engine("lm")

control <- control_resamples(save_pred = TRUE)

spline_res <- fit_resamples(lin_mod, spline_rec, folds, control = control)
#> → A | warning: prediction from rank-deficient fit; consider predict(., rankdeficient="NA")
#> There were issues with some computations   A: x1
#> There were issues with some computations   A: x2
#> There were issues with some computations   A: x5
#> 

spline_res
#> # Resampling results
#> # 5-fold cross-validation 
#> # A tibble: 5 × 5
#>   splits         id    .metrics         .notes           .predictions
#>   <list>         <chr> <list>           <list>           <list>      
#> 1 <split [25/7]> Fold1 <tibble [2 × 4]> <tibble [1 × 4]> <tibble>    
#> 2 <split [25/7]> Fold2 <tibble [2 × 4]> <tibble [1 × 4]> <tibble>    
#> 3 <split [26/6]> Fold3 <tibble [2 × 4]> <tibble [1 × 4]> <tibble>    
#> 4 <split [26/6]> Fold4 <tibble [2 × 4]> <tibble [1 × 4]> <tibble>    
#> 5 <split [26/6]> Fold5 <tibble [2 × 4]> <tibble [1 × 4]> <tibble>    
#> 
#> There were issues with some computations:
#> 
#>   - Warning(s) x5: prediction from rank-deficient fit; consider predict(., r...
#> 
#> Run `show_notes(.Last.tune.result)` for more information.

show_best(spline_res, metric = "rmse")
#> # A tibble: 1 × 6
#>   .metric .estimator   mean     n std_err .config        
#>   <chr>   <chr>       <dbl> <int>   <dbl> <chr>          
#> 1 rmse    standard   47788.     5  47699. pre0_mod0_post0

# You can also wrap up a preprocessor and a model into a workflow, and
# supply that to `fit_resamples()` instead. Here, a workflows "variables"
# preprocessor is used, which lets you supply terms using dplyr selectors.
# The variables are used as-is, no preprocessing is done to them.
wf <- workflow() |>
  add_variables(outcomes = mpg, predictors = everything()) |>
  add_model(lin_mod)

wf_res <- fit_resamples(wf, folds)
```
