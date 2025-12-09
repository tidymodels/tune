# Fit the final best model to the training set and evaluate the test set

`last_fit()` emulates the process where, after determining the best
model, the final fit on the entire training set is needed and is then
evaluated on the test set.

## Usage

``` r
last_fit(object, ...)

# S3 method for class 'model_spec'
last_fit(
  object,
  preprocessor,
  split,
  ...,
  metrics = NULL,
  eval_time = NULL,
  control = control_last_fit(),
  add_validation_set = FALSE
)

# S3 method for class 'workflow'
last_fit(
  object,
  split,
  ...,
  metrics = NULL,
  eval_time = NULL,
  control = control_last_fit(),
  add_validation_set = FALSE
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

- split:

  An `rsplit` object created from
  [`rsample::initial_split()`](https://rsample.tidymodels.org/reference/initial_split.html)
  or
  [`rsample::initial_validation_split()`](https://rsample.tidymodels.org/reference/initial_validation_split.html).

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
  [`control_last_fit()`](https://tune.tidymodels.org/dev/reference/control_last_fit.md)
  object used to fine tune the last fit process.

- add_validation_set:

  For 3-way splits into training, validation, and test set via
  [`rsample::initial_validation_split()`](https://rsample.tidymodels.org/reference/initial_validation_split.html),
  should the validation set be included in the data set used to train
  the model. If not, only the training set is used.

## Value

A single row tibble that emulates the structure of
[`fit_resamples()`](https://tune.tidymodels.org/dev/reference/fit_resamples.md).
However, a list column called `.workflow` is also attached with the
fitted model (and recipe, if any) that used the training set. Helper
functions for formatting tuning results like
[`collect_metrics()`](https://tune.tidymodels.org/dev/reference/collect_predictions.md)
and
[`collect_predictions()`](https://tune.tidymodels.org/dev/reference/collect_predictions.md)
can be used with `last_fit()` output.

## Details

This function is intended to be used after fitting a *variety of models*
and the final tuning parameters (if any) have been finalized. The next
step would be to fit using the entire training set and verify
performance using the test data.

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

For functions such as
[`fit_resamples()`](https://tune.tidymodels.org/dev/reference/fit_resamples.md)
and the `tune_*()` functions, the model must be contained inside of a
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

## See also

`last_fit()` is closely related to
[`fit_best()`](https://tune.tidymodels.org/dev/reference/fit_best.md).
They both give you access to a workflow fitted on the training data but
are situated somewhat differently in the modeling workflow.
[`fit_best()`](https://tune.tidymodels.org/dev/reference/fit_best.md)
picks up after a tuning function like
[`tune_grid()`](https://tune.tidymodels.org/dev/reference/tune_grid.md)
to take you from tuning results to fitted workflow, ready for you to
predict and assess further. `last_fit()` assumes you have made your
choice of hyperparameters and finalized your workflow to then take you
from finalized workflow to fitted workflow and further to performance
assessment on the test data. While
[`fit_best()`](https://tune.tidymodels.org/dev/reference/fit_best.md)
gives a fitted workflow, `last_fit()` gives you the performance results.
If you want the fitted workflow, you can extract it from the result of
`last_fit()` via
[extract_workflow()](https://tune.tidymodels.org/dev/reference/extract-tune.md).

## Examples

``` r
library(recipes)
library(rsample)
library(parsnip)

set.seed(6735)
tr_te_split <- initial_split(mtcars)

spline_rec <- recipe(mpg ~ ., data = mtcars) |>
  step_spline_natural(disp)

lin_mod <- linear_reg() |>
  set_engine("lm")

spline_res <- last_fit(lin_mod, spline_rec, split = tr_te_split)
spline_res
#> # Resampling results
#> # Manual resampling 
#> # A tibble: 1 × 6
#>   splits         id           .metrics .notes   .predictions .workflow 
#>   <list>         <chr>        <list>   <list>   <list>       <list>    
#> 1 <split [24/8]> train/test … <tibble> <tibble> <tibble>     <workflow>

# test set metrics
collect_metrics(spline_res)
#> # A tibble: 2 × 4
#>   .metric .estimator .estimate .config        
#>   <chr>   <chr>          <dbl> <chr>          
#> 1 rmse    standard       2.44  pre0_mod0_post0
#> 2 rsq     standard       0.799 pre0_mod0_post0

# test set predictions
collect_predictions(spline_res)
#> # A tibble: 8 × 5
#>   .pred id                 mpg  .row .config        
#>   <dbl> <chr>            <dbl> <int> <chr>          
#> 1  21.8 train/test split  21       1 pre0_mod0_post0
#> 2  23.2 train/test split  22.8     3 pre0_mod0_post0
#> 3  17.4 train/test split  14.3     7 pre0_mod0_post0
#> 4  17.3 train/test split  19.2    10 pre0_mod0_post0
#> 5  30.3 train/test split  32.4    18 pre0_mod0_post0
#> 6  22.4 train/test split  19.2    25 pre0_mod0_post0
#> 7  31.2 train/test split  27.3    26 pre0_mod0_post0
#> 8  19.5 train/test split  21.4    32 pre0_mod0_post0

# or use a workflow

library(workflows)
spline_wfl <-
  workflow() |>
  add_recipe(spline_rec) |>
  add_model(lin_mod)

last_fit(spline_wfl, split = tr_te_split)
#> # Resampling results
#> # Manual resampling 
#> # A tibble: 1 × 6
#>   splits         id           .metrics .notes   .predictions .workflow 
#>   <list>         <chr>        <list>   <list>   <list>       <list>    
#> 1 <split [24/8]> train/test … <tibble> <tibble> <tibble>     <workflow>
```
