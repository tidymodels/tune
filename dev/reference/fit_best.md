# Fit a model to the numerically optimal configuration

`fit_best()` takes the results from model tuning and fits it to the
training set using tuning parameters associated with the best
performance.

## Usage

``` r
fit_best(x, ...)

# Default S3 method
fit_best(x, ...)

# S3 method for class 'tune_results'
fit_best(
  x,
  ...,
  metric = NULL,
  eval_time = NULL,
  parameters = NULL,
  verbose = FALSE,
  add_validation_set = NULL
)
```

## Arguments

- x:

  The results of class `tune_results` (coming from functions such as
  [`tune_grid()`](https://tune.tidymodels.org/dev/reference/tune_grid.md),
  [`tune_bayes()`](https://tune.tidymodels.org/dev/reference/tune_bayes.md),
  etc). The control option
  [`save_workflow = TRUE`](https://tune.tidymodels.org/dev/reference/control_grid.md)
  should have been used.

- ...:

  Not currently used, must be empty.

- metric:

  A character string (or `NULL`) for which metric to optimize. If
  `NULL`, the first metric is used.

- eval_time:

  A single numeric time point where dynamic event time metrics should be
  chosen (e.g., the time-dependent ROC curve, etc). The values should be
  consistent with the values used to create `x`. The `NULL` default will
  automatically use the first evaluation time used by `x`.

- parameters:

  An optional 1-row tibble of tuning parameter settings, with a column
  for each tuning parameter. This tibble should have columns for each
  tuning parameter identifier (e.g. `"my_param"` if `tune("my_param")`
  was used). If `NULL`, this argument will be set to
  [`select_best(metric, eval_time)`](https://tune.tidymodels.org/dev/reference/show_best.md).
  If not `NULL`, `parameters` overwrites the specification via `metric`,
  and `eval_time`.

- verbose:

  A logical for printing logging.

- add_validation_set:

  When the resamples embedded in `x` are a split into training set and
  validation set, should the validation set be included in the data set
  used to train the model? If not, only the training set is used. If
  `NULL`, the validation set is not used for resamples originating from
  [`rsample::validation_set()`](https://rsample.tidymodels.org/reference/validation_set.html)
  while it is used for resamples originating from
  [`rsample::validation_split()`](https://rsample.tidymodels.org/reference/validation_split.html).

## Value

A fitted workflow.

## Details

This function is a shortcut for the manual steps of:

      best_param <- select_best(tune_results, metric) # or other `select_*()`
      wflow <- finalize_workflow(wflow, best_param)  # or just `finalize_model()`
      wflow_fit <- fit(wflow, data_set)

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

## See also

[`last_fit()`](https://tune.tidymodels.org/dev/reference/last_fit.md) is
closely related to `fit_best()`. They both give you access to a workflow
fitted on the training data but are situated somewhat differently in the
modeling workflow. `fit_best()` picks up after a tuning function like
[`tune_grid()`](https://tune.tidymodels.org/dev/reference/tune_grid.md)
to take you from tuning results to fitted workflow, ready for you to
predict and assess further.
[`last_fit()`](https://tune.tidymodels.org/dev/reference/last_fit.md)
assumes you have made your choice of hyperparameters and finalized your
workflow to then take you from finalized workflow to fitted workflow and
further to performance assessment on the test data. While `fit_best()`
gives a fitted workflow,
[`last_fit()`](https://tune.tidymodels.org/dev/reference/last_fit.md)
gives you the performance results. If you want the fitted workflow, you
can extract it from the result of
[`last_fit()`](https://tune.tidymodels.org/dev/reference/last_fit.md)
via
[extract_workflow()](https://tune.tidymodels.org/dev/reference/extract-tune.md).

## Examples

``` r
library(recipes)
library(rsample)
library(parsnip)
library(dplyr)

data(meats, package = "modeldata")
meats <- meats |> select(-water, -fat)

set.seed(1)
meat_split <- initial_split(meats)
meat_train <- training(meat_split)
meat_test  <- testing(meat_split)

set.seed(2)
meat_rs <- vfold_cv(meat_train, v = 10)

pca_rec <-
  recipe(protein ~ ., data = meat_train) |>
  step_normalize(all_numeric_predictors()) |>
  step_pca(all_numeric_predictors(), num_comp = tune())

knn_mod <- nearest_neighbor(neighbors = tune()) |> set_mode("regression")

ctrl <- control_grid(save_workflow = TRUE)

set.seed(128)
knn_pca_res <-
  tune_grid(knn_mod, pca_rec, resamples = meat_rs, grid = 10, control = ctrl)

knn_fit <- fit_best(knn_pca_res, verbose = TRUE)
#> Using rmse as the metric, the optimal parameters were:
#>   neighbors: 7
#>   num_comp:  4
#> 
#> ℹ Fitting using 161 data points...
#> ✔ Done.
predict(knn_fit, meat_test)
#> # A tibble: 54 × 1
#>    .pred
#>    <dbl>
#>  1  19.7
#>  2  20.1
#>  3  15.3
#>  4  13.3
#>  5  19.5
#>  6  21.1
#>  7  19.9
#>  8  18.7
#>  9  19.6
#> 10  17.9
#> # ℹ 44 more rows
```
