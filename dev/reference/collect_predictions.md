# Obtain and format results produced by tuning functions

Obtain and format results produced by tuning functions

## Usage

``` r
collect_predictions(x, ...)

# Default S3 method
collect_predictions(x, ...)

# S3 method for class 'tune_results'
collect_predictions(x, ..., summarize = FALSE, parameters = NULL)

collect_metrics(x, ...)

# S3 method for class 'tune_results'
collect_metrics(x, ..., summarize = TRUE, type = c("long", "wide"))

collect_notes(x, ...)

# S3 method for class 'tune_results'
collect_notes(x, ...)

collect_extracts(x, ...)

# S3 method for class 'tune_results'
collect_extracts(x, ...)
```

## Arguments

- x:

  The results of
  [`tune_grid()`](https://tune.tidymodels.org/dev/reference/tune_grid.md),
  [`tune_bayes()`](https://tune.tidymodels.org/dev/reference/tune_bayes.md),
  [`fit_resamples()`](https://tune.tidymodels.org/dev/reference/fit_resamples.md),
  or
  [`last_fit()`](https://tune.tidymodels.org/dev/reference/last_fit.md).
  For `collect_predictions()`, the control option `save_pred = TRUE`
  should have been used.

- ...:

  Not currently used.

- summarize:

  A logical; should metrics be summarized over resamples (`TRUE`) or
  return the values for each individual resample. Note that, if `x` is
  created by
  [`last_fit()`](https://tune.tidymodels.org/dev/reference/last_fit.md),
  `summarize` has no effect. For the other object types, the method of
  summarizing predictions is detailed below.

- parameters:

  An optional tibble of tuning parameter values that can be used to
  filter the predicted values before processing. This tibble should only
  have columns for each tuning parameter identifier (e.g. `"my_param"`
  if `tune("my_param")` was used).

- type:

  One of `"long"` (the default) or `"wide"`. When `type = "long"`,
  output has columns `.metric` and one of `.estimate` or `mean`.
  `.estimate`/`mean` gives the values for the `.metric`. When
  `type = "wide"`, each metric has its own column and the `n` and
  `std_err` columns are removed, if they exist.

## Value

A tibble. The column names depend on the results and the mode of the
model.

For `collect_metrics()` and `collect_predictions()`, when unsummarized,
there are columns for each tuning parameter (using the `id` from
[`tune()`](https://hardhat.tidymodels.org/reference/tune.html), if any).

`collect_metrics()` also has columns `.metric`, and `.estimator` by
default. For `collect_metrics()` methods that have a `type` argument,
supplying `type = "wide"` will pivot the output such that each metric
has its own column. When the results are summarized, there are columns
for `mean`, `n`, and `std_err`. When not summarized, the additional
columns for the resampling identifier(s) and `.estimate`.

For `collect_predictions()`, there are additional columns for the
resampling identifier(s), columns for the predicted values (e.g.,
`.pred`, `.pred_class`, etc.), and a column for the outcome(s) using the
original column name(s) in the data.

`collect_predictions()` can summarize the various results over replicate
out-of-sample predictions. For example, when using the bootstrap, each
row in the original training set has multiple holdout predictions
(across assessment sets). To convert these results to a format where
every training set same has a single predicted value, the results are
averaged over replicate predictions.

For regression cases, the numeric predictions are simply averaged.

For classification models, the problem is more complex. When class
probabilities are used, these are averaged and then re-normalized to
make sure that they add to one. If hard class predictions also exist in
the data, then these are determined from the summarized probability
estimates (so that they match). If only hard class predictions are in
the results, then the mode is used to summarize.

With censored outcome models, the predicted survival probabilities (if
any) are averaged while the static predicted event times are summarized
using the median.

`collect_notes()` returns a tibble with columns for the resampling
indicators, the location (preprocessor, model, etc.), type (error or
warning), and the notes.

`collect_extracts()` collects objects extracted from fitted workflows
via the `extract` argument to [control
functions](https://tune.tidymodels.org/dev/reference/control_grid.md).
The function returns a tibble with columns for the resampling
indicators, the location (preprocessor, model, etc.), and extracted
objects.

## Hyperparameters and extracted objects

When making use of submodels, tune can generate predictions and
calculate metrics for multiple model `.config`urations using only one
model fit. However, this means that if a function was supplied to a
[control
function's](https://tune.tidymodels.org/dev/reference/control_grid.md)
`extract` argument, tune can only execute that extraction on the one
model that was fitted. As a result, in the `collect_extracts()` output,
tune opts to associate the extracted objects with the hyperparameter
combination used to fit that one model workflow, rather than the
hyperparameter combination of a submodel. In the output, this appears
like a hyperparameter entry is recycled across many `.config`
entries—this is intentional.

See <https://parsnip.tidymodels.org/articles/Submodels.html> to learn
more about submodels.

## Examples

``` r
data("example_ames_knn")
# The parameters for the model:
extract_parameter_set_dials(ames_wflow)
#> Collection of 5 parameters for tuning
#> 
#>   identifier        type    object
#>            K   neighbors nparam[+]
#>  weight_func weight_func dparam[+]
#>   dist_power  dist_power nparam[+]
#>          lon    deg_free nparam[+]
#>          lat    deg_free nparam[+]
#> 

# Summarized over resamples
collect_metrics(ames_grid_search)
#> # A tibble: 20 × 11
#>        K weight_func  dist_power   lon   lat .metric .estimator   mean
#>    <int> <chr>             <dbl> <int> <int> <chr>   <chr>       <dbl>
#>  1    21 triweight         0.626     1     4 rmse    standard   0.0729
#>  2    21 triweight         0.626     1     4 rsq     standard   0.833 
#>  3     5 gaussian          0.411     2     7 rmse    standard   0.0742
#>  4     5 gaussian          0.411     2     7 rsq     standard   0.826 
#>  5    35 gaussian          1.29      3    13 rmse    standard   0.0791
#>  6    35 gaussian          1.29      3    13 rsq     standard   0.814 
#>  7    12 epanechnikov      1.53      4     7 rmse    standard   0.0762
#>  8    12 epanechnikov      1.53      4     7 rsq     standard   0.817 
#>  9    35 rank              1.32      8     1 rmse    standard   0.0791
#> 10    35 rank              1.32      8     1 rsq     standard   0.813 
#> 11     4 biweight          0.311     8     4 rmse    standard   0.0783
#> 12     4 biweight          0.311     8     4 rsq     standard   0.805 
#> 13    32 triangular        0.165     9    15 rmse    standard   0.0769
#> 14    32 triangular        0.165     9    15 rsq     standard   0.818 
#> 15    33 triweight         0.511    10     3 rmse    standard   0.0727
#> 16    33 triweight         0.511    10     3 rsq     standard   0.835 
#> 17     3 gaussian          1.86     10    15 rmse    standard   0.0834
#> 18     3 gaussian          1.86     10    15 rsq     standard   0.775 
#> 19    40 triangular        0.167    11     7 rmse    standard   0.0775
#> 20    40 triangular        0.167    11     7 rsq     standard   0.815 
#> # ℹ 3 more variables: n <int>, std_err <dbl>, .config <chr>

# Per-resample values
collect_metrics(ames_grid_search, summarize = FALSE)
#> # A tibble: 200 × 10
#>    id         K weight_func dist_power   lon   lat .metric .estimator
#>    <chr>  <int> <chr>            <dbl> <int> <int> <chr>   <chr>     
#>  1 Fold01    21 triweight        0.626     1     4 rmse    standard  
#>  2 Fold01    21 triweight        0.626     1     4 rsq     standard  
#>  3 Fold02    21 triweight        0.626     1     4 rmse    standard  
#>  4 Fold02    21 triweight        0.626     1     4 rsq     standard  
#>  5 Fold03    21 triweight        0.626     1     4 rmse    standard  
#>  6 Fold03    21 triweight        0.626     1     4 rsq     standard  
#>  7 Fold04    21 triweight        0.626     1     4 rmse    standard  
#>  8 Fold04    21 triweight        0.626     1     4 rsq     standard  
#>  9 Fold05    21 triweight        0.626     1     4 rmse    standard  
#> 10 Fold05    21 triweight        0.626     1     4 rsq     standard  
#> # ℹ 190 more rows
#> # ℹ 2 more variables: .estimate <dbl>, .config <chr>


# ---------------------------------------------------------------------------

library(parsnip)
library(rsample)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(recipes)
#> 
#> Attaching package: ‘recipes’
#> The following object is masked from ‘package:stats’:
#> 
#>     step
library(tibble)

lm_mod <- linear_reg() |> set_engine("lm")
set.seed(93599150)
car_folds <- vfold_cv(mtcars, v = 2, repeats = 3)
ctrl <- control_resamples(save_pred = TRUE, extract = extract_fit_engine)

spline_rec <-
  recipe(mpg ~ ., data = mtcars) |>
  step_spline_natural(disp, deg_free = tune("df"))

grid <- tibble(df = 3:6)

resampled <-
  lm_mod |>
  tune_grid(spline_rec, resamples = car_folds, control = ctrl, grid = grid)

collect_predictions(resampled) |> arrange(.row)
#> # A tibble: 384 × 7
#>    .pred id      id2     mpg  .row    df .config        
#>    <dbl> <chr>   <chr> <dbl> <int> <int> <chr>          
#>  1  16.5 Repeat1 Fold2    21     1     3 pre1_mod0_post0
#>  2  19.0 Repeat2 Fold1    21     1     3 pre1_mod0_post0
#>  3  20.0 Repeat3 Fold1    21     1     3 pre1_mod0_post0
#>  4  15.1 Repeat1 Fold2    21     1     4 pre2_mod0_post0
#>  5  17.7 Repeat2 Fold1    21     1     4 pre2_mod0_post0
#>  6  20.1 Repeat3 Fold1    21     1     4 pre2_mod0_post0
#>  7  17.9 Repeat1 Fold2    21     1     5 pre3_mod0_post0
#>  8  18.3 Repeat2 Fold1    21     1     5 pre3_mod0_post0
#>  9  20.4 Repeat3 Fold1    21     1     5 pre3_mod0_post0
#> 10  15.1 Repeat1 Fold2    21     1     6 pre4_mod0_post0
#> # ℹ 374 more rows
collect_predictions(resampled, summarize = TRUE) |> arrange(.row)
#> # A tibble: 128 × 5
#>    .pred   mpg  .row    df .config        
#>    <dbl> <dbl> <int> <int> <chr>          
#>  1  18.5  21       1     3 pre1_mod0_post0
#>  2  17.6  21       1     4 pre2_mod0_post0
#>  3  18.9  21       1     5 pre3_mod0_post0
#>  4  16.7  21       1     6 pre4_mod0_post0
#>  5  19.4  21       2     3 pre1_mod0_post0
#>  6  19.0  21       2     4 pre2_mod0_post0
#>  7  18.7  21       2     5 pre3_mod0_post0
#>  8  16.4  21       2     6 pre4_mod0_post0
#>  9  31.8  22.8     3     3 pre1_mod0_post0
#> 10  23.8  22.8     3     4 pre2_mod0_post0
#> # ℹ 118 more rows
collect_predictions(
  resampled,
  summarize = TRUE,
  parameters = grid[1, ]
) |> arrange(.row)
#> # A tibble: 32 × 5
#>    .pred   mpg  .row    df .config        
#>    <dbl> <dbl> <int> <int> <chr>          
#>  1  18.5  21       1     3 pre1_mod0_post0
#>  2  19.4  21       2     3 pre1_mod0_post0
#>  3  31.8  22.8     3     3 pre1_mod0_post0
#>  4  20.2  21.4     4     3 pre1_mod0_post0
#>  5  18.4  18.7     5     3 pre1_mod0_post0
#>  6  20.6  18.1     6     3 pre1_mod0_post0
#>  7  13.5  14.3     7     3 pre1_mod0_post0
#>  8  19.2  24.4     8     3 pre1_mod0_post0
#>  9  34.8  22.8     9     3 pre1_mod0_post0
#> 10  16.6  19.2    10     3 pre1_mod0_post0
#> # ℹ 22 more rows

collect_extracts(resampled)
#> # A tibble: 24 × 5
#>    id      id2      df .extracts .config        
#>    <chr>   <chr> <int> <list>    <chr>          
#>  1 Repeat1 Fold1     3 <lm>      pre1_mod0_post0
#>  2 Repeat1 Fold1     4 <lm>      pre2_mod0_post0
#>  3 Repeat1 Fold1     5 <lm>      pre3_mod0_post0
#>  4 Repeat1 Fold1     6 <lm>      pre4_mod0_post0
#>  5 Repeat1 Fold2     3 <lm>      pre1_mod0_post0
#>  6 Repeat1 Fold2     4 <lm>      pre2_mod0_post0
#>  7 Repeat1 Fold2     5 <lm>      pre3_mod0_post0
#>  8 Repeat1 Fold2     6 <lm>      pre4_mod0_post0
#>  9 Repeat2 Fold1     3 <lm>      pre1_mod0_post0
#> 10 Repeat2 Fold1     4 <lm>      pre2_mod0_post0
#> # ℹ 14 more rows
```
