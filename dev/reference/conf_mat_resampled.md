# Compute average confusion matrix across resamples

For classification problems, `conf_mat_resampled()` computes a separate
confusion matrix for each resample then averages the cell counts.

## Usage

``` r
conf_mat_resampled(x, ..., parameters = NULL, tidy = TRUE)
```

## Arguments

- x:

  An object with class `tune_results` that was used with a
  classification model that was run with `control_*(save_pred = TRUE)`.

- ...:

  Currently unused, must be empty.

- parameters:

  A tibble with a single tuning parameter combination. Only one tuning
  parameter combination (if any were used) is allowed here.

- tidy:

  Should the results come back in a tibble (`TRUE`) or a `conf_mat`
  object like
  [`yardstick::conf_mat()`](https://yardstick.tidymodels.org/reference/conf_mat.html)
  (`FALSE`)?

## Value

A tibble or `conf_mat` with the average cell count across resamples.

## Examples

``` r
# example code

library(parsnip)
library(rsample)
library(dplyr)

data(two_class_dat, package = "modeldata")

set.seed(2393)
res <-
  logistic_reg() |>
  set_engine("glm") |>
  fit_resamples(
    Class ~ .,
    resamples = vfold_cv(two_class_dat, v = 3),
    control = control_resamples(save_pred = TRUE)
  )

conf_mat_resampled(res)
#> # A tibble: 4 × 3
#>   Prediction Truth   Freq
#>   <fct>      <fct>  <dbl>
#> 1 Class1     Class1 123  
#> 2 Class1     Class2  25.7
#> 3 Class2     Class1  22.7
#> 4 Class2     Class2  92.3
conf_mat_resampled(res, tidy = FALSE)
#>           Class1    Class2
#> Class1 123.00000  25.66667
#> Class2  22.66667  92.33333
```
