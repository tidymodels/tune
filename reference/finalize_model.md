# Splice final parameters into objects

The `finalize_*` functions take a list or tibble of tuning parameter
values and update objects with those values.

## Usage

``` r
finalize_model(x, parameters)

finalize_recipe(x, parameters)

finalize_workflow(x, parameters)

finalize_tailor(x, parameters)
```

## Arguments

- x:

  A recipe, parsnip model specification, tailor postprocessor, or
  workflow.

- parameters:

  A list or 1-row tibble of parameter values. Note that the column names
  of the tibble should be the `id` fields attached to
  [`tune()`](https://hardhat.tidymodels.org/reference/tune.html). For
  example, in the `Examples` section below, the model has `tune("K")`.
  In this case, the parameter tibble should be "K" and not "neighbors".

## Value

An updated version of `x`.

## Examples

``` r
data("example_ames_knn")

library(parsnip)
knn_model <-
  nearest_neighbor(
    mode = "regression",
    neighbors = tune("K"),
    weight_func = tune(),
    dist_power = tune()
  ) |>
  set_engine("kknn")

lowest_rmse <- select_best(ames_grid_search, metric = "rmse")
lowest_rmse
#> # A tibble: 1 × 6
#>       K weight_func dist_power   lon   lat .config          
#>   <int> <chr>            <dbl> <int> <int> <chr>            
#> 1    33 triweight        0.511    10     3 pre08_mod07_post0

knn_model
#> K-Nearest Neighbor Model Specification (regression)
#> 
#> Main Arguments:
#>   neighbors = tune("K")
#>   weight_func = tune()
#>   dist_power = tune()
#> 
#> Computational engine: kknn 
#> 
finalize_model(knn_model, lowest_rmse)
#> K-Nearest Neighbor Model Specification (regression)
#> 
#> Main Arguments:
#>   neighbors = 33
#>   weight_func = triweight
#>   dist_power = 0.511191629664972
#> 
#> Computational engine: kknn 
#> 
```
