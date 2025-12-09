# Merge parameter grid values into objects

[`merge()`](https://rdrr.io/r/base/merge.html) can be used to easily
update any of the arguments in a parsnip model or recipe.

## Usage

``` r
# S3 method for class 'recipe'
merge(x, y, ...)

# S3 method for class 'model_spec'
merge(x, y, ...)
```

## Arguments

- x:

  A recipe or model specification object.

- y:

  A data frame or a parameter grid resulting from one of the `grid_*`
  functions. The column names should correspond to the parameter names
  (or their annotations) in the object.

- ...:

  Not used but required for S3 completeness.

## Value

A tibble with a column `x` that has as many rows as were in `y`.

## Examples

``` r
library(tibble)
library(recipes)
library(parsnip)
library(dials)
#> Loading required package: scales
#> 
#> Attaching package: ‘scales’
#> The following object is masked from ‘package:purrr’:
#> 
#>     discard

pca_rec <-
  recipe(mpg ~ ., data = mtcars) |>
  step_impute_knn(all_predictors(), neighbors = tune()) |>
  step_pca(all_predictors(), num_comp = tune())

pca_grid <-
  tribble(
    ~neighbors, ~num_comp,
             1,         1,
             5,         1,
             1,         2,
             5,         2
  )

merge(pca_rec, pca_grid)
#> # A tibble: 4 × 1
#>   x       
#>   <list>  
#> 1 <recipe>
#> 2 <recipe>
#> 3 <recipe>
#> 4 <recipe>

spline_rec <-
  recipe(mpg ~ ., data = mtcars) |>
  step_spline_natural(disp, deg_free = tune("disp df")) |>
  step_spline_natural(wt, deg_free = tune("wt df"))

spline_grid <-
  tribble(
    ~"disp df", ~ "wt df",
    3,         3,
    5,         3,
    3,         5,
    5,         5
  )

merge(pca_rec, pca_grid)
#> # A tibble: 4 × 1
#>   x       
#>   <list>  
#> 1 <recipe>
#> 2 <recipe>
#> 3 <recipe>
#> 4 <recipe>

data(hpc_data, package = "modeldata")

xgb_mod <-
  boost_tree(trees = tune(), min_n = tune()) |>
  set_engine("xgboost")

set.seed(254)
xgb_grid <-
  extract_parameter_set_dials(xgb_mod) |>
  finalize(hpc_data) |>
  grid_max_entropy(size = 3)
#> Warning: `grid_max_entropy()` was deprecated in dials 1.3.0.
#> ℹ Please use `grid_space_filling()` instead.

merge(xgb_mod, xgb_grid)
#> # A tibble: 3 × 1
#>   x        
#>   <list>   
#> 1 <spec[?]>
#> 2 <spec[?]>
#> 3 <spec[?]>
```
