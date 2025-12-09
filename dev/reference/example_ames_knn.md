# Example Analysis of Ames Housing Data

Example Analysis of Ames Housing Data

## Value

- ames_wflow:

  A workflow object

- ames_grid_search,ames_iter_search:

  Results of model tuning.

## Details

These objects are the results of an analysis of the Ames housing data. A
K-nearest neighbors model was used with a small predictor set that
included natural spline transformations of the `Longitude` and
`Latitude` predictors. The code used to generate these examples was:

    library(tidymodels)
    library(tune)
    library(AmesHousing)

    # ------------------------------------------------------------------------------

    ames <- make_ames()

    set.seed(4595)
    data_split <- initial_split(ames, strata = "Sale_Price")

    ames_train <- training(data_split)

    set.seed(2453)
    rs_splits <- vfold_cv(ames_train, strata = "Sale_Price")

    # ------------------------------------------------------------------------------

    ames_rec <-
      recipe(Sale_Price ~ ., data = ames_train) |>
      step_log(Sale_Price, base = 10) |>
      step_YeoJohnson(Lot_Area, Gr_Liv_Area) |>
      step_other(Neighborhood, threshold = .1)  |>
      step_dummy(all_nominal()) |>
      step_zv(all_predictors()) |>
      step_spline_natural(Longitude, deg_free = tune("lon")) |>
      step_spline_natural(Latitude, deg_free = tune("lat"))

    knn_model <-
      nearest_neighbor(
        mode = "regression",
        neighbors = tune("K"),
        weight_func = tune(),
        dist_power = tune()
      ) |>
      set_engine("kknn")

    ames_wflow <-
      workflow() |>
      add_recipe(ames_rec) |>
      add_model(knn_model)

    ames_set <-
      extract_parameter_set_dials(ames_wflow) |>
      update(K = neighbors(c(1, 50)))

    set.seed(7014)
    ames_grid <-
      ames_set |>
      grid_max_entropy(size = 10)

    ames_grid_search <-
      tune_grid(
        ames_wflow,
        resamples = rs_splits,
        grid = ames_grid
      )

    set.seed(2082)
    ames_iter_search <-
      tune_bayes(
        ames_wflow,
        resamples = rs_splits,
        param_info = ames_set,
        initial = ames_grid_search,
        iter = 15
      )

**important note**: Since the `rsample` split columns contain a
reference to the same data, saving them to disk can results in large
object sizes when the object is later used. In essence, R replaces all
of those references with the actual data. For this reason, we saved
zero-row tibbles in their place. This doesn't affect how we use these
objects in examples but be advised that using some `rsample` functions
on them will cause issues.

## Examples

``` r
library(tune)

ames_grid_search
#> # Tuning results
#> # 10-fold cross-validation using stratification 
#> # A tibble: 10 × 4
#>    splits           id     .metrics          .notes          
#>    <list>           <chr>  <list>            <list>          
#>  1 <split [1976/0]> Fold01 <tibble [20 × 9]> <tibble [0 × 4]>
#>  2 <split [1976/0]> Fold02 <tibble [20 × 9]> <tibble [0 × 4]>
#>  3 <split [1976/0]> Fold03 <tibble [20 × 9]> <tibble [0 × 4]>
#>  4 <split [1976/0]> Fold04 <tibble [20 × 9]> <tibble [0 × 4]>
#>  5 <split [1977/0]> Fold05 <tibble [20 × 9]> <tibble [0 × 4]>
#>  6 <split [1977/0]> Fold06 <tibble [20 × 9]> <tibble [0 × 4]>
#>  7 <split [1978/0]> Fold07 <tibble [20 × 9]> <tibble [0 × 4]>
#>  8 <split [1978/0]> Fold08 <tibble [20 × 9]> <tibble [0 × 4]>
#>  9 <split [1979/0]> Fold09 <tibble [20 × 9]> <tibble [0 × 4]>
#> 10 <split [1980/0]> Fold10 <tibble [20 × 9]> <tibble [0 × 4]>
ames_iter_search
#> # Tuning results
#> # 10-fold cross-validation using stratification 
#> # A tibble: 110 × 5
#>    splits           id     .metrics          .notes           .iter
#>    <list>           <chr>  <list>            <list>           <int>
#>  1 <split [1976/0]> Fold01 <tibble [20 × 9]> <tibble [0 × 4]>     0
#>  2 <split [1976/0]> Fold02 <tibble [20 × 9]> <tibble [0 × 4]>     0
#>  3 <split [1976/0]> Fold03 <tibble [20 × 9]> <tibble [0 × 4]>     0
#>  4 <split [1976/0]> Fold04 <tibble [20 × 9]> <tibble [0 × 4]>     0
#>  5 <split [1977/0]> Fold05 <tibble [20 × 9]> <tibble [0 × 4]>     0
#>  6 <split [1977/0]> Fold06 <tibble [20 × 9]> <tibble [0 × 4]>     0
#>  7 <split [1978/0]> Fold07 <tibble [20 × 9]> <tibble [0 × 4]>     0
#>  8 <split [1978/0]> Fold08 <tibble [20 × 9]> <tibble [0 × 4]>     0
#>  9 <split [1979/0]> Fold09 <tibble [20 × 9]> <tibble [0 × 4]>     0
#> 10 <split [1980/0]> Fold10 <tibble [20 × 9]> <tibble [0 × 4]>     0
#> # ℹ 100 more rows
```
