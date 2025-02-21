# tune model only - failure in recipe is caught elegantly

    Code
      cars_res <- tune_grid(helper_objects$svm_mod, preprocessor = rec, resamples = data_folds,
      grid = cars_grid, control = control_grid(extract = function(x) {
        1
      }, save_pred = TRUE))
    Message
      x Fold1: preprocessor 1/1:
        Error in `step_spline_b()`:
        Caused by error in `prep()`:
        ! `deg_free` must be a whole number, not a numeric `NA`.
      x Fold2: preprocessor 1/1:
        Error in `step_spline_b()`:
        Caused by error in `prep()`:
        ! `deg_free` must be a whole number, not a numeric `NA`.
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.

# tune model only - failure in formula is caught elegantly

    Code
      cars_res <- tune_grid(helper_objects$svm_mod, y ~ z, resamples = data_folds,
      grid = cars_grid, control = control_grid(extract = function(x) {
        1
      }, save_pred = TRUE))
    Message
      x Fold1: preprocessor 1/1:
        Error in `hardhat::mold()`:
        ! The following predictor was not found in `data`: "z".
      x Fold2: preprocessor 1/1:
        Error in `hardhat::mold()`:
        ! The following predictor was not found in `data`: "z".
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.

# argument order gives errors for recipes

    Code
      tune_grid(helper_objects$rec_tune_1, helper_objects$lm_mod, rsample::vfold_cv(
        mtcars, v = 2))
    Condition
      Error in `tune_grid()`:
      ! The first argument to `tune_grid()` should be either a model or workflow, not a <recipe> object.

# argument order gives errors for formula

    Code
      tune_grid(mpg ~ ., helper_objects$lm_mod, rsample::vfold_cv(mtcars, v = 2))
    Condition
      Error in `tune_grid()`:
      ! The first argument to `tune_grid()` should be either a model or workflow, not a <formula> object.

# ellipses with tune_grid

    Code
      tune_grid(wflow, resamples = folds, grid = 3, something = "wrong")
    Condition
      Warning:
      The `...` are not used in this function but 1 object was passed: "something"
    Output
      # Tuning results
      # 10-fold cross-validation 
      # A tibble: 10 x 4
         splits         id     .metrics         .notes          
         <list>         <chr>  <list>           <list>          
       1 <split [28/4]> Fold01 <tibble [6 x 5]> <tibble [0 x 4]>
       2 <split [28/4]> Fold02 <tibble [6 x 5]> <tibble [0 x 4]>
       3 <split [29/3]> Fold03 <tibble [6 x 5]> <tibble [0 x 4]>
       4 <split [29/3]> Fold04 <tibble [6 x 5]> <tibble [0 x 4]>
       5 <split [29/3]> Fold05 <tibble [6 x 5]> <tibble [0 x 4]>
       6 <split [29/3]> Fold06 <tibble [6 x 5]> <tibble [0 x 4]>
       7 <split [29/3]> Fold07 <tibble [6 x 5]> <tibble [0 x 4]>
       8 <split [29/3]> Fold08 <tibble [6 x 5]> <tibble [0 x 4]>
       9 <split [29/3]> Fold09 <tibble [6 x 5]> <tibble [0 x 4]>
      10 <split [29/3]> Fold10 <tibble [6 x 5]> <tibble [0 x 4]>

