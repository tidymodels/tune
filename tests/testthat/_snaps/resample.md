# failure in recipe is caught elegantly

    Code
      result <- fit_resamples(lin_mod, rec, folds, control = control)
    Message
      x Fold1: preprocessor 1/1:
        Error in `step_ns()`:
        Caused by error in `if (...) NULL`:
        ! missing value where TRUE/FALSE needed
      x Fold2: preprocessor 1/1:
        Error in `step_ns()`:
        Caused by error in `if (...) NULL`:
        ! missing value where TRUE/FALSE needed
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.

# failure in variables tidyselect specification is caught elegantly

    Code
      result <- fit_resamples(workflow, folds, control = control)
    Message
      x Fold1: preprocessor 1/1:
        Error in `fit()`:
        ! Can't subset columns that don't exist.
        x Column `foobar` doesn't exist.
      x Fold2: preprocessor 1/1:
        Error in `fit()`:
        ! Can't subset columns that don't exist.
        x Column `foobar` doesn't exist.
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.

# classification models generate correct error message

    Code
      result <- fit_resamples(log_mod, rec, folds, control = control)
    Message
      x Fold1: preprocessor 1/1, model 1/1:
        Error in `check_outcome()`:
        ! For a classification model, the outcome should be a `factor`, not a ...
      x Fold2: preprocessor 1/1, model 1/1:
        Error in `check_outcome()`:
        ! For a classification model, the outcome should be a `factor`, not a ...
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.

# `tune_grid()` falls back to `fit_resamples()` - formula

    Code
      result <- tune_grid(lin_mod, mpg ~ ., folds)
    Condition
      Warning:
      No tuning parameters have been detected, performance will be evaluated using the resamples with no tuning. Did you want to [tune()] parameters?

# `tune_grid()` falls back to `fit_resamples()` - workflow variables

    Code
      result <- tune_grid(wf, folds)
    Condition
      Warning:
      No tuning parameters have been detected, performance will be evaluated using the resamples with no tuning. Did you want to [tune()] parameters?

# `tune_grid()` ignores `grid` if there are no tuning parameters

    Code
      result <- lin_mod %>% tune_grid(mpg ~ ., grid = data.frame(x = 1), folds)
    Condition
      Warning:
      No tuning parameters have been detected, performance will be evaluated using the resamples with no tuning. Did you want to [tune()] parameters?

# cannot autoplot `fit_resamples()` results

    Code
      autoplot(result)
    Condition
      Error in `autoplot()`:
      ! There is no `autoplot()` implementation for `resample_results`.

# ellipses with fit_resamples

    Code
      lin_mod %>% fit_resamples(mpg ~ ., folds, something = "wrong")
    Condition
      Warning:
      The `...` are not used in this function but one or more objects were passed: 'something'
    Output
      # Resampling results
      # 2-fold cross-validation 
      # A tibble: 2 x 4
        splits          id    .metrics         .notes          
        <list>          <chr> <list>           <list>          
      1 <split [16/16]> Fold1 <tibble [2 x 4]> <tibble [0 x 3]>
      2 <split [16/16]> Fold2 <tibble [2 x 4]> <tibble [0 x 3]>

# argument order gives errors for recipe/formula

    Code
      fit_resamples(rec, lin_mod, folds)
    Condition
      Error in `fit_resamples()`:
      ! The first argument to [fit_resamples()] should be either a model or workflow.

---

    Code
      fit_resamples(mpg ~ ., lin_mod, folds)
    Condition
      Error in `fit_resamples()`:
      ! The first argument to [fit_resamples()] should be either a model or workflow.

# retain extra attributes

    Code
      fit_resamples(lin_mod, recipes::recipe(mpg ~ ., mtcars[rep(1:32, 3000), ]),
      folds, control = control_resamples(save_workflow = TRUE))
    Message
      i The workflow being saved contains a recipe, which is 8.07 Mb in i memory. If
      this was not intentional, please set the control setting i `save_workflow =
      FALSE`.
    Output
      # Resampling results
      # 2-fold cross-validation 
      # A tibble: 2 x 4
        splits          id    .metrics         .notes          
        <list>          <chr> <list>           <list>          
      1 <split [16/16]> Fold1 <tibble [2 x 4]> <tibble [0 x 3]>
      2 <split [16/16]> Fold2 <tibble [2 x 4]> <tibble [0 x 3]>

# `fit_resamples()` when objects need tuning

    2 arguments have been tagged for tuning in these components: model_spec and recipe. 
    Please use one of the tuning functions (e.g. `tune_grid()`) to optimize them.

---

    1 argument has been tagged for tuning in this component: model_spec. 
    Please use one of the tuning functions (e.g. `tune_grid()`) to optimize them.

---

    1 argument has been tagged for tuning in this component: recipe. 
    Please use one of the tuning functions (e.g. `tune_grid()`) to optimize them.

