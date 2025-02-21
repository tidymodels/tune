# tune recipe only

    Code
      tune_bayes(wflow, resamples = folds, param_info = pset, initial = iter1, iter = iter2,
        control = control_bayes(verbose = TRUE))
    Message
      
      >  Generating a set of 2 initial parameter results
      v Initialization complete
      
      i Gaussian process model
      ! The Gaussian process model is being fit using 1 features but only has 2
        data points to do so. This may cause errors or a poor model fit.
      v Gaussian process model
      i Generating 3 candidates
      i Predicted candidates
      i Estimating performance
      i Fold01: preprocessor 1/1
      v Fold01: preprocessor 1/1
      i Fold01: preprocessor 1/1, model 1/1
      v Fold01: preprocessor 1/1, model 1/1
      i Fold01: preprocessor 1/1, model 1/1 (extracts)
      i Fold01: preprocessor 1/1, model 1/1 (predictions)
      i Fold02: preprocessor 1/1
      v Fold02: preprocessor 1/1
      i Fold02: preprocessor 1/1, model 1/1
      v Fold02: preprocessor 1/1, model 1/1
      i Fold02: preprocessor 1/1, model 1/1 (extracts)
      i Fold02: preprocessor 1/1, model 1/1 (predictions)
      i Fold03: preprocessor 1/1
      v Fold03: preprocessor 1/1
      i Fold03: preprocessor 1/1, model 1/1
      v Fold03: preprocessor 1/1, model 1/1
      i Fold03: preprocessor 1/1, model 1/1 (extracts)
      i Fold03: preprocessor 1/1, model 1/1 (predictions)
      i Fold04: preprocessor 1/1
      v Fold04: preprocessor 1/1
      i Fold04: preprocessor 1/1, model 1/1
      v Fold04: preprocessor 1/1, model 1/1
      i Fold04: preprocessor 1/1, model 1/1 (extracts)
      i Fold04: preprocessor 1/1, model 1/1 (predictions)
      i Fold05: preprocessor 1/1
      v Fold05: preprocessor 1/1
      i Fold05: preprocessor 1/1, model 1/1
      v Fold05: preprocessor 1/1, model 1/1
      i Fold05: preprocessor 1/1, model 1/1 (extracts)
      i Fold05: preprocessor 1/1, model 1/1 (predictions)
      i Fold06: preprocessor 1/1
      v Fold06: preprocessor 1/1
      i Fold06: preprocessor 1/1, model 1/1
      v Fold06: preprocessor 1/1, model 1/1
      i Fold06: preprocessor 1/1, model 1/1 (extracts)
      i Fold06: preprocessor 1/1, model 1/1 (predictions)
      i Fold07: preprocessor 1/1
      v Fold07: preprocessor 1/1
      i Fold07: preprocessor 1/1, model 1/1
      v Fold07: preprocessor 1/1, model 1/1
      i Fold07: preprocessor 1/1, model 1/1 (extracts)
      i Fold07: preprocessor 1/1, model 1/1 (predictions)
      i Fold08: preprocessor 1/1
      v Fold08: preprocessor 1/1
      i Fold08: preprocessor 1/1, model 1/1
      v Fold08: preprocessor 1/1, model 1/1
      i Fold08: preprocessor 1/1, model 1/1 (extracts)
      i Fold08: preprocessor 1/1, model 1/1 (predictions)
      i Fold09: preprocessor 1/1
      v Fold09: preprocessor 1/1
      i Fold09: preprocessor 1/1, model 1/1
      v Fold09: preprocessor 1/1, model 1/1
      i Fold09: preprocessor 1/1, model 1/1 (extracts)
      i Fold09: preprocessor 1/1, model 1/1 (predictions)
      i Fold10: preprocessor 1/1
      v Fold10: preprocessor 1/1
      i Fold10: preprocessor 1/1, model 1/1
      v Fold10: preprocessor 1/1, model 1/1
      i Fold10: preprocessor 1/1, model 1/1 (extracts)
      i Fold10: preprocessor 1/1, model 1/1 (predictions)
      v Estimating performance
      i Gaussian process model
      v Gaussian process model
      i Generating 2 candidates
      i Predicted candidates
      i Estimating performance
      i Fold01: preprocessor 1/1
      v Fold01: preprocessor 1/1
      i Fold01: preprocessor 1/1, model 1/1
      v Fold01: preprocessor 1/1, model 1/1
      i Fold01: preprocessor 1/1, model 1/1 (extracts)
      i Fold01: preprocessor 1/1, model 1/1 (predictions)
      i Fold02: preprocessor 1/1
      v Fold02: preprocessor 1/1
      i Fold02: preprocessor 1/1, model 1/1
      v Fold02: preprocessor 1/1, model 1/1
      i Fold02: preprocessor 1/1, model 1/1 (extracts)
      i Fold02: preprocessor 1/1, model 1/1 (predictions)
      i Fold03: preprocessor 1/1
      v Fold03: preprocessor 1/1
      i Fold03: preprocessor 1/1, model 1/1
      v Fold03: preprocessor 1/1, model 1/1
      i Fold03: preprocessor 1/1, model 1/1 (extracts)
      i Fold03: preprocessor 1/1, model 1/1 (predictions)
      i Fold04: preprocessor 1/1
      v Fold04: preprocessor 1/1
      i Fold04: preprocessor 1/1, model 1/1
      v Fold04: preprocessor 1/1, model 1/1
      i Fold04: preprocessor 1/1, model 1/1 (extracts)
      i Fold04: preprocessor 1/1, model 1/1 (predictions)
      i Fold05: preprocessor 1/1
      v Fold05: preprocessor 1/1
      i Fold05: preprocessor 1/1, model 1/1
      v Fold05: preprocessor 1/1, model 1/1
      i Fold05: preprocessor 1/1, model 1/1 (extracts)
      i Fold05: preprocessor 1/1, model 1/1 (predictions)
      i Fold06: preprocessor 1/1
      v Fold06: preprocessor 1/1
      i Fold06: preprocessor 1/1, model 1/1
      v Fold06: preprocessor 1/1, model 1/1
      i Fold06: preprocessor 1/1, model 1/1 (extracts)
      i Fold06: preprocessor 1/1, model 1/1 (predictions)
      i Fold07: preprocessor 1/1
      v Fold07: preprocessor 1/1
      i Fold07: preprocessor 1/1, model 1/1
      v Fold07: preprocessor 1/1, model 1/1
      i Fold07: preprocessor 1/1, model 1/1 (extracts)
      i Fold07: preprocessor 1/1, model 1/1 (predictions)
      i Fold08: preprocessor 1/1
      v Fold08: preprocessor 1/1
      i Fold08: preprocessor 1/1, model 1/1
      v Fold08: preprocessor 1/1, model 1/1
      i Fold08: preprocessor 1/1, model 1/1 (extracts)
      i Fold08: preprocessor 1/1, model 1/1 (predictions)
      i Fold09: preprocessor 1/1
      v Fold09: preprocessor 1/1
      i Fold09: preprocessor 1/1, model 1/1
      v Fold09: preprocessor 1/1, model 1/1
      i Fold09: preprocessor 1/1, model 1/1 (extracts)
      i Fold09: preprocessor 1/1, model 1/1 (predictions)
      i Fold10: preprocessor 1/1
      v Fold10: preprocessor 1/1
      i Fold10: preprocessor 1/1, model 1/1
      v Fold10: preprocessor 1/1, model 1/1
      i Fold10: preprocessor 1/1, model 1/1 (extracts)
      i Fold10: preprocessor 1/1, model 1/1 (predictions)
      v Estimating performance
    Output
      # Tuning results
      # 10-fold cross-validation 
      # A tibble: 30 x 5
         splits         id     .metrics         .notes           .iter
         <list>         <chr>  <list>           <list>           <int>
       1 <split [28/4]> Fold01 <tibble [4 x 5]> <tibble [0 x 4]>     0
       2 <split [28/4]> Fold02 <tibble [4 x 5]> <tibble [0 x 4]>     0
       3 <split [29/3]> Fold03 <tibble [4 x 5]> <tibble [0 x 4]>     0
       4 <split [29/3]> Fold04 <tibble [4 x 5]> <tibble [0 x 4]>     0
       5 <split [29/3]> Fold05 <tibble [4 x 5]> <tibble [0 x 4]>     0
       6 <split [29/3]> Fold06 <tibble [4 x 5]> <tibble [0 x 4]>     0
       7 <split [29/3]> Fold07 <tibble [4 x 5]> <tibble [0 x 4]>     0
       8 <split [29/3]> Fold08 <tibble [4 x 5]> <tibble [0 x 4]>     0
       9 <split [29/3]> Fold09 <tibble [4 x 5]> <tibble [0 x 4]>     0
      10 <split [29/3]> Fold10 <tibble [4 x 5]> <tibble [0 x 4]>     0
      # i 20 more rows

---

    Code
      tune_bayes(wflow, resamples = folds, param_info = pset, initial = iter1, iter = iter2,
        control = control_bayes(verbose_iter = TRUE))
    Message
      Optimizing rmse using the expected improvement
      
      -- Iteration 1 -----------------------------------------------------------------
      
      i Current best:		rmse=2.461 (@iter 0)
      i Gaussian process model
      ! The Gaussian process model is being fit using 1 features but only has 2
        data points to do so. This may cause errors or a poor model fit.
      v Gaussian process model
      i Generating 3 candidates
      i Predicted candidates
      i num_comp=5
      i Estimating performance
      v Estimating performance
      <3 Newest results:	rmse=2.453 (+/-0.381)
      
      -- Iteration 2 -----------------------------------------------------------------
      
      i Current best:		rmse=2.453 (@iter 1)
      i Gaussian process model
      v Gaussian process model
      i Generating 2 candidates
      i Predicted candidates
      i num_comp=1
      i Estimating performance
      v Estimating performance
      (x) Newest results:	rmse=2.646 (+/-0.286)
    Output
      # Tuning results
      # 10-fold cross-validation 
      # A tibble: 30 x 5
         splits         id     .metrics         .notes           .iter
         <list>         <chr>  <list>           <list>           <int>
       1 <split [28/4]> Fold01 <tibble [4 x 5]> <tibble [0 x 4]>     0
       2 <split [28/4]> Fold02 <tibble [4 x 5]> <tibble [0 x 4]>     0
       3 <split [29/3]> Fold03 <tibble [4 x 5]> <tibble [0 x 4]>     0
       4 <split [29/3]> Fold04 <tibble [4 x 5]> <tibble [0 x 4]>     0
       5 <split [29/3]> Fold05 <tibble [4 x 5]> <tibble [0 x 4]>     0
       6 <split [29/3]> Fold06 <tibble [4 x 5]> <tibble [0 x 4]>     0
       7 <split [29/3]> Fold07 <tibble [4 x 5]> <tibble [0 x 4]>     0
       8 <split [29/3]> Fold08 <tibble [4 x 5]> <tibble [0 x 4]>     0
       9 <split [29/3]> Fold09 <tibble [4 x 5]> <tibble [0 x 4]>     0
      10 <split [29/3]> Fold10 <tibble [4 x 5]> <tibble [0 x 4]>     0
      # i 20 more rows

---

    Code
      tune_bayes(wflow, resamples = folds, param_info = pset, initial = iter1, iter = iter2,
        control = control_bayes(verbose_iter = TRUE, verbose = TRUE))
    Message
      
      >  Generating a set of 2 initial parameter results
      v Initialization complete
      
      Optimizing rmse using the expected improvement
      
      -- Iteration 1 -----------------------------------------------------------------
      
      i Current best:		rmse=2.461 (@iter 0)
      i Gaussian process model
      ! The Gaussian process model is being fit using 1 features but only has 2
        data points to do so. This may cause errors or a poor model fit.
      v Gaussian process model
      i Generating 3 candidates
      i Predicted candidates
      i num_comp=5
      i Estimating performance
      i Fold01: preprocessor 1/1
      v Fold01: preprocessor 1/1
      i Fold01: preprocessor 1/1, model 1/1
      v Fold01: preprocessor 1/1, model 1/1
      i Fold01: preprocessor 1/1, model 1/1 (extracts)
      i Fold01: preprocessor 1/1, model 1/1 (predictions)
      i Fold02: preprocessor 1/1
      v Fold02: preprocessor 1/1
      i Fold02: preprocessor 1/1, model 1/1
      v Fold02: preprocessor 1/1, model 1/1
      i Fold02: preprocessor 1/1, model 1/1 (extracts)
      i Fold02: preprocessor 1/1, model 1/1 (predictions)
      i Fold03: preprocessor 1/1
      v Fold03: preprocessor 1/1
      i Fold03: preprocessor 1/1, model 1/1
      v Fold03: preprocessor 1/1, model 1/1
      i Fold03: preprocessor 1/1, model 1/1 (extracts)
      i Fold03: preprocessor 1/1, model 1/1 (predictions)
      i Fold04: preprocessor 1/1
      v Fold04: preprocessor 1/1
      i Fold04: preprocessor 1/1, model 1/1
      v Fold04: preprocessor 1/1, model 1/1
      i Fold04: preprocessor 1/1, model 1/1 (extracts)
      i Fold04: preprocessor 1/1, model 1/1 (predictions)
      i Fold05: preprocessor 1/1
      v Fold05: preprocessor 1/1
      i Fold05: preprocessor 1/1, model 1/1
      v Fold05: preprocessor 1/1, model 1/1
      i Fold05: preprocessor 1/1, model 1/1 (extracts)
      i Fold05: preprocessor 1/1, model 1/1 (predictions)
      i Fold06: preprocessor 1/1
      v Fold06: preprocessor 1/1
      i Fold06: preprocessor 1/1, model 1/1
      v Fold06: preprocessor 1/1, model 1/1
      i Fold06: preprocessor 1/1, model 1/1 (extracts)
      i Fold06: preprocessor 1/1, model 1/1 (predictions)
      i Fold07: preprocessor 1/1
      v Fold07: preprocessor 1/1
      i Fold07: preprocessor 1/1, model 1/1
      v Fold07: preprocessor 1/1, model 1/1
      i Fold07: preprocessor 1/1, model 1/1 (extracts)
      i Fold07: preprocessor 1/1, model 1/1 (predictions)
      i Fold08: preprocessor 1/1
      v Fold08: preprocessor 1/1
      i Fold08: preprocessor 1/1, model 1/1
      v Fold08: preprocessor 1/1, model 1/1
      i Fold08: preprocessor 1/1, model 1/1 (extracts)
      i Fold08: preprocessor 1/1, model 1/1 (predictions)
      i Fold09: preprocessor 1/1
      v Fold09: preprocessor 1/1
      i Fold09: preprocessor 1/1, model 1/1
      v Fold09: preprocessor 1/1, model 1/1
      i Fold09: preprocessor 1/1, model 1/1 (extracts)
      i Fold09: preprocessor 1/1, model 1/1 (predictions)
      i Fold10: preprocessor 1/1
      v Fold10: preprocessor 1/1
      i Fold10: preprocessor 1/1, model 1/1
      v Fold10: preprocessor 1/1, model 1/1
      i Fold10: preprocessor 1/1, model 1/1 (extracts)
      i Fold10: preprocessor 1/1, model 1/1 (predictions)
      v Estimating performance
      <3 Newest results:	rmse=2.453 (+/-0.381)
      
      -- Iteration 2 -----------------------------------------------------------------
      
      i Current best:		rmse=2.453 (@iter 1)
      i Gaussian process model
      v Gaussian process model
      i Generating 2 candidates
      i Predicted candidates
      i num_comp=1
      i Estimating performance
      i Fold01: preprocessor 1/1
      v Fold01: preprocessor 1/1
      i Fold01: preprocessor 1/1, model 1/1
      v Fold01: preprocessor 1/1, model 1/1
      i Fold01: preprocessor 1/1, model 1/1 (extracts)
      i Fold01: preprocessor 1/1, model 1/1 (predictions)
      i Fold02: preprocessor 1/1
      v Fold02: preprocessor 1/1
      i Fold02: preprocessor 1/1, model 1/1
      v Fold02: preprocessor 1/1, model 1/1
      i Fold02: preprocessor 1/1, model 1/1 (extracts)
      i Fold02: preprocessor 1/1, model 1/1 (predictions)
      i Fold03: preprocessor 1/1
      v Fold03: preprocessor 1/1
      i Fold03: preprocessor 1/1, model 1/1
      v Fold03: preprocessor 1/1, model 1/1
      i Fold03: preprocessor 1/1, model 1/1 (extracts)
      i Fold03: preprocessor 1/1, model 1/1 (predictions)
      i Fold04: preprocessor 1/1
      v Fold04: preprocessor 1/1
      i Fold04: preprocessor 1/1, model 1/1
      v Fold04: preprocessor 1/1, model 1/1
      i Fold04: preprocessor 1/1, model 1/1 (extracts)
      i Fold04: preprocessor 1/1, model 1/1 (predictions)
      i Fold05: preprocessor 1/1
      v Fold05: preprocessor 1/1
      i Fold05: preprocessor 1/1, model 1/1
      v Fold05: preprocessor 1/1, model 1/1
      i Fold05: preprocessor 1/1, model 1/1 (extracts)
      i Fold05: preprocessor 1/1, model 1/1 (predictions)
      i Fold06: preprocessor 1/1
      v Fold06: preprocessor 1/1
      i Fold06: preprocessor 1/1, model 1/1
      v Fold06: preprocessor 1/1, model 1/1
      i Fold06: preprocessor 1/1, model 1/1 (extracts)
      i Fold06: preprocessor 1/1, model 1/1 (predictions)
      i Fold07: preprocessor 1/1
      v Fold07: preprocessor 1/1
      i Fold07: preprocessor 1/1, model 1/1
      v Fold07: preprocessor 1/1, model 1/1
      i Fold07: preprocessor 1/1, model 1/1 (extracts)
      i Fold07: preprocessor 1/1, model 1/1 (predictions)
      i Fold08: preprocessor 1/1
      v Fold08: preprocessor 1/1
      i Fold08: preprocessor 1/1, model 1/1
      v Fold08: preprocessor 1/1, model 1/1
      i Fold08: preprocessor 1/1, model 1/1 (extracts)
      i Fold08: preprocessor 1/1, model 1/1 (predictions)
      i Fold09: preprocessor 1/1
      v Fold09: preprocessor 1/1
      i Fold09: preprocessor 1/1, model 1/1
      v Fold09: preprocessor 1/1, model 1/1
      i Fold09: preprocessor 1/1, model 1/1 (extracts)
      i Fold09: preprocessor 1/1, model 1/1 (predictions)
      i Fold10: preprocessor 1/1
      v Fold10: preprocessor 1/1
      i Fold10: preprocessor 1/1, model 1/1
      v Fold10: preprocessor 1/1, model 1/1
      i Fold10: preprocessor 1/1, model 1/1 (extracts)
      i Fold10: preprocessor 1/1, model 1/1 (predictions)
      v Estimating performance
      (x) Newest results:	rmse=2.646 (+/-0.286)
    Output
      # Tuning results
      # 10-fold cross-validation 
      # A tibble: 30 x 5
         splits         id     .metrics         .notes           .iter
         <list>         <chr>  <list>           <list>           <int>
       1 <split [28/4]> Fold01 <tibble [4 x 5]> <tibble [0 x 4]>     0
       2 <split [28/4]> Fold02 <tibble [4 x 5]> <tibble [0 x 4]>     0
       3 <split [29/3]> Fold03 <tibble [4 x 5]> <tibble [0 x 4]>     0
       4 <split [29/3]> Fold04 <tibble [4 x 5]> <tibble [0 x 4]>     0
       5 <split [29/3]> Fold05 <tibble [4 x 5]> <tibble [0 x 4]>     0
       6 <split [29/3]> Fold06 <tibble [4 x 5]> <tibble [0 x 4]>     0
       7 <split [29/3]> Fold07 <tibble [4 x 5]> <tibble [0 x 4]>     0
       8 <split [29/3]> Fold08 <tibble [4 x 5]> <tibble [0 x 4]>     0
       9 <split [29/3]> Fold09 <tibble [4 x 5]> <tibble [0 x 4]>     0
      10 <split [29/3]> Fold10 <tibble [4 x 5]> <tibble [0 x 4]>     0
      # i 20 more rows

# tune model only - failure in recipe is caught elegantly

    Code
      cars_res <- tune_bayes(svm_mod, preprocessor = rec, resamples = data_folds)
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
    Message
      x Optimization stopped prematurely; returning current results.

# tune model only - failure in formula is caught elegantly

    Code
      cars_res <- tune_bayes(wflow, resamples = data_folds, control = control_bayes(
        extract = function(x) {
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
    Message
      x Optimization stopped prematurely; returning current results.

# argument order gives an error for recipes

    Code
      tune_bayes(rec_tune_1, model = lm_mod, resamples = rsample::vfold_cv(mtcars, v = 2),
      param_info = extract_parameter_set_dials(rec_tune_1), iter = iter1, initial = iter2)
    Condition
      Error in `tune_bayes()`:
      ! The first argument to `tune_bayes()` should be either a model or workflow, not a <recipe> object.

# argument order gives an error for formula

    Code
      tune_bayes(mpg ~ ., svm_mod, resamples = rsample::vfold_cv(mtcars, v = 2),
      param_info = extract_parameter_set_dials(svm_mod), initial = iter1, iter = iter2)
    Condition
      Error in `tune_bayes()`:
      ! The first argument to `tune_bayes()` should be either a model or workflow, not a <formula> object.

# retain extra attributes and saved GP candidates

    Code
      setdiff(new_obj, current_objs)
    Output
      [1] "candidates" "gp_fit"     "i"          "score_card"

---

    Code
      res2 <- tune_bayes(wflow, resamples = folds, param_info = pset, initial = iter1,
        iter = iter2, control = control_bayes(save_workflow = TRUE))
    Message
      ! The Gaussian process model is being fit using 1 features but only has 2
        data points to do so. This may cause errors or a poor model fit.

# too few starting values

    Code
      tune:::check_bayes_initial_size(5, 3, FALSE)
    Message
      ! There are 5 tuning parameters and 3 grid points were requested.
      * There are as many tuning parameters as there are initial points. This is
        likely to cause numerical issues in the first few search iterations.

---

    Code
      tune:::check_bayes_initial_size(5, 3, TRUE)
    Message
      ! There are 5 tuning parameters and 3 grid points were requested.
      * There are as many tuning parameters as there are initial points. This is
        likely to cause numerical issues in the first few search iterations.
      * With racing, only completely resampled parameters are used.

---

    Code
      tune:::check_bayes_initial_size(2, 2, FALSE)
    Message
      ! There are 2 tuning parameters and 2 grid points were requested.
      * There are as many tuning parameters as there are initial points. This is
        likely to cause numerical issues in the first few search iterations.

---

    Code
      tune:::check_bayes_initial_size(5, 1, FALSE)
    Condition
      Error in `tune:::check_bayes_initial_size()`:
      ! There are 5 tuning parameters and 1 grid point was requested.
      i The GP model requires 2+ initial points. For best performance, supply more initial points than there are tuning parameters.

---

    Code
      tune:::check_bayes_initial_size(5, 1, TRUE)
    Condition
      Error in `tune:::check_bayes_initial_size()`:
      ! There are 5 tuning parameters and 1 grid point was requested.
      i The GP model requires 2+ initial points. For best performance, supply more initial points than there are tuning parameters.
      i With racing, only completely resampled parameters are used.

---

    Code
      tune:::check_bayes_initial_size(1, 1, FALSE)
    Condition
      Error in `tune:::check_bayes_initial_size()`:
      ! There is 1 tuning parameter and 1 grid point was requested.
      i The GP model requires 2+ initial points. For best performance, supply more initial points than there are tuning parameters.

# missing performance values

    Code
      set.seed(1)
      res <- mod %>% tune_bayes(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built +
        Bldg_Type + Latitude + Longitude, resamples = folds, initial = 3, metrics = yardstick::metric_set(
        rsq), param_info = parameters(dials::cost_complexity(c(-2, 0))))
    Message
      ! validation: internal: A correlation computation is required, but `estimate` is constant and ha...
      ! For the rsq estimates, 1 missing value was found and removed before fitting
        the Gaussian process model.
      ! The Gaussian process model is being fit using 1 features but only has 2
        data points to do so. This may cause errors or a poor model fit.
      ! For the rsq estimates, 1 missing value was found and removed before fitting
        the Gaussian process model.
      ! validation: internal: A correlation computation is required, but `estimate` is constant and ha...
      ! For the rsq estimates, 2 missing values were found and removed before
        fitting the Gaussian process model.
      ! validation: internal: A correlation computation is required, but `estimate` is constant and ha...
      ! For the rsq estimates, 3 missing values were found and removed before
        fitting the Gaussian process model.
      ! validation: internal: A correlation computation is required, but `estimate` is constant and ha...
      ! For the rsq estimates, 4 missing values were found and removed before
        fitting the Gaussian process model.
      ! validation: internal: A correlation computation is required, but `estimate` is constant and ha...
      ! For the rsq estimates, 5 missing values were found and removed before
        fitting the Gaussian process model.
      ! validation: internal: A correlation computation is required, but `estimate` is constant and ha...
      ! For the rsq estimates, 6 missing values were found and removed before
        fitting the Gaussian process model.
      ! validation: internal: A correlation computation is required, but `estimate` is constant and ha...
      ! For the rsq estimates, 7 missing values were found and removed before
        fitting the Gaussian process model.
      ! validation: internal: A correlation computation is required, but `estimate` is constant and ha...
      ! For the rsq estimates, 8 missing values were found and removed before
        fitting the Gaussian process model.
      ! validation: internal: A correlation computation is required, but `estimate` is constant and ha...
      ! For the rsq estimates, 9 missing values were found and removed before
        fitting the Gaussian process model.
      ! validation: internal: A correlation computation is required, but `estimate` is constant and ha...
      ! No improvement for 10 iterations; returning current results.

---

    Code
      set.seed(2)
      res_fail <- mod %>% tune_bayes(Sale_Price ~ Neighborhood + Gr_Liv_Area +
        Year_Built + Bldg_Type + Latitude + Longitude, resamples = folds, initial = 5,
      metrics = yardstick::metric_set(rsq), param_info = parameters(dials::cost_complexity(
        c(0.5, 0))))
    Message
      ! validation: internal: A correlation computation is required, but `estimate` is constant and ha...
      ! validation: internal: A correlation computation is required, but `estimate` is constant and ha...
      ! validation: internal: A correlation computation is required, but `estimate` is constant and ha...
      ! validation: internal: A correlation computation is required, but `estimate` is constant and ha...
      ! validation: internal: A correlation computation is required, but `estimate` is constant and ha...
      ! All of the rsq estimates were missing. The Gaussian process model cannot be
        fit to the data.
      ! Gaussian process model: no non-missing arguments to min; returning Inf, no non-missing arguments...
      x Gaussian process model: Error in seq_len(n - 1L): argument must be coercible to non-negative int...
    Condition
      Error in `check_gp_failure()`:
      ! Gaussian process model was not fit.
    Message
      x Optimization stopped prematurely; returning current results.

# tune_bayes() output for `iter` edge cases (#721)

    Code
      tune_bayes(wf, boots, iter = -1)
    Condition
      Error in `tune_bayes()`:
      ! The `iter` argument must be a non-negative integer.

---

    Code
      tune_bayes(wf, boots, iter = c(-1, 0, 1))
    Condition
      Error in `tune_bayes()`:
      ! The `iter` argument must be a non-negative integer.

---

    Code
      tune_bayes(wf, boots, iter = c(0, 1, 2))
    Condition
      Error in `tune_bayes()`:
      ! The `iter` argument must be a non-negative integer.

---

    Code
      tune_bayes(wf, boots, iter = NA)
    Condition
      Error in `tune_bayes()`:
      ! The `iter` argument must be a non-negative integer.

---

    Code
      tune_bayes(wf, boots, iter = NULL)
    Condition
      Error in `tune_bayes()`:
      ! The `iter` argument must be a non-negative integer.

