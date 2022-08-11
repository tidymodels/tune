# tune model only - failure in recipe is caught elegantly

    Code
      cars_res <- tune_bayes(svm_mod, preprocessor = rec, resamples = data_folds)
    Message
      x Fold1: preprocessor 1/1: Error in if (!is.null(args$df) && is.null(args$knots) && args$df - degre...
      x Fold2: preprocessor 1/1: Error in if (!is.null(args$df) && is.null(args$knots) && args$df - degre...
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.
      Error in `estimate_tune_results()`:
      ! All of the models failed. See the .notes column.
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
        Error in `get_all_predictors()`:
        ! The following predictors were not found in `data`: 'z'.
      x Fold2: preprocessor 1/1:
        Error in `get_all_predictors()`:
        ! The following predictors were not found in `data`: 'z'.
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.
      Error in `estimate_tune_results()`:
      ! All of the models failed. See the .notes column.
    Message
      x Optimization stopped prematurely; returning current results.

# argument order gives an error for recipes

    Code
      tune_bayes(rec_tune_1, model = lm_mod, resamples = rsample::vfold_cv(mtcars, v = 2),
      param_info = extract_parameter_set_dials(rec_tune_1), iter = iter1, initial = iter2)
    Condition
      Error in `tune_bayes()`:
      ! The first argument to [tune_bayes()] should be either a model or workflow.

# argument order gives an error for formula

    Code
      tune_bayes(mpg ~ ., svm_mod, resamples = rsample::vfold_cv(mtcars, v = 2),
      param_info = extract_parameter_set_dials(svm_mod), initial = iter1, iter = iter2)
    Condition
      Error in `tune_bayes()`:
      ! The first argument to [tune_bayes()] should be either a model or workflow.

# retain extra attributes and saved GP candidates

    Code
      res2 <- tune_bayes(wflow, resamples = folds, param_info = pset, initial = iter1,
        iter = iter2, control = control_bayes(save_workflow = TRUE))
    Message
      ! The Gaussian process model is being fit using 1 features but only has 2
        data points to do so. This may cause errors or a poor model fit.
      ! Gaussian process model: X should be in range (0, 1)

# too few starting values

    Code
      tune:::check_bayes_initial_size(5, 3, FALSE)
    Message
      ! There are 5 tuning parameters and 3 grid points were requested.
      * There are more tuning parameters than there are initial points. This is likely to cause numerical issues in the first few search iterations.

---

    Code
      tune:::check_bayes_initial_size(5, 3, TRUE)
    Message
      ! There are 5 tuning parameters and 3 grid points were requested.
      * There are more tuning parameters than there are initial points. This is likely to cause numerical issues in the first few search iterations.
      * With racing, only completely resampled parameters are used.

---

    Code
      tune:::check_bayes_initial_size(2, 2, FALSE)
    Message
      ! There are 2 tuning parameters and 2 grid points were requested.
      * There are as many tuning parameters as there are initial points. This is likely to cause numerical issues in the first few search iterations.

---

    Code
      tune:::check_bayes_initial_size(5, 1, FALSE)
    Condition
      Error:
      ! There are 5 tuning parameters and 1 grid point was requested.
      * The GP model requires 2+ initial points. For best performance, supply more initial points than there are tuning parameters.

---

    Code
      tune:::check_bayes_initial_size(5, 1, TRUE)
    Condition
      Error:
      ! There are 5 tuning parameters and 1 grid point was requested.
      * The GP model requires 2+ initial points. For best performance, supply more initial points than there are tuning parameters.
      * With racing, only completely resampled parameters are used.

---

    Code
      tune:::check_bayes_initial_size(1, 1, FALSE)
    Condition
      Error:
      ! There is 1 tuning parameter and 1 grid point was requested.
      * The GP model requires 2+ initial points. For best performance, supply more initial points than there are tuning parameters.

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
      ! Gaussian process model: X should be in range (0, 1)
      ! For the rsq estimates, 1 missing value was found and removed before fitting
        the Gaussian process model.
      ! Gaussian process model: X should be in range (0, 1)
      ! validation: internal: A correlation computation is required, but `estimate` is constant and ha...
      ! For the rsq estimates, 2 missing values were found and removed before
        fitting the Gaussian process model.
      ! Gaussian process model: X should be in range (0, 1)
      ! validation: internal: A correlation computation is required, but `estimate` is constant and ha...
      ! For the rsq estimates, 3 missing values were found and removed before
        fitting the Gaussian process model.
      ! Gaussian process model: X should be in range (0, 1)
      ! validation: internal: A correlation computation is required, but `estimate` is constant and ha...
      ! For the rsq estimates, 4 missing values were found and removed before
        fitting the Gaussian process model.
      ! Gaussian process model: X should be in range (0, 1)
      ! validation: internal: A correlation computation is required, but `estimate` is constant and ha...
      ! For the rsq estimates, 5 missing values were found and removed before
        fitting the Gaussian process model.
      ! Gaussian process model: X should be in range (0, 1)
      ! validation: internal: A correlation computation is required, but `estimate` is constant and ha...
      ! For the rsq estimates, 6 missing values were found and removed before
        fitting the Gaussian process model.
      ! Gaussian process model: X should be in range (0, 1)
      ! validation: internal: A correlation computation is required, but `estimate` is constant and ha...
      ! For the rsq estimates, 7 missing values were found and removed before
        fitting the Gaussian process model.
      ! Gaussian process model: X should be in range (0, 1)
      ! validation: internal: A correlation computation is required, but `estimate` is constant and ha...
      ! For the rsq estimates, 8 missing values were found and removed before
        fitting the Gaussian process model.
      ! Gaussian process model: X should be in range (0, 1)
      ! validation: internal: A correlation computation is required, but `estimate` is constant and ha...
      ! For the rsq estimates, 9 missing values were found and removed before
        fitting the Gaussian process model.
      ! Gaussian process model: X should be in range (0, 1)
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

