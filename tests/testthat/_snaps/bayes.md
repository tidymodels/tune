# tune model only - failure in recipe is caught elegantly

    Code
      cars_res <- tune_bayes(svm_mod, preprocessor = rec, resamples = data_folds)
    Message <simpleMessage>
      x Fold1: preprocessor 1/1: Error in if (!is.null(args$df) && is.null(args$knots) ...
      x Fold2: preprocessor 1/1: Error in if (!is.null(args$df) && is.null(args$knots) ...
    Warning <rlang_warning>
      All models failed. See the `.notes` column.
    Error <rlang_error>
      All of the models failed. See the .notes column.

# tune model only - failure in formula is caught elegantly

    Code
      cars_res <- tune_bayes(wflow, resamples = data_folds, control = control_bayes(
        extract = function(x) {
          1
        }, save_pred = TRUE))
    Message <simpleMessage>
      x Fold1: preprocessor 1/1: Error in `glubort()`:
      ! The following predictors were ...
      x Fold2: preprocessor 1/1: Error in `glubort()`:
      ! The following predictors were ...
    Warning <rlang_warning>
      All models failed. See the `.notes` column.
    Error <rlang_error>
      All of the models failed. See the .notes column.

# argument order gives an error for recipes

    Code
      tune_bayes(rec_tune_1, model = lm_mod, resamples = rsample::vfold_cv(mtcars, v = 2),
      param_info = extract_parameter_set_dials(rec_tune_1), iter = iter1, initial = iter2)
    Error <rlang_error>
      The first argument to [tune_bayes()] should be either a model or workflow.

# argument order gives an error for formula

    Code
      tune_bayes(mpg ~ ., svm_mod, resamples = rsample::vfold_cv(mtcars, v = 2),
      param_info = extract_parameter_set_dials(svm_mod), initial = iter1, iter = iter2)
    Error <rlang_error>
      The first argument to [tune_bayes()] should be either a model or workflow.

# retain extra attributes and saved GP candidates

    Code
      res2 <- tune_bayes(wflow, resamples = folds, param_info = pset, initial = iter1,
        iter = iter2, control = control_bayes(save_workflow = TRUE))
    Message <simpleMessage>
      ! The Gaussian process model is being fit using 1 features but only has 2
        data points to do so. This may cause errors or a poor model fit.
      ! Gaussian process model: X should be in range (0, 1)

# too few starting values

    Code
      tune:::check_bayes_initial_size(5, 3, FALSE)
    Message <simpleMessage>
      ! There are 5 tuning parameters and 3 grid points were requested. This is likely to cause numerical issues in the
        first few search iterations.

---

    Code
      tune:::check_bayes_initial_size(5, 3, TRUE)
    Message <simpleMessage>
      ! There are 5 tuning parameters and 3 grid points were requested. This is likely to cause numerical issues in the
        first few search iterations. With racing, only completely resampled parameters are used.

---

    Code
      tune:::check_bayes_initial_size(5, 1, FALSE)
    Error <rlang_error>
      ! There are 5 tuning parameters and a single grid point was requested. The GP model requires 2+ initial points but there should be more initial points than there are tuning paramters. 

---

    Code
      tune:::check_bayes_initial_size(5, 1, TRUE)
    Error <rlang_error>
      ! There are 5 tuning parameters and a single grid point was requested. The GP model requires 2+ initial points but there should be more initial points than there are tuning paramters. With racing, only completely resampled parameters are used.

---

    Code
      tune:::check_bayes_initial_size(1, 1, FALSE)
    Error <rlang_error>
      ! There is one tuning parameter and a single grid point was requested. The GP model requires 2+ initial points but there should be more initial points than there are tuning paramters. 

