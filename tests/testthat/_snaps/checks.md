# rsample objects

    Code
      tune:::check_rset(obj_loo)
    Condition
      Error in `tune:::check_rset()`:
      ! Leave-one-out cross-validation is not currently supported with tune.

---

    Code
      tune:::check_rset(obj_nst)
    Condition
      Error in `tune:::check_rset()`:
      ! Nested resampling is not currently supported with tune.

---

    Code
      tune:::check_rset(obj_permut)
    Condition
      Error in `tune:::check_rset()`:
      ! Permutation samples are not suitable for tuning.

# grid objects

    Code
      tune:::check_grid(rbind(grid_1, grid_1), chi_wflow)
    Condition
      Warning:
      Duplicate rows in grid of tuning combinations found and removed.
    Output
      # A tibble: 10 x 6
         penalty mixture imputation threshold deg_free degree
           <int>   <int>      <int>     <int>    <int>  <int>
       1       1       1          1         1        1      1
       2       2       2          2         2        2      2
       3       3       3          3         3        3      3
       4       4       4          4         4        4      4
       5       5       5          5         5        5      5
       6       6       6          6         6        6      6
       7       7       7          7         7        7      7
       8       8       8          8         8        8      8
       9       9       9          9         9        9      9
      10      10      10         10        10       10     10

---

    Code
      tune:::check_grid(chi_wflow, chi_wflow)
    Condition
      Error in `tune:::check_grid()`:
      ! `grid` should be a positive integer or a data frame.

# Unknown `grid` columns are caught

    Code
      tune:::check_grid(grid, workflow)
    Condition
      Error in `tune:::check_grid()`:
      ! The provided `grid` has the following parameter columns that have not been marked for tuning by `tune()`: 'other1', 'other2'.

# Missing required `grid` columns are caught

    Code
      tune:::check_grid(grid, workflow)
    Condition
      Error in `tune:::check_grid()`:
      ! The provided `grid` is missing the following parameter columns that have been marked for tuning by `tune()`: 'deg_free'.

# workflow objects

    Code
      tune:::check_workflow(x = wflow_2, check_dials = TRUE)
    Condition
      Error in `tune:::check_workflow()`:
      ! The workflow has arguments whose ranges are not finalized: 'mtry'

---

    Code
      tune:::check_workflow(wflow_3)
    Condition
      Error in `tune:::check_workflow()`:
      ! A formula, recipe, or variables preprocessor is required.

---

    Code
      tune:::check_workflow(wflow_4)
    Condition
      Error in `tune:::check_workflow()`:
      ! A parsnip model is required.

# errors informatively when needed package isn't installed

    Code
      check_workflow(stan_wflow)
    Condition
      Error:
      ! Package install is required for rstanarm.

---

    Code
      fit_resamples(stan_wflow, rsample::bootstraps(mtcars))
    Condition
      Error in `fit_resamples()`:
      ! Package install is required for rstanarm.

# workflow objects (will not tune, tidymodels/tune#548)

    Code
      tune_grid(lr_lm_1, rec_bare, rsample::bootstraps(Chicago, 2))
    Condition
      Error in `tune_grid()`:
      ! The parameter `penalty` was marked with `tune()`, though will not be tuned.
      i This usually means that the current modeling engine `lm` does not support tuning `penalty`.

---

    Code
      tune_bayes(lr_lm_2, rec_tune, rsample::bootstraps(Chicago, 2))
    Condition
      Error in `tune_bayes()`:
      ! The parameters `penalty` and `mixture` were marked with `tune()`, though will not be tuned.
      i This usually means that the current modeling engine `lm` does not support tuning `penalty` and `mixture`.

# yardstick objects

    Code
      tune:::check_metrics(yardstick::rmse, chi_wflow)
    Condition
      Error in `tune:::check_metrics()`:
      ! The `metrics` argument should be the results of [yardstick::metric_set()].

# metrics must match the parsnip engine

    Code
      tune:::check_metrics(metric_set1, workflow1)
    Condition
      Error in `tune:::check_metrics()`:
      ! The parsnip model has `mode = 'regression'`, but `metrics` is a metric set for a different model mode.

---

    Code
      tune:::check_metrics(metric_set2, workflow2)
    Condition
      Error in `tune:::check_metrics()`:
      ! The parsnip model has `mode = 'classification'`, but `metrics` is a metric set for a different model mode.

# grid control objects

    Code
      control_grid(tomato = 1)
    Condition
      Error in `control_grid()`:
      ! unused argument (tomato = 1)

---

    Code
      control_grid(verbose = 1)
    Condition
      Error in `val_class_and_single()`:
      ! Argument 'verbose' should be a single logical value in `control_grid()`

---

    Code
      control_grid(verbose = rep(TRUE, 2))
    Condition
      Error in `val_class_and_single()`:
      ! Argument 'verbose' should be a single logical value in `control_grid()`

---

    Code
      control_grid(allow_par = 1)
    Condition
      Error in `val_class_and_single()`:
      ! Argument 'allow_par' should be a single logical value in `control_grid()`

---

    Code
      control_grid(save_pred = "no")
    Condition
      Error in `val_class_and_single()`:
      ! Argument 'save_pred' should be a single logical value in `control_grid()`

---

    Code
      control_grid(extract = Inf)
    Condition
      Error in `val_class_or_null()`:
      ! Argument 'extract' should be a function or NULL in `control_grid()`

---

    Code
      control_grid(pkgs = Inf)
    Condition
      Error in `val_class_or_null()`:
      ! Argument 'pkgs' should be a character or NULL in `control_grid()`

# Bayes control objects

    Code
      control_bayes(tomato = 1)
    Condition
      Error in `control_bayes()`:
      ! unused argument (tomato = 1)

---

    Code
      control_bayes(verbose = 1)
    Condition
      Error in `val_class_and_single()`:
      ! Argument 'verbose' should be a single logical value in `control_bayes()`

---

    Code
      control_bayes(verbose = rep(TRUE, 2))
    Condition
      Error in `val_class_and_single()`:
      ! Argument 'verbose' should be a single logical value in `control_bayes()`

---

    Code
      control_bayes(no_improve = FALSE)
    Condition
      Error in `val_class_and_single()`:
      ! Argument 'no_improve' should be a single numeric or integer value in `control_bayes()`

---

    Code
      control_bayes(uncertain = FALSE)
    Condition
      Error in `val_class_and_single()`:
      ! Argument 'uncertain' should be a single numeric or integer value in `control_bayes()`

---

    Code
      control_bayes(seed = FALSE)
    Condition
      Error in `val_class_and_single()`:
      ! Argument 'seed' should be a single numeric or integer value in `control_bayes()`

---

    Code
      control_bayes(save_pred = "no")
    Condition
      Error in `val_class_and_single()`:
      ! Argument 'save_pred' should be a single logical value in `control_bayes()`

---

    Code
      control_bayes(extract = Inf)
    Condition
      Error in `val_class_or_null()`:
      ! Argument 'extract' should be a function or NULL in `control_bayes()`

---

    Code
      control_bayes(pkgs = Inf)
    Condition
      Error in `val_class_or_null()`:
      ! Argument 'pkgs' should be a character or NULL in `control_bayes()`

---

    Code
      control_bayes(time_limit = "a")
    Condition
      Error in `val_class_and_single()`:
      ! Argument 'time_limit' should be a single logical or numeric value in `control_bayes()`

---

    Code
      tmp <- control_bayes(no_improve = 2, uncertain = 5)
    Message
      ! Uncertainty sample scheduled after 5 poor iterations but the search will stop after 2.

# initial values

    Code
      tune:::check_initial(data.frame(), pset = extract_parameter_set_dials(wflow_1),
      wflow = wflow_1, resamples = mtfolds, metrics = yardstick::metric_set(
        yardstick::rsq), ctrl = control_bayes())
    Condition
      Error in `tune:::check_initial()`:
      ! `initial` should be a positive integer or the results of [tune_grid()]

# Acquisition function objects

    Code
      tune:::check_direction(1)
    Condition
      Error in `tune:::check_direction()`:
      ! `maximize` should be a single logical.

---

    Code
      tune:::check_direction(rep(TRUE, 2))
    Condition
      Error in `tune:::check_direction()`:
      ! `maximize` should be a single logical.

---

    Code
      tune:::check_best(FALSE)
    Condition
      Error in `tune:::check_best()`:
      ! `best` should be a single, non-missing numeric.

---

    Code
      tune:::check_best(rep(2, 2))
    Condition
      Error in `tune:::check_best()`:
      ! `best` should be a single, non-missing numeric.

---

    Code
      tune:::check_best(NA)
    Condition
      Error in `tune:::check_best()`:
      ! `best` should be a single, non-missing numeric.

# check parameter finalization

    Code
      expect_error(p1 <- tune:::check_parameters(w1, data = mtcars, grid_names = character(
        0)), regex = NA)
    Message
      i Creating pre-processing data to finalize unknown parameter: mtry

---

    Code
      expect_error(p2 <- tune:::check_parameters(w2, data = mtcars), regex = NA)
    Message
      i Creating pre-processing data to finalize unknown parameter: mtry

---

    Code
      expect_error(p3_a <- tune:::check_parameters(w3, data = mtcars), regex = NA)
    Message
      i Creating pre-processing data to finalize unknown parameter: mtry

---

    Code
      tune:::check_parameters(w4, data = mtcars)
    Condition
      Error in `tune:::check_parameters()`:
      ! Some model parameters require finalization but there are recipe parameters that require tuning. Please use  `extract_parameter_set_dials()` to set parameter ranges  manually and supply the output to the `param_info` argument.

