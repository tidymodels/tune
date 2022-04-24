# rsample objects

    Code
      tune:::check_rset(obj_loo)
    Error <rlang_error>
      Leave-one-out cross-validation is not currently supported with tune.

---

    Code
      tune:::check_rset(obj_nst)
    Error <rlang_error>
      Nested resampling is not currently supported with tune.

# grid objects

    Code
      tune:::check_grid(rbind(grid_1, grid_1), chi_wflow)
    Warning <rlang_warning>
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
    Error <rlang_error>
      `grid` should be a positive integer or a data frame.

# Unknown `grid` columns are caught

    Code
      tune:::check_grid(grid, workflow)
    Error <rlang_error>
      The provided `grid` has the following parameter columns that have not been marked for tuning by `tune()`: 'other1', 'other2'.

# Missing required `grid` columns are caught

    Code
      tune:::check_grid(grid, workflow)
    Error <rlang_error>
      The provided `grid` is missing the following parameter columns that have been marked for tuning by `tune()`: 'deg_free'.

# workflow objects

    Code
      tune:::check_workflow(x = wflow_2, check_dials = TRUE)
    Error <rlang_error>
      The workflow has arguments whose ranges are not finalized: 'mtry'

---

    Code
      tune:::check_workflow(wflow_3)
    Error <rlang_error>
      A formula, recipe, or variables preprocessor is required.

---

    Code
      tune:::check_workflow(wflow_4)
    Error <rlang_error>
      A parsnip model is required.

# yardstick objects

    Code
      tune:::check_metrics(yardstick::rmse, chi_wflow)
    Error <rlang_error>
      The `metrics` argument should be the results of [yardstick::metric_set()].

# metrics must match the parsnip engine

    Code
      tune:::check_metrics(metric_set1, workflow1)
    Error <rlang_error>
      The parsnip model has `mode = 'regression'`, but `metrics` is a metric set for class / probability metrics.

---

    Code
      tune:::check_metrics(metric_set2, workflow2)
    Error <rlang_error>
      The parsnip model has `mode = 'classification'`, but `metrics` is a metric set for regression metrics.

# grid control objects

    Code
      control_grid(tomato = 1)
    Error <simpleError>
      unused argument (tomato = 1)

---

    Code
      control_grid(verbose = 1)
    Error <rlang_error>
      Argument 'verbose' should be a single logical value in `control_grid()`

---

    Code
      control_grid(verbose = rep(TRUE, 2))
    Error <rlang_error>
      Argument 'verbose' should be a single logical value in `control_grid()`

---

    Code
      control_grid(allow_par = 1)
    Error <rlang_error>
      Argument 'allow_par' should be a single logical value in `control_grid()`

---

    Code
      control_grid(save_pred = "no")
    Error <rlang_error>
      Argument 'save_pred' should be a single logical value in `control_grid()`

---

    Code
      control_grid(extract = Inf)
    Error <rlang_error>
      Argument 'extract' should be a function or NULL in `control_grid()`

---

    Code
      control_grid(pkgs = Inf)
    Error <rlang_error>
      Argument 'pkgs' should be a character or NULL in `control_grid()`

# Bayes control objects

    Code
      control_bayes(tomato = 1)
    Error <simpleError>
      unused argument (tomato = 1)

---

    Code
      control_bayes(verbose = 1)
    Error <rlang_error>
      Argument 'verbose' should be a single logical value in `control_bayes()`

---

    Code
      control_bayes(verbose = rep(TRUE, 2))
    Error <rlang_error>
      Argument 'verbose' should be a single logical value in `control_bayes()`

---

    Code
      control_bayes(no_improve = FALSE)
    Error <rlang_error>
      Argument 'no_improve' should be a single numeric or integer value in `control_bayes()`

---

    Code
      control_bayes(uncertain = FALSE)
    Error <rlang_error>
      Argument 'uncertain' should be a single numeric or integer value in `control_bayes()`

---

    Code
      control_bayes(seed = FALSE)
    Error <rlang_error>
      Argument 'seed' should be a single numeric or integer value in `control_bayes()`

---

    Code
      control_bayes(save_pred = "no")
    Error <rlang_error>
      Argument 'save_pred' should be a single logical value in `control_bayes()`

---

    Code
      control_bayes(extract = Inf)
    Error <rlang_error>
      Argument 'extract' should be a function or NULL in `control_bayes()`

---

    Code
      control_bayes(pkgs = Inf)
    Error <rlang_error>
      Argument 'pkgs' should be a character or NULL in `control_bayes()`

---

    Code
      control_bayes(time_limit = "a")
    Error <rlang_error>
      Argument 'time_limit' should be a single logical or numeric value in `control_bayes()`

---

    Code
      tmp <- control_bayes(no_improve = 2, uncertain = 5)
    Message <cliMessage>
      ! Uncertainty sample scheduled after 5 poor iterations but the search will stop after 2.

# initial values

    Code
      tune:::check_initial(data.frame(), extract_parameter_set_dials(wflow_1),
      wflow_1, mtfolds, yardstick::metric_set(yardstick::rsq), control_bayes())
    Error <rlang_error>
      `initial` should be a positive integer or the results of [tune_grid()]

# Acquisition function objects

    Code
      tune:::check_direction(1)
    Error <rlang_error>
      `maximize` should be a single logical.

---

    Code
      tune:::check_direction(rep(TRUE, 2))
    Error <rlang_error>
      `maximize` should be a single logical.

---

    Code
      tune:::check_best(FALSE)
    Error <rlang_error>
      `best` should be a single, non-missing numeric.

---

    Code
      tune:::check_best(rep(2, 2))
    Error <rlang_error>
      `best` should be a single, non-missing numeric.

---

    Code
      tune:::check_best(NA)
    Error <rlang_error>
      `best` should be a single, non-missing numeric.

# check parameter finalization

    Code
      expect_error(p1 <- tune:::check_parameters(w1, data = mtcars, grid_names = character(
        0)), regex = NA)
    Message <simpleMessage>
      i Creating pre-processing data to finalize unknown parameter: mtry

---

    Code
      expect_error(p2 <- tune:::check_parameters(w2, data = mtcars), regex = NA)
    Message <simpleMessage>
      i Creating pre-processing data to finalize unknown parameter: mtry

---

    Code
      expect_error(p3_a <- tune:::check_parameters(w3, data = mtcars), regex = NA)
    Message <simpleMessage>
      i Creating pre-processing data to finalize unknown parameter: mtry

---

    Code
      tune:::check_parameters(w4, data = mtcars)
    Error <rlang_error>
      Some tuning parameters require finalization but there are recipe parameters that require tuning. Please use `parameters()` to finalize the parameter ranges.

