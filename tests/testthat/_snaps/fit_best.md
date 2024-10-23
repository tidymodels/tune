# fit_best

    Code
      fit_best(knn_pca_res, verbose = TRUE)
    Output
      Using rmse as the metric, the optimal parameters were:
        neighbors: 1
        num_comp:  4
      
    Message
      i Fitting using 161 data points...
      v Done.
    Output
      == Workflow [trained] ==========================================================
      Preprocessor: Recipe
      Model: nearest_neighbor()
      
      -- Preprocessor ----------------------------------------------------------------
      1 Recipe Step
      
      * step_pca()
      
      -- Model -----------------------------------------------------------------------
      
      Call:
      kknn::train.kknn(formula = ..y ~ ., data = data, ks = min_rows(1L,     data, 5))
      
      Type of response variable: continuous
      minimal mean absolute error: 1.015528
      Minimal mean squared error: 2.448261
      Best kernel: optimal
      Best k: 1

---

    Code
      tmp <- fit_best(knn_pca_res, verbose = TRUE, parameters = tibble(neighbors = 1,
        num_comp = 1))
    Message
      i Fitting using 161 data points...
      v Done.

---

    No `fit_best()` exists for an integer.

---

    No `fit_best()` exists for a tibble.

---

    "WAT" was not in the metric set. Please choose from: "rmse" and "rsq".

---

    The parameters `neighbors` and `num_comp` are still marked for tuning.

---

    The parameter `num_comp` is still marked for tuning.

---

    `...` must be empty.
    x Problematic argument:
    * chickens = 2

---

    x The control option `save_workflow = TRUE` should be used when tuning.

# fit_best() warns when metric or eval_time are specified in addition to parameters

    Code
      manual_wf <- fit_best(res, metric = "rmse", parameters = tune_params)
    Condition
      Warning:
      `metric` is being ignored because `parameters` has been specified.

---

    Code
      manual_wf <- fit_best(res, metric = "rmse", eval_time = 10, parameters = tune_params)
    Condition
      Warning:
      `metric` is being ignored because `parameters` has been specified.
      Warning:
      `eval_time` is being ignored because `parameters` has been specified.

