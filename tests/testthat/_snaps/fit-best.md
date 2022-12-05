# fit_best

    Code
      fit_best(knn_pca_res, verbose = TRUE)
    Output
      Using rmse as the metric, the optimal parameters were:
        neighbors: 10
        num_comp:  3
      
    Message
      Fitting using 161 data points...
      done
    Output
      == Workflow [trained] ==========================================================
      Preprocessor: Recipe
      Model: nearest_neighbor()
      
      -- Preprocessor ----------------------------------------------------------------
      1 Recipe Step
      
      * step_pca()
      
      -- Model -----------------------------------------------------------------------
      
      Call:
      kknn::train.kknn(formula = ..y ~ ., data = data, ks = min_rows(10L,     data, 5))
      
      Type of response variable: continuous
      minimal mean absolute error: 1.690086
      Minimal mean squared error: 4.571625
      Best kernel: optimal
      Best k: 10

---

    Code
      tmp <- fit_best(knn_pca_res, verbose = TRUE, parameters = tibble(neighbors = 1,
        num_comp = 1))
    Message
      Fitting using 161 data points...
      done

---

    Please check the value of `metric`.

---

    There are 2 parameters still marked for tuning: 'neighbors', 'num_comp'

---

    x The `...` are not used by this function.

---

    x The control option `save_workflow = TRUE` should be used when tuning.

