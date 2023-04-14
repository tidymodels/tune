# predict errors informatively when supplied tuning results

    Code
      predict(ames_grid_search)
    Condition
      Error in `predict()`:
      ! `predict()` is not well-defined for tuning results.
      i To predict with the optimal model configuration from tuning results, ensure that the tuning result was generated with the control option (`?tune::control_grid()`) `save_workflow = TRUE`, run `fit_best()` (`?tune::fit_best()`), and then predict using `predict()` (`?workflows::predict.workflow()`) on its output.
      i To collect predictions from tuning results, ensure that the tuning result was generated with the control option (`?tune::control_grid()`) `save_pred = TRUE` and run `collect_predictions()` (`?tune::collect_predictions()`).

# predict errors informatively when supplied last_fit() results

    Code
      predict(spline_res)
    Condition
      Error in `predict()`:
      ! `predict()` is not well-defined for `last_fit()` results.
      i To predict with the model fitted in `last_fit()`, first run `extract_workflow()` (`?tune::extract_workflow.last_fit()`) and then predict using `predict()` (`?workflows::predict.workflow()`) on its output.

