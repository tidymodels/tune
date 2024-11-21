# not marginal plot with grid search

    Code
      autoplot(knn_results, type = "performance")
    Condition
      Error in `autoplot()`:
      ! `type = "performance"` is only used with iterative search results.

---

    Code
      autoplot(knn_results, type = "parameters")
    Condition
      Error in `autoplot()`:
      ! `type = "parameters"` is only used with iterative search results.

# coord_obs_pred

    Removed 1 row containing missing values or values outside the scale range (`geom_point()`).

# regular grid plot

    Code
      autoplot(res)
    Condition
      Error in `autoplot()`:
      ! Only one observation per metric was present. Unable to create meaningful plot.

# evaluation time warning for non-survival model

    Code
      foo <- autoplot(res, metric = "rmse", eval_time = 10)
    Condition
      Warning in `autoplot()`:
      `eval_time` is only used for dynamic survival metrics.

