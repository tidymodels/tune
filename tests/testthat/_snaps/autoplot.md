# not marginal plot with grid search

    Code
      autoplot(knn_results, type = "performance")
    Condition
      Error in `autoplot()`:
      ! `type = performance` is only used with iterative search results.

---

    Code
      autoplot(knn_results, type = "parameters")
    Condition
      Error in `autoplot()`:
      ! `type = parameters` is only used with iterative search results.

# coord_obs_pred

    Removed 1 rows containing missing values (`geom_point()`).

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
      Evaluation times are only required when the results of a dynamic survival metric are being visualized (and will be ignored).

