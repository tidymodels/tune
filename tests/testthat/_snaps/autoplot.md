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
      Error in `plot_regular_grid()`:
      ! Only one observation per metric was present. Unable to create meaningful plot.

