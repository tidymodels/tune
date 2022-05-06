# select_best()

    Code
      select_best(rcv_results, metric = "rsq", maximize = TRUE)
    Condition
      Warning:
      The `maximize` argument is no longer needed. This value was ignored.
    Output
      # A tibble: 1 x 5
        deg_free degree `wt df` `wt degree` .config 
           <int>  <int>   <int>       <int> <chr>   
      1       10      2       2           2 Recipe24

---

    Code
      select_best(rcv_results, metric = "random")
    Condition
      Error in `is_metric_maximize()`:
      ! Please check the value of `metric`.

---

    Code
      select_best(rcv_results, metric = c("rmse", "rsq"))
    Condition
      Error in `is_metric_maximize()`:
      ! Please specify a single character value for `metric`.

---

    Code
      best_default_metric <- select_best(rcv_results)
    Condition
      Warning:
      No value of `metric` was given; metric 'rmse' will be used.
    Code
      best_rmse <- select_best(rcv_results, metric = "rmse")

# show_best()

    Code
      best_default_metric <- show_best(rcv_results)
    Condition
      Warning:
      No value of `metric` was given; metric 'rmse' will be used.
    Code
      best_rmse <- show_best(rcv_results, metric = "rmse")

# one-std error rule

    Code
      select_by_one_std_err(knn_results, metric = "accuracy", K, maximize = TRUE)
    Condition
      Warning:
      The `maximize` argument is no longer needed. This value was ignored.
    Output
      # A tibble: 1 x 11
            K weight_func exponent .metric  .estimator  mean     n std_err .config
        <int> <chr>          <dbl> <chr>    <chr>      <dbl> <int>   <dbl> <chr>  
      1    25 rank            1.99 accuracy binary     0.819    50 0.00517 Model06
      # ... with 2 more variables: .best <dbl>, .bound <dbl>

---

    Code
      select_by_one_std_err(rcv_results, metric = "random", deg_free)
    Condition
      Error in `is_metric_maximize()`:
      ! Please check the value of `metric`.

---

    Code
      select_by_one_std_err(rcv_results, metric = c("rmse", "rsq"), deg_free)
    Condition
      Error in `is_metric_maximize()`:
      ! Please specify a single character value for `metric`.

---

    Code
      select_via_default_metric <- select_by_one_std_err(knn_results, K)
    Condition
      Warning:
      No value of `metric` was given; metric 'roc_auc' will be used.
    Code
      select_via_roc <- select_by_one_std_err(knn_results, K, metric = "roc_auc")

---

    Code
      select_by_one_std_err(rcv_results, metric = "random")
    Condition
      Error in `select_by_one_std_err()`:
      ! Please choose at least one tuning parameter to sort in `...`.

---

    Code
      select_by_one_std_err(mtcars, metric = "disp")
    Condition
      Error in `select_by_one_std_err()`:
      ! No `select_by_one_std_err()` exists for this type of object.

# percent loss

    Code
      select_by_pct_loss(knn_results, metric = "accuracy", K, maximize = TRUE)
    Condition
      Warning:
      The `maximize` argument is no longer needed. This value was ignored.
    Output
      # A tibble: 1 x 11
            K weight_func  exponent .metric  .estimator  mean     n std_err .config
        <int> <chr>           <dbl> <chr>    <chr>      <dbl> <int>   <dbl> <chr>  
      1    12 epanechnikov     1.96 accuracy binary     0.806    50 0.00569 Model02
      # ... with 2 more variables: .best <dbl>, .loss <dbl>

---

    Code
      select_by_pct_loss(rcv_results, metric = "random", deg_free)
    Condition
      Error in `is_metric_maximize()`:
      ! Please check the value of `metric`.

---

    Code
      select_by_pct_loss(rcv_results, metric = c("rmse", "rsq"), deg_free)
    Condition
      Error in `is_metric_maximize()`:
      ! Please specify a single character value for `metric`.

---

    Code
      select_via_default_metric <- select_by_pct_loss(knn_results, K)
    Condition
      Warning:
      No value of `metric` was given; metric 'roc_auc' will be used.
    Code
      select_via_roc <- select_by_pct_loss(knn_results, K, metric = "roc_auc")

---

    Code
      select_by_pct_loss(rcv_results, metric = "random")
    Condition
      Error in `select_by_pct_loss()`:
      ! Please choose at least one tuning parameter to sort in `...`.

