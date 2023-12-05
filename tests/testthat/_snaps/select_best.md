# select_best()

    Code
      select_best(rcv_results, metric = "random")
    Condition
      Error in `select_best()`:
      ! "random" was not in the metric set. Please choose from: "rmse" and "rsq".

---

    Code
      select_best(rcv_results, metric = c("rmse", "rsq"))
    Condition
      Warning in `select_best()`:
      2 metrics were given; "rmse" will be used.
    Output
      # A tibble: 1 x 5
        deg_free degree `wt df` `wt degree` .config              
           <int>  <int>   <int>       <int> <chr>                
      1        6      2       2           1 Preprocessor05_Model1

---

    Code
      best_default_metric <- select_best(rcv_results)
    Condition
      Warning in `select_best()`:
      No value of `metric` was given; "rmse" will be used.
    Code
      best_rmse <- select_best(rcv_results, metric = "rmse")

---

    Code
      select_best(mtcars, metric = "disp")
    Condition
      Error in `select_best()`:
      ! No `select_best()` exists for this type of object.

# show_best()

    Code
      best_default_metric <- show_best(rcv_results)
    Condition
      Warning in `show_best()`:
      No value of `metric` was given; "rmse" will be used.
    Code
      best_rmse <- show_best(rcv_results, metric = "rmse")

---

    Code
      show_best(mtcars, metric = "disp")
    Condition
      Error in `show_best()`:
      ! No `show_best()` exists for this type of object.

# one-std error rule

    Code
      select_by_one_std_err(rcv_results, metric = "random", deg_free)
    Condition
      Error in `select_by_one_std_err()`:
      ! "random" was not in the metric set. Please choose from: "rmse" and "rsq".

---

    Code
      select_by_one_std_err(rcv_results, metric = c("rmse", "rsq"), deg_free)
    Condition
      Warning in `select_by_one_std_err()`:
      2 metrics were given; "rmse" will be used.
    Output
      # A tibble: 1 x 5
        deg_free degree `wt df` `wt degree` .config              
           <int>  <int>   <int>       <int> <chr>                
      1        6      2       2           1 Preprocessor05_Model1

---

    Code
      select_via_default_metric <- select_by_one_std_err(knn_results, K)
    Condition
      Warning in `select_by_one_std_err()`:
      No value of `metric` was given; "roc_auc" will be used.
    Code
      select_via_roc <- select_by_one_std_err(knn_results, K, metric = "roc_auc")

---

    Code
      select_by_one_std_err(rcv_results, metric = "random")
    Condition
      Error in `select_by_one_std_err()`:
      ! "random" was not in the metric set. Please choose from: "rmse" and "rsq".

---

    Code
      select_by_one_std_err(mtcars, metric = "disp")
    Condition
      Error in `select_by_one_std_err()`:
      ! No `select_by_one_std_err()` exists for this type of object.

---

    Code
      select_by_one_std_err(knn_results, metric = "roc_auc", weight_funk)
    Condition
      Error in `select_by_one_std_err()`:
      ! Could not sort results by `weight_funk`.

---

    Code
      select_by_one_std_err(knn_results, metric = "roc_auc", weight_funk, K)
    Condition
      Error in `select_by_one_std_err()`:
      ! Could not sort results by `weight_funk`.

---

    Code
      select_by_one_std_err(knn_results, metric = "roc_auc", weight_funk, Kay)
    Condition
      Error in `select_by_one_std_err()`:
      ! Could not sort results by `weight_funk` and `Kay`.

---

    Code
      select_by_one_std_err(knn_results, metric = "roc_auc", weight_funk, desc(K))
    Condition
      Error in `select_by_one_std_err()`:
      ! Could not sort results by `weight_funk` and `desc(K)`.

# percent loss

    Code
      select_by_pct_loss(rcv_results, metric = "random", deg_free)
    Condition
      Error in `select_by_pct_loss()`:
      ! "random" was not in the metric set. Please choose from: "rmse" and "rsq".

---

    Code
      select_by_pct_loss(rcv_results, metric = c("rmse", "rsq"), deg_free)
    Condition
      Warning in `select_by_pct_loss()`:
      2 metrics were given; "rmse" will be used.
    Output
      # A tibble: 1 x 5
        deg_free degree `wt df` `wt degree` .config              
           <int>  <int>   <int>       <int> <chr>                
      1        6      2       2           1 Preprocessor05_Model1

---

    Code
      select_via_default_metric <- select_by_pct_loss(knn_results, K)
    Condition
      Warning in `select_by_pct_loss()`:
      No value of `metric` was given; "roc_auc" will be used.
    Code
      select_via_roc <- select_by_pct_loss(knn_results, K, metric = "roc_auc")

---

    Code
      select_by_pct_loss(rcv_results, metric = "random")
    Condition
      Error in `select_by_pct_loss()`:
      ! "random" was not in the metric set. Please choose from: "rmse" and "rsq".

---

    Code
      select_by_pct_loss(mtcars, metric = "disp")
    Condition
      Error in `select_by_pct_loss()`:
      ! No `select_by_pct_loss()` exists for this type of object.

---

    Code
      select_by_pct_loss(knn_results, metric = "roc_auc", weight_funk)
    Condition
      Error in `select_by_pct_loss()`:
      ! Could not sort results by `weight_funk`.

---

    Code
      select_by_pct_loss(knn_results, metric = "roc_auc", weight_funk, K)
    Condition
      Error in `select_by_pct_loss()`:
      ! Could not sort results by `weight_funk`.

---

    Code
      select_by_pct_loss(knn_results, metric = "roc_auc", weight_funk, Kay)
    Condition
      Error in `select_by_pct_loss()`:
      ! Could not sort results by `weight_funk` and `Kay`.

---

    Code
      select_by_pct_loss(knn_results, metric = "roc_auc", weight_funk, desc(K))
    Condition
      Error in `select_by_pct_loss()`:
      ! Could not sort results by `weight_funk` and `desc(K)`.

