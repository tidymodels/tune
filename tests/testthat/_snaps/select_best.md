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
      1        6      2       2           1 Preprocessor19_Model1

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
      1        6      2       2           1 Preprocessor19_Model1

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
      1        6      2       2           1 Preprocessor19_Model1

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

# show_best with survival models

    Code
      show_best(surv_res)
    Condition
      Warning in `show_best()`:
      No value of `metric` was given; "brier_survival" will be used.
      Warning in `show_best()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).
    Output
      # A tibble: 5 x 8
        trees .metric        .estimator .eval_time  mean     n std_err .config        
        <dbl> <chr>          <chr>           <dbl> <dbl> <int>   <dbl> <chr>          
      1   100 brier_survival standard           10 0.118    10  0.0177 Preprocessor1_~
      2    20 brier_survival standard           10 0.136    10  0.0153 Preprocessor1_~
      3    10 brier_survival standard           10 0.155    10  0.0175 Preprocessor1_~
      4     5 brier_survival standard           10 0.172    10  0.0198 Preprocessor1_~
      5     1 brier_survival standard           10 0.194    10  0.0221 Preprocessor1_~

---

    Code
      show_best(surv_res, metric = "concordance_survival")
    Output
      # A tibble: 5 x 8
        trees .metric              .estimator .eval_time  mean     n std_err .config  
        <dbl> <chr>                <chr>           <dbl> <dbl> <int>   <dbl> <chr>    
      1    20 concordance_survival standard           NA 0.677    10  0.0354 Preproce~
      2   100 concordance_survival standard           NA 0.670    10  0.0329 Preproce~
      3    10 concordance_survival standard           NA 0.668    10  0.0376 Preproce~
      4     5 concordance_survival standard           NA 0.663    10  0.0363 Preproce~
      5     1 concordance_survival standard           NA 0.626    10  0.0326 Preproce~

---

    Code
      show_best(surv_res, metric = "brier_survival_integrated")
    Output
      # A tibble: 5 x 8
        trees .metric                .estimator .eval_time  mean     n std_err .config
        <dbl> <chr>                  <chr>           <dbl> <dbl> <int>   <dbl> <chr>  
      1   100 brier_survival_integr~ standard           NA 0.117    10 0.00699 Prepro~
      2    20 brier_survival_integr~ standard           NA 0.127    10 0.00981 Prepro~
      3    10 brier_survival_integr~ standard           NA 0.141    10 0.0134  Prepro~
      4     5 brier_survival_integr~ standard           NA 0.151    10 0.0159  Prepro~
      5     1 brier_survival_integr~ standard           NA 0.164    10 0.0182  Prepro~

---

    Code
      show_best(surv_res, metric = "brier_survival")
    Condition
      Warning in `show_best()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).
    Output
      # A tibble: 5 x 8
        trees .metric        .estimator .eval_time  mean     n std_err .config        
        <dbl> <chr>          <chr>           <dbl> <dbl> <int>   <dbl> <chr>          
      1   100 brier_survival standard           10 0.118    10  0.0177 Preprocessor1_~
      2    20 brier_survival standard           10 0.136    10  0.0153 Preprocessor1_~
      3    10 brier_survival standard           10 0.155    10  0.0175 Preprocessor1_~
      4     5 brier_survival standard           10 0.172    10  0.0198 Preprocessor1_~
      5     1 brier_survival standard           10 0.194    10  0.0221 Preprocessor1_~

---

    Code
      show_best(surv_res, metric = c("brier_survival", "roc_auc_survival"))
    Condition
      Warning in `show_best()`:
      2 metrics were given; "brier_survival" will be used.
      Warning in `show_best()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).
    Output
      # A tibble: 5 x 8
        trees .metric        .estimator .eval_time  mean     n std_err .config        
        <dbl> <chr>          <chr>           <dbl> <dbl> <int>   <dbl> <chr>          
      1   100 brier_survival standard           10 0.118    10  0.0177 Preprocessor1_~
      2    20 brier_survival standard           10 0.136    10  0.0153 Preprocessor1_~
      3    10 brier_survival standard           10 0.155    10  0.0175 Preprocessor1_~
      4     5 brier_survival standard           10 0.172    10  0.0198 Preprocessor1_~
      5     1 brier_survival standard           10 0.194    10  0.0221 Preprocessor1_~

---

    Code
      show_best(surv_res, metric = "brier_survival", eval_time = 1)
    Output
      # A tibble: 5 x 8
        trees .metric        .estimator .eval_time   mean     n std_err .config       
        <dbl> <chr>          <chr>           <dbl>  <dbl> <int>   <dbl> <chr>         
      1    20 brier_survival standard            1 0.0381    10  0.0149 Preprocessor1~
      2    10 brier_survival standard            1 0.0386    10  0.0151 Preprocessor1~
      3   100 brier_survival standard            1 0.0386    10  0.0147 Preprocessor1~
      4     5 brier_survival standard            1 0.0389    10  0.0152 Preprocessor1~
      5     1 brier_survival standard            1 0.0392    10  0.0153 Preprocessor1~

---

    Code
      show_best(surv_res, metric = "concordance_survival", eval_time = 1)
    Condition
      Warning in `show_best()`:
      `eval_time` is only used for dynamic survival metrics.
    Output
      # A tibble: 5 x 8
        trees .metric              .estimator .eval_time  mean     n std_err .config  
        <dbl> <chr>                <chr>           <dbl> <dbl> <int>   <dbl> <chr>    
      1    20 concordance_survival standard           NA 0.677    10  0.0354 Preproce~
      2   100 concordance_survival standard           NA 0.670    10  0.0329 Preproce~
      3    10 concordance_survival standard           NA 0.668    10  0.0376 Preproce~
      4     5 concordance_survival standard           NA 0.663    10  0.0363 Preproce~
      5     1 concordance_survival standard           NA 0.626    10  0.0326 Preproce~

---

    Code
      show_best(surv_res, metric = "concordance_survival", eval_time = 1.1)
    Condition
      Warning in `show_best()`:
      `eval_time` is only used for dynamic survival metrics.
    Output
      # A tibble: 5 x 8
        trees .metric              .estimator .eval_time  mean     n std_err .config  
        <dbl> <chr>                <chr>           <dbl> <dbl> <int>   <dbl> <chr>    
      1    20 concordance_survival standard           NA 0.677    10  0.0354 Preproce~
      2   100 concordance_survival standard           NA 0.670    10  0.0329 Preproce~
      3    10 concordance_survival standard           NA 0.668    10  0.0376 Preproce~
      4     5 concordance_survival standard           NA 0.663    10  0.0363 Preproce~
      5     1 concordance_survival standard           NA 0.626    10  0.0326 Preproce~

---

    Code
      show_best(surv_res, metric = "brier_survival", eval_time = 1.1)
    Condition
      Error in `show_best()`:
      ! Evaluation time 1.1 is not in the results.

---

    Code
      show_best(surv_res, metric = "brier_survival", eval_time = 1:2)
    Condition
      Warning in `show_best()`:
      2 evaluation times are available; the first will be used (i.e. `eval_time = 1`).
    Output
      # A tibble: 5 x 8
        trees .metric        .estimator .eval_time   mean     n std_err .config       
        <dbl> <chr>          <chr>           <dbl>  <dbl> <int>   <dbl> <chr>         
      1    20 brier_survival standard            1 0.0381    10  0.0149 Preprocessor1~
      2    10 brier_survival standard            1 0.0386    10  0.0151 Preprocessor1~
      3   100 brier_survival standard            1 0.0386    10  0.0147 Preprocessor1~
      4     5 brier_survival standard            1 0.0389    10  0.0152 Preprocessor1~
      5     1 brier_survival standard            1 0.0392    10  0.0153 Preprocessor1~

---

    Code
      show_best(surv_res, metric = "brier_survival", eval_time = 3:4)
    Condition
      Warning in `show_best()`:
      2 evaluation times are available; the first will be used (i.e. `eval_time = 3`).
      Error in `show_best()`:
      ! Evaluation time 3 is not in the results.

# select_best with survival models

    Code
      select_best(surv_res)
    Condition
      Warning in `select_best()`:
      No value of `metric` was given; "brier_survival" will be used.
      Warning in `select_best()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1   100 Preprocessor1_Model5

---

    Code
      select_best(surv_res, metric = "concordance_survival")
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1    20 Preprocessor1_Model4

---

    Code
      select_best(surv_res, metric = "brier_survival_integrated")
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1   100 Preprocessor1_Model5

---

    Code
      select_best(surv_res, metric = "brier_survival")
    Condition
      Warning in `select_best()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1   100 Preprocessor1_Model5

---

    Code
      select_best(surv_res, metric = c("brier_survival", "roc_auc_survival"))
    Condition
      Warning in `select_best()`:
      2 metrics were given; "brier_survival" will be used.
      Warning in `select_best()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1   100 Preprocessor1_Model5

---

    Code
      select_best(surv_res, metric = "brier_survival", eval_time = 1)
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1    20 Preprocessor1_Model4

---

    Code
      select_best(surv_res, metric = "concordance_survival", eval_time = 1)
    Condition
      Warning in `select_best()`:
      `eval_time` is only used for dynamic survival metrics.
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1    20 Preprocessor1_Model4

---

    Code
      select_best(surv_res, metric = "concordance_survival", eval_time = 1.1)
    Condition
      Warning in `select_best()`:
      `eval_time` is only used for dynamic survival metrics.
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1    20 Preprocessor1_Model4

---

    Code
      select_best(surv_res, metric = "brier_survival", eval_time = 1.1)
    Condition
      Error in `select_best()`:
      ! Evaluation time 1.1 is not in the results.

---

    Code
      select_best(surv_res, metric = "brier_survival", eval_time = 1:2)
    Condition
      Warning in `select_best()`:
      2 evaluation times are available; the first will be used (i.e. `eval_time = 1`).
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1    20 Preprocessor1_Model4

---

    Code
      select_best(surv_res, metric = "brier_survival", eval_time = 3:4)
    Condition
      Warning in `select_best()`:
      2 evaluation times are available; the first will be used (i.e. `eval_time = 3`).
      Error in `select_best()`:
      ! Evaluation time 3 is not in the results.

# select_by_one_std_err with survival models

    Code
      select_by_one_std_err(surv_res, trees)
    Condition
      Warning in `select_by_one_std_err()`:
      No value of `metric` was given; "brier_survival" will be used.
      Warning in `select_by_one_std_err()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1    20 Preprocessor1_Model4

---

    Code
      select_by_one_std_err(surv_res, metric = "concordance_survival", trees)
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1     5 Preprocessor1_Model2

---

    Code
      select_by_one_std_err(surv_res, metric = "brier_survival_integrated", trees)
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1   100 Preprocessor1_Model5

---

    Code
      select_by_one_std_err(surv_res, metric = "brier_survival", trees)
    Condition
      Warning in `select_by_one_std_err()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1    20 Preprocessor1_Model4

---

    Code
      select_by_one_std_err(surv_res, metric = c("brier_survival", "roc_auc_survival"),
      trees)
    Condition
      Warning in `select_by_one_std_err()`:
      2 metrics were given; "brier_survival" will be used.
      Warning in `select_by_one_std_err()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1    20 Preprocessor1_Model4

---

    Code
      select_by_one_std_err(surv_res, metric = "brier_survival", eval_time = 1, trees)
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1     1 Preprocessor1_Model1

---

    Code
      select_by_one_std_err(surv_res, metric = "concordance_survival", eval_time = 1,
        trees)
    Condition
      Warning in `select_by_one_std_err()`:
      `eval_time` is only used for dynamic survival metrics.
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1     5 Preprocessor1_Model2

---

    Code
      select_by_one_std_err(surv_res, metric = "concordance_survival", eval_time = 1.1,
        trees)
    Condition
      Warning in `select_by_one_std_err()`:
      `eval_time` is only used for dynamic survival metrics.
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1     5 Preprocessor1_Model2

---

    Code
      select_by_one_std_err(surv_res, metric = "brier_survival", eval_time = 1.1,
        trees)
    Condition
      Error in `select_by_one_std_err()`:
      ! Evaluation time 1.1 is not in the results.

---

    Code
      select_by_one_std_err(surv_res, metric = "brier_survival", eval_time = 1:2,
      trees)
    Condition
      Warning in `select_by_one_std_err()`:
      2 evaluation times are available; the first will be used (i.e. `eval_time = 1`).
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1     1 Preprocessor1_Model1

---

    Code
      select_by_one_std_err(surv_res, metric = "brier_survival", eval_time = 3:4,
      trees)
    Condition
      Warning in `select_by_one_std_err()`:
      2 evaluation times are available; the first will be used (i.e. `eval_time = 3`).
      Error in `select_by_one_std_err()`:
      ! Evaluation time 3 is not in the results.

# select_by_pct_loss with survival models

    Code
      select_by_pct_loss(surv_res, trees)
    Condition
      Warning in `select_by_pct_loss()`:
      No value of `metric` was given; "brier_survival" will be used.
      Warning in `select_by_pct_loss()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1   100 Preprocessor1_Model5

---

    Code
      select_by_pct_loss(surv_res, metric = "concordance_survival", trees)
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1    10 Preprocessor1_Model3

---

    Code
      select_by_pct_loss(surv_res, metric = "brier_survival_integrated", trees)
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1   100 Preprocessor1_Model5

---

    Code
      select_by_pct_loss(surv_res, metric = "brier_survival", trees)
    Condition
      Warning in `select_by_pct_loss()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1   100 Preprocessor1_Model5

---

    Code
      select_by_pct_loss(surv_res, metric = c("brier_survival", "roc_auc_survival"),
      trees)
    Condition
      Warning in `select_by_pct_loss()`:
      2 metrics were given; "brier_survival" will be used.
      Warning in `select_by_pct_loss()`:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1   100 Preprocessor1_Model5

---

    Code
      select_by_pct_loss(surv_res, metric = "brier_survival", eval_time = 1, trees)
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1     5 Preprocessor1_Model2

---

    Code
      select_by_pct_loss(surv_res, metric = "concordance_survival", eval_time = 1,
        trees)
    Condition
      Warning in `select_by_pct_loss()`:
      `eval_time` is only used for dynamic survival metrics.
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1    10 Preprocessor1_Model3

---

    Code
      select_by_pct_loss(surv_res, metric = "concordance_survival", eval_time = 1.1,
        trees)
    Condition
      Warning in `select_by_pct_loss()`:
      `eval_time` is only used for dynamic survival metrics.
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1    10 Preprocessor1_Model3

---

    Code
      select_by_pct_loss(surv_res, metric = "brier_survival", eval_time = 1.1, trees)
    Condition
      Error in `select_by_pct_loss()`:
      ! Evaluation time 1.1 is not in the results.

---

    Code
      select_by_pct_loss(surv_res, metric = "brier_survival", eval_time = 1:2, trees)
    Condition
      Warning in `select_by_pct_loss()`:
      2 evaluation times are available; the first will be used (i.e. `eval_time = 1`).
    Output
      # A tibble: 1 x 2
        trees .config             
        <dbl> <chr>               
      1     5 Preprocessor1_Model2

---

    Code
      select_by_pct_loss(surv_res, metric = "brier_survival", eval_time = 3:4, trees)
    Condition
      Warning in `select_by_pct_loss()`:
      2 evaluation times are available; the first will be used (i.e. `eval_time = 3`).
      Error in `select_by_pct_loss()`:
      ! Evaluation time 3 is not in the results.

