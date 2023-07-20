# survival analysis - tuning via Bayesian search

    Code
      sr_tune_res <- sr_tune_wflow %>% tune_bayes(resamples = churn_rs, metrics = event_metrics,
        eval_time = eval_times, param_info = sr_tune_param, initial = 5, iter = 2,
        control = control_bayes(save_pred = TRUE))
    Message
      ! Bootstrap1: preprocessor 2/5, model 1/1: Ran out of iterations and did not converge
      ! Bootstrap1: preprocessor 3/5, model 1/1: Ran out of iterations and did not converge
      ! Bootstrap2: preprocessor 3/5, model 1/1: Ran out of iterations and did not converge
      ! Bootstrap1: preprocessor 1/1, model 1/1: Ran out of iterations and did not converge

---

    Code
      sr_rs_aug <- augment(sr_tune_res)
    Condition
      Warning:
      No evaluation time was set; a value of 100 was used.
      Warning:
      The orginal data had 400 rows but there were 232 hold-out predictions.

---

    Code
      show_best(sr_tune_res)
    Condition
      Warning:
      No value of `metric` was given; metric 'brier_survival' will be used.
      Warning:
      No evaluation time was set; a value of 100 was used.
    Output
      # A tibble: 5 x 10
        dist  degree .metric .estimator .eval_time    mean     n std_err .config .iter
        <chr>  <dbl> <chr>   <chr>           <dbl>   <dbl> <int>   <dbl> <chr>   <int>
      1 logl~   5.87 brier_~ standard          100 0.00809     2 4.94e-4 Iter1       1
      2 logn~   9.99 brier_~ standard          100 0.00819     2 5.81e-4 Iter2       2
      3 logl~   9.09 brier_~ standard          100 0.00963     2 1.29e-3 Prepro~     0
      4 logn~   4.70 brier_~ standard          100 0.00998     2 1.53e-4 Prepro~     0
      5 logl~   7.08 brier_~ standard          100 0.0113      2 1.25e-3 Prepro~     0

---

    Code
      show_best(sr_tune_res, metric = "brier_survival", eval_time = -1)
    Condition
      Error in `choose_eval_time()`:
      ! No evaluation times matched a value of -1.

---

    Code
      show_best(sr_tune_res, metric = "brier_survival", eval_time = 10)
    Output
      # A tibble: 5 x 10
        dist    degree .metric .estimator .eval_time  mean     n std_err .config .iter
        <chr>    <dbl> <chr>   <chr>           <dbl> <dbl> <int>   <dbl> <chr>   <int>
      1 lognor~   2.16 brier_~ standard           10     0     2       0 Prepro~     0
      2 loglog~   7.08 brier_~ standard           10     0     2       0 Prepro~     0
      3 loglog~   9.09 brier_~ standard           10     0     2       0 Prepro~     0
      4 lognor~   4.70 brier_~ standard           10     0     2       0 Prepro~     0
      5 loglog~   3.56 brier_~ standard           10     0     2       0 Prepro~     0

---

    Code
      show_best(sr_tune_res, metric = "brier_survival_integrated")
    Output
      # A tibble: 5 x 10
        dist   degree .metric .estimator .eval_time   mean     n std_err .config .iter
        <chr>   <dbl> <chr>   <chr>           <dbl>  <dbl> <int>   <dbl> <chr>   <int>
      1 logno~   9.99 brier_~ standard           NA 0.0108     2 1.27e-4 Iter2       2
      2 loglo~   5.87 brier_~ standard           NA 0.0120     2 1.72e-3 Iter1       1
      3 loglo~   9.09 brier_~ standard           NA 0.0123     2 1.72e-3 Prepro~     0
      4 loglo~   7.08 brier_~ standard           NA 0.0138     2 2.23e-3 Prepro~     0
      5 loglo~   3.56 brier_~ standard           NA 0.0143     2 8.43e-4 Prepro~     0

---

    Code
      select_best(sr_tune_res)
    Condition
      Warning:
      No value of `metric` was given; metric 'brier_survival' will be used.
      Warning:
      No evaluation time was set; a value of 100 was used.
    Output
      # A tibble: 1 x 3
        dist        degree .config
        <chr>        <dbl> <chr>  
      1 loglogistic   5.87 Iter1  

---

    Code
      select_best(sr_tune_res, metric = "brier_survival", eval_time = -1)
    Condition
      Error in `choose_eval_time()`:
      ! No evaluation times matched a value of -1.

---

    Code
      select_best(sr_tune_res, metric = "brier_survival", eval_time = 10)
    Output
      # A tibble: 1 x 3
        dist      degree .config             
        <chr>      <dbl> <chr>               
      1 lognormal   2.16 Preprocessor1_Model1

---

    Code
      select_best(sr_tune_res, metric = "brier_survival_integrated")
    Output
      # A tibble: 1 x 3
        dist      degree .config
        <chr>      <dbl> <chr>  
      1 lognormal   9.99 Iter2  

---

    Code
      tune_plot_1 <- autoplot(sr_tune_res)
    Condition
      Warning in `filter_plot_eval_time()`:
      No evaluation time was set; a value of 100 was used.

