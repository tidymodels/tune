# survival analysis - resampled

    Code
      sr_rs_aug <- augment(sr_rs_res)
    Condition
      Warning:
      The orginal data had 400 rows but there were 232 hold-out predictions.

---

    Code
      show_best(sr_rs_res)
    Condition
      Warning:
      No value of `metric` was given; metric 'brier_survival' will be used.
      Warning:
      No evaluation time was set; a value of 100 was used.
    Output
      # A tibble: 1 x 7
        .metric        .estimator .eval_time   mean     n std_err .config             
        <chr>          <chr>           <dbl>  <dbl> <int>   <dbl> <chr>               
      1 brier_survival standard          100 0.0148     2 0.00209 Preprocessor1_Model1

---

    Code
      show_best(sr_rs_res, metric = "brier_survival", eval_time = -1)
    Condition
      Error in `choose_eval_time()`:
      ! No evaluation times matched a value of -1.

---

    Code
      show_best(sr_rs_res, metric = "brier_survival", eval_time = 10)
    Output
      # A tibble: 1 x 7
        .metric        .estimator .eval_time     mean     n  std_err .config          
        <chr>          <chr>           <dbl>    <dbl> <int>    <dbl> <chr>            
      1 brier_survival standard           10 6.08e-23     2 6.08e-23 Preprocessor1_Mo~

---

    Code
      show_best(sr_rs_res, metric = "brier_survival_integrated")
    Output
      # A tibble: 1 x 7
        .metric                   .estimator .eval_time   mean     n std_err .config  
        <chr>                     <chr>           <dbl>  <dbl> <int>   <dbl> <chr>    
      1 brier_survival_integrated standard           NA 0.0258     2 0.00604 Preproce~

