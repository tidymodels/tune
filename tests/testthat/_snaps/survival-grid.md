# survival analysis - tuning via grid search

    Code
      sr_rs_aug <- augment(sr_tune_res)
    Condition
      Warning:
      No evaluation time was set; a value of 100 was used.
      Warning:
      The orginal data had 400 rows but there were 232 hold-out predictions.

---

    Code
      sr_rs_logn_aug <- augment(sr_tune_res, parameters = tibble(dist = "lognormal"))
    Condition
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
      # A tibble: 2 x 8
        dist        .metric        .estimator .eval_time   mean     n std_err .config 
        <chr>       <chr>          <chr>           <dbl>  <dbl> <int>   <dbl> <chr>   
      1 loglogistic brier_survival standard          100 0.0184     2 0.00188 Preproc~
      2 lognormal   brier_survival standard          100 0.0332     2 0.00145 Preproc~

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
      # A tibble: 2 x 8
        dist        .metric      .estimator .eval_time     mean     n  std_err .config
        <chr>       <chr>        <chr>           <dbl>    <dbl> <int>    <dbl> <chr>  
      1 lognormal   brier_survi~ standard           10 4.04e-28     2 4.04e-28 Prepro~
      2 loglogistic brier_survi~ standard           10 2.61e-23     2 2.61e-23 Prepro~

---

    Code
      show_best(sr_tune_res, metric = "brier_survival_integrated")
    Output
      # A tibble: 2 x 8
        dist        .metric         .estimator .eval_time   mean     n std_err .config
        <chr>       <chr>           <chr>           <dbl>  <dbl> <int>   <dbl> <chr>  
      1 loglogistic brier_survival~ standard           NA 0.0282     2 0.00591 Prepro~
      2 lognormal   brier_survival~ standard           NA 0.0399     2 0.00479 Prepro~

---

    Code
      select_best(sr_tune_res)
    Condition
      Warning:
      No value of `metric` was given; metric 'brier_survival' will be used.
      Warning:
      No evaluation time was set; a value of 100 was used.
    Output
      # A tibble: 1 x 2
        dist        .config             
        <chr>       <chr>               
      1 loglogistic Preprocessor1_Model1

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
      # A tibble: 1 x 2
        dist      .config             
        <chr>     <chr>               
      1 lognormal Preprocessor1_Model2

---

    Code
      select_best(sr_tune_res, metric = "brier_survival_integrated")
    Output
      # A tibble: 1 x 2
        dist        .config             
        <chr>       <chr>               
      1 loglogistic Preprocessor1_Model1

---

    Code
      tune_plot_1 <- autoplot(sr_tune_res)
    Condition
      Warning in `filter_plot_eval_time()`:
      No evaluation time was set; a value of 100 was used.

