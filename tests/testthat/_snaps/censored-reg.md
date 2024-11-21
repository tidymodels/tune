# evaluation time

    Code
      spec %>% tune_grid(Surv(time, status) ~ ., resamples = rs, metrics = mtr)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric". Only 0 unique times were given.

---

    Code
      spec %>% tune_grid(Surv(time, status) ~ ., resamples = rs, metrics = reg_mtr)
    Condition
      Error in `tune_grid()`:
      ! The parsnip model has `mode` value of "censored regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      linear_reg() %>% tune_grid(age ~ ., resamples = rs, metrics = reg_mtr,
      eval_time = 1)
    Condition
      Warning in `tune_grid()`:
      `eval_time` is only used for models with mode "censored regression".
      Warning:
      No tuning parameters have been detected, performance will be evaluated using the resamples with no tuning.
      Did you want to assign any parameters with a value of `tune()`?
    Output
      # Tuning results
      # 10-fold cross-validation using stratification 
      # A tibble: 10 x 4
         splits           id     .metrics         .notes          
         <list>           <chr>  <list>           <list>          
       1 <split [164/20]> Fold01 <tibble [1 x 4]> <tibble [0 x 4]>
       2 <split [165/19]> Fold02 <tibble [1 x 4]> <tibble [0 x 4]>
       3 <split [165/19]> Fold03 <tibble [1 x 4]> <tibble [0 x 4]>
       4 <split [166/18]> Fold04 <tibble [1 x 4]> <tibble [0 x 4]>
       5 <split [166/18]> Fold05 <tibble [1 x 4]> <tibble [0 x 4]>
       6 <split [166/18]> Fold06 <tibble [1 x 4]> <tibble [0 x 4]>
       7 <split [166/18]> Fold07 <tibble [1 x 4]> <tibble [0 x 4]>
       8 <split [166/18]> Fold08 <tibble [1 x 4]> <tibble [0 x 4]>
       9 <split [166/18]> Fold09 <tibble [1 x 4]> <tibble [0 x 4]>
      10 <split [166/18]> Fold10 <tibble [1 x 4]> <tibble [0 x 4]>

---

    Code
      no_usable_times <- spec %>% tune_grid(Surv(time, status) ~ ., resamples = rs,
      metrics = mtr, eval_time = c(-1, Inf))
    Condition
      Error:
      ! There were no usable evaluation times (finite, non-missing, and >= 0).

