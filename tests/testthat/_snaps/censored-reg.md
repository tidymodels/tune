# evaluation time

    Code
      spec %>% tune_grid(Surv(time, status) ~ ., resamples = rs, metrics = mtr)
    Condition
      Error in `check_eval_times()`:
      ! 1+ metric require the specification of time points in the `eval_times` argument.

---

    Code
      spec %>% tune_grid(Surv(time, status) ~ ., resamples = rs, metrics = reg_mtr)
    Condition
      Error in `check_metrics()`:
      ! The parsnip model has `mode = 'censored regression'`, but `metrics` is a metric set for other model modes.

---

    Code
      spec %>% tune_grid(Surv(time, status) ~ ., resamples = rs, metrics = mtr,
      eval_times = -1)
    Condition
      Error in `check_eval_times()`:
      ! There were no usable evaluation times.

