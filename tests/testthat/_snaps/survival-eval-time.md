# evaluation time

    Code
      spec %>% tune_grid(Surv(time, status) ~ ., resamples = rs, metrics = mtr)
    Condition
      Error:
      ! One or more metric requires the specification of time points in the `eval_time` argument.

---

    Code
      spec %>% tune_grid(Surv(time, status) ~ ., resamples = rs, metrics = reg_mtr)
    Condition
      Error in `check_metrics()`:
      ! The parsnip model has `mode = 'censored regression'`, but `metrics` is a metric set for a different model mode.

---

    Code
      linear_reg() %>% tune_grid(age ~ ., resamples = rs, metrics = reg_mtr,
      eval_time = 1)
    Condition
      Error:
      ! Evaluation times are only used for dynamic and integrated survival metrics.

---

    Code
      show_notes(no_usable_times)
    Output
      unique notes:
      ------------------------------------------------------------------------
      Error:
      ! There were no usable evaluation times (finite, non-missing, and >= 0).

