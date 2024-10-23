# tuning with engine parameters without dials objects

    Code
      rf_tune <- rf_mod %>% tune_grid(mpg ~ ., resamples = rs, grid = 3)
    Condition
      Error in `check_param_objects()`:
      ! The workflow has arguments to be tuned that are missing some parameter objects: "corr.bias"

---

    Code
      p <- autoplot(rf_tune)
    Condition
      Error in `autoplot()`:
      ! Some parameters do not have corresponding parameter objects and cannot be used with `autoplot()`: `corr.bias`.

---

    Code
      rf_search <- rf_mod %>% tune_bayes(mpg ~ ., resamples = rs)
    Condition
      Error in `check_param_objects()`:
      ! The workflow has arguments to be tuned that are missing some parameter objects: "corr.bias"

