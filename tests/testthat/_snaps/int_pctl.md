# percentile intervals - resamples only

    Code
      int_res_1 <- int_pctl(lm_res, times = 200)
    Condition
      Warning:
      Recommend at least 1000 non-missing bootstrap resamples for terms: `rmse`, `rsq`.

---

    Code
      int_pctl(lm_res, times = 2000, metrics = "rmse")
    Condition
      Error in `int_pctl()`:
      ! `metrics` should be a yardstick `metric_set()`.

# percentile intervals - last fit

    Code
      int_res_1 <- int_pctl(lm_res, times = 200)
    Condition
      Warning:
      Recommend at least 1000 non-missing bootstrap resamples for term `mae`.

---

    Code
      int_res_2 <- int_pctl(lm_res, times = 200)
    Condition
      Warning:
      Recommend at least 1000 non-missing bootstrap resamples for term `mae`.

# percentile intervals - tuning

    Code
      int_res_1 <- int_pctl(c5_res, eval_time = 2)
    Condition
      Warning:
      The 'eval_time' argument is not needed for this data set.

