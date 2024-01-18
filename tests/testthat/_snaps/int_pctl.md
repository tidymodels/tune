# percentile intervals - resamples only

    Code
      int_res_1 <- int_pctl(lm_res, times = 500)
    Condition
      Warning:
      Recommend at least 1000 non-missing bootstrap resamples for terms: `rmse`, `rsq`.

---

    Code
      int_res_2 <- int_pctl(lm_res, times = 500, alpha = 0.25)
    Condition
      Warning:
      Recommend at least 1000 non-missing bootstrap resamples for terms: `rmse`, `rsq`.

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

# percentile intervals - grid + bayes tuning

    Code
      int_res_1 <- int_pctl(c5_res)

# percentile intervals - grid tuning with validation set

    Code
      int_res_1 <- int_pctl(c5_res)

