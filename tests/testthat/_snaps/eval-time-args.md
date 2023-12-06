# eval time inputs are checked for regression models

    Code
      check_eval_time_arg(NULL, met_reg)
    Output
      NULL

---

    Code
      check_eval_time_arg(times, met_reg)
    Condition
      Warning:
      Evaluation times are only required when the model mode is "censored regression" (and will be ignored).
    Output
      NULL

---

    Code
      res <- fit_resamples(wflow, rs, eval_time = times)
    Condition
      Warning:
      Evaluation times are only required when the model mode is "censored regression" (and will be ignored).

---

    Code
      set.seed(1)
      res <- tune_grid(wflow_tune, rs, eval_time = times)
    Condition
      Warning:
      Evaluation times are only required when the model mode is "censored regression" (and will be ignored).

---

    Code
      set.seed(1)
      res <- tune_bayes(wflow_tune, rs, iter = 1, eval_time = times)
    Condition
      Warning:
      Evaluation times are only required when the model mode is "censored regression" (and will be ignored).

---

    Code
      res <- last_fit(wflow, split, eval_time = times)
    Condition
      Warning:
      Evaluation times are only required when the model mode is "censored regression" (and will be ignored).

# eval time are checked for classification models

    Code
      check_eval_time_arg(NULL, met_cls)
    Output
      NULL

---

    Code
      check_eval_time_arg(times, met_cls)
    Condition
      Warning:
      Evaluation times are only required when the model mode is "censored regression" (and will be ignored).
    Output
      NULL

---

    Code
      res <- fit_resamples(wflow, rs, eval_time = times)
    Condition
      Warning:
      Evaluation times are only required when the model mode is "censored regression" (and will be ignored).

---

    Code
      set.seed(1)
      res <- tune_grid(wflow_tune, rs, eval_time = times)
    Condition
      Warning:
      Evaluation times are only required when the model mode is "censored regression" (and will be ignored).

---

    Code
      set.seed(1)
      res <- tune_bayes(wflow_tune, rs, iter = 1, eval_time = times)
    Condition
      Warning:
      Evaluation times are only required when the model mode is "censored regression" (and will be ignored).

---

    Code
      res <- last_fit(wflow, split, eval_time = times)
    Condition
      Warning:
      Evaluation times are only required when the model mode is "censored regression" (and will be ignored).

