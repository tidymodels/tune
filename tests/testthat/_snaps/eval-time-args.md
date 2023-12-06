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

