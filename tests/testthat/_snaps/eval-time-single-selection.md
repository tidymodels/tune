# selecting single eval time - pure metric sets

    Code
      first_eval_time(met_dyn, eval_time = NULL)
    Condition
      Error:
      ! A single evaluation time is required to use this metric.

---

    Code
      first_eval_time(met_dyn, metric = "brier_survival", eval_time = NULL)
    Condition
      Error:
      ! A single evaluation time is required to use this metric.

---

    Code
      dyn_multi <- first_eval_time(met_dyn, eval_time = times_2)
    Condition
      Warning:
      2 evaluation times are available; the first will be used (i.e. `eval_time = 0.71429`).

# selecting single eval time - mixed metric sets - dynamic first

    Code
      first_eval_time(met_mix_dyn, eval_time = NULL)
    Condition
      Error:
      ! A single evaluation time is required to use this metric.

---

    Code
      dyn_multi <- first_eval_time(met_mix_dyn, eval_time = times_2)
    Condition
      Warning:
      2 evaluation times are available; the first will be used (i.e. `eval_time = 0.71429`).

---

    Code
      first_eval_time(met_mix_dyn_all, eval_time = NULL)
    Condition
      Error:
      ! A single evaluation time is required to use this metric.

---

    Code
      dyn_multi <- first_eval_time(met_mix_dyn_all, eval_time = times_2)
    Condition
      Warning:
      2 evaluation times are available; the first will be used (i.e. `eval_time = 0.71429`).

# selecting an evaluation time

    Code
      choose_eval_time(surv_res, "brier_survival")
    Condition
      Warning:
      4 evaluation times are available; the first will be used (i.e. `eval_time = 10`).
    Output
      [1] 10

---

    Code
      choose_eval_time(surv_res, "concordance_survival")
    Output
      NULL

---

    Code
      choose_eval_time(surv_res, "concordance_survival", eval_time = 10)
    Condition
      Warning:
      `eval_time` is only used for dynamic survival metrics.
    Output
      NULL

---

    Code
      choose_eval_time(ames_grid_search, "rmse", eval_time = 1)
    Condition
      Warning:
      `eval_time` is only used for models with mode "censored regression".
    Output
      NULL

