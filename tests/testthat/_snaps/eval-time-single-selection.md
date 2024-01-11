# selecting single eval time - pure metric sets

    Code
      first_eval_time(met_dyn, eval_time = NULL)
    Condition
      Error in `first_eval_time()`:
      ! A single evaluation time is required to use this metric.

---

    Code
      first_eval_time(met_dyn, "brier_survival", eval_time = NULL)
    Condition
      Error in `first_eval_time()`:
      ! A single evaluation time is required to use this metric.

---

    Code
      dyn_multi <- first_eval_time(met_dyn, eval_time = times_2)
    Condition
      Warning:
      2 evaluation times were specified during tuning; the first (0.714) will be used.

# selecting single eval time - mixed metric sets - dynamic first

    Code
      first_eval_time(met_mix_dyn, eval_time = NULL)
    Condition
      Error in `first_eval_time()`:
      ! A single evaluation time is required to use this metric.

---

    Code
      dyn_multi <- first_eval_time(met_mix_dyn, eval_time = times_2)
    Condition
      Warning:
      2 evaluation times were specified during tuning; the first (0.714) will be used.

---

    Code
      first_eval_time(met_mix_dyn_all, eval_time = NULL)
    Condition
      Error in `first_eval_time()`:
      ! A single evaluation time is required to use this metric.

---

    Code
      dyn_multi <- first_eval_time(met_mix_dyn_all, eval_time = times_2)
    Condition
      Warning:
      2 evaluation times were specified during tuning; the first (0.714) will be used.

# selecting an evaluation time

    Code
      choose_eval_time(surv_res, "brier_survival")
    Condition
      Warning:
      4 evaluation times were specified during tuning; the first (10) will be used.
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
      Evaluation times are only required when dynmanic or integrated metrics are selected as the primary metric (and will be ignored).
    Output
      NULL

---

    Code
      choose_eval_time(ames_grid_search, "rmse", 1)
    Condition
      Warning:
      Evaluation times are only required when the model mode is "censored regression" (and will be ignored).
    Output
      NULL

