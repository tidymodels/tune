# selecting single eval time - pure metric sets

    Evaluation times are only required when dynmanic or integrated metrics are selected as the primary metric.

---

    Evaluation times are only required when dynmanic or integrated metrics are selected as the primary metric.

---

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

    2 evaluation times were available; the first (0.714) will be used.

# selecting single eval time - mixed metric sets - dynamic first

    Code
      first_eval_time(met_mix_dyn, eval_time = NULL)
    Condition
      Error in `first_eval_time()`:
      ! A single evaluation time is required to use this metric.

---

    Code
      first_eval_time(met_mix_dyn_all, eval_time = NULL)
    Condition
      Error in `first_eval_time()`:
      ! A single evaluation time is required to use this metric.

