# selecting single eval time - pure metric sets

    Code
      stc_one <- first_eval_time(met_stc, eval_time = times_1)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are selected as the primary metric.

---

    Code
      stc_multi <- first_eval_time(met_stc, eval_time = times_2)
    Condition
      Warning:
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

    Code
      dyn_multi <- first_eval_time(met_dyn, eval_time = times_2)
    Condition
      Warning:
      2 evaluation times were available; the first (0.714) will be used.

---

    Code
      int_1 <- first_eval_time(met_int, eval_time = times_1)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are selected as the primary metric.

---

    Code
      int_multi <- first_eval_time(met_int, eval_time = times_2)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are selected as the primary metric.

# selecting single eval time - mixed metric sets - static first

    Code
      stc_1 <- first_eval_time(met_mix_stc, eval_time = times_1)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are selected as the primary metric.

---

    Code
      stc_multi <- first_eval_time(met_mix_stc, eval_time = times_2)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are selected as the primary metric.

---

    Code
      stc_1 <- first_eval_time(met_mix_stc_all, eval_time = times_1)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are selected as the primary metric.

---

    Code
      stc_multi <- first_eval_time(met_mix_stc_all, eval_time = times_2)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are selected as the primary metric.

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
      2 evaluation times were available; the first (0.714) will be used.

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
      2 evaluation times were available; the first (0.714) will be used.

# selecting single eval time - mixed metric sets - integrated first

    Code
      first_eval_time(met_mix_int, eval_time = times_1)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are selected as the primary metric.
    Output
      NULL

---

    Code
      int_multi <- first_eval_time(met_mix_int, eval_time = times_2)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are selected as the primary metric.

---

    Code
      first_eval_time(met_mix_int_all, eval_time = times_1)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are selected as the primary metric.
    Output
      NULL

---

    Code
      int_multi <- first_eval_time(met_mix_int_all, eval_time = times_2)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are selected as the primary metric.

