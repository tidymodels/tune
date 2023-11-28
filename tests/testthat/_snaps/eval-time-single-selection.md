# selecting single eval time - pure metric sets

    Evaluation times are only required for dynmanic or integrated metrics.

---

    Evaluation times are only required for dynmanic or integrated metrics.

---

    Code
      select_eval_time(met_dyn, eval_time = NULL, single = TRUE)
    Condition
      Error in `select_eval_time()`:
      ! A single evaluation time is required; please choose one.

---

    2 evaluation times were selected; the first (0.714) will be used.

---

    Code
      select_eval_time(met_int, eval_time = NULL, single = TRUE)
    Condition
      Error in `select_eval_time()`:
      ! 2+ evaluation times are required.

---

    Code
      select_eval_time(met_int, eval_time = times_1, single = TRUE)
    Condition
      Error in `select_eval_time()`:
      ! 2+ evaluation times are required.

# selecting single eval time - mixed metric sets - dynamic first

    Code
      select_eval_time(met_mix_dyn, eval_time = NULL, single = TRUE)
    Condition
      Error in `select_eval_time()`:
      ! A single evaluation time is required; please choose one.

---

    Code
      select_eval_time(met_mix_dyn_all, eval_time = NULL, single = TRUE)
    Condition
      Error in `select_eval_time()`:
      ! A single evaluation time is required; please choose one.

# selecting single eval time - mixed metric sets - integrated first

    Code
      select_eval_time(met_mix_int, eval_time = NULL, single = TRUE)
    Condition
      Error in `select_eval_time()`:
      ! 2+ evaluation times are required.

---

    Code
      select_eval_time(met_mix_int, eval_time = times_1, single = TRUE)
    Condition
      Error in `select_eval_time()`:
      ! 2+ evaluation times are required.

---

    Code
      select_eval_time(met_mix_int_all, eval_time = NULL, single = TRUE)
    Condition
      Error in `select_eval_time()`:
      ! 2+ evaluation times are required.

---

    Code
      select_eval_time(met_mix_int_all, eval_time = times_1, single = TRUE)
    Condition
      Error in `select_eval_time()`:
      ! 2+ evaluation times are required.

