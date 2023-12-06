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

---

    Code
      check_eval_time_arg(NULL, met_stc)
    Output
      NULL

---

    Code
      check_eval_time_arg(NULL, met_dyn)
    Condition
      Error:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric". Only 0 unique times were given.

---

    Code
      check_eval_time_arg(NULL, met_int)
    Condition
      Error:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      check_eval_time_arg(NULL, met_stc_dyn)
    Condition
      Error:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      check_eval_time_arg(NULL, met_stc_int)
    Condition
      Error:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      check_eval_time_arg(NULL, met_dyn_stc)
    Condition
      Error:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      check_eval_time_arg(NULL, met_dyn_int)
    Condition
      Error:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      check_eval_time_arg(NULL, met_int_stc)
    Condition
      Error:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      check_eval_time_arg(NULL, met_int_dyn)
    Condition
      Error:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      check_eval_time_arg(2, met_stc)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are used (and will be ignored here).
    Output
      NULL

---

    Code
      check_eval_time_arg(2, met_dyn)
    Output
      [1] 2

---

    Code
      check_eval_time_arg(2, met_int)
    Condition
      Error:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      check_eval_time_arg(2, met_stc_dyn)
    Output
      [1] 2

---

    Code
      check_eval_time_arg(2, met_stc_int)
    Condition
      Error:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 1 unique time was given.

---

    Code
      check_eval_time_arg(2, met_dyn_stc)
    Output
      [1] 2

---

    Code
      check_eval_time_arg(2, met_dyn_int)
    Condition
      Error:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      check_eval_time_arg(2, met_int_stc)
    Condition
      Error:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 1 unique time was given.

---

    Code
      check_eval_time_arg(2, met_int_dyn)
    Condition
      Error:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      check_eval_time_arg(1:3, met_stc)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are used (and will be ignored here).
    Output
      NULL

---

    Code
      check_eval_time_arg(1:3, met_dyn)
    Output
      [1] 1 2 3

---

    Code
      check_eval_time_arg(1:3, met_int)
    Output
      [1] 1 2 3

---

    Code
      check_eval_time_arg(1:3, met_stc_dyn)
    Output
      [1] 1 2 3

---

    Code
      check_eval_time_arg(1:3, met_stc_int)
    Output
      [1] 1 2 3

---

    Code
      check_eval_time_arg(1:3, met_dyn_stc)
    Output
      [1] 1 2 3

---

    Code
      check_eval_time_arg(1:3, met_dyn_int)
    Output
      [1] 1 2 3

---

    Code
      check_eval_time_arg(1:3, met_int_stc)
    Output
      [1] 1 2 3

---

    Code
      check_eval_time_arg(1:3, met_int_dyn)
    Output
      [1] 1 2 3

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_stc)

---

    Code
      fit_resamples(wflow, rs, metrics = met_dyn)
    Condition
      Error in `fit_resamples()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric". Only 0 unique times were given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_int)
    Condition
      Error in `fit_resamples()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_stc_dyn)
    Condition
      Error in `fit_resamples()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_stc_int)
    Condition
      Error in `fit_resamples()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_dyn_stc)
    Condition
      Error in `fit_resamples()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_dyn_int)
    Condition
      Error in `fit_resamples()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_int_stc)
    Condition
      Error in `fit_resamples()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_int_dyn)
    Condition
      Error in `fit_resamples()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_stc, eval_time = 2)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are used (and will be ignored here).

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_dyn, eval_time = 2)

---

    Code
      fit_resamples(wflow, rs, metrics = met_int, eval_time = 2)
    Condition
      Error in `fit_resamples()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_stc_dyn, eval_time = 2)

---

    Code
      fit_resamples(wflow, rs, metrics = met_stc_int, eval_time = 2)
    Condition
      Error in `fit_resamples()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 1 unique time was given.

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_dyn_stc, eval_time = 2)

---

    Code
      fit_resamples(wflow, rs, metrics = met_dyn_int, eval_time = 2)
    Condition
      Error in `fit_resamples()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_int_stc, eval_time = 2)
    Condition
      Error in `fit_resamples()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 1 unique time was given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_int_dyn, eval_time = 2)
    Condition
      Error in `fit_resamples()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_stc, eval_time = 1:3)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are used (and will be ignored here).

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_dyn, eval_time = 1:3)

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_int, eval_time = 1:3)

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_stc_dyn, eval_time = 1:3)

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_stc_int, eval_time = 1:3)

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_dyn_stc, eval_time = 1:3)

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_dyn_int, eval_time = 1:3)

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_int_stc, eval_time = 1:3)

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_int_dyn, eval_time = 1:3)

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_stc)

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_dyn)
    Condition
      Error in `tune_grid()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric". Only 0 unique times were given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_int)
    Condition
      Error in `tune_grid()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_stc_dyn)
    Condition
      Error in `tune_grid()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_stc_int)
    Condition
      Error in `tune_grid()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_dyn_stc)
    Condition
      Error in `tune_grid()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_dyn_int)
    Condition
      Error in `tune_grid()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_int_stc)
    Condition
      Error in `tune_grid()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_int_dyn)
    Condition
      Error in `tune_grid()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_stc, eval_time = 2)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are used (and will be ignored here).

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_dyn, eval_time = 2)

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_int, eval_time = 2)
    Condition
      Error in `tune_grid()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_stc_dyn, eval_time = 2)

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_stc_int, eval_time = 2)
    Condition
      Error in `tune_grid()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 1 unique time was given.

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_dyn_stc, eval_time = 2)

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_dyn_int, eval_time = 2)
    Condition
      Error in `tune_grid()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_int_stc, eval_time = 2)
    Condition
      Error in `tune_grid()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 1 unique time was given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_int_dyn, eval_time = 2)
    Condition
      Error in `tune_grid()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_stc, eval_time = 1:3)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are used (and will be ignored here).

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_dyn, eval_time = 1:3)

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_int, eval_time = 1:3)

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_stc_dyn, eval_time = 1:3)

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_stc_int, eval_time = 1:3)

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_dyn_stc, eval_time = 1:3)

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_dyn_int, eval_time = 1:3)

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_int_stc, eval_time = 1:3)

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_int_dyn, eval_time = 1:3)

---

    Code
      last_fit(wflow, split, metrics = met_dyn)
    Condition
      Error in `last_fit()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric". Only 0 unique times were given.

---

    Code
      last_fit(wflow, split, metrics = met_int)
    Condition
      Error in `last_fit()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      last_fit(wflow, split, metrics = met_stc_dyn)
    Condition
      Error in `last_fit()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      last_fit(wflow, split, metrics = met_stc_int)
    Condition
      Error in `last_fit()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      last_fit(wflow, split, metrics = met_dyn_stc)
    Condition
      Error in `last_fit()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      last_fit(wflow, split, metrics = met_dyn_int)
    Condition
      Error in `last_fit()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      last_fit(wflow, split, metrics = met_int_stc)
    Condition
      Error in `last_fit()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      last_fit(wflow, split, metrics = met_int_dyn)
    Condition
      Error in `last_fit()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      res <- last_fit(wflow, split, metrics = met_stc, eval_time = 2)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are used (and will be ignored here).

---

    Code
      res <- last_fit(wflow, split, metrics = met_dyn, eval_time = 2)

---

    Code
      last_fit(wflow, split, metrics = met_int, eval_time = 2)
    Condition
      Error in `last_fit()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      res <- last_fit(wflow, split, metrics = met_stc_dyn, eval_time = 2)

---

    Code
      last_fit(wflow, split, metrics = met_stc_int, eval_time = 2)
    Condition
      Error in `last_fit()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 1 unique time was given.

---

    Code
      res <- last_fit(wflow, split, metrics = met_dyn_stc, eval_time = 2)

---

    Code
      last_fit(wflow, split, metrics = met_dyn_int, eval_time = 2)
    Condition
      Error in `last_fit()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      last_fit(wflow, split, metrics = met_int_stc, eval_time = 2)
    Condition
      Error in `last_fit()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 1 unique time was given.

---

    Code
      last_fit(wflow, split, metrics = met_int_dyn, eval_time = 2)
    Condition
      Error in `last_fit()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      res <- last_fit(wflow, split, metrics = met_stc, eval_time = 1:3)
    Condition
      Warning:
      Evaluation times are only required when dynmanic or integrated metrics are used (and will be ignored here).

---

    Code
      res <- last_fit(wflow, split, metrics = met_dyn, eval_time = 1:3)

---

    Code
      res <- last_fit(wflow, split, metrics = met_int, eval_time = 1:3)

---

    Code
      res <- last_fit(wflow, split, metrics = met_stc_dyn, eval_time = 1:3)

---

    Code
      res <- last_fit(wflow, split, metrics = met_stc_int, eval_time = 1:3)

---

    Code
      res <- last_fit(wflow, split, metrics = met_dyn_stc, eval_time = 1:3)

---

    Code
      res <- last_fit(wflow, split, metrics = met_dyn_int, eval_time = 1:3)

---

    Code
      res <- last_fit(wflow, split, metrics = met_int_stc, eval_time = 1:3)

---

    Code
      res <- last_fit(wflow, split, metrics = met_int_dyn, eval_time = 1:3)

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

