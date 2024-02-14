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
      `eval_time` is only used for models with mode "censored regression".
    Output
      NULL

---

    Code
      res <- fit_resamples(wflow, rs, eval_time = times)
    Condition
      Warning in `fit_resamples()`:
      `eval_time` is only used for models with mode "censored regression".

---

    Code
      set.seed(1)
      res <- tune_grid(wflow_tune, rs, eval_time = times)
    Condition
      Warning in `tune_grid()`:
      `eval_time` is only used for models with mode "censored regression".

---

    Code
      set.seed(1)
      res <- tune_bayes(wflow_tune, rs, iter = 1, eval_time = times)
    Condition
      Warning in `tune_bayes()`:
      `eval_time` is only used for models with mode "censored regression".

---

    Code
      res <- last_fit(wflow, split, eval_time = times)
    Condition
      Warning in `last_fit()`:
      `eval_time` is only used for models with mode "censored regression".

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
      `eval_time` is only used for models with mode "censored regression".
    Output
      NULL

---

    Code
      res <- fit_resamples(wflow, rs, eval_time = times)
    Condition
      Warning in `fit_resamples()`:
      `eval_time` is only used for models with mode "censored regression".

---

    Code
      set.seed(1)
      res <- tune_grid(wflow_tune, rs, eval_time = times)
    Condition
      Warning in `tune_grid()`:
      `eval_time` is only used for models with mode "censored regression".

---

    Code
      set.seed(1)
      res <- tune_bayes(wflow_tune, rs, iter = 1, eval_time = times)
    Condition
      Warning in `tune_bayes()`:
      `eval_time` is only used for models with mode "censored regression".

---

    Code
      res <- last_fit(wflow, split, eval_time = times)
    Condition
      Warning in `last_fit()`:
      `eval_time` is only used for models with mode "censored regression".

# eval time inputs are checked for censored regression models

    Code
      check_eval_time_arg(NULL, met_stc)
    Output
      NULL

---

    Code
      check_eval_time_arg(NULL, met_dyn)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric". Only 0 unique times were given.

---

    Code
      check_eval_time_arg(NULL, met_int)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      check_eval_time_arg(NULL, met_stc_dyn)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      check_eval_time_arg(NULL, met_stc_int)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      check_eval_time_arg(NULL, met_dyn_stc)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      check_eval_time_arg(NULL, met_dyn_int)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      check_eval_time_arg(NULL, met_int_stc)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      check_eval_time_arg(NULL, met_int_dyn)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      check_eval_time_arg(2, met_stc)
    Condition
      Warning:
      `eval_time` is only used for dynamic or integrated survival metrics.
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
      Error in `check_enough_eval_times()`:
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
      Error in `check_enough_eval_times()`:
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
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      check_eval_time_arg(2, met_int_stc)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 1 unique time was given.

---

    Code
      check_eval_time_arg(2, met_int_dyn)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      check_eval_time_arg(1:3, met_stc)
    Condition
      Warning:
      `eval_time` is only used for dynamic or integrated survival metrics.
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
      Error in `check_enough_eval_times()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric". Only 0 unique times were given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_int)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_stc_dyn)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_stc_int)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_dyn_stc)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_dyn_int)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_int_stc)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_int_dyn)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_stc, eval_time = 2)
    Condition
      Warning in `fit_resamples()`:
      `eval_time` is only used for dynamic or integrated survival metrics.

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_dyn, eval_time = 2)

---

    Code
      fit_resamples(wflow, rs, metrics = met_int, eval_time = 2)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_stc_dyn, eval_time = 2)

---

    Code
      fit_resamples(wflow, rs, metrics = met_stc_int, eval_time = 2)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 1 unique time was given.

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_dyn_stc, eval_time = 2)

---

    Code
      fit_resamples(wflow, rs, metrics = met_dyn_int, eval_time = 2)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_int_stc, eval_time = 2)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 1 unique time was given.

---

    Code
      fit_resamples(wflow, rs, metrics = met_int_dyn, eval_time = 2)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      res <- fit_resamples(wflow, rs, metrics = met_stc, eval_time = 1:3)
    Condition
      Warning in `fit_resamples()`:
      `eval_time` is only used for dynamic or integrated survival metrics.

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
      Error in `check_enough_eval_times()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric". Only 0 unique times were given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_int)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_stc_dyn)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_stc_int)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_dyn_stc)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_dyn_int)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_int_stc)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_int_dyn)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_stc, eval_time = 2)
    Condition
      Warning in `tune_grid()`:
      `eval_time` is only used for dynamic or integrated survival metrics.

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_dyn, eval_time = 2)

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_int, eval_time = 2)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_stc_dyn, eval_time = 2)

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_stc_int, eval_time = 2)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 1 unique time was given.

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_dyn_stc, eval_time = 2)

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_dyn_int, eval_time = 2)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_int_stc, eval_time = 2)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 1 unique time was given.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_int_dyn, eval_time = 2)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      res <- tune_grid(wflow_tune, rs, metrics = met_stc, eval_time = 1:3)
    Condition
      Warning in `tune_grid()`:
      `eval_time` is only used for dynamic or integrated survival metrics.

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
      Error in `check_enough_eval_times()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric". Only 0 unique times were given.

---

    Code
      last_fit(wflow, split, metrics = met_int)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      last_fit(wflow, split, metrics = met_stc_dyn)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      last_fit(wflow, split, metrics = met_stc_int)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      last_fit(wflow, split, metrics = met_dyn_stc)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 1 evaluation time is required for the metric type(s) requested: "dynamic_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      last_fit(wflow, split, metrics = met_dyn_int)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      last_fit(wflow, split, metrics = met_int_stc)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 0 unique times were given.

---

    Code
      last_fit(wflow, split, metrics = met_int_dyn)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 0 unique times were given.

---

    Code
      res <- last_fit(wflow, split, metrics = met_stc, eval_time = 2)
    Condition
      Warning in `last_fit()`:
      `eval_time` is only used for dynamic or integrated survival metrics.

---

    Code
      res <- last_fit(wflow, split, metrics = met_dyn, eval_time = 2)

---

    Code
      last_fit(wflow, split, metrics = met_int, eval_time = 2)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      res <- last_fit(wflow, split, metrics = met_stc_dyn, eval_time = 2)

---

    Code
      last_fit(wflow, split, metrics = met_stc_int, eval_time = 2)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 1 unique time was given.

---

    Code
      res <- last_fit(wflow, split, metrics = met_dyn_stc, eval_time = 2)

---

    Code
      last_fit(wflow, split, metrics = met_dyn_int, eval_time = 2)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      last_fit(wflow, split, metrics = met_int_stc, eval_time = 2)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "integrated_survival_metric" and "static_survival_metric". Only 1 unique time was given.

---

    Code
      last_fit(wflow, split, metrics = met_int_dyn, eval_time = 2)
    Condition
      Error in `check_enough_eval_times()`:
      ! At least 2 evaluation times are required for the metric type(s) requested: "dynamic_survival_metric" and "integrated_survival_metric". Only 1 unique time was given.

---

    Code
      res <- last_fit(wflow, split, metrics = met_stc, eval_time = 1:3)
    Condition
      Warning in `last_fit()`:
      `eval_time` is only used for dynamic or integrated survival metrics.

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

