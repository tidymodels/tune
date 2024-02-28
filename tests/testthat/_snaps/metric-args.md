# metric inputs are checked for regression models

    Code
      check_metrics_arg(NULL, wflow)
    Output
      A metric set, consisting of:
      - `rmse()`, a numeric metric | direction: minimize
      - `rsq()`, a numeric metric  | direction: maximize

---

    Code
      check_metrics_arg(met_reg, wflow)
    Output
      A metric set, consisting of:
      - `rmse()`, a numeric metric | direction: minimize

---

    Code
      check_metrics_arg(met_cls, wflow)
    Condition
      Error:
      ! The parsnip model has `mode` value of "regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      check_metrics_arg(met_mix_int, wflow)
    Condition
      Error:
      ! The parsnip model has `mode` value of "regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      fit_resamples(wflow, rs, metrics = met_cls)
    Condition
      Error in `fit_resamples()`:
      ! The parsnip model has `mode` value of "regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      fit_resamples(wflow, rs, metrics = met_mix_int)
    Condition
      Error in `fit_resamples()`:
      ! The parsnip model has `mode` value of "regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_cls)
    Condition
      Error in `tune_grid()`:
      ! The parsnip model has `mode` value of "regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_mix_int)
    Condition
      Error in `tune_grid()`:
      ! The parsnip model has `mode` value of "regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      tune_bayes(wflow_tune, rs, metrics = met_cls)
    Condition
      Error in `tune_bayes()`:
      ! The parsnip model has `mode` value of "regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      tune_bayes(wflow_tune, rs, metrics = met_mix_int)
    Condition
      Error in `tune_bayes()`:
      ! The parsnip model has `mode` value of "regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      last_fit(wflow, split, metrics = met_cls)
    Condition
      Error in `last_fit()`:
      ! The parsnip model has `mode` value of "regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      last_fit(wflow, split, metrics = met_mix_int)
    Condition
      Error in `last_fit()`:
      ! The parsnip model has `mode` value of "regression", but the `metrics` is a metric set for a different model mode.

# metric inputs are checked for classification models

    Code
      check_metrics_arg(NULL, wflow)
    Output
      A metric set, consisting of:
      - `roc_auc()`, a probability metric     | direction: maximize
      - `accuracy()`, a class metric          | direction: maximize
      - `brier_class()`, a probability metric | direction: minimize

---

    Code
      check_metrics_arg(met_reg, wflow)
    Condition
      Error:
      ! The parsnip model has `mode` value of "classification", but the `metrics` is a metric set for a different model mode.

---

    Code
      check_metrics_arg(met_cls, wflow)
    Output
      A metric set, consisting of:
      - `brier_class()`, a probability metric | direction: minimize

---

    Code
      check_metrics_arg(met_mix_int, wflow)
    Condition
      Error:
      ! The parsnip model has `mode` value of "classification", but the `metrics` is a metric set for a different model mode.

---

    Code
      fit_resamples(wflow, rs, metrics = met_reg)
    Condition
      Error in `fit_resamples()`:
      ! The parsnip model has `mode` value of "classification", but the `metrics` is a metric set for a different model mode.

---

    Code
      fit_resamples(wflow, rs, metrics = met_mix_int)
    Condition
      Error in `fit_resamples()`:
      ! The parsnip model has `mode` value of "classification", but the `metrics` is a metric set for a different model mode.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_reg)
    Condition
      Error in `tune_grid()`:
      ! The parsnip model has `mode` value of "classification", but the `metrics` is a metric set for a different model mode.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_mix_int)
    Condition
      Error in `tune_grid()`:
      ! The parsnip model has `mode` value of "classification", but the `metrics` is a metric set for a different model mode.

---

    Code
      tune_bayes(wflow_tune, rs, metrics = met_reg)
    Condition
      Error in `tune_bayes()`:
      ! The parsnip model has `mode` value of "classification", but the `metrics` is a metric set for a different model mode.

---

    Code
      tune_bayes(wflow_tune, rs, metrics = met_mix_int)
    Condition
      Error in `tune_bayes()`:
      ! The parsnip model has `mode` value of "classification", but the `metrics` is a metric set for a different model mode.

---

    Code
      last_fit(wflow, split, metrics = met_reg)
    Condition
      Error in `last_fit()`:
      ! The parsnip model has `mode` value of "classification", but the `metrics` is a metric set for a different model mode.

---

    Code
      last_fit(wflow, split, metrics = met_mix_int)
    Condition
      Error in `last_fit()`:
      ! The parsnip model has `mode` value of "classification", but the `metrics` is a metric set for a different model mode.

# metric inputs are checked for censored regression models

    Code
      check_metrics_arg(NULL, wflow)
    Output
      A metric set, consisting of:
      - `brier_survival()`, a dynamic survival metric | direction: minimize

---

    Code
      check_metrics_arg(met_reg, wflow)
    Condition
      Error:
      ! The parsnip model has `mode` value of "censored regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      check_metrics_arg(met_cls, wflow)
    Condition
      Error:
      ! The parsnip model has `mode` value of "censored regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      check_metrics_arg(met_srv, wflow)
    Output
      A metric set, consisting of:
      - `concordance_survival()`, a static survival metric | direction: maximize

---

    Code
      fit_resamples(wflow, rs, metrics = met_cls)
    Condition
      Error in `fit_resamples()`:
      ! The parsnip model has `mode` value of "censored regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      fit_resamples(wflow, rs, metrics = met_reg)
    Condition
      Error in `fit_resamples()`:
      ! The parsnip model has `mode` value of "censored regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_cls)
    Condition
      Error in `tune_grid()`:
      ! The parsnip model has `mode` value of "censored regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      tune_grid(wflow_tune, rs, metrics = met_reg)
    Condition
      Error in `tune_grid()`:
      ! The parsnip model has `mode` value of "censored regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      tune_bayes(wflow_tune, rs, metrics = met_cls)
    Condition
      Error in `tune_bayes()`:
      ! The parsnip model has `mode` value of "censored regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      tune_bayes(wflow_tune, rs, metrics = met_reg)
    Condition
      Error in `tune_bayes()`:
      ! The parsnip model has `mode` value of "censored regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      last_fit(wflow, split, metrics = met_cls)
    Condition
      Error in `last_fit()`:
      ! The parsnip model has `mode` value of "censored regression", but the `metrics` is a metric set for a different model mode.

---

    Code
      last_fit(wflow, split, metrics = met_reg)
    Condition
      Error in `last_fit()`:
      ! The parsnip model has `mode` value of "censored regression", but the `metrics` is a metric set for a different model mode.

