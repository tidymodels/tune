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

