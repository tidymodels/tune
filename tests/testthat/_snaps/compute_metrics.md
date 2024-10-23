# `metrics` argument works (differing class metric types)

    Code
      compute_metrics(res_auc, m_set_acc)
    Condition
      Error in `compute_metrics()`:
      ! The supplied `metrics` argument has metrics of type "class", while the metrics used to generate predictions only used "prob" metrics.
      i To save predictions for class metrics, generate `x` with metrics of that type.

---

    Code
      compute_metrics(res_acc, m_set_auc)
    Condition
      Error in `compute_metrics()`:
      ! The supplied `metrics` argument has metrics of type "prob", while the metrics used to generate predictions only used "class" metrics.
      i To save predictions for prob metrics, generate `x` with metrics of that type.

# errors informatively with bad input

    Code
      compute_metrics(res_rmse, metric_set(rsq))
    Condition
      Error in `compute_metrics()`:
      ! `x` must have been generated with the control argument `save_pred = TRUE`.

---

    Code
      compute_metrics("boop", metric_set(rsq))
    Condition
      Error in `compute_metrics()`:
      ! No `compute_metrics()` method exists for a string.

---

    Code
      compute_metrics(res_rmse_save_pred, "wheee")
    Condition
      Error in `compute_metrics()`:
      ! `metrics` must be a metric set.

