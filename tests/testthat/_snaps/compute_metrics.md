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

# `metrics` argument works (iterative tuning)

    Code
      set.seed(1)
      res_rmse <- tune_bayes(nearest_neighbor("regression", neighbors = tune()), mpg ~
        ., vfold_cv(mtcars, v = 3), metrics = m_set_rmse, control = tune::control_bayes(
        save_pred = TRUE), iter = 2, initial = 3)
    Message
      (x) GP has a LOO R² of 0% and is unreliable.
      i Generating a candidate as far away from existing points as possible.
      i Generating 15 candidates

---

    Code
      set.seed(1)
      res_both <- tune_bayes(nearest_neighbor("regression", neighbors = tune()), mpg ~
        ., vfold_cv(mtcars, v = 3), metrics = m_set_both, control = tune::control_bayes(
        save_pred = TRUE), iter = 2, initial = 3)
    Message
      (x) GP has a LOO R² of 0% and is unreliable.
      i Generating a candidate as far away from existing points as possible.
      i Generating 15 candidates

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

