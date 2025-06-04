# maker static object

    Code
      tune:::make_static(1, param_info = extract_parameter_set_dials(wflow), metrics = metric_set(
        accuracy), eval_time = NULL, split_args = mc_cv_args, control = control_resamples())
    Condition
      Error in `tune:::make_static()`:
      ! `workflow` should be a <workflow> object.

---

    Code
      tune:::make_static(wflow, param_info = 2, metrics = metric_set(accuracy),
      eval_time = NULL, split_args = mc_cv_args, control = control_resamples())
    Condition
      Error in `tune:::make_static()`:
      ! `param_info` should be a <parameters> object.

---

    Code
      tune:::make_static(wflow, param_info = extract_parameter_set_dials(wflow),
      metrics = 3, eval_time = NULL, split_args = mc_cv_args, control = control_resamples())
    Condition
      Error in `tune:::make_static()`:
      ! `metrics` should be a <metric_set> object.

---

    Code
      tune:::make_static(wflow, param_info = extract_parameter_set_dials(wflow),
      metrics = metric_set(accuracy), eval_time = "four", split_args = mc_cv_args,
      control = control_resamples())
    Condition
      Error in `tune:::make_static()`:
      ! `eval_time` should be a numeric vector.

---

    Code
      tune:::update_static(res, bad_part)
    Condition
      Error in `check_static_data()`:
      ! Element `data` should be a tibble in the fit slot.

---

    Code
      tune:::update_static(res, bad_part)
    Condition
      Error in `check_static_data()`:
      ! Element `ind` should be an integer in the fit slot.

