# bad argss

    Code
      conf_mat_resampled(svm_results)
    Condition
      Error in `conf_mat_resampled()`:
      ! It looks like there are 5 tuning parameter combination in the data. Please use the `parameters` argument to select one combination of parameters.

---

    Code
      conf_mat_resampled(mt_knn_bo)
    Condition
      Error in `conf_mat_resampled()`:
      ! Cannot find the predicted classes. Was this a classification model?

---

    Code
      conf_mat_resampled(broke_results)
    Condition
      Error in `conf_mat_resampled()`:
      ! The function was not run with the `save_pred = TRUE` option. Please re-run with that option.

---

    Code
      conf_mat_resampled(tibble::as_tibble(svm_results))
    Condition
      Error in `conf_mat_resampled()`:
      ! The first argument needs to be <tune_results> object, not a data frame.

---

    Code
      conf_mat_resampled(broke_results)
    Condition
      Error in `conf_mat_resampled()`:
      ! Cannot find the predicted classes. Was this a classification model?

---

    Code
      conf_mat_resampled(broke_results, parameters = select_best(broke_results,
        metric = "accuracy"))
    Condition
      Error in `conf_mat_resampled()`:
      ! Cannot determine the proper outcome name

---

    Code
      conf_mat_resampled(svm_results, argument_that_doesnt_exist = TRUE)
    Condition
      Error in `conf_mat_resampled()`:
      ! `...` must be empty.
      x Problematic argument:
      * argument_that_doesnt_exist = TRUE

