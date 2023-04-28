# errors informatively with bad input

    Code
      weight_propensity(res_fit_resamples_bad, silly_wt_fn)
    Condition
      Error in `weight_propensity()`:
      ! `object` must have been generated with the control option (`?tune::control_grid()`) `extract = identity`.

---

    Code
      weight_propensity(res_fit_resamples)
    Condition
      Error in `weight_propensity()`:
      ! `wt_fn` must be a function.

---

    Code
      weight_propensity(res_fit_resamples, "boop")
    Condition
      Error in `weight_propensity()`:
      ! `wt_fn` must be a function.

---

    Code
      weight_propensity(res_fit_resamples, function(...) {
        -1L
      })
    Condition
      Error in `hardhat::importance_weights()`:
      ! `x` can't contain negative weights.

---

    Code
      weight_propensity(res_fit_resamples, silly_wt_fn, data = two_class_dat)
    Condition
      Error in `weight_propensity()`:
      ! The <tune_results> method for `weight_propensity()` does not take a `data` argument, but one was supplied.

