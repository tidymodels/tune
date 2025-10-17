# add_resample_weights() validates inputs correctly

    Code
      add_resample_weights("not_an_rset", c(0.5, 0.3, 0.2))
    Condition
      Error in `add_resample_weights()`:
      ! `rset` must be an rset object.

---

    Code
      add_resample_weights(folds, c("a", "b", "c"))
    Condition
      Error in `.validate_resample_weights()`:
      ! `weights` must be numeric.

---

    Code
      add_resample_weights(folds, c(0.5, 0.3))
    Condition
      Error in `.validate_resample_weights()`:
      ! Length of `weights` (2) must equal number of resamples (3).

---

    Code
      add_resample_weights(folds, c(-0.1, 0.5, 0.6))
    Condition
      Error in `.validate_resample_weights()`:
      ! `weights` must be non-negative.

---

    Code
      add_resample_weights(folds, c(0, 0, 0))
    Condition
      Error in `.validate_resample_weights()`:
      ! At least one weight must be positive.

# get_resample_weights() validates input types

    Code
      get_resample_weights("not_valid_input")
    Condition
      Error in `get_resample_weights()`:
      ! `x` must be an rset or tune_results object.

---

    Code
      get_resample_weights(data.frame(x = 1:3))
    Condition
      Error in `get_resample_weights()`:
      ! `x` must be an rset or tune_results object.

