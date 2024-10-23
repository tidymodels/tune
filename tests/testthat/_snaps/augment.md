# augment fit_resamples

    Code
      augment(fit_1, hey = "you")
    Condition
      Error in `augment()`:
      ! `...` must be empty.
      x Problematic argument:
      * hey = "you"

---

    Code
      aug_2 <- augment(fit_2)
    Condition
      Warning:
      The original data had 791 rows but there were 593 hold-out predictions.

# augment tune_grid

    Code
      augment(fit_1, parameters = list(cost = 3))
    Condition
      Error in `augment()`:
      ! `parameters` should be a single row data frame.

---

    Code
      augment(fit_1, parameters = data.frame(cost = 3:4))
    Condition
      Error in `augment()`:
      ! `parameters` should be a single row data frame.

---

    Code
      augment(fit_1, cost = 4)
    Condition
      Error in `augment()`:
      ! `...` must be empty.
      x Problematic argument:
      * cost = 4

# augment last_fit

    Code
      augment(fit_1, potato = TRUE)
    Condition
      Error in `augment()`:
      ! `...` must be empty.
      x Problematic argument:
      * potato = TRUE

