# augment fit_resamples

    Code
      augment(fit_1, hey = "you")
    Condition
      Error in `augment()`:
      ! The only argument for `augment.fit_resamples()` is 'x'. Others were passed: 'hey'

---

    Code
      aug_2 <- augment(fit_2)
    Condition
      Warning:
      The original data had 791 rows but there were 593 hold-out predictions.

# augment tune_grid

    Code
      aug_1 <- augment(fit_1)
    Condition
      Warning in `augment()`:
      No value of `metric` was given; "rmse" will be used.

---

    Code
      augment(fit_1, parameters = list(cost = 3))
    Condition
      Error in `augment()`:
      ! 'parameters' should be a single row data frame

---

    Code
      augment(fit_1, parameters = data.frame(cost = 3:4))
    Condition
      Error in `augment()`:
      ! 'parameters' should be a single row data frame

---

    Code
      augment(fit_1, cost = 4)
    Condition
      Error in `augment()`:
      ! The only two arguments for `augment.tune_results()` are 'x' and 'parameters'. Others were passed: 'cost'

---

    Code
      aug_3 <- augment(fit_2)
    Condition
      Warning in `augment()`:
      No value of `metric` was given; "rmse" will be used.

# augment last_fit

    Code
      augment(fit_1, potato = TRUE)
    Condition
      Error in `augment()`:
      ! The only argument for `augment.last_fit()` is 'x'. Others were passed: 'potato'

