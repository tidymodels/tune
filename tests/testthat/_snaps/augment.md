# augment fit_resamples

    Code
      aug_2 <- augment(fit_2)
    Warning <rlang_warning>
      The orginal data had 791 rows but there were 593 hold-out predictions.

---

    Code
      augment(fit_1, hey = "you")
    Error <rlang_error>
      The only argument for `augment.fit_resamples()` is 'x'. Others were passed: 'hey'

# augment tune_grid

    Code
      augment(fit_1, parameters = list(cost = 3))
    Error <rlang_error>
      'parameters' should be a single row data frame

---

    Code
      augment(fit_1, parameters = data.frame(cost = 3:4))
    Error <rlang_error>
      'parameters' should be a single row data frame

---

    Code
      augment(fit_1, cost = 4)
    Error <rlang_error>
      The only two arguments for `augment.tune_results()` are 'x' and 'parameters'. Others were passed: 'cost'

# augment last_fit

    Code
      augment(fit_1, potato = TRUE)
    Error <rlang_error>
      The only argument for `augment.last_fit()` is 'x'. Others were passed: 'potato'

