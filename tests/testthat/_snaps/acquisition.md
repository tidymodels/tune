# conf_bound interface

    Code
      conf_bound("a")
    Condition
      Error in `conf_bound()`:
      ! `kappa` should be a number or a function.

---

    Code
      conf_bound(function() 1)
    Condition
      Error in `conf_bound()`:
      ! The `trade_off()` function should have at least one argument.

---

    Code
      predict(conf_bound(), test_res, maximize = 2, iter = 1)
    Condition
      Error in `predict()`:
      ! `maximize` must be `TRUE` or `FALSE`, not the number 2.

# prob_improve interface

    Code
      prob_improve("a")
    Condition
      Error in `prob_improve()`:
      ! `trade_off` should be a number or a function.

---

    Code
      prob_improve(function() 1)
    Condition
      Error in `prob_improve()`:
      ! The `trade_off()` function should have at least one argument.

---

    Code
      predict(prob_improve(), test_res, maximize = 2, iter = 1)
    Condition
      Error in `predict()`:
      ! `maximize` must be `TRUE` or `FALSE`, not the number 2.

---

    Code
      predict(prob_improve(), test_res, maximize = TRUE, iter = 1, best = NA)
    Condition
      Error in `predict()`:
      ! `best` must be a number, not `NA`.

---

    Code
      predict(prob_improve(), test_res, maximize = TRUE, iter = 1, best = "WAT")
    Condition
      Error in `predict()`:
      ! `best` must be a number, not the string "WAT".

# exp_improve interface

    Code
      exp_improve("a")
    Condition
      Error in `exp_improve()`:
      ! `trade_off` should be a number or a function.

---

    Code
      exp_improve(function() 2)
    Condition
      Error in `exp_improve()`:
      ! The `trade_off()` function should have at least one argument.

---

    Code
      predict(exp_improve(), test_res, maximize = 2, iter = 1)
    Condition
      Error in `predict()`:
      ! `maximize` must be `TRUE` or `FALSE`, not the number 2.

---

    Code
      predict(exp_improve(), test_res, maximize = TRUE, iter = 1, best = NA)
    Condition
      Error in `predict()`:
      ! `best` must be a number, not `NA`.

---

    Code
      predict(exp_improve(), test_res, maximize = TRUE, iter = 1, best = "WAT")
    Condition
      Error in `predict()`:
      ! `best` must be a number, not the string "WAT".

