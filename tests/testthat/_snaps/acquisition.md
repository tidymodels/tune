# conf_bound interface

    Code
      conf_bound("a")
    Error <simpleError>
      `kappa` should be a number or a function.

---

    Code
      conf_bound(function() 1)
    Error <simpleError>
      The `trade_off` function should have at least one argument.

---

    Code
      predict(conf_bound(), test_res, maximize = 2, iter = 1)
    Error <simpleError>
      `maximize` should be a single logical.

# prob_improve interface

    Code
      prob_improve("a")
    Error <simpleError>
      `trade_off` should be a number or a function.

---

    Code
      prob_improve(function() 1)
    Error <simpleError>
      The `trade_off` function should have at least one argument.

---

    Code
      predict(prob_improve(), test_res, maximize = 2, iter = 1)
    Error <simpleError>
      `maximize` should be a single logical.

---

    Code
      predict(prob_improve(), test_res, maximize = TRUE, iter = 1, best = NA)
    Error <simpleError>
      `best` should be a single, non-missing numeric

---

    Code
      predict(prob_improve(), test_res, maximize = TRUE, iter = 1, best = "WAT")
    Error <simpleError>
      `best` should be a single, non-missing numeric

# exp_improve interface

    Code
      exp_improve("a")
    Error <simpleError>
      `trade_off` should be a number or a function.

---

    Code
      exp_improve(function() 2)
    Error <simpleError>
      The `trade_off` function should have at least one argument.

---

    Code
      predict(exp_improve(), test_res, maximize = 2, iter = 1)
    Error <simpleError>
      `maximize` should be a single logical.

---

    Code
      predict(exp_improve(), test_res, maximize = TRUE, iter = 1, best = NA)
    Error <simpleError>
      `best` should be a single, non-missing numeric

---

    Code
      predict(exp_improve(), test_res, maximize = TRUE, iter = 1, best = "WAT")
    Error <simpleError>
      `best` should be a single, non-missing numeric

