test_that("catch errors", {
  res_1 <- tune:::catcher(log("a"))
  expect_true(class(res_1$res) == "try-error")
  expect_true(length(res_1$signals) == 0)

  res_2 <- tune:::catcher(log(1))
  expect_true(res_2$res == log(1))
  expect_true(length(res_2$signals) == 0)

  res_3 <- tune:::catcher(log(-1))
  expect_true(is.nan(res_3$res))
  expect_true(class(res_3$signals) == "list")
  expect_true(inherits(res_3$signals[[1]], "simpleWarning"))
  expect_true(inherits(res_3$signals[[1]], "warning"))
  expect_true(inherits(res_3$signals[[1]], "condition"))
})
