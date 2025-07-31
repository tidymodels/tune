test_that("catch errors", {
  res_1 <- tune:::catcher(log("a"))
  expect_true(class(res_1) == "try-error")
  expect_true(length(attr(res_1, "notes")) == 0)

  res_2 <- tune:::catcher(log(1))
  expect_true(res_2 == log(1))
  expect_true(length(attr(res_2, "notes")) == 0)

  res_3 <- tune:::catcher(log(-1))
  expect_true(is.nan(res_3))
  expect_true(class(attr(res_3, "notes")) == "list")
  expect_true(inherits(attr(res_3, "notes")[[1]], "simpleWarning"))
  expect_true(inherits(attr(res_3, "notes")[[1]], "warning"))
  expect_true(inherits(attr(res_3, "notes")[[1]], "condition"))
})
