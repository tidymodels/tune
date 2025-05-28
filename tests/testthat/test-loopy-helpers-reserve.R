test_that("prediction reserves - regression", {
  preds_1 <- tibble::tibble(.pred = 1:7)
  res_1 <- tune:::initialize_pred_reserve(preds_1, 2)

  expect_equal(res_1[0, ], tibble::tibble(.pred = integer(0)))
  expect_equal(nrow(res_1), nrow(preds_1) * 2)
  expect_equal(sum(complete.cases(res_1)), 0L)

  ## iter 1
  res_1_1 <- tune:::update_reserve(
    res_1,
    grid_size = 2,
    predictions = preds_1,
    iter = 1
  )
  expect_equal(res_1_1[0, ], res_1[0, ])
  expect_equal(nrow(res_1_1), nrow(res_1))
  expect_equal(sum(complete.cases(res_1_1)), nrow(preds_1))

  ## iter 2
  res_1_2 <- tune:::update_reserve(
    res_1_1,
    grid_size = 2,
    predictions = preds_1[7:1, ],
    iter = 2
  )
  expect_equal(res_1_2[0, ], res_1[0, ])
  expect_equal(nrow(res_1_2), nrow(res_1))
  expect_equal(sum(complete.cases(res_1_2)), nrow(preds_1) * 2)

  # ----------------------------------------------------------------------------

  preds_2 <- data.frame(.pred = (10:13) / 2)
  res_2 <- tune:::initialize_pred_reserve(preds_2, 3)

  expect_equal(res_2[0, ], tibble::tibble(.pred = double(0)))
  expect_equal(nrow(res_2), nrow(preds_2) * 3)
  expect_equal(sum(complete.cases(res_2)), 0L)

  ## iter 1
  res_2_2 <- tune:::update_reserve(
    res_2,
    grid_size = 2,
    predictions = preds_2,
    iter = 1
  )
  expect_equal(res_2_2[0, ], res_2[0, ])
  expect_equal(nrow(res_2_2), nrow(res_2))
  expect_equal(sum(complete.cases(res_2_2)), nrow(preds_2))

  ## iter 2
  res_2_2 <- tune:::update_reserve(
    res_2_2,
    grid_size = 2,
    predictions = preds_2[4:1, , drop = FALSE],
    iter = 2
  )
  expect_equal(res_2_2[0, ], res_2[0, ])
  expect_equal(nrow(res_2_2), nrow(res_2))
  expect_equal(sum(complete.cases(res_2_2)), nrow(preds_2) * 2)

  ## iter 3
  res_2_3 <- tune:::update_reserve(
    res_2_2,
    grid_size = 2,
    predictions = preds_2[rep(1, 4), , drop = FALSE],
    iter = 3
  )
  expect_equal(res_2_3[0, ], res_2[0, ])
  expect_equal(nrow(res_2_3), nrow(res_2))
  expect_equal(sum(complete.cases(res_2_3)), nrow(preds_2) * 3)
})

test_that("prediction reserves - classification", {
  let <- factor(letters[1:2][c(1, 2, 1, 1, 2)])

  preds_1 <- tibble::tibble(.pred_class = let)
  res_1 <- tune:::initialize_pred_reserve(preds_1, 2)

  expect_equal(res_1[0, ], tibble::tibble(.pred_class = let[0]))
  expect_equal(nrow(res_1), nrow(preds_1) * 2)
  expect_equal(sum(complete.cases(res_1)), 0L)

  ## iter 1
  res_1_1 <- tune:::update_reserve(
    res_1,
    grid_size = 2,
    predictions = preds_1,
    iter = 1
  )
  expect_equal(res_1_1[0, ], res_1[0, ])
  expect_equal(nrow(res_1_1), nrow(res_1))
  expect_equal(sum(complete.cases(res_1_1)), nrow(preds_1))

  ## iter 2
  res_1_2 <- tune:::update_reserve(
    res_1_1,
    grid_size = 2,
    predictions = preds_1[5:1, ],
    iter = 2
  )
  expect_equal(res_1_2[0, ], res_1[0, ])
  expect_equal(nrow(res_1_2), nrow(res_1))
  expect_equal(sum(complete.cases(res_1_2)), nrow(preds_1) * 2)

  # ----------------------------------------------------------------------------

  preds_2 <- data.frame(
    .pred_class = let[1:3],
    .pred_a = (1:3) / 3,
    .pred_b = (3:1) / 3
  )
  res_2 <- tune:::initialize_pred_reserve(preds_2, 1)

  expect_equal(
    res_2[0, ],
    tibble::tibble(
      .pred_class = let[0],
      .pred_a = double(0),
      .pred_b = double(0)
    )
  )
  expect_equal(nrow(res_2), nrow(preds_2))
  expect_equal(sum(complete.cases(res_2)), 0L)

  # check with null reserve
  res_2_1 <- tune:::update_reserve(
    NULL,
    grid_size = 2,
    predictions = preds_2,
    iter = 1
  )
  expect_equal(res_1_1[0, ], res_1[0, ])
  expect_equal(nrow(res_1_1), nrow(res_1))
  expect_equal(sum(complete.cases(res_1_1)), nrow(preds_1))
})

# TODO
# test_that("prediction reserves - censored regressioj", {
#
#
# })
