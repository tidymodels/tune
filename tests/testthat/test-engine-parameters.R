test_that("check for finalization with engine parameters", {
  pset_1 <- parameters(dials::mtry(), dials::penalty(), dials::mixture())
  pset_2 <- pset_1
  pset_2$object[[3]] <- NA

  pset_3 <- parameters(dials::mtry(1:2), dials::penalty(), dials::mixture())
  pset_4 <- pset_3
  pset_4$object[[3]] <- NA

  expect_true(needs_finalization(pset_1))
  expect_true(needs_finalization(pset_2))
  expect_true(needs_finalization(pset_1, "potato"))
  expect_true(needs_finalization(pset_2, "potato"))

  expect_false(needs_finalization(pset_1, "mtry"))
  expect_false(needs_finalization(pset_2, "mtry"))
  expect_false(needs_finalization(pset_3, "mtry"))
  expect_false(needs_finalization(pset_4, "mtry"))
  expect_false(needs_finalization(pset_3))
  expect_false(needs_finalization(pset_4))
})

## -----------------------------------------------------------------------------

test_that("tuning with engine parameters with dials objects", {
  skip_if_not_installed("randomForest")
  skip_if(utils::packageVersion("dials") <= "0.0.7")

  rf_mod <-
    parsnip::rand_forest(min_n = tune()) %>%
    parsnip::set_engine("randomForest", maxnodes = tune()) %>%
    parsnip::set_mode("regression")

  set.seed(192)
  rs <- rsample::vfold_cv(mtcars)

  set.seed(19828)
  expect_no_error(
    suppressMessages(
      rf_tune <- rf_mod %>% tune_grid(mpg ~ ., resamples = rs, grid = 3)
    )
  )
  expect_no_error(
    p <- autoplot(rf_tune)
  )

  set.seed(283)
  expect_no_error(
    suppressMessages(
      rf_search <- rf_mod %>% tune_bayes(mpg ~ ., resamples = rs, initial = 3, iter = 2)
    )
  )
  expect_no_error(
    p <- autoplot(rf_search)
  )
})

## -----------------------------------------------------------------------------

test_that("tuning with engine parameters without dials objects", {
  skip_if_not_installed("randomForest")
  skip_if(utils::packageVersion("dials") <= "0.0.7")

  ## ---------------------------------------------------------------------------

  rf_mod <-
    parsnip::rand_forest(min_n = tune()) %>%
    parsnip::set_engine("randomForest", corr.bias = tune()) %>%
    parsnip::set_mode("regression")

  grid <-
    data.frame(
      min_n = c(5, 10, 5, 10),
      corr.bias = c(TRUE, TRUE, FALSE, FALSE)
    )

  set.seed(192)
  rs <- rsample::vfold_cv(mtcars)

  ## ---------------------------------------------------------------------------

  expect_snapshot(error = TRUE, {
    rf_tune <- rf_mod %>% tune_grid(mpg ~ ., resamples = rs, grid = 3)
  })

  ## ---------------------------------------------------------------------------

  expect_no_error(
    suppressMessages(
      rf_tune <- rf_mod %>% tune_grid(mpg ~ ., resamples = rs, grid = grid)
    )
  )
  expect_snapshot(error = TRUE, {
    p <- autoplot(rf_tune)
  })

  ## ---------------------------------------------------------------------------

  set.seed(283)
  expect_snapshot(error = TRUE, {
    rf_search <- rf_mod %>% tune_bayes(mpg ~ ., resamples = rs)
  })
})
