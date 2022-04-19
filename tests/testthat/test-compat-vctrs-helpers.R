# ------------------------------------------------------------------------------
# tune_results_can_reconstruct()

test_that("identical results are reconstructable", {
  x <- helper_tune_results$tune
  expect_true(tune_results_can_reconstruct(x, x))
})

test_that("column order doesn't matter", {
  to <- helper_tune_results$tune
  x <- as_tibble(to)[rev(names(to))]
  expect_true(tune_results_can_reconstruct(x, to))
})

test_that("all columns must exist", {
  to <- helper_tune_results$tune
  x <- as_tibble(to)["id"]
  expect_false(tune_results_can_reconstruct(x, to))
})

test_that("can have extra columns", {
  to <- helper_tune_results$tune
  x <- as_tibble(to)
  x[["extra"]] <- 1
  expect_true(tune_results_can_reconstruct(x, to))
})

test_that("can't have different number of rows", {
  to <- helper_tune_results$tune
  x <- as_tibble(to)

  a <- vec_slice(x, integer())
  b <- vec_slice(x, c(1, vec_seq_along(x)))

  expect_false(tune_results_can_reconstruct(a, to))
  expect_false(tune_results_can_reconstruct(b, to))
})

test_that("can't rename a column to unknown name", {
  to <- helper_tune_results$tune
  x <- as_tibble(to)

  names <- names(x)
  names[[1]] <- "foo"
  names(x) <- names

  expect_false(tune_results_can_reconstruct(x, to))
})

test_that("swapping column names is caught", {
  to <- helper_tune_results$tune
  x <- as_tibble(to)

  names <- names(x)
  one <- names[[1]]
  names[[1]] <- names[[3]]
  names[[3]] <- one
  names(x) <- names

  expect_false(tune_results_can_reconstruct(x, to))
})

test_that("column type can't change", {
  to <- helper_tune_results$tune
  x <- as_tibble(to)

  x[["id"]] <- 1

  expect_false(tune_results_can_reconstruct(x, to))
})

test_that("row order doesn't matter", {
  to <- helper_tune_results$tune
  x <- as_tibble(to)

  idx <- rev(seq_len(nrow(x)))
  expect_true(tune_results_can_reconstruct(x[idx, ], to))
})

# ------------------------------------------------------------------------------
# resample_results_can_reconstruct()

# The same as `tune_results_can_reconstruct()`, so just make sure it works

test_that("identical results are reconstructable", {
  x <- helper_tune_results$resample
  expect_true(resample_results_can_reconstruct(x, x))
})
