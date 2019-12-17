context("extracting predictions")

library(purrr)
library(tibble)
library(rsample)

check_predictions <- function(split, pred, tune_df) {
  assess <- rsample::assessment(split)
  n_te <- nrow(assess)
  n_pm <- nrow(tune_df)
  ind_te <- as.integer(split, data = "assessment")
  expect_true(tibble::is_tibble(pred))
  expect_equal(nrow(pred), n_te * n_pm)
  exp_nms <- c(".pred", ".row", names(tune_df), "mpg")
  expect_equal(names(pred), exp_nms)
  expect_equal(sort(unique(ind_te)), sort(unique(pred$.row)))
  TRUE
}

load(test_path("test_objects.RData"))

# ------------------------------------------------------------------------------

test_that("recipe only", {
  grid <- tibble(deg_free = c(4, 5, 7, 8, 9, 11, 12, 13, 15))

  purrr::map2(
    mt_spln_lm_grid$splits,
    mt_spln_lm_grid$.predictions,
    check_predictions,
    grid
  )

  # initial values for Bayes opt
  init <- mt_spln_lm_bo %>% dplyr::filter(.iter == 0)
  init_grid <- tibble(deg_free = c(3, 10, 14))

  purrr::map2(
    init$splits,
    init$.predictions,
    check_predictions,
    init_grid
  )

  # Now search iterations with a dummy grid
  bo <- mt_spln_lm_bo %>% dplyr::filter(.iter > 0)
  bo_grid <- init_grid %>% dplyr::slice(1)

  purrr::map2(
    bo$splits,
    bo$.predictions,
    check_predictions,
    bo_grid
  )

})

# ------------------------------------------------------------------------------

test_that("model only", {
  grid <- tibble(neighbors = c(2, 3, 5, 6, 7, 8, 10, 12, 13, 15))

  purrr::map2(
    mt_knn_grid$splits,
    mt_knn_grid$.predictions,
    check_predictions,
    grid
  )

  # initial values for Bayes opt
  init <- mt_knn_bo %>% dplyr::filter(.iter == 0)
  init_grid <- tibble(neighbors = c(1, 9, 14))

  purrr::map2(
    init$splits,
    init$.predictions,
    check_predictions,
    init_grid
  )

  # Now search iterations with a dummy grid
  bo <- mt_knn_bo %>% dplyr::filter(.iter > 0)
  bo_grid <- init_grid %>% dplyr::slice(1)

  purrr::map2(
    bo$splits,
    bo$.predictions,
    check_predictions,
    bo_grid
  )

})


# ------------------------------------------------------------------------------

test_that("model and recipe", {
  grid <-
    tibble::tribble(
      ~deg_free, ~neighbors,
      3L,         1L,
      3L,         8L,
      3L,        15L,
      9L,         1L,
      9L,         8L,
      9L,        15L,
      15L,         1L,
      15L,         8L,
      15L,        15L
    )

  purrr::map2(
    mt_spln_knn_grid$splits,
    mt_spln_knn_grid$.predictions,
    check_predictions,
    grid
  )

  # initial values for Bayes opt
  init <- mt_spln_knn_bo %>% dplyr::filter(.iter == 0)
  init_grid <-
    tibble::tribble(
      ~deg_free, ~neighbors,
      8L,        14L,
      11L,         5L,
      4L,         6L
    )

  purrr::map2(
    init$splits,
    init$.predictions,
    check_predictions,
    init_grid
  )

  # Now search iterations with a dummy grid
  bo <- mt_spln_knn_bo %>% dplyr::filter(.iter > 0)
  bo_grid <- init_grid %>% dplyr::slice(1)

  purrr::map2(
    bo$splits,
    bo$.predictions,
    check_predictions,
    bo_grid
  )

})
