check_predictions <- function(split, pred, tune_df) {

  assess <- rsample::assessment(split)
  n_te <- nrow(assess)
  n_pm <- nrow(tune_df)
  ind_te <- as.integer(split, data = "assessment")
  expect_true(tibble::is_tibble(pred))
  expect_equal(nrow(pred), n_te * n_pm)
  exp_nms <- c(".pred", ".row", names(tune_df), "mpg", ".config")
  expect_equal(names(pred), exp_nms)
  expect_equal(sort(unique(ind_te)), sort(unique(pred$.row)))
  TRUE
}

load(test_path("test_objects.RData"))

# ------------------------------------------------------------------------------

test_that("recipe only", {

  grid <- collect_metrics(mt_spln_lm_grid) %>%
    dplyr::select(deg_free) %>%
    dplyr::distinct()

  purrr::map2(
    mt_spln_lm_grid$splits,
    mt_spln_lm_grid$.predictions,
    check_predictions,
    grid
  )

  # initial values for Bayes opt
  init <- mt_spln_lm_bo %>% dplyr::filter(.iter == 0)
  init_grid <-
    collect_metrics(mt_spln_lm_bo) %>%
    dplyr::filter(.iter == 0) %>%
    dplyr::select(deg_free) %>%
    dplyr::distinct()

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

  grid <-
    collect_metrics(mt_knn_grid) %>%
    dplyr::select(neighbors) %>%
    dplyr::distinct()

  purrr::map2(
    mt_knn_grid$splits,
    mt_knn_grid$.predictions,
    check_predictions,
    grid
  )

  # initial values for Bayes opt
  init <- mt_knn_bo %>% dplyr::filter(.iter == 0)
  init_grid <-
    collect_metrics(mt_knn_bo) %>%
    dplyr::filter(.iter == 0) %>%
    dplyr::select(neighbors) %>%
    distinct()

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
    collect_metrics(mt_spln_knn_grid) %>%
    dplyr::select(deg_free, neighbors) %>%
    dplyr::distinct()

  purrr::map2(
    mt_spln_knn_grid$splits,
    mt_spln_knn_grid$.predictions,
    check_predictions,
    grid
  )

  # initial values for Bayes opt
  init <- mt_spln_knn_bo %>% dplyr::filter(.iter == 0)
  init_grid <-
    collect_metrics(mt_spln_knn_bo) %>%
    dplyr::filter(.iter == 0) %>%
    dplyr::select(deg_free, neighbors) %>%
    dplyr::distinct()

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
