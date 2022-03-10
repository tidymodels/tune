test_that("recipe only", {
  load(test_path("data", "test_objects.RData"))
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
  load(test_path("data", "test_objects.RData"))
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
  load(test_path("data", "test_objects.RData"))
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
