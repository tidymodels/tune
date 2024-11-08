
test_that("tune recipe only", {
  skip_if_not_installed("kernlab")

  helper_objects <- helper_objects_tune()
  set.seed(363)
  mt_folds <- rsample::vfold_cv(mtcars, v = 5)

  extr_1_1 <- function(x) {
    extract_recipe(x) %>% tidy(number = 2)
  }
  before_kind <- RNGkind()[[1]]
  expect_no_error(
    res_1_1 <-
      workflow() %>%
      add_recipe(helper_objects$rec_tune_1) %>%
      add_model(helper_objects$lm_mod) %>%
      tune_grid(resamples = mt_folds, control = control_grid(extract = extr_1_1))
  )
  after_kind <- RNGkind()[[1]]
  expect_equal(before_kind, after_kind)
  expect_no_error(extract_1_1 <- dplyr::bind_rows(res_1_1$.extracts))

  expect_true(all(names(extract_1_1) == c("num_comp", ".extracts", ".config")))
  expect_true(
    all(purrr::map_lgl(extract_1_1$.extracts, ~ tibble::is_tibble(.x))),
  )
})

# ------------------------------------------------------------------------------

test_that("tune model only", {
  skip_if_not_installed("kernlab")

  helper_objects <- helper_objects_tune()
  set.seed(363)
  mt_folds <- rsample::vfold_cv(mtcars, v = 5)

  extr_2_1 <- function(x) {
    mod <- extract_fit_engine(x)
    tibble(index = mod@alphaindex[[1]], estimate = mod@coef[[1]])
  }

  expect_no_error(
    res_2_1 <-
      workflow() %>%
      add_recipe(helper_objects$rec_no_tune_1) %>%
      add_model(helper_objects$svm_mod) %>%
      tune_grid(
        resamples = mt_folds,
        grid = 2,
        control = control_grid(extract = extr_2_1)
      )
  )
  expect_no_error(extract_2_1 <- dplyr::bind_rows(res_2_1$.extracts))

  expect_true(all(names(extract_2_1) == c("cost", ".extracts", ".config")))
  expect_true(
    all(purrr::map_lgl(extract_2_1$.extracts, ~ tibble::is_tibble(.x))),
  )
  expect_true(
    all(purrr::map_lgl(extract_2_1$.extracts, ~ all(names(.x) == c("index", "estimate")))),
  )

  extr_2_2 <- function(x) {
    tibble(is_null_rec = is.null(extract_recipe(x)))
  }

  # should not fail:
  expect_no_error(
    res_2_2 <-
      workflow() %>%
      add_recipe(helper_objects$rec_tune_1) %>%
      add_model(helper_objects$lm_mod) %>%
      tune_grid(
        resamples = mt_folds,
        grid = 2,
        control = control_grid(extract = extr_2_2)
      )
  )

  expect_no_error(
    extract_2_2 <-
      dplyr::bind_rows(res_2_2$.extracts) %>%
      tidyr::unnest(cols = c(.extracts))
  )
  expect_true(all(!extract_2_2$is_null_rec))
})

test_that("mis-specified extract function", {
  wf <-
    workflows::workflow(
      preprocessor = mpg ~ .,
      spec = parsnip::linear_reg()
    )

  set.seed(1)
  boots <- rsample::bootstraps(mtcars, 3)

  raise_warning <- function(x) {warning("AHHH"); TRUE}
  raise_error <- function(x) {stop("AHHH"); TRUE}
  raise_both <- function(x) {warning("AH"); stop("AHHH"); TRUE}
  raise_error_once <- local({
    first <- TRUE

    function(x) {
      if (first) {
        first <<- FALSE
        stop("oh no")
      }

      "hi"
    }
  })

  expect_snapshot(
    res_extract_warning <-
      fit_resamples(wf, boots, control = control_resamples(extract = raise_warning))
  )

  expect_snapshot(
    res_extract_error <-
      fit_resamples(wf, boots, control = control_resamples(extract = raise_error))
  )

  expect_snapshot(
    res_extract_both <-
      fit_resamples(wf, boots, control = control_resamples(extract = raise_both))
  )

  expect_snapshot(
    res_extract_error_once <-
      fit_resamples(wf, boots, control = control_resamples(extract = raise_error_once))
  )

  expect_snapshot(res_extract_warning)
  expect_snapshot(res_extract_error)
  expect_snapshot(res_extract_both)
  expect_snapshot(res_extract_error_once)

  expect_true(res_extract_warning$.extracts[[1]]$.extracts[[1]])
  expect_snapshot(res_extract_warning$.notes[[1]])

  expect_snapshot(res_extract_error$.extracts[[1]]$.extracts[[1]])
  expect_snapshot(res_extract_error$.notes[[1]])

  expect_snapshot(res_extract_both$.extracts[[1]]$.extracts[[1]])

  expect_snapshot(res_extract_error_once$.extracts[[1]]$.extracts[[1]])
  expect_equal(res_extract_error_once$.extracts[[2]]$.extracts[[1]], "hi")
  expect_equal(res_extract_error_once$.extracts[[3]]$.extracts[[1]], "hi")
})

# ------------------------------------------------------------------------------

test_that("tune model and recipe", {
  skip_if_not_installed("kernlab")

  helper_objects <- helper_objects_tune()
  set.seed(363)
  mt_folds <- rsample::vfold_cv(mtcars, v = 5)

  extr_3_1 <- function(x) {
    x
  }

  wflow_3 <-
    workflow() %>%
    add_recipe(helper_objects$rec_tune_1) %>%
    add_model(helper_objects$svm_mod)
  set.seed(35)
  grid_3 <-
    extract_parameter_set_dials(wflow_3) %>%
    update(num_comp = dials::num_comp(c(2, 5))) %>%
    dials::grid_space_filling(size = 4)

  expect_no_error(
    res_3_1 <- tune_grid(
      wflow_3,
      resamples = mt_folds,
      grid = grid_3,
      control = control_grid(extract = extr_3_1)
    )
  )
  expect_no_error(extract_3_1 <- dplyr::bind_rows(res_3_1$.extracts))

  expect_true(all(names(extract_3_1) == c("num_comp", "cost", ".extracts", ".config")))
  expect_true(
    all(purrr::map_lgl(extract_3_1$.extracts, ~ inherits(.x, "workflow"))),
  )
})

# ------------------------------------------------------------------------------


test_that("check .config in extracts", {
  load(test_path("data", "test_objects.RData"))

  # recipe only
  for (i in 1:nrow(mt_spln_lm_grid)) {
    expect_true(any(names(mt_spln_lm_grid$.extracts[[i]]) == ".config"))
  }

  for (i in 1:nrow(mt_spln_lm_bo)) {
    expect_true(any(names(mt_spln_lm_bo$.extracts[[i]]) == ".config"))
  }

  recipe_only_configs <-
    full_join(
      mt_spln_lm_bo %>%
        filter(id == first(id)) %>%
        select(.iter, .metrics) %>%
        unnest(cols = .metrics) %>%
        filter(.metric == first(.metric)),
      mt_spln_lm_bo %>%
        filter(id == first(id)) %>%
        select(.iter, .extracts) %>%
        unnest(cols = .extracts),
      by = c(".iter", "deg_free")
    )

  expect_equal(
    recipe_only_configs$.config.x,
    recipe_only_configs$.config.y
  )

  # recipe and model
  for (i in 1:nrow(mt_spln_knn_grid)) {
    expect_true(any(names(mt_spln_knn_grid$.extracts[[i]]) == ".config"))
  }

  for (i in 1:nrow(mt_spln_knn_bo)) {
    expect_true(any(names(mt_spln_knn_bo$.extracts[[i]]) == ".config"))
  }

  # model only
  for (i in 1:nrow(mt_knn_grid)) {
    expect_true(any(names(mt_knn_grid$.extracts[[i]]) == ".config"))
  }

  for (i in 1:nrow(mt_knn_bo)) {
    expect_true(any(names(mt_knn_bo$.extracts[[i]]) == ".config"))
  }
})
