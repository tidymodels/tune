load(test_path("data", "test_objects.RData"))

# ------------------------------------------------------------------------------

test_that('tune recipe only', {
  helper_objects <- helper_objects_tune()
  set.seed(363)
  mt_folds <- rsample::vfold_cv(mtcars, v = 5)

  extr_1_1 <- function(x) {
    extract_recipe(x) %>% tidy(number = 2)
  }
  before_kind <- RNGkind()[[1]]
  expect_error(
    res_1_1 <-
      workflow() %>%
      add_recipe(helper_objects$rec_tune_1) %>%
      add_model(helper_objects$lm_mod) %>%
      tune_grid(resamples = mt_folds, control = control_grid(extract = extr_1_1)),
    NA
  )
  after_kind <- RNGkind()[[1]]
  expect_equal(before_kind, after_kind)
  expect_error(extract_1_1 <- dplyr::bind_rows(res_1_1$.extracts), NA)

  expect_true(all(names(extract_1_1) == c("num_comp", ".extracts", ".config")))
  expect_true(
    all(purrr::map_lgl(extract_1_1$.extracts, ~ tibble::is_tibble(.x))),
  )

})

# ------------------------------------------------------------------------------

test_that('tune model only', {
  helper_objects <- helper_objects_tune()
  set.seed(363)
  mt_folds <- rsample::vfold_cv(mtcars, v = 5)

  extr_2_1 <- function(x) {
    mod <- extract_fit_engine(x)
    tibble(index = mod@alphaindex[[1]], estimate = mod@coef[[1]])
  }

  expect_error(
    res_2_1 <-
      workflow() %>%
      add_recipe(helper_objects$rec_no_tune_1) %>%
      add_model(helper_objects$svm_mod) %>%
      tune_grid(
        resamples = mt_folds,
        grid = 2,
        control = control_grid(extract = extr_2_1
        )
      ),
    NA
  )
  expect_error(extract_2_1 <- dplyr::bind_rows(res_2_1$.extracts), NA)

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
  expect_error(
    res_2_2 <-
      workflow() %>%
      add_recipe(helper_objects$rec_tune_1) %>%
      add_model(helper_objects$lm_mod) %>%
      tune_grid(
        resamples = mt_folds,
        grid = 2,
        control = control_grid(extract = extr_2_2)
      ),
    NA
  )

  expect_error(
    extract_2_2 <-
      dplyr::bind_rows(res_2_2$.extracts) %>%
      tidyr::unnest(cols = c(.extracts)),
    NA
  )
  expect_true(all(!extract_2_2$is_null_rec))

})

# ------------------------------------------------------------------------------

test_that('tune model and recipe', {
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
    dials::grid_latin_hypercube(size = 4)

  expect_error(
    res_3_1 <- tune_grid(wflow_3, resamples = mt_folds, grid = grid_3,
                         control = control_grid(extract = extr_3_1)),
    NA
  )
  expect_error(extract_3_1 <- dplyr::bind_rows(res_3_1$.extracts), NA)

  expect_true(all(names(extract_3_1) == c("num_comp", "cost", ".extracts", ".config")))
  expect_true(
    all(purrr::map_lgl(extract_3_1$.extracts, ~ inherits(.x, "workflow"))),
  )

})

# ------------------------------------------------------------------------------


test_that('check .config in extracts', {

  # recipe only
  for (i in 1:nrow(mt_spln_lm_grid)) {
    expect_true(any(names(mt_spln_lm_grid$.extracts[[i]]) == ".config"))
  }

  for (i in 1:nrow(mt_spln_lm_bo)) {
    expect_true(any(names(mt_spln_lm_bo$.extracts[[i]]) == ".config"))
  }

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

