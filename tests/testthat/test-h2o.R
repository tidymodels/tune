library(recipes)
library(parsnip)
library(tailor)

two_row_data <-
  tibble::tribble(
    ~A,
    ~B,
    ~Class,
    2.06,
    1.63,
    "Class1",
    2.01,
    1.03,
    "Class1"
  )

form <- y ~ .
rec <- recipe(Class ~ ., data = two_row_data)
rec_pca <- rec |>
  step_pca(all_numeric_predictors(), num_comp = tune())

lr_spec <- logistic_reg()
glmn_spec <-
  logistic_reg(penalty = tune("lambda"), mixture = tune()) |>
  set_engine("glmnet")
# engine specific args
mlp_spec <-
  mlp(learn_rate = tune(), epochs = tune()) |>
  set_engine("brulee", stop_iter = tune()) |>
  set_mode("classification")
knn_cls_spec <-
  parsnip::nearest_neighbor(neighbors = tune()) |>
  set_mode("classification")

tlr_thresh_half <- tailor() |> adjust_probability_threshold(1 / 5)
tlr_thresh_tune <- tailor() |> adjust_probability_threshold(threshold = tune())

tbl_1r <- structure(
  list(),
  names = character(0),
  row.names = c(NA, -1L),
  class = c("tbl_df", "tbl", "data.frame")
)

add_empty_post_stage <- function(x) {
  n <- nrow(x)
  dplyr::mutate(x, post_stage = purrr::map(1:n, ~tbl_1r))
}

# ------------------------------------------------------------------------------

test_that("h2o grids - formula, simple model, and no postprocessor", {
  wflow <- workflow(form, lr_spec)
  prm <- extract_parameter_set_dials(wflow)

  no_grd <- tibble()
  no_grd_scd <- schedule_grid(no_grd, wflow)
  no_grd_info <- late_stage_grid(no_grd_scd)

  expect_equal(
    no_grd_info,
    tibble::tibble(post_stage = list())
  )

  no_grd_mod_info <- no_grd_info |> dplyr::select(-post_stage)
  expect_equal(no_grd_mod_info, tibble::tibble())
})

# ----------------------------------------------------------------------------

test_that("h2o grids - formula, simple model, and static prob thresh", {
  wflow <- workflow(form, lr_spec, tlr_thresh_half)
  prm <- extract_parameter_set_dials(wflow)

  no_grd <- tibble()
  no_grd_scd <- schedule_grid(no_grd, wflow)
  no_grd_info <- late_stage_grid(no_grd_scd)

  expect_equal(
    no_grd_info,
    tibble::tibble(post_stage = list())
  )

  no_grd_mod_info <- no_grd_info |> dplyr::select(-post_stage)
  expect_equal(no_grd_mod_info, tibble::tibble())
})

# ----------------------------------------------------------------------------

test_that("h2o grids - formula, simple model, and tuned prob thresh", {
  wflow <- workflow(form, lr_spec, tlr_thresh_tune)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  for (pr in 1:nrow(reg_scd)) {
    reg_info <- late_stage_grid(reg_scd |> dplyr::slice(pr))
    expect_equal(
      reg_info,
      reg_scd$model_stage[[1]]$predict_stage[[1]],
      ignore_attr = "row.names"
    )

    reg_mod_info <- reg_info |> dplyr::select(-post_stage)
    expect_equal(
      reg_mod_info, # there may be row names
      tibble::tibble(),
      ignore_attr = "row.names"
    )
  }

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  for (pr in 1:nrow(reg_ish_scd)) {
    reg_ish_info <- late_stage_grid(reg_ish_scd |> dplyr::slice(pr))
    expect_equal(
      reg_ish_info,
      reg_ish_scd$model_stage[[1]]$predict_stage[[1]],
      ignore_attr = "row.names"
    )
    reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
    expect_equal(
      reg_ish_mod_info, # there may be row names
      tibble::tibble(),
      ignore_attr = "row.names"
    )
  }

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  for (pr in 1:nrow(sfd_scd)) {
    sfd_info <- late_stage_grid(sfd_scd |> dplyr::slice(pr))
    expect_equal(
      sfd_info,
      sfd_scd$model_stage[[1]]$predict_stage[[1]],
      ignore_attr = "row.names"
    )

    sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
    expect_equal(
      sfd_mod_info, # there may be row names
      tibble::tibble(),
      ignore_attr = "row.names"
    )
  }
})

# ----------------------------------------------------------------------------

test_that("h2o grids - formula, glmnet, and no postprocessor", {
  wflow <- workflow(form, glmn_spec)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  reg_info <- late_stage_grid(reg_scd)
  expect_equal(
    reg_info,
    reg_grd |> add_empty_post_stage() |> select(mixture, lambda, post_stage)
  )

  reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_mod_info,
    reg_grd |> dplyr::select(mixture, lambda)
  )

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  reg_ish_info <- late_stage_grid(reg_ish_scd)
  expect_equal(
    reg_ish_info,
    reg_ish_grd |> add_empty_post_stage() |> select(mixture, lambda, post_stage)
  )

  reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_ish_mod_info,
    reg_ish_grd |> dplyr::select(mixture, lambda)
  )

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  sfd_info <- late_stage_grid(sfd_scd)
  expect_equal(
    sfd_info,
    sfd_grd |> add_empty_post_stage() |> select(mixture, lambda, post_stage)
  )

  sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  expect_equal(
    sfd_mod_info,
    sfd_grd |> dplyr::select(mixture, lambda)
  )
})

# ----------------------------------------------------------------------------

test_that("h2o grids - formula, glmnet, and static prob thresh", {
  wflow <- workflow(form, glmn_spec, tlr_thresh_half)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  reg_info <- late_stage_grid(reg_scd)
  expect_equal(
    reg_info,
    reg_grd |> add_empty_post_stage() |> select(mixture, lambda, post_stage)
  )

  reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_mod_info,
    reg_grd |> dplyr::select(mixture, lambda)
  )

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  reg_ish_info <- late_stage_grid(reg_ish_scd)
  expect_equal(
    reg_ish_info,
    reg_ish_grd |> add_empty_post_stage() |> select(mixture, lambda, post_stage)
  )

  reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_ish_mod_info,
    reg_ish_grd |> dplyr::select(mixture, lambda)
  )

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  sfd_info <- late_stage_grid(sfd_scd)
  expect_equal(
    sfd_info,
    sfd_grd |> add_empty_post_stage() |> select(mixture, lambda, post_stage)
  )

  sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  expect_equal(
    sfd_mod_info,
    sfd_grd |> dplyr::select(mixture, lambda)
  )
})

# ----------------------------------------------------------------------------

test_that("h2o grids - formula, glmnet, and tuned prob thresh", {
  wflow <- workflow(form, glmn_spec, tlr_thresh_tune)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  reg_info <- late_stage_grid(reg_scd)
  expect_equal(
    reg_info,
    reg_scd$model_stage[[1]] |>
      dplyr::select(-lambda) |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_mod_info,
    reg_scd$model_stage[[1]] |>
      dplyr::select(-lambda) |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  reg_ish_info <- late_stage_grid(reg_ish_scd)
  expect_equal(
    reg_ish_info,
    reg_ish_scd$model_stage[[1]] |>
      dplyr::select(-lambda) |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_ish_mod_info,
    reg_ish_scd$model_stage[[1]] |>
      dplyr::select(-lambda) |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  sfd_info <- late_stage_grid(sfd_scd)
  expect_equal(
    sfd_info,
    sfd_scd$model_stage[[1]] |>
      dplyr::select(-lambda) |>
      tidyr::unnest(cols = predict_stage)
  )

  sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  expect_equal(
    sfd_mod_info,
    sfd_scd$model_stage[[1]] |>
      dplyr::select(-lambda) |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )
})

# ----------------------------------------------------------------------------

test_that("h2o grids - formula, tuned engine param, and no postprocessor", {
  wflow <- workflow(form, mlp_spec)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  reg_info <- late_stage_grid(reg_scd)
  expect_equal(
    reg_info,
    reg_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_mod_info,
    reg_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  reg_ish_info <- late_stage_grid(reg_ish_scd)
  expect_equal(
    reg_ish_info,
    reg_ish_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_ish_mod_info,
    reg_ish_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  sfd_info <- late_stage_grid(sfd_scd)
  expect_equal(
    sfd_info,
    sfd_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage)
  )

  sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  expect_equal(
    sfd_mod_info,
    sfd_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )
})

# ----------------------------------------------------------------------------

test_that("h2o grids - formula, tuned engine param, and static prob thresh", {
  wflow <- workflow(form, mlp_spec, tlr_thresh_half)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  reg_info <- late_stage_grid(reg_scd)
  expect_equal(
    reg_info,
    reg_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_mod_info,
    reg_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  reg_ish_info <- late_stage_grid(reg_ish_scd)
  expect_equal(
    reg_ish_info,
    reg_ish_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_ish_mod_info,
    reg_ish_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  sfd_info <- late_stage_grid(sfd_scd)
  expect_equal(
    sfd_info,
    sfd_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage)
  )

  sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  expect_equal(
    sfd_mod_info,
    sfd_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )
})

# ----------------------------------------------------------------------------

test_that("h2o grids - formula, tuned engine param, and tuned prob thresh", {
  wflow <- workflow(form, mlp_spec, tlr_thresh_tune)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  reg_info <- late_stage_grid(reg_scd)
  expect_equal(
    reg_info,
    reg_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_mod_info,
    reg_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  reg_ish_info <- late_stage_grid(reg_ish_scd)
  expect_equal(
    reg_ish_info,
    reg_ish_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage)
  )

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  sfd_info <- late_stage_grid(sfd_scd)
  expect_equal(
    sfd_info,
    sfd_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage)
  )

  sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  expect_equal(
    sfd_mod_info,
    sfd_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )
})

# ----------------------------------------------------------------------------

test_that("h2o grids - formula, tuned knn, and no postprocessor", {
  wflow <- workflow(form, knn_cls_spec)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  reg_info <- late_stage_grid(reg_scd)
  expect_equal(
    reg_info,
    reg_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_mod_info,
    reg_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  reg_ish_info <- late_stage_grid(reg_ish_scd)
  expect_equal(
    reg_ish_info,
    reg_ish_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_ish_mod_info,
    reg_ish_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  sfd_info <- late_stage_grid(sfd_scd)
  expect_equal(
    sfd_info,
    sfd_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage)
  )

  sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  expect_equal(
    sfd_mod_info,
    sfd_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )
})

# ----------------------------------------------------------------------------

test_that("h2o grids - formula, tuned knn, and static prob thresh", {
  wflow <- workflow(form, knn_cls_spec, tlr_thresh_half)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  reg_info <- late_stage_grid(reg_scd)
  expect_equal(
    reg_info,
    reg_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_mod_info,
    reg_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  reg_ish_info <- late_stage_grid(reg_ish_scd)
  expect_equal(
    reg_ish_info,
    reg_ish_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_ish_mod_info,
    reg_ish_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  sfd_info <- late_stage_grid(sfd_scd)
  expect_equal(
    sfd_info,
    sfd_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage)
  )

  sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  expect_equal(
    sfd_mod_info,
    sfd_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )
})

# ----------------------------------------------------------------------------

test_that("h2o grids - formula, tuned knn, and tuned prob thresh", {
  wflow <- workflow(form, knn_cls_spec, tlr_thresh_tune)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  reg_info <- late_stage_grid(reg_scd)
  expect_equal(
    reg_info,
    reg_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_mod_info,
    reg_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  reg_ish_info <- late_stage_grid(reg_ish_scd)
  expect_equal(
    reg_ish_info,
    reg_ish_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_ish_mod_info,
    reg_ish_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  sfd_info <- late_stage_grid(sfd_scd)
  expect_equal(
    sfd_info,
    sfd_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage)
  )

  sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  expect_equal(
    sfd_mod_info,
    sfd_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )
})

# ----------------------------------------------------------------------------

test_that("h2o grids - recipe, simple model, and no postprocessor", {
  wflow <- workflow(rec, lr_spec)
  prm <- extract_parameter_set_dials(wflow)

  no_grd <- tibble()
  no_grd_scd <- schedule_grid(no_grd, wflow)
  no_grd_info <- late_stage_grid(no_grd_scd)

  expect_equal(
    no_grd_info,
    tibble::tibble(post_stage = list())
  )

  no_grd_mod_info <- no_grd_info |> dplyr::select(-post_stage)
  expect_equal(no_grd_mod_info, tibble::tibble())
})

# ----------------------------------------------------------------------------

test_that("h2o grids - recipe, simple model, and static prob thresh", {
  wflow <- workflow(rec, lr_spec, tlr_thresh_half)
  prm <- extract_parameter_set_dials(wflow)

  no_grd <- tibble()
  no_grd_scd <- schedule_grid(no_grd, wflow)
  no_grd_info <- late_stage_grid(no_grd_scd)

  expect_equal(
    no_grd_info,
    tibble::tibble(post_stage = list())
  )

  no_grd_mod_info <- no_grd_info |> dplyr::select(-post_stage)
  expect_equal(no_grd_mod_info, tibble::tibble())
})

# ----------------------------------------------------------------------------

test_that("h2o grids - recipe, simple model, and tuned prob thresh", {
  wflow <- workflow(rec, lr_spec, tlr_thresh_tune)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  reg_info <- late_stage_grid(reg_scd)
  expect_equal(
    reg_info,
    reg_scd$model_stage[[1]]$predict_stage[[1]]
  )

  reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_mod_info,
    tibble::tibble(),
    ignore_attr = "row.names"
  )

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  reg_ish_info <- late_stage_grid(reg_ish_scd)
  expect_equal(
    reg_ish_info,
    reg_ish_scd$model_stage[[1]]$predict_stage[[1]]
  )

  reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_ish_mod_info,
    tibble::tibble(),
    ignore_attr = "row.names"
  )

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  sfd_info <- late_stage_grid(sfd_scd)
  expect_equal(
    sfd_info,
    sfd_scd$model_stage[[1]]$predict_stage[[1]]
  )

  sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  expect_equal(
    sfd_mod_info,
    tibble::tibble(),
    ignore_attr = "row.names"
  )
})

# ----------------------------------------------------------------------------

test_that("h2o grids - recipe, glmnet, and no postprocessor", {
  wflow <- workflow(rec, glmn_spec)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  reg_info <- late_stage_grid(reg_scd)
  expect_equal(
    reg_info,
    reg_grd |> add_empty_post_stage() |> select(mixture, lambda, post_stage)
  )

  reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_mod_info,
    reg_grd |> dplyr::select(mixture, lambda)
  )

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  reg_ish_info <- late_stage_grid(reg_ish_scd)
  expect_equal(
    reg_ish_info,
    reg_ish_grd |> add_empty_post_stage() |> select(mixture, lambda, post_stage)
  )

  reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_ish_mod_info,
    reg_ish_grd |> dplyr::select(mixture, lambda)
  )

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  sfd_info <- late_stage_grid(sfd_scd)
  expect_equal(
    sfd_info,
    sfd_grd |> add_empty_post_stage() |> select(mixture, lambda, post_stage)
  )

  sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  expect_equal(
    sfd_mod_info,
    sfd_grd |> dplyr::select(mixture, lambda)
  )
})

# ----------------------------------------------------------------------------

test_that("h2o grids - recipe, glmnet, and static prob thresh", {
  wflow <- workflow(rec, glmn_spec, tlr_thresh_half)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  reg_info <- late_stage_grid(reg_scd)
  expect_equal(
    reg_info,
    reg_grd |> add_empty_post_stage() |> select(mixture, lambda, post_stage)
  )

  reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_mod_info,
    reg_grd |> dplyr::select(mixture, lambda)
  )

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  reg_ish_info <- late_stage_grid(reg_ish_scd)
  expect_equal(
    reg_ish_info,
    reg_ish_grd |> add_empty_post_stage() |> select(mixture, lambda, post_stage)
  )

  reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_ish_mod_info,
    reg_ish_grd |> dplyr::select(mixture, lambda)
  )

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  sfd_info <- late_stage_grid(sfd_scd)
  expect_equal(
    sfd_info,
    sfd_grd |> add_empty_post_stage() |> select(mixture, lambda, post_stage)
  )

  sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  expect_equal(
    sfd_mod_info,
    sfd_grd |> dplyr::select(mixture, lambda)
  )
})

# ----------------------------------------------------------------------------

test_that("h2o grids - recipe, glmnet, and tuned prob thresh", {
  wflow <- workflow(rec, glmn_spec, tlr_thresh_tune)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  reg_info <- late_stage_grid(reg_scd)
  expect_equal(
    reg_info,
    reg_scd$model_stage[[1]] |>
      dplyr::select(-lambda) |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_mod_info,
    reg_scd$model_stage[[1]] |>
      dplyr::select(-lambda) |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  reg_ish_info <- late_stage_grid(reg_ish_scd)
  expect_equal(
    reg_ish_info,
    reg_ish_scd$model_stage[[1]] |>
      dplyr::select(-lambda) |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_ish_mod_info,
    reg_ish_scd$model_stage[[1]] |>
      dplyr::select(-lambda) |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  sfd_info <- late_stage_grid(sfd_scd)
  expect_equal(
    sfd_info,
    sfd_scd$model_stage[[1]] |>
      dplyr::select(-lambda) |>
      tidyr::unnest(cols = predict_stage)
  )

  sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  expect_equal(
    sfd_mod_info,
    sfd_scd$model_stage[[1]] |>
      dplyr::select(-lambda) |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )
})

# ----------------------------------------------------------------------------

test_that("h2o grids - recipe, tuned engine param, and no postprocessor", {
  wflow <- workflow(rec, mlp_spec)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  reg_info <- late_stage_grid(reg_scd)
  expect_equal(
    reg_info,
    reg_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_mod_info,
    reg_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  reg_ish_info <- late_stage_grid(reg_ish_scd)
  expect_equal(
    reg_ish_info,
    reg_ish_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_ish_mod_info,
    reg_ish_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  sfd_info <- late_stage_grid(sfd_scd)
  expect_equal(
    sfd_info,
    sfd_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage)
  )

  sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  expect_equal(
    sfd_mod_info,
    sfd_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )
})

# ----------------------------------------------------------------------------

test_that("h2o grids - recipe, tuned engine param, and static prob thresh", {
  wflow <- workflow(rec, mlp_spec, tlr_thresh_half)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  reg_info <- late_stage_grid(reg_scd)
  expect_equal(
    reg_info,
    reg_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_mod_info,
    reg_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  reg_ish_info <- late_stage_grid(reg_ish_scd)
  expect_equal(
    reg_ish_info,
    reg_ish_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_ish_mod_info,
    reg_ish_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  sfd_info <- late_stage_grid(sfd_scd)
  expect_equal(
    sfd_info,
    sfd_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage)
  )

  sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  expect_equal(
    sfd_mod_info,
    sfd_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )
})

# ----------------------------------------------------------------------------

test_that("h2o grids - recipe, tuned engine param, and tuned prob thresh", {
  wflow <- workflow(rec, mlp_spec, tlr_thresh_tune)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  reg_info <- late_stage_grid(reg_scd)
  expect_equal(
    reg_info,
    reg_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_mod_info,
    reg_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  reg_ish_info <- late_stage_grid(reg_ish_scd)
  expect_equal(
    reg_ish_info,
    reg_ish_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage)
  )

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  sfd_info <- late_stage_grid(sfd_scd)
  expect_equal(
    sfd_info,
    sfd_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage)
  )

  sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  expect_equal(
    sfd_mod_info,
    sfd_scd$model_stage[[1]] |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )
})

# ----------------------------------------------------------------------------

test_that("h2o grids - recipe, tuned knn, and no postprocessor", {
  wflow <- workflow(rec, knn_cls_spec)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  reg_info <- late_stage_grid(reg_scd)
  expect_equal(
    reg_info,
    reg_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_mod_info,
    reg_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  reg_ish_info <- late_stage_grid(reg_ish_scd)
  expect_equal(
    reg_ish_info,
    reg_ish_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_ish_mod_info,
    reg_ish_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  sfd_info <- late_stage_grid(sfd_scd)
  expect_equal(
    sfd_info,
    sfd_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage)
  )

  sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  expect_equal(
    sfd_mod_info,
    sfd_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )
})

# ----------------------------------------------------------------------------

test_that("h2o grids - recipe, tuned knn, and static prob thresh", {
  wflow <- workflow(rec, knn_cls_spec, tlr_thresh_half)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  reg_info <- late_stage_grid(reg_scd)
  expect_equal(
    reg_info,
    reg_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_mod_info,
    reg_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  reg_ish_info <- late_stage_grid(reg_ish_scd)
  expect_equal(
    reg_ish_info,
    reg_ish_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_ish_mod_info,
    reg_ish_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  sfd_info <- late_stage_grid(sfd_scd)
  expect_equal(
    sfd_info,
    sfd_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage)
  )

  sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  expect_equal(
    sfd_mod_info,
    sfd_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )
})

# ----------------------------------------------------------------------------

test_that("h2o grids - recipe, tuned knn, and tuned prob thresh", {
  wflow <- workflow(rec, knn_cls_spec, tlr_thresh_tune)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  reg_info <- late_stage_grid(reg_scd)
  expect_equal(
    reg_info,
    reg_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_mod_info,
    reg_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  reg_ish_info <- late_stage_grid(reg_ish_scd)
  expect_equal(
    reg_ish_info,
    reg_ish_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage)
  )

  reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
  expect_equal(
    reg_ish_mod_info,
    reg_ish_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  sfd_info <- late_stage_grid(sfd_scd)
  expect_equal(
    sfd_info,
    sfd_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage)
  )

  sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  expect_equal(
    sfd_mod_info,
    sfd_scd$model_stage[[1]] |>
      dplyr::select(-neighbors) |>
      tidyr::unnest(cols = predict_stage) |>
      dplyr::select(-post_stage)
  )
})

# ----------------------------------------------------------------------------

test_that("h2o grids - tuned recipe, simple model, and no postprocessor", {
  wflow <- workflow(rec_pca, lr_spec)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  for (pr in 1:nrow(reg_scd)) {
    reg_info <- late_stage_grid(reg_scd |> dplyr::slice(pr))
    expect_equal(
      reg_info,
      tibble::tibble(post_stage = list(tbl_1r))
    )

    reg_mod_info <- reg_info |> dplyr::select(-post_stage)
    expect_equal(
      reg_mod_info,
      tbl_1r
    )
  }

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  for (pr in 1:nrow(reg_ish_scd)) {
    reg_ish_info <- late_stage_grid(reg_ish_scd |> dplyr::slice(pr))
    expect_equal(
      reg_ish_info,
      tibble::tibble(post_stage = list(tbl_1r))
    )

    reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
    expect_equal(
      reg_ish_mod_info,
      tbl_1r
    )
  }

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  for (pr in 1:nrow(sfd_scd)) {
    sfd_info <- late_stage_grid(sfd_scd |> dplyr::slice(pr))
    expect_equal(
      sfd_info,
      tibble::tibble(post_stage = list(tbl_1r))
    )

    sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
    expect_equal(
      sfd_mod_info,
      tbl_1r
    )
  }
})

# ----------------------------------------------------------------------------

test_that("h2o grids - tuned recipe, simple model, and static prob thresh", {
  wflow <- workflow(rec_pca, lr_spec, tlr_thresh_half)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  for (pr in 1:nrow(reg_scd)) {
    reg_info <- late_stage_grid(reg_scd |> dplyr::slice(pr))
    expect_equal(
      reg_info,
      tibble::tibble(post_stage = list(tbl_1r))
    )

    reg_mod_info <- reg_info |> dplyr::select(-post_stage)
    expect_equal(
      reg_mod_info,
      tbl_1r
    )
  }

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  for (pr in 1:nrow(reg_ish_scd)) {
    reg_ish_info <- late_stage_grid(reg_ish_scd |> dplyr::slice(pr))
    expect_equal(
      reg_ish_info,
      tibble::tibble(post_stage = list(tbl_1r))
    )

    reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
    expect_equal(
      reg_ish_mod_info,
      tbl_1r
    )
  }

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  for (pr in 1:nrow(sfd_scd)) {
    sfd_info <- late_stage_grid(sfd_scd |> dplyr::slice(pr))
    expect_equal(
      sfd_info,
      tibble::tibble(post_stage = list(tbl_1r))
    )

    sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
    expect_equal(
      sfd_mod_info,
      tbl_1r
    )
  }
})

# ----------------------------------------------------------------------------

test_that("h2o grids - tuned recipe, simple model, and tuned prob thresh", {
  wflow <- workflow(rec_pca, lr_spec, tlr_thresh_tune)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  for (pr in 1:nrow(reg_scd)) {
    reg_info <- late_stage_grid(reg_scd |> dplyr::slice(pr))
    expect_equal(
      reg_info,
      reg_scd$model_stage[[pr]]$predict_stage[[1]]
    )

    reg_mod_info <- reg_info |> dplyr::select(-post_stage)
    expect_equal(
      reg_mod_info,
      tbl_1r
    )
  }

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  for (pr in 1:nrow(reg_ish_scd)) {
    reg_ish_info <- late_stage_grid(reg_ish_scd |> dplyr::slice(pr))
    expect_equal(
      reg_ish_info,
      reg_ish_scd$model_stage[[pr]]$predict_stage[[1]]
    )

    reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
    expect_equal(
      reg_ish_mod_info,
      tbl_1r
    )
  }

  ### space-filling design row

  sfd_grd <- grid_regular(prm, levels = 3)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  for (pr in 1:nrow(sfd_scd)) {
    sfd_info <- late_stage_grid(sfd_scd |> dplyr::slice(pr))
    expect_equal(
      sfd_info,
      sfd_scd$model_stage[[pr]]$predict_stage[[1]]
    )

    sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
    expect_equal(
      sfd_mod_info,
      tbl_1r
    )
  }
})

# ----------------------------------------------------------------------------

test_that("h2o grids - tuned recipe, glmnet, and no postprocessor", {
  wflow <- workflow(rec_pca, glmn_spec)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  for (pr in 1:nrow(reg_scd)) {
    reg_info <- reg_scd |> dplyr::slice(pr) |> late_stage_grid()
    expect_equal(
      reg_info,
      reg_scd$model_stage[[pr]] |>
        select(-lambda) |>
        unnest(predict_stage)
    )
  }

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  for (pr in 1:nrow(reg_ish_scd)) {
    reg_ish_info <- reg_ish_scd |> dplyr::slice(pr) |> late_stage_grid()
    expect_equal(
      reg_ish_info,
      reg_ish_scd$model_stage[[pr]] |>
        select(-lambda) |>
        unnest(predict_stage)
    )
  }

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  for (pr in 1:nrow(sfd_scd)) {
    sfd_info <- sfd_scd |> dplyr::slice(pr) |> late_stage_grid()
    expect_equal(
      sfd_info,
      sfd_scd$model_stage[[pr]] |>
        select(-lambda) |>
        unnest(predict_stage)
    )
  }
})

# ----------------------------------------------------------------------------

test_that("h2o grids - tuned recipe, glmnet, and static prob thresh", {
  wflow <- workflow(rec_pca, glmn_spec, tlr_thresh_half)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  for (pr in 1:nrow(reg_scd)) {
    reg_info <- reg_scd |> dplyr::slice(pr) |> late_stage_grid()
    expect_equal(
      reg_info,
      reg_scd$model_stage[[pr]] |>
        select(-lambda) |>
        unnest(predict_stage)
    )
  }

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  for (pr in 1:nrow(reg_ish_scd)) {
    reg_ish_info <- reg_ish_scd |> dplyr::slice(pr) |> late_stage_grid()
    expect_equal(
      reg_ish_info,
      reg_ish_scd$model_stage[[pr]] |>
        select(-lambda) |>
        unnest(predict_stage)
    )
  }

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  for (pr in 1:nrow(sfd_scd)) {
    sfd_info <- sfd_scd |> dplyr::slice(pr) |> late_stage_grid()
    expect_equal(
      sfd_info,
      sfd_scd$model_stage[[pr]] |>
        select(-lambda) |>
        unnest(predict_stage)
    )
  }
})

# ----------------------------------------------------------------------------

test_that("h2o grids - tuned recipe, glmnet, and tuned prob thresh", {
  wflow <- workflow(rec_pca, glmn_spec, tlr_thresh_tune)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  for (pr in 1:nrow(reg_scd)) {
    reg_info <- late_stage_grid(reg_scd |> dplyr::slice(pr))

    reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  }

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  for (pr in 1:nrow(reg_ish_scd)) {
    reg_ish_info <- late_stage_grid(reg_ish_scd |> dplyr::slice(pr))

    reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
  }

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  for (pr in 1:nrow(sfd_scd)) {
    sfd_info <- late_stage_grid(sfd_scd |> dplyr::slice(pr))

    sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  }
})

# ----------------------------------------------------------------------------

test_that("h2o grids - tuned recipe, tuned engine param, and no postprocessor", {
  wflow <- workflow(rec_pca, mlp_spec)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  for (pr in 1:nrow(reg_scd)) {
    reg_info <- late_stage_grid(reg_scd |> dplyr::slice(pr))

    reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  }

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  for (pr in 1:nrow(reg_ish_scd)) {
    reg_ish_info <- late_stage_grid(reg_ish_scd |> dplyr::slice(pr))

    reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
  }

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  for (pr in 1:nrow(sfd_scd)) {
    sfd_info <- late_stage_grid(sfd_scd |> dplyr::slice(pr))

    sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  }
})

# ----------------------------------------------------------------------------

test_that("h2o grids - tuned recipe, tuned engine param, and static prob thresh", {
  wflow <- workflow(rec_pca, mlp_spec, tlr_thresh_half)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  for (pr in 1:nrow(reg_scd)) {
    reg_info <- late_stage_grid(reg_scd |> dplyr::slice(pr))

    reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  }

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  for (pr in 1:nrow(reg_ish_scd)) {
    reg_ish_info <- late_stage_grid(reg_ish_scd |> dplyr::slice(pr))

    reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
  }

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  for (pr in 1:nrow(sfd_scd)) {
    sfd_info <- late_stage_grid(sfd_scd |> dplyr::slice(pr))

    sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  }
})

# ----------------------------------------------------------------------------

test_that("h2o grids - tuned recipe, tuned engine param, and tuned prob thresh", {
  wflow <- workflow(rec_pca, mlp_spec, tlr_thresh_tune)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  for (pr in 1:nrow(reg_scd)) {
    reg_info <- late_stage_grid(reg_scd |> dplyr::slice(pr))

    reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  }

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  for (pr in 1:nrow(reg_ish_scd)) {
    reg_ish_info <- late_stage_grid(reg_ish_scd |> dplyr::slice(pr))

    reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
  }

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  for (pr in 1:nrow(sfd_scd)) {
    sfd_info <- late_stage_grid(sfd_scd |> dplyr::slice(pr))

    sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  }
})

# ----------------------------------------------------------------------------

test_that("h2o grids - tuned recipe, tuned knn, and no postprocessor", {
  wflow <- workflow(rec_pca, knn_cls_spec)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  for (pr in 1:nrow(reg_scd)) {
    reg_info <- late_stage_grid(reg_scd |> dplyr::slice(pr))

    reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  }

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  for (pr in 1:nrow(reg_ish_scd)) {
    reg_ish_info <- late_stage_grid(reg_ish_scd |> dplyr::slice(pr))

    reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
  }

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  for (pr in 1:nrow(sfd_scd)) {
    sfd_info <- late_stage_grid(sfd_scd |> dplyr::slice(pr))

    sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  }
})

# ----------------------------------------------------------------------------

test_that("h2o grids - tuned recipe, tuned knn, and static prob thresh", {
  wflow <- workflow(rec_pca, knn_cls_spec, tlr_thresh_half)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  for (pr in 1:nrow(reg_scd)) {
    reg_info <- late_stage_grid(reg_scd |> dplyr::slice(pr))

    reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  }

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  for (pr in 1:nrow(reg_ish_scd)) {
    reg_ish_info <- late_stage_grid(reg_ish_scd |> dplyr::slice(pr))

    reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
  }

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  for (pr in 1:nrow(sfd_scd)) {
    sfd_info <- late_stage_grid(sfd_scd |> dplyr::slice(pr))

    sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  }
})

# ----------------------------------------------------------------------------

test_that("h2o grids - tuned recipe, tuned knn, and tuned prob thresh", {
  wflow <- workflow(rec_pca, knn_cls_spec, tlr_thresh_tune)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  for (pr in 1:nrow(reg_scd)) {
    reg_info <- late_stage_grid(reg_scd |> dplyr::slice(pr))

    reg_mod_info <- reg_info |> dplyr::select(-post_stage)
  }

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  for (pr in 1:nrow(reg_ish_scd)) {
    reg_ish_info <- late_stage_grid(reg_ish_scd |> dplyr::slice(pr))

    reg_ish_mod_info <- reg_ish_info |> dplyr::select(-post_stage)
  }

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  for (pr in 1:nrow(sfd_scd)) {
    sfd_info <- late_stage_grid(sfd_scd |> dplyr::slice(pr))

    sfd_mod_info <- sfd_info |> dplyr::select(-post_stage)
  }
})
