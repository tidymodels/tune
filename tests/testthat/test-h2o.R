test_that("h2o grids - formula, simple model, and no postprocessor", {
  wflow <- workflow(form, lr_spec)
  prm <- extract_parameter_set_dials(wflow)

  no_grd <- tibble::tibble()
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

  no_grd <- tibble::tibble()
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
  }
})

# ----------------------------------------------------------------------------

test_that("h2o grids - formula, glmnet, and no postprocessor", {
  wflow <- workflow(form, glmn_lambda_spec)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  reg_info <- late_stage_grid(reg_scd)
  expect_equal(
    reg_info,
    reg_grd |> add_empty_post_stage() |> select(mixture, lambda, post_stage)
  )

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  reg_ish_info <- late_stage_grid(reg_ish_scd)
  expect_equal(
    reg_ish_info,
    reg_ish_grd |> add_empty_post_stage() |> select(mixture, lambda, post_stage)
  )

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  sfd_info <- late_stage_grid(sfd_scd)
  expect_equal(
    sfd_info,
    sfd_grd |> add_empty_post_stage() |> select(mixture, lambda, post_stage)
  )
})

# ----------------------------------------------------------------------------

test_that("h2o grids - formula, glmnet, and static prob thresh", {
  wflow <- workflow(form, glmn_lambda_spec, tlr_thresh_half)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  reg_info <- late_stage_grid(reg_scd)
  expect_equal(
    reg_info,
    reg_grd |> add_empty_post_stage() |> select(mixture, lambda, post_stage)
  )

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  reg_ish_info <- late_stage_grid(reg_ish_scd)
  expect_equal(
    reg_ish_info,
    reg_ish_grd |> add_empty_post_stage() |> select(mixture, lambda, post_stage)
  )

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  sfd_info <- late_stage_grid(sfd_scd)
  expect_equal(
    sfd_info,
    sfd_grd |> add_empty_post_stage() |> select(mixture, lambda, post_stage)
  )
})

# ----------------------------------------------------------------------------

test_that("h2o grids - formula, glmnet, and tuned prob thresh", {
  wflow <- workflow(form, glmn_lambda_spec, tlr_thresh_tune)
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
})

# ----------------------------------------------------------------------------

test_that("h2o grids - formula, tuned knn, and no postprocessor", {
  wflow <- workflow(form, knn_submod_spec)
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
})

# ----------------------------------------------------------------------------

test_that("h2o grids - formula, tuned knn, and static prob thresh", {
  wflow <- workflow(form, knn_submod_spec, tlr_thresh_half)
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
})

# ----------------------------------------------------------------------------

test_that("h2o grids - formula, tuned knn, and tuned prob thresh", {
  wflow <- workflow(form, knn_submod_spec, tlr_thresh_tune)
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
})

# ----------------------------------------------------------------------------

test_that("h2o grids - recipe, simple model, and no postprocessor", {
  wflow <- workflow(rec, lr_spec)
  prm <- extract_parameter_set_dials(wflow)

  no_grd <- tibble::tibble()
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

  no_grd <- tibble::tibble()
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

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  reg_ish_info <- late_stage_grid(reg_ish_scd)
  expect_equal(
    reg_ish_info,
    reg_ish_scd$model_stage[[1]]$predict_stage[[1]]
  )

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  sfd_info <- late_stage_grid(sfd_scd)
  expect_equal(
    sfd_info,
    sfd_scd$model_stage[[1]]$predict_stage[[1]]
  )
})

# ----------------------------------------------------------------------------

test_that("h2o grids - recipe, glmnet, and no postprocessor", {
  wflow <- workflow(rec, glmn_lambda_spec)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  reg_info <- late_stage_grid(reg_scd)
  expect_equal(
    reg_info,
    reg_grd |> add_empty_post_stage() |> select(mixture, lambda, post_stage)
  )

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  reg_ish_info <- late_stage_grid(reg_ish_scd)
  expect_equal(
    reg_ish_info,
    reg_ish_grd |> add_empty_post_stage() |> select(mixture, lambda, post_stage)
  )

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  sfd_info <- late_stage_grid(sfd_scd)
  expect_equal(
    sfd_info,
    sfd_grd |> add_empty_post_stage() |> select(mixture, lambda, post_stage)
  )
})

# ----------------------------------------------------------------------------

test_that("h2o grids - recipe, glmnet, and static prob thresh", {
  wflow <- workflow(rec, glmn_lambda_spec, tlr_thresh_half)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  reg_info <- late_stage_grid(reg_scd)
  expect_equal(
    reg_info,
    reg_grd |> add_empty_post_stage() |> select(mixture, lambda, post_stage)
  )

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  reg_ish_info <- late_stage_grid(reg_ish_scd)
  expect_equal(
    reg_ish_info,
    reg_ish_grd |> add_empty_post_stage() |> select(mixture, lambda, post_stage)
  )

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  sfd_info <- late_stage_grid(sfd_scd)
  expect_equal(
    sfd_info,
    sfd_grd |> add_empty_post_stage() |> select(mixture, lambda, post_stage)
  )
})

# ----------------------------------------------------------------------------

test_that("h2o grids - recipe, glmnet, and tuned prob thresh", {
  wflow <- workflow(rec, glmn_lambda_spec, tlr_thresh_tune)
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
})

# ----------------------------------------------------------------------------

test_that("h2o grids - recipe, tuned knn, and no postprocessor", {
  wflow <- workflow(rec, knn_submod_spec)
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
})

# ----------------------------------------------------------------------------

test_that("h2o grids - recipe, tuned knn, and static prob thresh", {
  wflow <- workflow(rec, knn_submod_spec, tlr_thresh_half)
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
})

# ----------------------------------------------------------------------------

test_that("h2o grids - recipe, tuned knn, and tuned prob thresh", {
  wflow <- workflow(rec, knn_submod_spec, tlr_thresh_tune)
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
  }
})

# ----------------------------------------------------------------------------

test_that("h2o grids - tuned recipe, glmnet, and no postprocessor", {
  wflow <- workflow(rec_pca, glmn_lambda_spec)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  for (pr in 1:nrow(reg_scd)) {
    reg_info <- reg_scd |> dplyr::slice(pr) |> late_stage_grid()
    expect_equal(
      reg_info,
      reg_scd$model_stage[[pr]] |>
        dplyr::select(-lambda) |>
        tidyr::unnest(predict_stage)
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
        dplyr::select(-lambda) |>
        tidyr::unnest(predict_stage)
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
        dplyr::select(-lambda) |>
        tidyr::unnest(predict_stage)
    )
  }
})

# ----------------------------------------------------------------------------

test_that("h2o grids - tuned recipe, glmnet, and static prob thresh", {
  wflow <- workflow(rec_pca, glmn_lambda_spec, tlr_thresh_half)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  for (pr in 1:nrow(reg_scd)) {
    reg_info <- reg_scd |> dplyr::slice(pr) |> late_stage_grid()
    expect_equal(
      reg_info,
      reg_scd$model_stage[[pr]] |>
        dplyr::select(-lambda) |>
        tidyr::unnest(predict_stage)
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
        dplyr::select(-lambda) |>
        tidyr::unnest(predict_stage)
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
        dplyr::select(-lambda) |>
        tidyr::unnest(predict_stage)
    )
  }
})

# ----------------------------------------------------------------------------

test_that("h2o grids - tuned recipe, glmnet, and tuned prob thresh", {
  wflow <- workflow(rec_pca, glmn_lambda_spec, tlr_thresh_tune)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  for (pr in 1:nrow(reg_scd)) {
    reg_info <- late_stage_grid(reg_scd |> dplyr::slice(pr))
    expect_equal(
      reg_info,
      reg_scd$model_stage[[pr]] |>
        dplyr::select(-lambda) |>
        tidyr::unnest(predict_stage)
    )
  }

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  for (pr in 1:nrow(reg_ish_scd)) {
    reg_ish_info <- late_stage_grid(reg_ish_scd |> dplyr::slice(pr))
  }

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  for (pr in 1:nrow(sfd_scd)) {
    sfd_info <- late_stage_grid(sfd_scd |> dplyr::slice(pr))
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
    expect_equal(
      reg_info,
      reg_scd$model_stage[[pr]] |>
        tidyr::unnest(predict_stage)
    )
  }

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  for (pr in 1:nrow(reg_ish_scd)) {
    reg_ish_info <- late_stage_grid(reg_ish_scd |> dplyr::slice(pr))
    expect_equal(
      reg_ish_info,
      reg_ish_scd$model_stage[[pr]] |>
        tidyr::unnest(predict_stage)
    )
  }

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  for (pr in 1:nrow(sfd_scd)) {
    sfd_info <- late_stage_grid(sfd_scd |> dplyr::slice(pr))
    expect_equal(
      sfd_info,
      sfd_scd$model_stage[[pr]] |>
        tidyr::unnest(predict_stage)
    )
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
    expect_equal(
      reg_info,
      reg_scd$model_stage[[pr]] |>
        tidyr::unnest(predict_stage)
    )
  }

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  for (pr in 1:nrow(reg_ish_scd)) {
    reg_ish_info <- late_stage_grid(reg_ish_scd |> dplyr::slice(pr))
    expect_equal(
      reg_ish_info,
      reg_ish_scd$model_stage[[pr]] |>
        tidyr::unnest(predict_stage)
    )
  }

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  for (pr in 1:nrow(sfd_scd)) {
    sfd_info <- late_stage_grid(sfd_scd |> dplyr::slice(pr))
    expect_equal(
      sfd_info,
      sfd_scd$model_stage[[pr]] |>
        tidyr::unnest(predict_stage)
    )
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
    expect_equal(
      reg_info,
      reg_scd$model_stage[[pr]] |>
        tidyr::unnest(predict_stage)
    )
  }

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  for (pr in 1:nrow(reg_ish_scd)) {
    reg_ish_info <- late_stage_grid(reg_ish_scd |> dplyr::slice(pr))
    expect_equal(
      reg_ish_info,
      reg_ish_scd$model_stage[[pr]] |>
        tidyr::unnest(predict_stage)
    )
  }

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  for (pr in 1:nrow(sfd_scd)) {
    sfd_info <- late_stage_grid(sfd_scd |> dplyr::slice(pr))
    expect_equal(
      sfd_info,
      sfd_scd$model_stage[[pr]] |>
        tidyr::unnest(predict_stage)
    )
  }
})

# ----------------------------------------------------------------------------

test_that("h2o grids - tuned recipe, tuned knn, and no postprocessor", {
  wflow <- workflow(rec_pca, knn_submod_spec)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  for (pr in 1:nrow(reg_scd)) {
    reg_info <- late_stage_grid(reg_scd |> dplyr::slice(pr))
    expect_equal(
      reg_info,
      reg_scd$model_stage[[pr]] |>
        dplyr::select(-neighbors) |>
        tidyr::unnest(predict_stage)
    )
  }

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  for (pr in 1:nrow(reg_ish_scd)) {
    reg_ish_info <- late_stage_grid(reg_ish_scd |> dplyr::slice(pr))
    expect_equal(
      reg_ish_info,
      reg_ish_scd$model_stage[[pr]] |>
        dplyr::select(-neighbors) |>
        tidyr::unnest(predict_stage)
    )
  }

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  for (pr in 1:nrow(sfd_scd)) {
    sfd_info <- late_stage_grid(sfd_scd |> dplyr::slice(pr))
    expect_equal(
      sfd_info,
      sfd_scd$model_stage[[pr]] |>
        dplyr::select(-neighbors) |>
        tidyr::unnest(predict_stage)
    )
  }
})

# ----------------------------------------------------------------------------

test_that("h2o grids - tuned recipe, tuned knn, and static prob thresh", {
  wflow <- workflow(rec_pca, knn_submod_spec, tlr_thresh_half)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  for (pr in 1:nrow(reg_scd)) {
    reg_info <- late_stage_grid(reg_scd |> dplyr::slice(pr))
    expect_equal(
      reg_info,
      reg_scd$model_stage[[pr]] |>
        dplyr::select(-neighbors) |>
        tidyr::unnest(predict_stage)
    )
  }

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  for (pr in 1:nrow(reg_ish_scd)) {
    reg_ish_info <- late_stage_grid(reg_ish_scd |> dplyr::slice(pr))
    expect_equal(
      reg_ish_info,
      reg_ish_scd$model_stage[[pr]] |>
        dplyr::select(-neighbors) |>
        tidyr::unnest(predict_stage)
    )
  }

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  for (pr in 1:nrow(sfd_scd)) {
    sfd_info <- late_stage_grid(sfd_scd |> dplyr::slice(pr))
    expect_equal(
      sfd_info,
      sfd_scd$model_stage[[pr]] |>
        dplyr::select(-neighbors) |>
        tidyr::unnest(predict_stage)
    )
  }
})

# ----------------------------------------------------------------------------

test_that("h2o grids - tuned recipe, tuned knn, and tuned prob thresh", {
  wflow <- workflow(rec_pca, knn_submod_spec, tlr_thresh_tune)
  prm <- extract_parameter_set_dials(wflow)

  ### regular grid

  reg_grd <- grid_regular(prm, levels = 3)
  reg_scd <- schedule_grid(reg_grd, wflow)

  for (pr in 1:nrow(reg_scd)) {
    reg_info <- late_stage_grid(reg_scd |> dplyr::slice(pr))
    expect_equal(
      reg_info,
      reg_scd$model_stage[[pr]] |>
        dplyr::select(-neighbors) |>
        tidyr::unnest(predict_stage)
    )
  }

  ### regular grid minus 1 row

  reg_ish_grd <- grid_regular(prm, levels = 3) |> dplyr::slice(-1)
  reg_ish_scd <- schedule_grid(reg_ish_grd, wflow)

  for (pr in 1:nrow(reg_ish_scd)) {
    reg_ish_info <- late_stage_grid(reg_ish_scd |> dplyr::slice(pr))
    expect_equal(
      reg_ish_info,
      reg_ish_scd$model_stage[[pr]] |>
        dplyr::select(-neighbors) |>
        tidyr::unnest(predict_stage)
    )
  }

  ### space-filling design row

  sfd_grd <- grid_space_filling(prm, size = 5)
  sfd_scd <- schedule_grid(sfd_grd, wflow)

  for (pr in 1:nrow(sfd_scd)) {
    sfd_info <- late_stage_grid(sfd_scd |> dplyr::slice(pr))
    expect_equal(
      sfd_info,
      sfd_scd$model_stage[[pr]] |>
        dplyr::select(-neighbors) |>
        tidyr::unnest(predict_stage)
    )
  }
})
