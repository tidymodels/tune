test_that("stage helpers - recipe only", {
  wflow_1 <- workflows::workflow(puromycin_tune_rec, linear_reg())
  grid_1 <- wflow_1 |>
    extract_parameter_set_dials() |>
    grid_regular()
  sched_1 <- schedule_grid(grid_1, wflow_1)

  expect_equal(dim(tune:::remove_stage(sched_1)), c(3L, 1L))
  expect_s3_class(
    sched_1,
    c("grid_schedule", "schedule", "tbl_df", "tbl", "data.frame")
  )

  expect_true(tune:::has_pre_param(sched_1))
  expect_false(tune:::has_sub_param(sched_1$model_stage[[1]]$predict_stage))
})

test_that("stage helpers - model only", {
  # Tunable model with submodels

  wflow_1 <- workflows::workflow(class ~ ., glmn_spec)
  grid_1 <- wflow_1 |>
    extract_parameter_set_dials() |>
    grid_regular() |>
    slice(-1)
  sched_1 <- schedule_grid(grid_1, wflow_1)

  expect_equal(dim(tune:::remove_stage(sched_1)), c(1L, 0L))

  res_1 <- tune:::remove_stage(sched_1 |> mutate(col = 1))
  exp_1 <- tibble::tibble(col = 1)
  class(exp_1) <- c("grid_schedule", "schedule", class(exp_1))
  expect_equal(res_1, exp_1)

  expect_false(tune:::has_pre_param(sched_1))
  expect_true(tune:::has_mod_param(sched_1))
  expect_equal(
    purrr:::map_lgl(
      sched_1$model_stage[[1]]$predict_stage,
      tune:::has_sub_param
    ),
    rep(TRUE, 3)
  )
  expect_equal(
    purrr:::map_chr(
      sched_1$model_stage[[1]]$predict_stage,
      tune:::get_sub_param
    ),
    rep("penalty", 3)
  )

  # ----------------------------------------------------------------------------
  # Tunable model without submodels

  wflow_2 <- workflows::workflow(class ~ ., dt_spec)
  grid_2 <- wflow_2 |>
    extract_parameter_set_dials() |>
    grid_regular()
  sched_2 <- schedule_grid(grid_2, wflow_2)

  expect_equal(dim(tune:::remove_stage(sched_2)), c(1L, 0L))

  res_2 <- tune:::remove_stage(sched_2 |> mutate(col = 1))
  exp_2 <- tibble::tibble(col = 1)
  class(exp_2) <- c("grid_schedule", "schedule", class(exp_2))
  expect_equal(res_2, exp_2)

  expect_false(tune:::has_pre_param(sched_2))
  expect_true(tune:::has_mod_param(sched_2))
  expect_equal(
    purrr:::map_lgl(
      sched_2$model_stage[[1]]$predict_stage,
      tune:::has_sub_param
    ),
    rep(FALSE, 3)
  )
  expect_equal(
    purrr:::map(
      sched_2$model_stage[[1]]$predict_stage,
      tune:::get_sub_param
    ),
    list(character(0), character(0), character(0))
  )
})

test_that("stage helpers - post only", {
  skip_if_not_installed("probably")

  wflow_1 <- workflows::workflow(class ~ ., logistic_reg(), cls_post)
  grid_1 <- wflow_1 |>
    extract_parameter_set_dials() |>
    grid_regular()
  sched_1 <- schedule_grid(grid_1, wflow_1)

  expect_equal(dim(tune:::remove_stage(sched_1)), c(1L, 0L))
  expect_s3_class(
    sched_1,
    c("grid_schedule", "schedule", "tbl_df", "tbl", "data.frame")
  )

  expect_false(tune:::has_pre_param(sched_1))
  expect_false(tune:::has_mod_param(sched_1$model_stage[[1]]))
})

test_that("stage helpers - recipe and model", {
  # No submodels

  wflow_1 <- workflows::workflow(puromycin_tune_rec, dt_spec)
  grid_1 <- wflow_1 |>
    extract_parameter_set_dials() |>
    grid_regular() |>
    slice(-1)
  sched_1 <- schedule_grid(grid_1, wflow_1)

  expect_equal(dim(tune:::remove_stage(sched_1)), c(3L, 1L))
  expect_s3_class(
    sched_1,
    c("grid_schedule", "schedule", "tbl_df", "tbl", "data.frame")
  )

  expect_true(tune:::has_pre_param(sched_1))

  expect_equal(
    purrr::map_lgl(sched_1$model_stage, tune:::has_mod_param),
    rep(TRUE, 3)
  )

  expect_equal(
    purrr::map_lgl(
      sched_1$model_stage,
      ~ tune:::has_sub_param(.x$predict_stage)
    ),
    rep(FALSE, 3)
  )

  # With submodels

  wflow_2 <- workflows::workflow(puromycin_tune_rec, glmn_spec)
  grid_2 <- wflow_2 |>
    extract_parameter_set_dials() |>
    grid_regular() |>
    slice(-1)
  sched_2 <- schedule_grid(grid_2, wflow_2)

  expect_equal(dim(tune:::remove_stage(sched_2)), c(3L, 1L))
  expect_s3_class(
    sched_2,
    c("grid_schedule", "schedule", "tbl_df", "tbl", "data.frame")
  )

  expect_true(tune:::has_pre_param(sched_2))

  expect_equal(
    purrr::map_lgl(sched_2$model_stage, tune:::has_mod_param),
    rep(TRUE, 3)
  )

  expect_equal(
    purrr::map_lgl(
      sched_2$model_stage,
      ~ tune:::has_sub_param(.x$predict_stage[[1]])
    ),
    rep(TRUE, 3)
  )
})

test_that("stage helpers - recipe and post", {
  skip_if_not_installed("probably")

  wflow_1 <- workflows::workflow(puromycin_tune_rec, logistic_reg(), cls_post)
  grid_1 <- wflow_1 |>
    extract_parameter_set_dials() |>
    grid_regular()
  sched_1 <- schedule_grid(grid_1, wflow_1)

  expect_equal(dim(tune:::remove_stage(sched_1)), c(3L, 1L))
  expect_s3_class(
    sched_1,
    c("grid_schedule", "schedule", "tbl_df", "tbl", "data.frame")
  )

  expect_true(tune:::has_pre_param(sched_1))

  expect_false(tune:::has_mod_param(sched_1$model_stage[[1]]))
})
