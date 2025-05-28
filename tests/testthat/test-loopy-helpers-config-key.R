test_that("set configurations - single element", {
  skip_if_not_installed("probably")

  wflow_pre <- workflow(puromycin_tune_rec, linear_reg())
  wflow_mod <- workflow(Class ~ ., dt_spec)
  wflow_post <- workflow(Class ~ ., logistic_reg(), cls_post)

  # ------------------------------------------------------------------------------

  expect_snapshot(
    tune:::get_config_key(
      tibble::tibble(polynimial_degree = 1:2),
      wflow_pre
    ),
    error = TRUE
  )

  expect_snapshot(
    tune:::get_config_key(
      tibble::tibble(degree = 1:3),
      wflow_pre
    )
  )

  expect_snapshot(
    tune:::get_config_key(
      tibble::tibble(degree = 1),
      wflow_pre
    )
  )

  expect_snapshot(
    tune:::get_config_key(
      tibble::tibble(min_n = 10:12),
      wflow_mod
    )
  )

  expect_snapshot(
    tune:::get_config_key(
      tibble::tibble(cut = seq(0, 1, length.out = 5)),
      wflow_post
    )
  )
})

test_that("set configurations - two elements", {
  skip_if_not_installed("probably")

  wflow_1 <- workflow(puromycin_tune_rec, knn_reg_spec)
  wflow_2 <- workflow(Class ~ ., dt_spec, cls_post)
  wflow_3 <- workflow(puromycin_tune_rec, logistic_reg(), cls_post)

  # ------------------------------------------------------------------------------

  expect_snapshot(
    tune:::get_config_key(
      tidyr::crossing(degree = 1:3, neighbors = 1:2),
      wflow_1
    )
  )

  expect_snapshot(
    tune:::get_config_key(
      tidyr::crossing(min_n = 1:3, cut = (1:5) / 5) |> slice(-1),
      wflow_2
    )
  )

  grid <- grid <- dials:::grid_space_filling(
    parameters(degree_int(), threshold()),
    size = 10
  ) |>
    setNames(c("degree", "cut"))
  expect_snapshot(
    tune:::get_config_key(
      grid,
      wflow_3
    )
  )
})

test_that("set configurations - all or none", {
  skip_if_not_installed("probably")

  wflow_none <- workflow(Class ~ ., logistic_reg())
  wflow_all <- workflow(puromycin_tune_rec, dt_spec, cls_post)

  # ------------------------------------------------------------------------------

  expect_snapshot(
    tune:::get_config_key(
      tibble(),
      wflow_none
    )
  )

  grid <- grid <- dials:::grid_space_filling(
    parameters(degree_int(), min_n(), threshold())
  ) |>
    setNames(c("degree", "min_n", "cut"))

  expect_snapshot(
    tune:::get_config_key(
      grid,
      wflow_all
    )
  )
})
