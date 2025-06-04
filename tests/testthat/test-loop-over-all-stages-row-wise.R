test_that("convert grid to rowwise - no submodels", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("probably")

  # Model only
  wflow_1 <- workflow(Class ~ ., dt_spec, cls_est_post)
  param_1 <- wflow_1 |> extract_parameter_set_dials()
  grid_reg_1 <- grid_regular(param_1) |> arrange(min_n)

  res_reg_1 <- tune:::get_row_wise_grid(wflow_1, grid_reg_1)
  for (i in 1:nrow(grid_reg_1)) {
    sub <- grid_reg_1[i, ]
    expect_equal(res_reg_1[[i]], sub)
  }

  # recipe only
  wflow_2 <- workflow(puromycin_tune_rec, logistic_reg(), cls_est_post)
  param_2 <- wflow_2 |> extract_parameter_set_dials()
  grid_reg_2 <- grid_regular(param_2) |> arrange(degree)

  res_reg_2 <- tune:::get_row_wise_grid(wflow_2, grid_reg_2)
  for (i in 1:nrow(grid_reg_2)) {
    sub <- grid_reg_2[i, ]
    expect_equal(res_reg_2[[i]], sub)
  }

  # post only
  wflow_3 <- workflow(Class ~ ., logistic_reg(), cls_post)
  param_3 <- wflow_3 |> extract_parameter_set_dials()
  grid_reg_3 <- grid_regular(param_3) |> arrange(cut)

  res_reg_3 <- tune:::get_row_wise_grid(wflow_3, grid_reg_3)
  for (i in 1:nrow(grid_reg_3)) {
    sub <- grid_reg_3[i, ]
    expect_equal(res_reg_3[[i]], sub)
  }

  # Recipe + Model
  wflow_4 <- workflow(puromycin_tune_rec, dt_spec, cls_est_post)
  param_4 <- wflow_4 |> extract_parameter_set_dials()
  grid_reg_4 <- grid_regular(param_4) |> arrange(degree, min_n)

  res_reg_4 <- tune:::get_row_wise_grid(wflow_4, grid_reg_4)
  for (i in 1:nrow(grid_reg_4)) {
    sub <- grid_reg_4[i, ]
    expect_equal(res_reg_4[[i]], sub)
  }

  # Recipe + Post
  wflow_5 <- workflow(puromycin_tune_rec, logistic_reg(), cls_post)
  param_5 <- wflow_5 |> extract_parameter_set_dials()
  grid_reg_5 <- grid_regular(param_5) |> arrange(degree, cut)

  res_reg_5 <- tune:::get_row_wise_grid(wflow_5, grid_reg_5)
  for (i in 1:nrow(grid_reg_5)) {
    sub <- grid_reg_5[i, ]
    expect_equal(res_reg_5[[i]], sub)
  }

  # Model + Post
  wflow_5 <- workflow(Class ~ ., dt_spec, cls_post)
  param_5 <- wflow_5 |> extract_parameter_set_dials()
  grid_reg_5 <- grid_regular(param_5) |> arrange(cut, min_n)

  res_reg_5 <- tune:::get_row_wise_grid(wflow_5, grid_reg_5)
  for (i in 1:nrow(grid_reg_5)) {
    sub <- grid_reg_5[i, ]
    expect_equal(res_reg_5[[i]], sub)
  }

  # Recipe + Model + Post
  wflow_6 <- workflow(puromycin_tune_rec, dt_spec, cls_post)
  param_6 <- wflow_6 |> extract_parameter_set_dials()
  grid_reg_6 <- grid_regular(param_6) |> arrange(cut, degree, min_n)

  res_reg_6 <- tune:::get_row_wise_grid(wflow_6, grid_reg_6)
  for (i in 1:nrow(grid_reg_6)) {
    sub <- grid_reg_6[i, ]
    expect_equal(res_reg_6[[i]], sub)
  }
})

test_that("convert grid to rowwise - submodels", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("probably")

  # Model only
  wflow_1 <- workflow(Class ~ ., glmn_spec, cls_est_post)
  param_1 <- wflow_1 |> extract_parameter_set_dials()

  grid_reg_1 <- grid_regular(param_1) |> arrange(mixture, penalty)
  res_reg_1 <- tune:::get_row_wise_grid(wflow_1, grid_reg_1)
  distinct_reg_1 <- distinct(grid_reg_1, mixture)
  for (i in 1:nrow(distinct_reg_1)) {
    sub <- grid_reg_1 |>
      dplyr::filter(mixture == distinct_reg_1$mixture[i])
    expect_equal(res_reg_1[[i]], sub)
  }

  grid_sfd_1 <- grid_space_filling(param_1) |> arrange(mixture, penalty)
  res_sfd_1 <- tune:::get_row_wise_grid(wflow_1, grid_sfd_1)
  distinct_sfd_1 <- distinct(grid_sfd_1, mixture)
  for (i in 1:nrow(distinct_sfd_1)) {
    sub <- grid_sfd_1 |>
      dplyr::filter(mixture == distinct_sfd_1$mixture[i])
    expect_equal(res_sfd_1[[i]], sub)
  }

  # Recipe + Model
  wflow_2 <- workflow(puromycin_tune_rec, glmn_spec, cls_est_post)
  param_2 <- wflow_2 |> extract_parameter_set_dials()

  grid_reg_2 <- grid_regular(param_2)
  distinct_reg_2 <- distinct(grid_reg_2, mixture, degree) |>
    arrange(mixture, degree)

  res_reg_2 <- tune:::get_row_wise_grid(wflow_2, grid_reg_2)
  for (i in 1:nrow(distinct_reg_2)) {
    sub <- grid_reg_2 |>
      inner_join(distinct_reg_2[i, ], by = c("mixture", "degree"))
    expect_equal(res_reg_2[[i]], sub)
  }

  grid_sfd_2 <- grid_space_filling(param_2)
  distinct_sfd_2 <- distinct(grid_sfd_2, mixture, degree) |>
    arrange(mixture, degree)

  res_sfd_2 <- tune:::get_row_wise_grid(wflow_2, grid_sfd_2)
  for (i in 1:nrow(distinct_sfd_2)) {
    sub <- grid_sfd_2 |>
      inner_join(distinct_sfd_2[i, ], by = c("mixture", "degree"))
    expect_equal(res_sfd_2[[i]], sub)
  }

  # Model + Post
  wflow_3 <- workflow(Class ~ ., glmn_spec, cls_post)
  param_3 <- wflow_3 |> extract_parameter_set_dials()

  grid_reg_3 <- grid_regular(param_3)
  distinct_reg_3 <- distinct(grid_reg_3, cut, mixture) |>
    arrange(mixture, cut)

  res_reg_3 <- tune:::get_row_wise_grid(wflow_3, grid_reg_3)
  for (i in 1:nrow(distinct_reg_3)) {
    sub <- grid_reg_3 |>
      inner_join(distinct_reg_3[i, ], by = c("mixture", "cut"))
    expect_equal(res_reg_3[[i]], sub)
  }

  grid_sfd_3 <- grid_space_filling(param_3)
  distinct_sfd_3 <- distinct(grid_sfd_3, cut, mixture) |>
    arrange(mixture, cut)

  res_sfd_3 <- tune:::get_row_wise_grid(wflow_3, grid_sfd_3)
  for (i in 1:nrow(distinct_sfd_3)) {
    sub <- grid_sfd_3 |>
      inner_join(distinct_sfd_3[i, ], by = c("mixture", "cut"))
    expect_equal(res_sfd_3[[i]], sub)
  }

  # Recipe + Model + Post
  wflow_4 <- workflow(puromycin_tune_rec, glmn_spec, cls_post)
  param_4 <- wflow_4 |> extract_parameter_set_dials()

  grid_reg_4 <- grid_regular(param_4)
  distinct_reg_4 <- distinct(grid_reg_4, degree, cut, mixture) |>
    arrange(mixture, degree, cut)

  res_reg_4 <- tune:::get_row_wise_grid(wflow_4, grid_reg_4)
  for (i in 1:nrow(distinct_reg_4)) {
    sub <- grid_reg_4 |>
      inner_join(distinct_reg_4[i, ], by = c("mixture", "degree", "cut"))
    expect_equal(res_reg_4[[i]], sub)
  }

  grid_sfd_4 <- grid_space_filling(param_4)
  distinct_sfd_4 <- distinct(grid_sfd_4, degree, cut, mixture) |>
    arrange(mixture, degree, cut)

  res_sfd_4 <- tune:::get_row_wise_grid(wflow_4, grid_sfd_4)
  for (i in 1:nrow(distinct_sfd_4)) {
    sub <- grid_sfd_4 |>
      inner_join(distinct_sfd_4[i, ], by = c("mixture", "degree", "cut"))
    expect_equal(res_sfd_4[[i]], sub)
  }
})
