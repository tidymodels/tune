
test_that("grid processing sschedule - recipe only", {
  library(workflows)
  library(parsnip)
  library(recipes)
  library(dials)

  wflow_pre_only <- workflow(rec, logistic_reg())
  prm_used_pre_only <- extract_parameter_set_dials(wflow_pre_only)
  grid_pre_only <-
    grid_regular(prm_used_pre_only, levels = 3) %>%
    arrange(threshold, disp_df)
  sched_pre_only <-
    tune:::get_tune_schedule(wflow_pre_only, prm_used_pre_only, grid_pre_only)

  expect_named(sched_pre_only, c("threshold", "disp_df", "model_stage"))
  expect_equal(nrow(sched_pre_only), nrow(grid_pre_only))

  # All of the other nested tibbles should be empty
  expect_equal(
    sched_pre_only %>%
      tidyr::unnest(model_stage) %>%
      tidyr::unnest(predict_stage) %>%
      tidyr::unnest(post_stage),
    grid_pre_only
  )

})

test_that("grid processing sschedule - model only, no submodels", {
  library(workflows)
  library(parsnip)
  library(recipes)
  library(dials)

  wflow_rf_only <- workflow(outcome ~ ., mod_rf)
  prm_used_rf_only <-
    extract_parameter_set_dials(wflow_rf_only) %>%
    update(mtry = mtry(c(1, 10)))
  grid_rf_only <- grid_regular(prm_used_rf_only, levels = 3)
  sched_rf_only <-
    tune:::get_tune_schedule(wflow_rf_only, prm_used_rf_only, grid_rf_only)


  expect_named(sched_rf_only, c("model_stage"))
  expect_equal(nrow(sched_rf_only), 1L)

  rf_n <- length(sched_rf_only$model_stage)
  for (i in 1:rf_n) {
    expect_named(sched_rf_only$model_stage[[i]], c("mtry", "predict_stage"))
    expect_equal(
      sched_rf_only$model_stage[[i]] %>%
        tidyr::unnest(predict_stage) %>%
        tidyr::unnest(post_stage),
      grid_rf_only
    )
  }

})

test_that("grid processing sschedule - model only, submodels", {
  library(workflows)
  library(parsnip)
  library(recipes)
  library(dials)

  wflow_bst <- workflow(outcome ~ ., mod_bst)
  prm_used_bst <- extract_parameter_set_dials(wflow_bst)
  grid_bst <- grid_regular(prm_used_bst, levels = 3)

  min_n_only <- grid_bst %>% dplyr::distinct(min_n) %>% dplyr::arrange(min_n)
  trees_only <- grid_bst %>% dplyr::distinct(trees) %>% dplyr::arrange(trees)

  # ------------------------------------------------------------------------------
  # regular grid
  sched_bst <- tune:::get_tune_schedule(wflow_bst, prm_used_bst, grid_bst)

  expect_named(sched_bst, c("model_stage"))
  expect_equal(nrow(sched_bst), 1L)

  reg_n <- length(sched_bst$model_stage)
  for (i in 1:reg_n) {
    expect_named(sched_bst$model_stage[[i]], c("min_n", "predict_stage", "trees"))

    expect_equal(
      sched_bst$model_stage[[i]] %>%
        dplyr::select(-trees, -predict_stage),
      min_n_only
    )

    for (j in seq_along(sched_bst$model_stage[[i]]$predict_stage)) {
      expect_named(
        sched_bst$model_stage[[i]]$predict_stage[[j]],
        c("trees", "post_stage"))
      expect_equal(
        sched_bst$model_stage[[i]]$predict_stage[[j]] %>%
          dplyr::select(trees),
        trees_only
      )
    }

    expect_equal(
      sched_bst$model_stage[[i]] %>%
        dplyr::select(-trees) %>%
        tidyr::unnest(predict_stage) %>%
        tidyr::unnest(post_stage) %>%
        dplyr::select(trees, min_n),
      grid_bst
    )
  }

  # ------------------------------------------------------------------------------
  # irregular design (no overlap)

  grid_sfd_bst <- grid_space_filling(prm_used_bst, size = 5, type = "uniform")
  sched_sfd_bst <- tune:::get_tune_schedule(wflow_bst, prm_used_bst, grid_sfd_bst)

  expect_named(sched_sfd_bst, c("model_stage"))
  expect_equal(nrow(sched_sfd_bst), 1L)

  irreg_n <- length(sched_sfd_bst$model_stage)
  expect_equal(irreg_n, 1L)

  expect_named(sched_sfd_bst$model_stage[[1]], c("min_n", "predict_stage", "trees"))
  expect_equal(
    sched_sfd_bst$model_stage[[1]] %>%
      dplyr::select(-predict_stage) %>%
      dplyr::select(trees, min_n) %>%
      dplyr::arrange(trees, min_n),
    grid_sfd_bst %>%
      dplyr::select(trees, min_n) %>%
      dplyr::arrange(trees, min_n)
  )

  expect_equal(
    sched_sfd_bst$model_stage[[1]] %>%
      dplyr::select(-trees) %>%
      tidyr::unnest(predict_stage) %>%
      tidyr::unnest(post_stage) %>%
      dplyr::select(trees, min_n) %>%
      dplyr::arrange(trees, min_n),
    grid_sfd_bst %>%
      dplyr::select(trees, min_n) %>%
      dplyr::arrange(trees, min_n)
  )

  # ------------------------------------------------------------------------------
  # irregular design (1 overlap at min_n = 1)

  grid_odd_bst <- tibble(min_n = c(1, 1, 2, 3, 4, 5), trees = rep(1:2, 3))
  sched_odd_bst <- tune:::get_tune_schedule(wflow_bst, prm_used_bst, grid_odd_bst)

  expect_named(sched_odd_bst, c("model_stage"))
  expect_equal(nrow(sched_odd_bst), 1L)

  odd_n <- length(sched_odd_bst$model_stage)
  expect_equal(odd_n, 1L)

  expect_named(sched_odd_bst$model_stage[[1]], c("min_n", "predict_stage", "trees"))
  expect_equal(
    sched_odd_bst$model_stage[[1]] %>%
      dplyr::select(-predict_stage) %>%
      dplyr::select(trees, min_n),
    tibble(trees = c(2, 1, 2, 1, 2), min_n = c(1, 2, 3, 4, 5))
  )

  for (i in 1:nrow(sched_odd_bst$model_stage[[1]])) {
    prd <- sched_odd_bst$model_stage[[1]]$predict_stage[[i]]
    if (sched_odd_bst$model_stage[[1]]$min_n[i] == 1) {
      expect_equal(nrow(prd), 2L)
    } else {
      expect_equal(nrow(prd), 1L)
    }
    expect_true(
      all(purrr::map_int(prd$post_stage, nrow) == 1)
    )
  }

  # ------------------------------------------------------------------------------
  # 1-point design

  set.seed(1)
  grid_1_pt <- grid_random(prm_used_bst, size = 1)
  sched_1_pt <- tune:::get_tune_schedule(wflow_bst, prm_used_bst, grid_1_pt)

  expect_named(sched_1_pt, c("model_stage"))
  expect_equal(nrow(sched_1_pt), 1L)
  expect_equal(length(sched_1_pt$model_stage), 1L)
  expect_named(
    sched_1_pt$model_stage[[1]],
    c("min_n", "predict_stage", "trees")
  )

  expect_equal(
    length(sched_1_pt$model_stage[[1]]$predict_stage),
    1L
  )
  expect_named(
    sched_1_pt$model_stage[[1]]$predict_stage[[1]],
    c("trees", "post_stage")
  )

  expect_equal(
    length(sched_1_pt$model_stage[[1]]$predict_stage[[1]]$post_stage),
    1L
  )
  expect_equal(
    dim(sched_1_pt$model_stage[[1]]$predict_stage[[1]]$post_stage[[1]]),
    1:0
  )
})
