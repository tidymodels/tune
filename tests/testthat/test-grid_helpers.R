test_that("compute_grid_info - recipe only", {
  library(workflows)
  library(recipes)
  library(parsnip)
  library(dials)

  rec <- recipe(mpg ~ ., mtcars) %>% step_spline_natural(deg_free = tune())

  wflow <- workflow()
  wflow <- add_model(wflow, boost_tree(mode = "regression"))
  wflow <- add_recipe(wflow, rec)

  grid <- grid_space_filling(extract_parameter_set_dials(wflow))
  res <- compute_grid_info(wflow, grid)

  expect_equal(res$.iter_preprocessor, 1:5)
  expect_equal(res$.msg_preprocessor, paste0("preprocessor ", 1:5, "/5"))
  expect_equal(sort(res$deg_free), sort(grid$deg_free))
  expect_equal(res$.iter_model, rep(1, 5))
  expect_equal(res$.iter_config, as.list(paste0("Preprocessor", 1:5, "_Model1")))
  expect_equal(res$.msg_model, paste0("preprocessor ", 1:5, "/5, model 1/1"))
  expect_equal(res$.submodels, list(list(), list(), list(), list(), list()))
  expect_named(
    res,
    c(".iter_preprocessor", ".msg_preprocessor", "deg_free", ".iter_model",
      ".iter_config", ".msg_model", ".submodels"),
    ignore.order = TRUE
  )
  expect_equal(nrow(res), 5)
  expect_equal(vctrs::vec_unique_count(res$.iter_config), nrow(grid))
})

test_that("compute_grid_info - model only (no submodels)", {
  library(workflows)
  library(parsnip)
  library(dials)

  spec <- boost_tree(mode = "regression", learn_rate = tune())

  wflow <- workflow()
  wflow <- add_model(wflow, spec)
  wflow <- add_formula(wflow, mpg ~ .)

  grid <- grid_space_filling(extract_parameter_set_dials(wflow))
  res <- compute_grid_info(wflow, grid)

  expect_equal(res$.iter_preprocessor, rep(1, 5))
  expect_equal(res$.msg_preprocessor, rep("preprocessor 1/1", 5))
  expect_equal(res$learn_rate, grid$learn_rate)
  expect_equal(res$.iter_model, 1:5)
  expect_equal(res$.iter_config, as.list(paste0("Preprocessor1_Model", 1:5)))
  expect_equal(res$.msg_model, paste0("preprocessor 1/1, model ", 1:5, "/5"))
  expect_equal(res$.submodels, list(list(), list(), list(), list(), list()))
  expect_named(
    res,
    c(".iter_preprocessor", ".msg_preprocessor", "learn_rate", ".iter_model",
      ".iter_config", ".msg_model", ".submodels"),
    ignore.order = TRUE
  )
  expect_equal(nrow(res), 5)
  expect_equal(vctrs::vec_unique_count(res$.iter_config), nrow(grid))
})

test_that("compute_grid_info - model only (with submodels)", {
  library(workflows)
  library(parsnip)
  library(dials)

  spec <- boost_tree(mode = "regression", trees = tune())

  wflow <- workflow()
  wflow <- add_model(wflow, spec)
  wflow <- add_formula(wflow, mpg ~ .)

  grid <- grid_space_filling(extract_parameter_set_dials(wflow))
  res <- compute_grid_info(wflow, grid)

  expect_equal(res$.iter_preprocessor, 1)
  expect_equal(res$.msg_preprocessor, "preprocessor 1/1")
  expect_equal(res$trees, max(grid$trees))
  expect_equal(res$.iter_model, 1)
  expect_equal(res$.iter_config, list(paste0("Preprocessor1_Model", 1:5)))
  expect_equal(res$.msg_model, "preprocessor 1/1, model 1/1")
  expect_equal(res$.submodels, list(list(trees = grid$trees[-which.max(grid$trees)])))
  expect_named(
    res,
    c(".iter_preprocessor", ".msg_preprocessor", "trees", ".iter_model",
      ".iter_config", ".msg_model", ".submodels"),
    ignore.order = TRUE
  )
  expect_equal(nrow(res), 1)
})

test_that("compute_grid_info - recipe and model (no submodels)", {
  library(workflows)
  library(parsnip)
  library(recipes)
  library(dials)

  rec <- recipe(mpg ~ ., mtcars) %>% step_spline_natural(deg_free = tune())
  spec <- boost_tree(mode = "regression", learn_rate = tune())

  wflow <- workflow()
  wflow <- add_model(wflow, spec)
  wflow <- add_recipe(wflow, rec)

  grid <- grid_space_filling(extract_parameter_set_dials(wflow))
  res <- compute_grid_info(wflow, grid)

  expect_equal(res$.iter_preprocessor, 1:5)
  expect_equal(res$.msg_preprocessor, paste0("preprocessor ", 1:5, "/5"))
  expect_equal(sort(res$learn_rate), sort(grid$learn_rate))
  expect_equal(sort(res$deg_free), sort(grid$deg_free))
  expect_equal(res$.iter_model, rep(1, 5))
  expect_equal(res$.iter_config, as.list(paste0("Preprocessor", 1:5, "_Model1")))
  expect_equal(res$.msg_model, paste0("preprocessor ", 1:5, "/5, model 1/1"))
  expect_equal(res$.submodels, list(list(), list(), list(), list(), list()))
  expect_named(
    res,
    c(".iter_preprocessor", ".msg_preprocessor", "deg_free", "learn_rate",
      ".iter_model", ".iter_config", ".msg_model", ".submodels"),
    ignore.order = TRUE
  )
  expect_equal(nrow(res), 5)
  expect_equal(vctrs::vec_unique_count(res$.iter_config), nrow(grid))
})

test_that("compute_grid_info - recipe and model (with submodels)", {
  library(workflows)
  library(parsnip)
  library(recipes)
  library(dials)

  rec <- recipe(mpg ~ ., mtcars) %>% step_spline_natural(deg_free = tune())
  spec <- boost_tree(mode = "regression", trees = tune())

  wflow <- workflow()
  wflow <- add_model(wflow, spec)
  wflow <- add_recipe(wflow, rec)

  # use grid_regular to trigger submodel trick
  set.seed(1)
  grid <- grid_regular(extract_parameter_set_dials(wflow))
  res <- compute_grid_info(wflow, grid)

  expect_equal(res$.iter_preprocessor, 1:3)
  expect_equal(res$.msg_preprocessor, paste0("preprocessor ", 1:3, "/3"))
  expect_equal(res$trees, rep(max(grid$trees), 3))
  expect_equal(res$.iter_model, rep(1, 3))
  expect_equal(
    res$.iter_config,
    list(
      c("Preprocessor1_Model1", "Preprocessor1_Model2", "Preprocessor1_Model3"),
      c("Preprocessor2_Model1", "Preprocessor2_Model2", "Preprocessor2_Model3"),
      c("Preprocessor3_Model1", "Preprocessor3_Model2", "Preprocessor3_Model3")
    )
  )
  expect_equal(res$.msg_model, paste0("preprocessor ", 1:3, "/3, model 1/1"))
  expect_equal(
    res$.submodels,
    list(
      list(trees = c(1L, 1000L)),
      list(trees = c(1L, 1000L)),
      list(trees = c(1L, 1000L))
    )
  )
  expect_named(
    res,
    c(".iter_preprocessor", ".msg_preprocessor", "deg_free", "trees",
      ".iter_model", ".iter_config", ".msg_model", ".submodels"),
    ignore.order = TRUE
  )
  expect_equal(nrow(res), 3)
})

test_that("compute_grid_info - recipe and model (with and without submodels)", {
  library(workflows)
  library(parsnip)
  library(recipes)
  library(dials)

  rec <- recipe(mpg ~ ., mtcars) %>% step_spline_natural(deg_free = tune())
  spec <- boost_tree(mode = "regression", trees = tune(), loss_reduction = tune())

  wflow <- workflow()
  wflow <- add_model(wflow, spec)
  wflow <- add_recipe(wflow, rec)

  # use grid_regular to (partially) trigger submodel trick
  set.seed(1)
  param_set <- extract_parameter_set_dials(wflow)
  grid <-
    bind_rows(grid_regular(param_set), grid_space_filling(param_set)) %>%
    arrange(deg_free, loss_reduction, trees)
  res <- compute_grid_info(wflow, grid)

  expect_equal(length(unique(res$.iter_preprocessor)), 5)
  expect_equal(
    unique(res$.msg_preprocessor),
    paste0("preprocessor ", 1:5, "/5")
  )
  expect_equal(sort(res$trees), sort(c(rep(max(grid$trees), 10), 1)))
  expect_equal(unique(res$.iter_model), 1:3)
  expect_equal(
    res$.iter_config[res$.iter_preprocessor == 1],
    list(
      c("Preprocessor1_Model01", "Preprocessor1_Model02", "Preprocessor1_Model03", "Preprocessor1_Model04"),
      c("Preprocessor1_Model05", "Preprocessor1_Model06", "Preprocessor1_Model07"),
      c("Preprocessor1_Model08", "Preprocessor1_Model09", "Preprocessor1_Model10")
    )
  )
  expect_equal(
    res$.msg_model[res$.iter_preprocessor == 1],
    paste0("preprocessor 1/5, model ", 1:3, "/3")
  )
  expect_equal(
    res$.submodels[1:3],
    list(
      list(trees = c(1L, 1000L, 1000L)),
      list(trees = c(1L, 1000L)),
      list(trees = c(1L, 1000L))
    )
  )
  expect_equal(
    res %>%
      mutate(num_models = purrr::map_int(.iter_config, length)) %>%
      summarize(n = sum(num_models), .by = c(deg_free)),
    grid %>% count(deg_free)
  )
  expect_named(
    res,
    c(".iter_preprocessor", ".msg_preprocessor", "deg_free", "trees",
      "loss_reduction", ".iter_model", ".iter_config", ".msg_model", ".submodels"),
    ignore.order = TRUE
  )
  expect_equal(nrow(res), 11)
})

test_that("compute_grid_info - model (with and without submodels)", {
  library(workflows)
  library(parsnip)
  library(recipes)
  library(dials)

  rec <- recipe(mpg ~ ., mtcars)
  spec <- mars(num_terms = tune(), prod_degree = tune(), prune_method = tune()) %>%
    set_mode("classification") %>%
    set_engine("earth")

  wflow <- workflow()
  wflow <- add_model(wflow, spec)
  wflow <- add_recipe(wflow, rec)

  set.seed(123)
  params_grid <- grid_space_filling(
    num_terms() %>% range_set(c(1L, 12L)),
    prod_degree(),
    prune_method(values = c("backward", "none", "forward")),
    size = 7,
    type = "latin_hypercube"
  )

  res <- compute_grid_info(wflow, params_grid)

  expect_equal(res$.iter_preprocessor, rep(1, 5))
  expect_equal(res$.msg_preprocessor, rep("preprocessor 1/1", 5))
  expect_equal(length(unique(res$num_terms)), 5)
  expect_equal(res$.iter_model, 1:5)
  expect_equal(
    res$.iter_config,
    list(
      c("Preprocessor1_Model1", "Preprocessor1_Model2"),
      c("Preprocessor1_Model3", "Preprocessor1_Model4"),
      "Preprocessor1_Model5", "Preprocessor1_Model6", "Preprocessor1_Model7"
    )
  )
  expect_equal(
    unique(res$.msg_model),
    paste0("preprocessor 1/1, model ", 1:5,"/5")
  )
  expect_equal(
    res$.submodels,
    list(
      list(num_terms = c(1)),
      list(num_terms = c(3)),
      list(), list(), list()
    )
  )
  expect_named(
    res,
    c(".iter_preprocessor", ".msg_preprocessor", "num_terms", "prod_degree",
      "prune_method", ".iter_model", ".iter_config", ".msg_model", ".submodels"),
    ignore.order = TRUE
  )
  expect_equal(nrow(res), 5)
})

test_that("compute_grid_info - recipe and model (no submodels but has inner grid)", {
  library(workflows)
  library(parsnip)
  library(recipes)
  library(dials)

  set.seed(1)

  helper_objects <- helper_objects_tune()

  wflow <- workflow() %>%
    add_recipe(helper_objects$rec_tune_1) %>%
    add_model(helper_objects$svm_mod)

  pset <- extract_parameter_set_dials(wflow) %>%
    update(num_comp = dials::num_comp(c(1, 3)))

  grid <- dials::grid_regular(pset, levels = 3)

  res <- compute_grid_info(wflow, grid)

  expect_equal(res$.iter_preprocessor, rep(1:3, each = 3))
  expect_equal(res$.msg_preprocessor, rep(paste0("preprocessor ", 1:3, "/3"), each = 3))
  expect_equal(res$.iter_model, rep(1:3, times = 3))
  expect_equal(
    res$.iter_config,
    as.list(paste0(
      rep(paste0("Preprocessor", 1:3, "_Model"), each = 3),
      rep(1:3, times = 3)
    ))
  )
  expect_equal(
    unique(res$.msg_model),
    paste0(
      rep(paste0("preprocessor ", 1:3, "/3, model "), each = 3),
      paste0(rep(1:3, times = 3), "/3")
    )
  )
  expect_named(
    res,
    c("cost", "num_comp", ".submodels", ".iter_preprocessor", ".msg_preprocessor",
      ".iter_model", ".iter_config", ".msg_model"),
    ignore.order = TRUE
  )
  expect_equal(nrow(res), 9)
  expect_equal(vctrs::vec_unique_count(res$.iter_config), nrow(grid))
})

# ------------------------------------------------------------------------------

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

  expect_named(sched_pre_only, c("threshold", "disp_df", "second_stage"))
  expect_equal(nrow(sched_pre_only), nrow(grid_pre_only))

  # All of the other nested tibbles should be empty
  expect_equal(
    sched_pre_only %>%
      tidyr::unnest(second_stage) %>%
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


  expect_named(sched_rf_only, c("second_stage"))
  expect_equal(nrow(sched_rf_only), 1L)

  rf_n <- length(sched_rf_only$second_stage)
  for (i in 1:rf_n) {
    expect_named(sched_rf_only$second_stage[[i]], c("mtry", "predict_stage"))
    expect_equal(
      sched_rf_only$second_stage[[i]] %>%
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
  sched_bst <- tune:::get_tune_schedule(wflow_bst, prm_used_bst, grid_bst)
  min_n_only <- grid_bst %>% dplyr::distinct(min_n) %>% dplyr::arrange(min_n)
  trees_only <- grid_bst %>% dplyr::distinct(trees) %>% dplyr::arrange(trees)

  expect_named(sched_bst, c("second_stage"))
  expect_equal(nrow(sched_bst), 1L)

  bst_n <- length(sched_bst$second_stage)
  for (i in 1:rf_n) {
    expect_named(sched_bst$second_stage[[i]], c("min_n", "predict_stage", "trees"))

    expect_equal(
      sched_bst$second_stage[[i]] %>%
        dplyr::select(-trees, -predict_stage),
      min_n_only
    )

    for (j in seq_along(sched_bst$second_stage[[i]]$predict_stage)) {
      expect_named(
        sched_bst$second_stage[[i]]$predict_stage[[j]],
        c("trees", "post_stage"))
      expect_equal(
        sched_bst$second_stage[[i]]$predict_stage[[j]] %>%
          dplyr::select(trees),
        trees_only
      )
    }

    expect_equal(
      sched_bst$second_stage[[i]] %>%
        dplyr::select(-trees) %>%
        tidyr::unnest(predict_stage) %>%
        tidyr::unnest(post_stage) %>%
        dplyr::select(trees, min_n),
      grid_bst
    )
  }

  ### irregular design
  ## TODO trees ends up in the predict_stage tibbles
  grid_sfd_bst <- grid_space_filling(prm_used_bst, size = 5, type = "uniform")
  sched_sfd_bst <- tune:::get_tune_schedule(wflow_bst, prm_used_bst, grid_sfd_bst)

  expect_named(sched_sfd_bst, c("second_stage"))
  expect_equal(nrow(sched_sfd_bst), 1L)

  bst_n <- length(sched_sfd_bst$second_stage)
  for (i in 1:rf_n) {
    expect_named(sched_sfd_bst$second_stage[[i]], c("min_n", "predict_stage", "trees"))

    # TODO more here


    expect_equal(
      sched_sfd_bst$second_stage[[i]] %>%
        dplyr::select(-trees) %>%
        tidyr::unnest(predict_stage) %>%
        tidyr::unnest(post_stage) %>%
        dplyr::select(trees, min_n),
      grid_sfd_bst
    )
  }

})
