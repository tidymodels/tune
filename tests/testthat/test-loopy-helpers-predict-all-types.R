test_that("predict classification - no submodels - no calibration", {
  skip_if_not_installed("modeldata")

  cls <- make_post_data()

  pca_rec <- recipe(class ~ ., data = cls$data) |>
    step_pca(all_numeric_predictors(), num_comp = 2)

  wflow <- workflow(pca_rec, logistic_reg())
  wflow_fit <- fit(wflow, cls$data)

  class_only <- metric_set(accuracy)
  prob_only <- metric_set(brier_class)
  both_types <- metric_set(brier_class, accuracy)

  data_1 <- tune:::get_data_subsets(wflow, cls$rs$splits[[1]], cls$args)

  fac_0 <- factor(levels = levels(cls$data$class))

  ctrl <- tune::control_grid()

  # ----------------------------------------------------------------------------
  # Only predict classes

  static_class <- tune:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = class_only,
    eval_time = NULL,
    split_args = cls$args,
    control = ctrl
  )

  static_class <- tune:::update_static(static_class, data_1)
  static_class$y_name <- "class"

  class_res <- tune:::predict_all_types(
    wflow_fit,
    static_class,
    submodel_grid = NULL,
    predictee = "assessment"
  )

  expect_equal(
    class_res[0, ],
    tibble(.pred_class = fac_0, .row = integer(0), class = fac_0)
  )
  expect_equal(nrow(class_res), nrow(assessment(cls$rs$splits[[1]])))

  # ----------------------------------------------------------------------------
  # Only predict probabilities

  static_prob <- tune:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = prob_only,
    eval_time = NULL,
    split_args = cls$args,
    control = ctrl
  )

  static_prob <- tune:::update_static(static_prob, data_1)
  static_prob$y_name <- "class"

  prob_res <- tune:::predict_all_types(
    wflow_fit,
    static_prob,
    submodel_grid = NULL,
    predictee = "assessment"
  )

  expect_equal(
    prob_res[0, ],
    tibble(
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      .row = integer(0),
      class = fac_0
    )
  )
  expect_equal(nrow(prob_res), nrow(assessment(cls$rs$splits[[1]])))

  # ----------------------------------------------------------------------------
  # Both prediction types

  static_both <- tune:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = both_types,
    eval_time = NULL,
    split_args = cls$args,
    control = ctrl
  )

  static_both <- tune:::update_static(static_both, data_1)
  static_both$y_name <- "class"

  both_res <- tune:::predict_all_types(
    wflow_fit,
    static_both,
    submodel_grid = NULL,
    predictee = "assessment"
  )

  expect_equal(
    both_res[0, ],
    tibble(
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(both_res), nrow(assessment(cls$rs$splits[[1]])))

  # ------------------------------------------------------------------------------
  # bad arg

  expect_snapshot(
    tune:::predict_all_types(
      wflow_fit,
      static_both,
      submodel_grid = NULL,
      predictee = "potato"
    ),
    error = TRUE
  )

  static_bad <- static_both
  static_bad$post_estimation <- TRUE
  expect_snapshot(
    tune:::predict_all_types(
      wflow_fit,
      static_bad,
      submodel_grid = NULL,
      predictee = "calibration"
    ),
    error = TRUE
  )
})

test_that("predict classification - no submodels - with calibration", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("probably")
  skip_if_not_installed("mgcv")

  cls <- make_post_data()

  pca_rec <- recipe(class ~ ., data = cls$data) |>
    step_pca(all_numeric_predictors(), num_comp = 2)

  cal_pst <- tailor() |> adjust_probability_calibration()

  wflow <- workflow(pca_rec, logistic_reg(), cal_pst)
  wflow_fit <- fit(wflow, cls$data, calibration = cls$data)

  class_only <- metric_set(accuracy)
  prob_only <- metric_set(brier_class)
  both_types <- metric_set(brier_class, accuracy)

  data_1 <- tune:::get_data_subsets(wflow, cls$rs$splits[[1]], cls$args)

  fac_0 <- factor(levels = levels(cls$data$class))

  ctrl <- tune::control_grid()

  # ----------------------------------------------------------------------------
  # Only predict classes

  static_class <- tune:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = class_only,
    eval_time = NULL,
    split_args = cls$args,
    control = ctrl
  )

  static_class <- tune:::update_static(static_class, data_1)
  static_class$y_name <- "class"

  class_res_prd <- tune:::predict_all_types(
    wflow_fit,
    static_class,
    submodel_grid = NULL,
    predictee = "assessment"
  )

  expect_equal(
    class_res_prd[0, ],
    tibble(
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(class_res_prd), nrow(data_1$pred$data))

  ###

  class_res_cal <- tune:::predict_all_types(
    wflow_fit,
    static_class,
    submodel_grid = NULL,
    predictee = "calibration"
  )

  expect_equal(
    class_res_cal[0, ],
    tibble(
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(class_res_cal), nrow(data_1$cal$data))

  # ----------------------------------------------------------------------------
  # Only predict probabilities

  static_prob <- tune:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = prob_only,
    eval_time = NULL,
    split_args = cls$args,
    control = ctrl
  )

  static_prob <- tune:::update_static(static_prob, data_1)
  static_prob$y_name <- "class"

  prob_res_prd <- tune:::predict_all_types(
    wflow_fit,
    static_prob,
    submodel_grid = NULL,
    predictee = "assessment"
  )

  expect_equal(
    prob_res_prd[0, ],
    tibble(
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(prob_res_prd), nrow(data_1$pred$data))

  ###

  prob_res_cal <- tune:::predict_all_types(
    wflow_fit,
    static_prob,
    submodel_grid = NULL,
    predictee = "calibration"
  )

  expect_equal(
    prob_res_cal[0, ],
    tibble(
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(prob_res_cal), nrow(data_1$cal$data))

  # ----------------------------------------------------------------------------
  # Both prediction types

  static_both <- tune:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = both_types,
    eval_time = NULL,
    split_args = cls$args,
    control = ctrl
  )

  static_both <- tune:::update_static(static_both, data_1)
  static_both$y_name <- "class"

  both_res_prd <- tune:::predict_all_types(
    wflow_fit,
    static_both,
    submodel_grid = NULL,
    predictee = "assessment"
  )

  expect_equal(
    both_res_prd[0, ],
    tibble(
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(prob_res_prd), nrow(data_1$pred$data))

  ###

  both_res_cal <- tune:::predict_all_types(
    wflow_fit,
    static_both,
    submodel_grid = NULL,
    predictee = "calibration"
  )

  expect_equal(
    both_res_cal[0, ],
    tibble(
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(both_res_cal), nrow(data_1$cal$data))
})

test_that("predict classification - with submodels - no calibration", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("kknn")

  cls <- make_post_data()

  pca_rec <- recipe(class ~ ., data = cls$data) |>
    step_pca(all_numeric_predictors(), num_comp = 2)

  knn_cls_spec <- parsnip::nearest_neighbor(
    mode = "classification",
    neighbors = 12
  )

  five_neighbors <- tibble(neighbors = 5)

  wflow <- workflow(pca_rec, knn_cls_spec)
  wflow_fit <- fit(wflow, cls$data)

  class_only <- metric_set(accuracy)
  prob_only <- metric_set(brier_class)
  both_types <- metric_set(brier_class, accuracy)

  data_1 <- tune:::get_data_subsets(wflow, cls$rs$splits[[1]], cls$args)

  fac_0 <- factor(levels = levels(cls$data$class))

  ctrl <- tune::control_grid()

  # ----------------------------------------------------------------------------
  # Only predict classes

  static_class <- tune:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = class_only,
    eval_time = NULL,
    split_args = cls$args,
    control = ctrl
  )

  static_class <- tune:::update_static(static_class, data_1)
  static_class$y_name <- "class"

  class_res <- tune:::predict_all_types(
    wflow_fit,
    static_class,
    submodel_grid = five_neighbors,
    predictee = "assessment"
  )

  expect_equal(
    class_res[0, ],
    tibble(
      neighbors = double(0),
      .pred_class = fac_0,
      .row = integer(0),
      class = fac_0
    )
  )
  expect_equal(nrow(class_res), nrow(assessment(cls$rs$splits[[1]])))

  # ----------------------------------------------------------------------------
  # Only predict probabilities

  static_prob <- tune:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = prob_only,
    eval_time = NULL,
    split_args = cls$args,
    control = ctrl
  )

  static_prob <- tune:::update_static(static_prob, data_1)
  static_prob$y_name <- "class"

  prob_res <- tune:::predict_all_types(
    wflow_fit,
    static_prob,
    submodel_grid = five_neighbors,
    predictee = "assessment"
  )

  expect_equal(
    prob_res[0, ],
    tibble(
      neighbors = double(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      .row = integer(0),
      class = fac_0
    )
  )
  expect_equal(nrow(prob_res), nrow(assessment(cls$rs$splits[[1]])))

  # ----------------------------------------------------------------------------
  # Both prediction types

  static_both <- tune:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = both_types,
    eval_time = NULL,
    split_args = cls$args,
    control = ctrl
  )

  static_both <- tune:::update_static(static_both, data_1)
  static_both$y_name <- "class"

  both_res <- tune:::predict_all_types(
    wflow_fit,
    static_both,
    submodel_grid = five_neighbors,
    predictee = "assessment"
  )

  expect_equal(
    both_res[0, ],
    tibble(
      neighbors = double(0),
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(both_res), nrow(assessment(cls$rs$splits[[1]])))

  # ------------------------------------------------------------------------------
  # bad arg

  expect_snapshot(
    tune:::predict_all_types(
      wflow_fit,
      static_both,
      submodel_grid = five_neighbors,
      predictee = "potato"
    ),
    error = TRUE
  )

  static_bad <- static_both
  static_bad$post_estimation <- TRUE
  expect_snapshot(
    tune:::predict_all_types(
      wflow_fit,
      static_bad,
      submodel_grid = five_neighbors,
      predictee = "calibration"
    ),
    error = TRUE
  )
})

test_that("predict classification - with submodels - with calibration", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("kknn")
  skip_if_not_installed("probably")
  skip_if_not_installed("mgcv")

  cls <- make_post_data()

  pca_rec <- recipe(class ~ ., data = cls$data) |>
    step_pca(all_numeric_predictors(), num_comp = 2)

  knn_cls_spec <- parsnip::nearest_neighbor(
    mode = "classification",
    neighbors = 12
  )

  five_neighbors <- tibble(neighbors = 5)

  cal_pst <- tailor() |> adjust_probability_calibration()

  wflow <- workflow(pca_rec, knn_cls_spec, cal_pst)
  wflow_fit <- fit(wflow, cls$data, calibration = cls$data)

  class_only <- metric_set(accuracy)
  prob_only <- metric_set(brier_class)
  both_types <- metric_set(brier_class, accuracy)

  data_1 <- tune:::get_data_subsets(wflow, cls$rs$splits[[1]], cls$args)

  fac_0 <- factor(levels = levels(cls$data$class))

  ctrl <- tune::control_grid()

  # ----------------------------------------------------------------------------
  # Only predict classes

  static_class <- tune:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = class_only,
    eval_time = NULL,
    split_args = cls$args,
    control = ctrl
  )

  static_class <- tune:::update_static(static_class, data_1)
  static_class$y_name <- "class"

  class_res_prd <- tune:::predict_all_types(
    wflow_fit,
    static_class,
    submodel_grid = five_neighbors,
    predictee = "assessment"
  )

  expect_equal(
    class_res_prd[0, ],
    tibble(
      neighbors = double(0),
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(class_res_prd), nrow(data_1$pred$data))

  ###

  class_res_cal <- tune:::predict_all_types(
    wflow_fit,
    static_class,
    submodel_grid = five_neighbors,
    predictee = "calibration"
  )

  expect_equal(
    class_res_cal[0, ],
    tibble(
      neighbors = double(0),
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(class_res_cal), nrow(data_1$cal$data))

  # ----------------------------------------------------------------------------
  # Only predict probabilities

  static_prob <- tune:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = prob_only,
    eval_time = NULL,
    split_args = cls$args,
    control = ctrl
  )

  static_prob <- tune:::update_static(static_prob, data_1)
  static_prob$y_name <- "class"

  prob_res_prd <- tune:::predict_all_types(
    wflow_fit,
    static_prob,
    submodel_grid = five_neighbors,
    predictee = "assessment"
  )

  expect_equal(
    prob_res_prd[0, ],
    tibble(
      neighbors = double(0),
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(prob_res_prd), nrow(data_1$pred$data))

  ###

  prob_res_cal <- tune:::predict_all_types(
    wflow_fit,
    static_prob,
    submodel_grid = five_neighbors,
    predictee = "calibration"
  )

  expect_equal(
    prob_res_cal[0, ],
    tibble(
      neighbors = double(0),
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(prob_res_cal), nrow(data_1$cal$data))

  # ----------------------------------------------------------------------------
  # Both prediction types

  static_both <- tune:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = both_types,
    eval_time = NULL,
    split_args = cls$args,
    control = ctrl
  )

  static_both <- tune:::update_static(static_both, data_1)
  static_both$y_name <- "class"

  both_res_prd <- tune:::predict_all_types(
    wflow_fit,
    static_both,
    submodel_grid = five_neighbors,
    predictee = "assessment"
  )

  expect_equal(
    both_res_prd[0, ],
    tibble(
      neighbors = double(0),
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(prob_res_prd), nrow(data_1$pred$data))

  ###

  both_res_cal <- tune:::predict_all_types(
    wflow_fit,
    static_both,
    submodel_grid = five_neighbors,
    predictee = "calibration"
  )

  expect_equal(
    both_res_cal[0, ],
    tibble(
      neighbors = double(0),
      .pred_class = fac_0,
      .row = integer(0),
      .pred_class_1 = double(0),
      .pred_class_2 = double(0),
      class = fac_0
    )
  )
  expect_equal(nrow(both_res_cal), nrow(data_1$cal$data))
})

test_that("predict regression - no submodels - no calibration", {
  skip_if_not_installed("modeldata")

  reg <- make_post_data(mode = "regression")

  pca_rec <- recipe(outcome ~ ., data = reg$data) |>
    step_pca(all_numeric_predictors(), num_comp = 2)

  wflow <- workflow(pca_rec, linear_reg())
  wflow_fit <- fit(wflow, reg$data)

  reg_mtr <- metric_set(rmse)

  data_1 <- tune:::get_data_subsets(wflow, reg$rs$splits[[1]], reg$args)

  ctrl <- tune::control_grid()

  # ----------------------------------------------------------------------------

  static <- tune:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = reg_mtr,
    eval_time = NULL,
    split_args = reg$args,
    control = ctrl
  )

  static <- tune:::update_static(static, data_1)
  static$y_name <- "outcome"

  class_res <- tune:::predict_all_types(
    wflow_fit,
    static,
    submodel_grid = NULL,
    predictee = "assessment"
  )

  expect_equal(
    class_res[0, ],
    tibble(.pred = double(0), .row = integer(0), outcome = double(0))
  )
  expect_equal(nrow(class_res), nrow(assessment(reg$rs$splits[[1]])))
})

test_that("predict regression - no submodels - with calibration", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("probably")
  skip_if_not_installed("mgcv")

  reg <- make_post_data(mode = "regression")

  pca_rec <- recipe(outcome ~ ., data = reg$data) |>
    step_pca(all_numeric_predictors(), num_comp = 2)

  reg_pst <- tailor() |> adjust_numeric_calibration()

  wflow <- workflow(pca_rec, linear_reg(), reg_pst)
  wflow_fit <- fit(wflow, reg$data, calibration = reg$data)

  reg_mtr <- metric_set(rmse)

  data_1 <- tune:::get_data_subsets(wflow, reg$rs$splits[[1]], reg$args)

  ctrl <- tune::control_grid()

  # ----------------------------------------------------------------------------

  static <- tune:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = reg_mtr,
    eval_time = NULL,
    split_args = reg$args,
    control = ctrl
  )

  static <- tune:::update_static(static, data_1)
  static$y_name <- "outcome"

  class_res_prd <- tune:::predict_all_types(
    wflow_fit,
    static,
    submodel_grid = NULL,
    predictee = "assessment"
  )

  expect_equal(
    class_res_prd[0, ],
    tibble(
      .pred = double(0),
      .row = integer(0),
      outcome = double(0)
    )
  )
  expect_equal(nrow(class_res_prd), nrow(data_1$pred$data))
})

test_that("predict regression - with submodels - no calibration", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("kknn")

  reg <- make_post_data(mode = "regression")

  pca_rec <- recipe(outcome ~ ., data = reg$data) |>
    step_pca(all_numeric_predictors(), num_comp = 2)

  knn_reg_spec <- parsnip::nearest_neighbor(
    mode = "regression",
    neighbors = 12
  )

  five_neighbors <- tibble(neighbors = 5)

  wflow <- workflow(pca_rec, knn_reg_spec)
  wflow_fit <- fit(wflow, reg$data)

  reg_mtr <- metric_set(rmse)

  data_1 <- tune:::get_data_subsets(wflow, reg$rs$splits[[1]], reg$args)

  ctrl <- tune::control_grid()

  # ----------------------------------------------------------------------------

  static <- tune:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = reg_mtr,
    eval_time = NULL,
    split_args = reg$args,
    control = ctrl
  )

  static <- tune:::update_static(static, data_1)
  static$y_name <- "outcome"

  class_res <- tune:::predict_all_types(
    wflow_fit,
    static,
    submodel_grid = five_neighbors,
    predictee = "assessment"
  )

  expect_equal(
    class_res[0, ],
    tibble(
      neighbors = double(0),
      .pred = double(0),
      .row = integer(0),
      outcome = double(0)
    )
  )
  expect_equal(nrow(class_res), nrow(assessment(reg$rs$splits[[1]])))
})

test_that("predict regression - with submodels - with calibration", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("kknn")
  skip_if_not_installed("probably")
  skip_if_not_installed("mgcv")

  reg <- make_post_data(mode = "regression")

  pca_rec <- recipe(outcome ~ ., data = reg$data) |>
    step_pca(all_numeric_predictors(), num_comp = 2)

  knn_reg_spec <- parsnip::nearest_neighbor(
    mode = "regression",
    neighbors = 12
  )

  five_neighbors <- tibble(neighbors = 5)

  reg_pst <- tailor() |> adjust_numeric_calibration()

  wflow <- workflow(pca_rec, knn_reg_spec, reg_pst)
  wflow_fit <- fit(wflow, reg$data, calibration = reg$data)

  reg_mtr <- metric_set(rmse)

  data_1 <- tune:::get_data_subsets(wflow, reg$rs$splits[[1]], reg$args)

  ctrl <- tune::control_grid()

  # ----------------------------------------------------------------------------

  static <- tune:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = reg_mtr,
    eval_time = NULL,
    split_args = reg$args,
    control = ctrl
  )

  static <- tune:::update_static(static, data_1)
  static$y_name <- "outcome"

  class_res_prd <- tune:::predict_all_types(
    wflow_fit,
    static,
    submodel_grid = five_neighbors,
    predictee = "assessment"
  )

  expect_equal(
    class_res_prd[0, ],
    tibble(
      neighbors = double(0),
      .pred = double(0),
      .row = integer(0),
      outcome = double(0)
    )
  )
  expect_equal(nrow(class_res_prd), nrow(data_1$pred$data))

  ###

  class_res_cal <- tune:::predict_all_types(
    wflow_fit,
    static,
    submodel_grid = five_neighbors,
    predictee = "calibration"
  )

  expect_equal(
    class_res_cal[0, ],
    tibble(
      neighbors = double(0),
      .pred = double(0),
      .row = integer(0),
      outcome = double(0)
    )
  )
  expect_equal(nrow(class_res_cal), nrow(data_1$cal$data))
})

test_that("predict censored regression - no submodels - no calibration", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("censored")
  skip_if_not_installed("survival")

  library(censored)

  cens <- make_post_data(mode = "censored")

  pca_rec <- recipe(outcome ~ ., data = cens$data) |>
    step_pca(all_numeric_predictors(), num_comp = 2)

  wflow <- workflow(pca_rec, survival_reg())
  wflow_fit <- fit(wflow, cens$data)

  mtr_stc <- metric_set(concordance_survival)
  mtr_dyn <- metric_set(brier_survival)
  mtr_int <- metric_set(brier_survival_integrated)
  mtr_all <- metric_set(
    concordance_survival,
    brier_survival,
    brier_survival_integrated
  )

  .times <- c(15, 25)

  data_1 <- tune:::get_data_subsets(wflow, cens$rs$splits[[1]], cens$args)

  ctrl <- tune::control_grid()

  # ----------------------------------------------------------------------------
  # static metrics

  static_stc <- tune:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = mtr_stc,
    eval_time = .times,
    split_args = cens$args,
    control = ctrl
  )

  static_stc <- tune:::update_static(static_stc, data_1)
  static_stc$y_name <- "outcome"

  res_stc <- tune:::predict_all_types(
    wflow_fit,
    static_stc,
    submodel_grid = NULL,
    predictee = "assessment"
  )

  expect_equal(
    res_stc[0, ],
    tibble(.pred_time = numeric(0), .row = integer(0), outcome = surv_0)
  )
  expect_equal(nrow(res_stc), nrow(assessment(cens$rs$splits[[1]])))

  # ----------------------------------------------------------------------------
  # dynamic metrics

  static_dyn <- tune:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = mtr_dyn,
    eval_time = .times,
    split_args = cens$args,
    control = ctrl
  )

  static_dyn <- tune:::update_static(static_dyn, data_1)
  static_dyn$y_name <- "outcome"

  res_dyn <- tune:::predict_all_types(
    wflow_fit,
    static_dyn,
    submodel_grid = NULL,
    predictee = "assessment"
  )

  expect_equal(
    res_dyn[0, ],
    tibble(.pred = list(), .row = integer(0), outcome = surv_0)
  )

  expect_equal(
    res_dyn$.pred[[1]][0, ],
    pred_0
  )
  expect_equal(nrow(res_dyn), nrow(assessment(cens$rs$splits[[1]])))
})

# TODO put this in extratests
test_that("predict censored regression - submodels - no calibration", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("censored")
  skip_if_not_installed("survival")
  skip_if_not_installed("glmnet")

  skip("not working")

  library(censored)

  cens <- make_post_data(mode = "censored")

  pca_rec <- recipe(outcome ~ ., data = cens$data) |>
    step_pca(all_numeric_predictors(), num_comp = 2)

  glmn_cens <- proportional_hazards(penalty = tune()) |> set_engine("glmnet")

  wflow <- workflow(pca_rec, glmn_cens)
  wflow_fit <- fit(wflow, cens$data)

  mtr_stc <- metric_set(concordance_survival)
  mtr_dyn <- metric_set(brier_survival)
  mtr_int <- metric_set(brier_survival_integrated)
  mtr_all <- metric_set(
    concordance_survival,
    brier_survival,
    brier_survival_integrated
  )

  .times <- c(15, 25)

  data_1 <- tune:::get_data_subsets(wflow, cens$rs$splits[[1]], cens$args)

  ctrl <- tune::control_grid()

  # ----------------------------------------------------------------------------
  # static metrics

  static_stc <- tune:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = mtr_stc,
    eval_time = .times,
    split_args = cens$args,
    control = ctrl
  )

  static_stc <- tune:::update_static(static_stc, data_1)
  static_stc$y_name <- "outcome"

  # TODO error
  # Error in lambda[1] - s : non-numeric argument to binary operator
  # Called from: lambda.interp(lambda, s)

  res_stc <- tune:::predict_all_types(
    wflow_fit,
    static_stc,
    submodel_grid = NULL,
    predictee = "assessment"
  )

  expect_equal(
    res_stc[0, ],
    tibble(.pred_time = numeric(0), .row = integer(0), outcome = surv_0)
  )
  expect_equal(nrow(res_stc), nrow(assessment(cens$rs$splits[[1]])))

  # ----------------------------------------------------------------------------
  # dynamic metrics

  static_dyn <- tune:::make_static(
    wflow,
    param_info = wflow |> extract_parameter_set_dials(),
    metrics = mtr_dyn,
    eval_time = .times,
    split_args = cens$args,
    control = ctrl
  )

  static_dyn <- tune:::update_static(static_dyn, data_1)
  static_dyn$y_name <- "outcome"

  res_dyn <- tune:::predict_all_types(
    wflow_fit,
    static_dyn,
    submodel_grid = NULL,
    predictee = "assessment"
  )

  expect_equal(
    res_dyn[0, ],
    tibble(.pred = list(), .row = integer(0), outcome = surv_0)
  )

  expect_equal(
    res_dyn$.pred[[1]][0, ],
    pred_0
  )
  expect_equal(nrow(res_dyn), nrow(assessment(cens$rs$splits[[1]])))
})
