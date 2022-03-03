data("Chicago", package = "modeldata")

spline_grid <-
  tibble::tribble(
    ~imputation, ~threshold, ~deg_free, ~degree,
    3L,      0.088,       14L,      1L,
    6L,      0.058,        8L,      1L,
    8L,      0.051,       14L,      1L,
    9L,      0.007,       10L,      1L,
    1L,      0.032,       15L,      2L,
    8L,      0.018,        9L,      2L,
    1L,      0.036,        5L,      1L,
   10L,        0.1,       10L,      2L,
    9L,      0.094,       12L,      1L,
    4L,      0.025,       12L,      1L
  )

bst_grid <- tibble::tibble("funky name \n" = 1:4, rules = rep(c(TRUE, FALSE), each = 2))

# ------------------------------------------------------------------------------

test_that('recipe merges', {
  spline_rec <-
    recipes::recipe(ridership ~ ., data = head(Chicago)) %>%
    recipes::step_date(date) %>%
    recipes::step_holiday(date) %>%
    recipes::step_rm(date, dplyr::ends_with("away")) %>%
    recipes::step_impute_knn(recipes::all_predictors(), neighbors = tune("imputation")) %>%
    recipes::step_other(recipes::all_nominal(), threshold = tune()) %>%
    recipes::step_dummy(recipes::all_nominal()) %>%
    recipes::step_normalize(recipes::all_numeric_predictors()) %>%
    recipes::step_bs(recipes::all_predictors(), deg_free = tune(), degree = tune())

  expect_error(
    spline_updated <- merge(spline_rec, spline_grid),
    NA
  )
  check_merged_tibble(spline_updated)
  for (i in 1:nrow(spline_grid)) {
    expect_equal(
      spline_updated$x[[i]]$steps[[4]]$neighbors, spline_grid$imputation[[i]]
    )
    expect_equal(
      spline_updated$x[[i]]$steps[[5]]$threshold, spline_grid$threshold[[i]]
    )
    expect_equal(
      spline_updated$x[[i]]$steps[[8]]$deg_free, spline_grid$deg_free[[i]]
    )
    expect_equal(
      spline_updated$x[[i]]$steps[[8]]$degree, spline_grid$degree[[i]]
    )
  }

})

test_that('partially recipe merge', {
  spline_rec <-
    recipes::recipe(ridership ~ ., data = head(Chicago)) %>%
    recipes::step_date(date) %>%
    recipes::step_holiday(date) %>%
    recipes::step_rm(date, dplyr::ends_with("away")) %>%
    recipes::step_impute_knn(recipes::all_predictors(), neighbors = tune("imputation")) %>%
    recipes::step_other(recipes::all_nominal(), threshold = tune()) %>%
    recipes::step_dummy(recipes::all_nominal()) %>%
    recipes::step_normalize(recipes::all_numeric_predictors()) %>%
    recipes::step_bs(recipes::all_predictors(), deg_free = tune(), degree = tune())

  expect_error(
    spline_updated <- merge(spline_rec, spline_grid[, -1]),
    NA
  )
  check_merged_tibble(spline_updated, complete = FALSE)
  for (i in 1:nrow(spline_grid)) {
    expect_equal(
      spline_updated$x[[i]]$steps[[4]]$neighbors, tune("imputation")
    )
    expect_equal(
      spline_updated$x[[i]]$steps[[5]]$threshold, spline_grid$threshold[[i]]
    )
    expect_equal(
      spline_updated$x[[i]]$steps[[8]]$deg_free, spline_grid$deg_free[[i]]
    )
    expect_equal(
      spline_updated$x[[i]]$steps[[8]]$degree, spline_grid$degree[[i]]
    )
  }

})

test_that('umerged recipe merge', {
  spline_rec <-
    recipes::recipe(ridership ~ ., data = head(Chicago)) %>%
    recipes::step_date(date) %>%
    recipes::step_holiday(date) %>%
    recipes::step_rm(date, dplyr::ends_with("away")) %>%
    recipes::step_impute_knn(recipes::all_predictors(), neighbors = tune("imputation")) %>%
    recipes::step_other(recipes::all_nominal(), threshold = tune()) %>%
    recipes::step_dummy(recipes::all_nominal()) %>%
    recipes::step_normalize(recipes::all_numeric_predictors()) %>%
    recipes::step_bs(recipes::all_predictors(), deg_free = tune(), degree = tune())

  expect_error(
    spline_updated <- merge(spline_rec, bst_grid),
    NA
  )
  check_merged_tibble(spline_updated, complete = FALSE)
  for (i in 1:nrow(bst_grid)) {
    expect_equal(
      spline_updated$x[[i]]$steps[[4]]$neighbors, tune("imputation")
    )
    expect_equal(
      spline_updated$x[[i]]$steps[[5]]$threshold, tune()
    )
    expect_equal(
      spline_updated$x[[i]]$steps[[8]]$deg_free, tune()
    )
    expect_equal(
      spline_updated$x[[i]]$steps[[8]]$degree, tune()
    )
  }

})

# ------------------------------------------------------------------------------



test_that('model spec merges', {
  bst_model <-
    parsnip::boost_tree(mode = "classification", trees = tune("funky name \n")) %>%
    parsnip::set_engine("C5.0", rules = tune(), noGlobalPruning = TRUE)

  expect_error(
    bst_updated <- merge(bst_model, bst_grid),
    NA
  )
  check_merged_tibble(bst_updated, "model_spec")
  for (i in 1:nrow(bst_grid)) {
    expect_equal(
      bst_updated$x[[i]]$args$trees,
      rlang::as_quosure(bst_grid[["funky name \n"]][[i]], empty_env())
    )
    expect_equal(
      bst_updated$x[[i]]$eng_args$rules,
      rlang::as_quosure(bst_grid$rules[[i]], empty_env())
    )
  }

})

test_that('partially model spec merge', {
  bst_model <-
    parsnip::boost_tree(mode = "classification", trees = tune("funky name \n")) %>%
    parsnip::set_engine("C5.0", rules = tune(), noGlobalPruning = TRUE)

  expect_error(
    bst_updated <- merge(bst_model, bst_grid[, -1]),
    NA
  )
  check_merged_tibble(bst_updated, "model_spec", complete = FALSE)
  for (i in 1:nrow(bst_grid)) {
    expect_equal(
      rlang::get_expr(bst_updated$x[[i]]$args$trees), tune("funky name \n")
    )
    expect_equal(
      bst_updated$x[[i]]$eng_args$rules,
      rlang::as_quosure(bst_grid$rules[[i]], empty_env())
    )
  }

})

test_that('umerged model spec merge', {
  bst_model <-
    parsnip::boost_tree(mode = "classification", trees = tune("funky name \n")) %>%
    parsnip::set_engine("C5.0", rules = tune(), noGlobalPruning = TRUE)

  other_grid <- bst_grid
  names(bst_grid) <- letters[1:2]
  expect_error(
    bst_not_updated <- merge(bst_model, other_grid),
    NA
  )
  check_merged_tibble(bst_not_updated, "model_spec", complete = FALSE)
  # for (i in 1:nrow(other_grid)) {
  #   if (i == 1) {
  #     print(rlang::get_expr(bst_not_updated$x[[i]]$args$trees))
  #     print(rlang::get_expr(bst_not_updated$x[[i]]$eng_args$rules))
  #   }
  #   expect_equal(
  #     rlang::get_expr(bst_not_updated$x[[i]]$args$trees), tune("funky name \n")
  #   )
  #   expect_equal(
  #     rlang::get_expr(bst_not_updated$x[[i]]$eng_args$rules), tune()
  #   )
  # }

})

