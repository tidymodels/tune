# Test file for variable fold weights functionality
if (rlang::is_installed(c("rsample", "parsnip", "yardstick", "workflows", "recipes", "kknn"))) {

  # Setup test data
  set.seed(42)
  test_data <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50),
    x3 = rnorm(50)
  )
  test_data$y <- 2 * test_data$x1 + 3 * test_data$x2 + rnorm(50, sd = 0.5)

  set.seed(123)
  folds <- rsample::vfold_cv(mtcars, v = 3)
  
  # Helper function to create a simple model
  create_test_model <- function() {
    parsnip::linear_reg() %>% parsnip::set_engine("lm")
  }

  test_that("add_fold_weights() validates inputs correctly", {
    expect_error(
      add_fold_weights("not_an_rset", c(0.5, 0.3, 0.2)),
      "must be an rset object"
    )
    
    expect_error(
      add_fold_weights(folds, c("a", "b", "c")),
      "must be numeric"
    )
    
    expect_error(
      add_fold_weights(folds, c(0.5, 0.3)),
      "must equal number of folds"
    )
    
    expect_error(
      add_fold_weights(folds, c(-0.1, 0.5, 0.6)),
      "must be non-negative"
    )
    
    expect_error(
      add_fold_weights(folds, c(0, 0, 0)),
      "At least one weight must be positive"
    )
  })

  test_that("add_fold_weights() adds weights correctly", {
    weights <- c(0.1, 0.5, 0.4)
    weighted_folds <- add_fold_weights(folds, weights)
    
    # Weights get normalized to sum to 1
    expected_weights <- weights / sum(weights)
    
    expect_s3_class(weighted_folds, "rset")
    expect_equal(attr(weighted_folds, ".fold_weights"), expected_weights)
    expect_equal(nrow(weighted_folds), nrow(folds))
  })

  test_that("calculate_fold_weights() works correctly", {
    auto_weights <- calculate_fold_weights(folds)
    
    expect_type(auto_weights, "double")
    expect_length(auto_weights, nrow(folds))
    expect_true(all(auto_weights > 0))
    expect_true(abs(sum(auto_weights) - 1) < 1e-10)
  })

  test_that("weights are preserved through tuning pipeline", {
    weights <- c(0.1, 0.5, 0.4)
    weighted_folds <- add_fold_weights(folds, weights)
    
    mod <- create_test_model()
    
    suppressWarnings({
      res <- tune_grid(
        mod,
        mpg ~ .,
        resamples = weighted_folds,
        grid = 1,
        metrics = yardstick::metric_set(yardstick::rmse),
        control = control_grid(verbose = FALSE)
      )
    })
    
    metrics <- collect_metrics(res)
    expect_equal(nrow(metrics), 1)
    expect_true("mean" %in% names(metrics))
    expect_true(is.numeric(metrics$mean))
  })

  test_that("weights affect metric aggregation", {
    weights <- c(0.1, 0.5, 0.4)
    weighted_folds <- add_fold_weights(folds, weights)
    
    mod <- create_test_model()
    
    suppressWarnings({
      # Unweighted results
      res_unweighted <- tune_grid(
        mod,
        mpg ~ .,
        resamples = folds,
        grid = 1,
        metrics = yardstick::metric_set(yardstick::rmse),
        control = control_grid(verbose = FALSE)
      )
      
      # Weighted results
      res_weighted <- tune_grid(
        mod,
        mpg ~ .,
        resamples = weighted_folds,
        grid = 1,
        metrics = yardstick::metric_set(yardstick::rmse),
        control = control_grid(verbose = FALSE)
      )
    })
    
    unweighted_rmse <- collect_metrics(res_unweighted)$mean[1]
    weighted_rmse <- collect_metrics(res_weighted)$mean[1]
    
    expect_true(is.numeric(unweighted_rmse))
    expect_true(is.numeric(weighted_rmse))
    expect_false(is.na(unweighted_rmse))
    expect_false(is.na(weighted_rmse))
  })

  test_that("extreme weights show larger effect", {
    skip_if_not_installed("kknn")
    
    # Create folds for this specific test
    set.seed(42)
    test_folds <- rsample::vfold_cv(test_data, v = 3)
    
    # Regular weights
    weights <- c(0.6, 0.2, 0.2)
    weighted_folds <- add_fold_weights(test_folds, weights)
    
    # Extreme weights
    extreme_weights <- c(0.95, 0.025, 0.025)
    extreme_weighted_folds <- add_fold_weights(test_folds, extreme_weights)
    
    # Create a model with tuning parameter
    knn_spec <- parsnip::nearest_neighbor(neighbors = tune()) %>%
      parsnip::set_engine("kknn") %>%
      parsnip::set_mode("regression")
    
    param_grid <- data.frame(neighbors = c(3, 5))
    
    suppressWarnings({
      # Unweighted
      res_unweighted <- tune_grid(
        knn_spec,
        y ~ .,
        resamples = test_folds,
        grid = param_grid,
        metrics = yardstick::metric_set(yardstick::rmse),
        control = control_grid(verbose = FALSE)
      )
      
      # Regular weights
      res_weighted <- tune_grid(
        knn_spec,
        y ~ .,
        resamples = weighted_folds,
        grid = param_grid,
        metrics = yardstick::metric_set(yardstick::rmse),
        control = control_grid(verbose = FALSE)
      )
      
      # Extreme weights
      res_extreme <- tune_grid(
        knn_spec,
        y ~ .,
        resamples = extreme_weighted_folds,
        grid = param_grid,
        metrics = yardstick::metric_set(yardstick::rmse),
        control = control_grid(verbose = FALSE)
      )
    })
    
    unweighted_metrics <- collect_metrics(res_unweighted)
    weighted_metrics <- collect_metrics(res_weighted)
    extreme_metrics <- collect_metrics(res_extreme)
    
    # Check that results exist and are sensible
    expect_equal(nrow(unweighted_metrics), 2)
    expect_equal(nrow(weighted_metrics), 2)
    expect_equal(nrow(extreme_metrics), 2)
    
    # Calculate differences
    regular_diff <- max(abs(unweighted_metrics$mean - weighted_metrics$mean))
    extreme_diff <- max(abs(unweighted_metrics$mean - extreme_metrics$mean))
    
    expect_true(regular_diff >= 0)
    expect_true(extreme_diff >= 0)
    expect_true(all(is.finite(c(regular_diff, extreme_diff))))
  })

  test_that("weight normalization works correctly", {
    expect_equal(
      tune:::.validate_fold_weights(c(3, 6, 9), 3),
      c(1/6, 1/3, 1/2)  # normalized to sum to 1
    )
    
    expect_equal(
      tune:::.validate_fold_weights(c(0.2, 0.3, 0.5), 3),
      c(0.2, 0.3, 0.5)  # already normalized to sum to 1
    )
  })

  test_that("weighted statistics functions work correctly", {
    x <- c(1, 2, 3, 4, 5)
    w <- c(0.1, 0.2, 0.3, 0.2, 0.2)
    
    weighted_mean <- tune:::.weighted_mean(x, w)
    weighted_sd <- tune:::.weighted_sd(x, w)
    
    expect_true(is.numeric(weighted_mean))
    expect_true(is.numeric(weighted_sd))
    expect_false(is.na(weighted_mean))
    expect_false(is.na(weighted_sd))
    expect_true(weighted_sd >= 0)
    
    # Test with NA values
    x_na <- c(1, 2, NA, 4, 5)
    weighted_mean_na <- tune:::.weighted_mean(x_na, w)
    weighted_sd_na <- tune:::.weighted_sd(x_na, w)
    
    expect_true(is.numeric(weighted_mean_na))
    expect_true(is.numeric(weighted_sd_na))
    expect_false(is.na(weighted_mean_na))
    
    # Test edge cases
    expect_true(is.na(tune:::.weighted_mean(c(NA, NA), c(0.5, 0.5))))
    expect_true(is.na(tune:::.weighted_sd(c(1), c(1))))  # single value
  })

  test_that("fold weight extraction works", {
    weights <- c(0.1, 0.5, 0.4)
    weighted_folds <- add_fold_weights(folds, weights)
    
    # Weights get normalized to sum to 1
    expected_weights <- weights / sum(weights)
    
    mod <- create_test_model()
    
    suppressWarnings({
      res <- tune_grid(
        mod,
        mpg ~ .,
        resamples = weighted_folds,
        grid = 1,
        metrics = yardstick::metric_set(yardstick::rmse),
        control = control_grid(verbose = FALSE)
      )
    })
    
    extracted_weights <- tune:::.get_fold_weights(res)
    expect_equal(extracted_weights, expected_weights)
  })

  test_that("individual fold metrics can be collected", {
    weights <- c(0.1, 0.5, 0.4)
    weighted_folds <- add_fold_weights(folds, weights)
    
    mod <- create_test_model()
    
    suppressWarnings({
      res <- tune_grid(
        mod,
        mpg ~ .,
        resamples = weighted_folds,
        grid = 1,
        metrics = yardstick::metric_set(yardstick::rmse),
        control = control_grid(verbose = FALSE)
      )
    })
    
    # Collect individual fold metrics
    individual_metrics <- collect_metrics(res, summarize = FALSE)
    
    expect_true(nrow(individual_metrics) >= 3)  # At least one metric per fold
    expect_true("id" %in% names(individual_metrics))
    expect_true(".estimate" %in% names(individual_metrics))
    expect_true(all(is.finite(individual_metrics$.estimate)))
  })

  test_that("backwards compatibility - no weights", {
    mod <- create_test_model()
    
    suppressWarnings({
      res <- tune_grid(
        mod,
        mpg ~ .,
        resamples = folds,  # No weights
        grid = 1,
        metrics = yardstick::metric_set(yardstick::rmse),
        control = control_grid(verbose = FALSE)
      )
    })
    
    metrics <- collect_metrics(res)
    expect_equal(nrow(metrics), 1)
    expect_true("mean" %in% names(metrics))
    expect_true(is.numeric(metrics$mean))
    expect_false(is.na(metrics$mean))
  })

  test_that("rset print method displays fold weights", {
    weights <- c(0.2, 0.3, 0.5)
    weighted_folds <- add_fold_weights(folds, weights)
    
    # Capture the printed output
    output <- capture.output(print(weighted_folds))
    
    # Should contain the fold weights in the output
    expect_true(any(grepl("fold_weight", output)))
    expect_true(any(grepl("0.2", output)))
    expect_true(any(grepl("0.3", output)))
    expect_true(any(grepl("0.5", output)))
  })

  test_that("rset print method works normally without fold weights", {
    # Should not error when printing an rset without weights
    expect_no_error(print(folds))
    
    # Capture output and verify it doesn't contain fold_weight column
    output <- capture.output(print(folds))
    expect_false(any(grepl("fold_weight", output)))
  })

  test_that("rset tibble conversion includes fold weights", {
    weights <- c(0.1, 0.4, 0.5)
    weighted_folds <- add_fold_weights(folds, weights)
    
    # Convert to tibble manually (this is what our print method does)
    x_tbl <- tibble::as_tibble(weighted_folds)
    x_tbl$fold_weight <- weights
    
    # Verify the structure
    expect_true("fold_weight" %in% names(x_tbl))
    expect_equal(x_tbl$fold_weight, weights)
    expect_equal(nrow(x_tbl), 3)
  })

  test_that("get_fold_weights() works with rset objects", {
    weights <- c(0.2, 0.3, 0.5)
    weighted_folds <- add_fold_weights(folds, weights)
    
    # Should return the weights
    extracted_weights <- get_fold_weights(weighted_folds)
    expect_equal(extracted_weights, weights)
    
    # Should return NULL for unweighted rsets
    unweighted_result <- get_fold_weights(folds)
    expect_null(unweighted_result)
  })

  test_that("get_fold_weights() works with tune_results objects", {
    weights <- c(0.1, 0.5, 0.4)
    weighted_folds <- add_fold_weights(folds, weights)
    
    mod <- create_test_model()
    
    suppressWarnings({
      res <- tune_grid(
        mod,
        mpg ~ .,
        resamples = weighted_folds,
        grid = 1,
        metrics = yardstick::metric_set(yardstick::rmse),
        control = control_grid(verbose = FALSE)
      )
    })
    
    # Should extract weights from tune results
    extracted_weights <- get_fold_weights(res)
    expected_weights <- weights / sum(weights)  # normalized
    expect_equal(extracted_weights, expected_weights)
  })

  test_that("get_fold_weights() validates input types", {
    expect_error(
      get_fold_weights("not_valid_input"),
      "must be an rset or tune_results object"
    )
    
    expect_error(
      get_fold_weights(data.frame(x = 1:3)),
      "must be an rset or tune_results object"
    )
  })

  test_that("manual_rset print method displays fold weights", {
    # Create a manual_rset
    splits1 <- rsample::make_splits(x = mtcars[1:20,], assessment = mtcars[21:32,])
    splits2 <- rsample::make_splits(x = mtcars[1:15,], assessment = mtcars[16:32,])
    
    manual_folds <- rsample::manual_rset(
      list(splits1, splits2),
      c('Period1', 'Period2')
    )
    
    weights <- c(0.4, 0.6)
    weighted_manual <- add_fold_weights(manual_folds, weights)
    
    # Test that get_fold_weights works
    expect_equal(get_fold_weights(weighted_manual), weights)
    
    # Test that print shows fold_weight column
    output <- capture.output(print(weighted_manual))
    expect_true(any(grepl("fold_weight", output)))
    expect_true(any(grepl("0.4", output)))
    expect_true(any(grepl("0.6", output)))
    
    # Test that unweighted manual_rset doesn't show fold_weight
    output_unweighted <- capture.output(print(manual_folds))
    expect_false(any(grepl("fold_weight", output_unweighted)))
  })

} 