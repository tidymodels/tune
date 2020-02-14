
context("templating functions")

# ------------------------------------------------------------------------------

test_that('glmnet', {

  verify_output(test_path("templates", "glmn_dummies_tune.txt"), {
    template_glmnet(Sepal.Width ~ ., data = iris)
  })

  verify_output(test_path("templates", "glmn_dummies_verbose.txt"), {
    template_glmnet(Sepal.Width ~ ., data = iris, verbose = TRUE, tune = FALSE)
  })

  verify_output(test_path("templates", "glmn_dummies_tune_verbose.txt"), {
    template_glmnet(Sepal.Width ~ ., data = iris, verbose = TRUE)
  })

  verify_output(test_path("templates", "glmn_dummies.txt"), {
    template_glmnet(Sepal.Width ~ ., data = iris, verbose = FALSE, tune = FALSE)
  })

  verify_output(test_path("templates", "glmn_tune.txt"), {
    template_glmnet(Species ~ ., data = iris)
  })

  verify_output(test_path("templates", "glmn_verbose.txt"), {
    template_glmnet(Species ~ ., data = iris, verbose = TRUE, tune = FALSE)
  })

  verify_output(test_path("templates", "glmn_tune_verbose.txt"), {
    template_glmnet(Species ~ ., data = iris, verbose = TRUE)
  })

  verify_output(test_path("templates", "glmn.txt"), {
    template_glmnet(Species ~ ., data = iris, verbose = FALSE, tune = FALSE)
  })

})

# ------------------------------------------------------------------------------

test_that('knn', {

  verify_output(test_path("templates", "knn_dummies_tune.txt"), {
    set.seed(24214)
    template_knn(Sepal.Width ~ ., data = iris)
  })

  verify_output(test_path("templates", "knn_dummies_verbose.txt"), {
    set.seed(24214)
    template_knn(Sepal.Width ~ ., data = iris, verbose = TRUE, tune = FALSE)
  })

  verify_output(test_path("templates", "knn_dummies_tune_verbose.txt"), {
    set.seed(24214)
    template_knn(Sepal.Width ~ ., data = iris, verbose = TRUE)
  })

  verify_output(test_path("templates", "knn_dummies.txt"), {
    set.seed(24214)
    template_knn(Sepal.Width ~ ., data = iris, verbose = FALSE, tune = FALSE)
  })

  verify_output(test_path("templates", "knn_tune.txt"), {
    set.seed(24214)
    template_knn(Species ~ ., data = iris)
  })

  verify_output(test_path("templates", "knn_verbose.txt"), {
    set.seed(24214)
    template_knn(Species ~ ., data = iris, verbose = TRUE, tune = FALSE)
  })

  verify_output(test_path("templates", "knn_tune_verbose.txt"), {
    set.seed(24214)
    template_knn(Species ~ ., data = iris, verbose = TRUE)
  })

  verify_output(test_path("templates", "knn.txt"), {
    set.seed(24214)
    template_knn(Species ~ ., data = iris, verbose = FALSE, tune = FALSE)
  })

})

# ------------------------------------------------------------------------------

test_that('xgboost', {

  verify_output(test_path("templates", "xgb_dummies_tune.txt"), {
    set.seed(1290)
    template_xgboost(Sepal.Width ~ ., data = iris)
  })

  verify_output(test_path("templates", "xgb_dummies_verbose.txt"), {
    set.seed(1290)
    template_xgboost(Sepal.Width ~ ., data = iris, verbose = TRUE, tune = FALSE)
  })

  verify_output(test_path("templates", "xgb_dummies_tune_verbose.txt"), {
    set.seed(1290)
    template_xgboost(Sepal.Width ~ ., data = iris, verbose = TRUE)
  })

  verify_output(test_path("templates", "xgb_dummies.txt"), {
    set.seed(1290)
    template_xgboost(Sepal.Width ~ ., data = iris, verbose = FALSE, tune = FALSE)
  })

  verify_output(test_path("templates", "xgb_tune.txt"), {
    set.seed(1290)
    template_xgboost(Species ~ ., data = iris)
  })

  verify_output(test_path("templates", "xgb_verbose.txt"), {
    set.seed(1290)
    template_xgboost(Species ~ ., data = iris, verbose = TRUE, tune = FALSE)
  })

  verify_output(test_path("templates", "xgb_tune_verbose.txt"), {
    set.seed(1290)
    template_xgboost(Species ~ ., data = iris, verbose = TRUE)
  })

  verify_output(test_path("templates", "xgb.txt"), {
    set.seed(1290)
    template_xgboost(Species ~ ., data = iris, verbose = FALSE, tune = FALSE)
  })

})

