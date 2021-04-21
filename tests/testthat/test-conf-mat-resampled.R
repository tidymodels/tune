context("resampled confusion matrix")

# ------------------------------------------------------------------------------

load(test_path("test_objects.RData"))
svm_results <- readRDS(test_path("svm_results.rds"))

# ------------------------------------------------------------------------------

test_that('appropriate return values', {
  expect_error(
    cm_1 <- conf_mat_resampled(svm_results, select_best(svm_results, "accuracy")),
    regex = NA
  )
  expect_true(tibble::is_tibble(cm_1))

  expect_error(
    cm_2 <- conf_mat_resampled(svm_results, select_best(svm_results, "accuracy"), tidy = FALSE),
    regex = NA
  )
  expect_equal(class(cm_2), "conf_mat")

  expect_equal(
    cm_2$table["Class1", "Class2"],
    cm_1$Freq[cm_1$Prediction == "Class1" & cm_1$Truth == "Class2"]
  )
  expect_equal(
    cm_2$table["Class2", "Class1"],
    cm_1$Freq[cm_1$Prediction == "Class2" & cm_1$Truth == "Class1"]
  )

})

# ------------------------------------------------------------------------------

test_that('bad argss', {
  expect_error(
    conf_mat_resampled(svm_results),
    regex = "there are 5 tuning parameter"
  )
  expect_error(
    conf_mat_resampled(mt_knn_bo),
    regex = "Was this a classification model"
  )
  broke_results <- svm_results
  broke_results$.predictions <- NULL

  expect_error(
    conf_mat_resampled(broke_results),
    regex = "The function was not run with the"
  )
  expect_error(
    conf_mat_resampled(tibble::as_tibble(svm_results)),
    regex = "The first argument needs to be an object with class"
  )

  broke_results <- svm_results
  broke_results$.predictions <-
    purrr::map(broke_results$.predictions,
               ~ .x %>% dplyr::select(-.pred_class))

  expect_error(
    conf_mat_resampled(broke_results),
    regex = "Cannot find the predicted classes"
  )

  broke_results <- svm_results
  attr(broke_results, "outcomes") <- NULL

  expect_error(
    conf_mat_resampled(broke_results, select_best(broke_results, "accuracy")),
    regex = "Cannot determine the proper outcome name"
  )

  expect_error(
    conf_mat_resampled(svm_results),
    regex = ""
  )
})

