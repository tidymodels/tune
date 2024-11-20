test_that("appropriate return values", {
  svm_results <- readRDS(test_path("data", "svm_results.rds"))

  expect_no_error(
    cm_1 <-
      conf_mat_resampled(svm_results, parameters = select_best(svm_results, metric = "accuracy"))
  )
  expect_true(tibble::is_tibble(cm_1))

  expect_no_error(
    cm_2 <-
      conf_mat_resampled(
        svm_results,
        parameters = select_best(svm_results, metric = "accuracy"),
        tidy = FALSE
      )
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

test_that("bad argss", {
  load(test_path("data", "test_objects.RData"))
  svm_results <- readRDS(test_path("data", "svm_results.rds"))

  expect_snapshot(error = TRUE, {
    conf_mat_resampled(svm_results)
  })
  expect_snapshot(error = TRUE, {
    conf_mat_resampled(mt_knn_bo)
  })
  broke_results <- svm_results
  broke_results$.predictions <- NULL

  expect_snapshot(error = TRUE, {
    conf_mat_resampled(broke_results)
  })
  expect_snapshot(error = TRUE, {
    conf_mat_resampled(tibble::as_tibble(svm_results))
  })

  broke_results <- svm_results
  broke_results$.predictions <-
    purrr::map(
      broke_results$.predictions,
      ~ .x %>% dplyr::select(-.pred_class)
    )

  expect_snapshot(error = TRUE, {
    conf_mat_resampled(broke_results)
  })

  broke_results <- svm_results
  attr(broke_results, "outcomes") <- NULL

  expect_snapshot(error = TRUE, {
    conf_mat_resampled(broke_results, parameters = select_best(broke_results, metric = "accuracy"))
  })

  expect_snapshot(error = TRUE, {
    conf_mat_resampled(svm_results, argument_that_doesnt_exist = TRUE)
  })
})
