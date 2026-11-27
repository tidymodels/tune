test_that("appropriate return values", {
  svm_results <- readRDS(test_path("data", "svm_results.rds"))

  expect_no_error(
    cm_1 <-
      conf_mat_resampled(
        svm_results,
        parameters = select_best(svm_results, metric = "accuracy")
      )
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

test_that("conf_mat_resampled() averages across all resamples for repeated CV", {
  skip_if_not_installed("modeldata")
  data(two_class_dat, package = "modeldata")
  dat <- two_class_dat[1:100, ]

  set.seed(1)
  rs <- rsample::vfold_cv(dat, v = 5, repeats = 3)
  res <-
    parsnip::logistic_reg() |>
    fit_resamples(
      Class ~ .,
      resamples = rs,
      control = control_resamples(save_pred = TRUE)
    )

  cm <- conf_mat_resampled(res)
  expect_equal(sum(cm$Freq), nrow(dat) / 5)

  preds <- collect_predictions(res)
  expected <-
    preds |>
    dplyr::group_nest(id, id2) |>
    dplyr::mutate(
      cm = purrr::map(
        data,
        \(.x) as.data.frame(yardstick::conf_mat(.x, Class, .pred_class)$table)
      )
    ) |>
    dplyr::pull(cm) |>
    purrr::list_rbind() |>
    dplyr::summarize(Freq = mean(Freq), .by = c(Prediction, Truth)) |>
    dplyr::arrange(Prediction, Truth)

  expect_equal(cm, tibble::as_tibble(expected))
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
      \(.x) .x |> dplyr::select(-.pred_class)
    )

  expect_snapshot(error = TRUE, {
    conf_mat_resampled(broke_results)
  })

  broke_results <- svm_results
  attr(broke_results, "outcomes") <- NULL

  expect_snapshot(error = TRUE, {
    conf_mat_resampled(
      broke_results,
      parameters = select_best(broke_results, metric = "accuracy")
    )
  })

  expect_snapshot(error = TRUE, {
    conf_mat_resampled(svm_results, argument_that_doesnt_exist = TRUE)
  })
})
