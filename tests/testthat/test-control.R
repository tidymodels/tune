test_that("workflow size warning", {
  withr::local_options(width = 500)

  # A larger data set to trip the warning
  MTCARS <- mtcars[rep(1:32, each = 1000), ]

  lm_rec <- recipe(mpg ~ ., data = MTCARS)
  lm_wflow <- workflow(lm_rec, linear_reg() |> set_engine("lm", x = TRUE))
  # About 2.7MB when fit

  expect_silent({
    set.seed(1)
    no_warning <-
      lm_wflow |>
      fit_resamples(
        resamples = vfold_cv(MTCARS),
        control = control_resamples(save_workflow = TRUE, workflow_size = Inf)
      )
  })

  expect_snapshot({
    set.seed(1)
    warns <-
      lm_wflow |>
      fit_resamples(
        resamples = vfold_cv(MTCARS),
        control = control_resamples(save_workflow = TRUE, workflow_size = 2)
      )
  })

  expect_silent({
    set.seed(1)
    no_save <-
      lm_wflow |>
      fit_resamples(
        resamples = vfold_cv(MTCARS),
        control = control_resamples(save_workflow = FALSE, workflow_size = 2)
      )
  })
})
