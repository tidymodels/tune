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

test_that("control object print methods", {
  expect_snapshot(control_grid())
  expect_snapshot(control_grid(verbose = TRUE, save_pred = TRUE))
  expect_snapshot(control_grid(pkgs = c("pkg1", "pkg2"), extract = I))

  set.seed(123)
  expect_snapshot(control_bayes())
  expect_snapshot(control_bayes(
    verbose_iter = TRUE,
    no_improve = 5,
    save_gp_scoring = TRUE
  ))

  expect_snapshot(control_last_fit())
  expect_snapshot(control_last_fit(verbose = TRUE))
})

test_that("control object print methods with default = TRUE", {
  expect_snapshot(print(
    control_grid(verbose = TRUE, pkgs = c("pkg1", "pkg2")),
    default = TRUE
  ))
  set.seed(456)
  expect_snapshot(print(
    control_bayes(verbose_iter = TRUE, no_improve = 5),
    default = TRUE
  ))
  expect_snapshot(print(control_last_fit(verbose = TRUE), default = TRUE))
})
