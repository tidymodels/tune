test_that("control functions respect hierarchy with condense_control", {

  ctrl <- parsnip::condense_control(
    control_grid(),
    control_resamples()
  )

  expect_equal(
    ctrl,
    control_resamples()
  )

  ctrl <- parsnip::condense_control(
    control_last_fit(),
    control_grid()
  )

  expect_equal(
    ctrl,
    control_grid(
      extract = control_last_fit()$extract,
      save_pred = TRUE,
      allow_par = FALSE
    )
  )

  ctrl <- parsnip::condense_control(
    control_last_fit(),
    control_resamples()
  )

  expect_equal(
    ctrl,
    control_resamples(
      extract = control_last_fit()$extract,
      save_pred = TRUE,
      allow_par = FALSE
    )
  )

  ctrl <- parsnip::condense_control(
    control_bayes(),
    control_grid()
  )

  expect_equal(
    ctrl,
    control_grid()
  )

  ctrl <- parsnip::condense_control(
    control_bayes(),
    control_resamples()
  )

  expect_equal(
    ctrl,
    control_resamples()
  )
})
