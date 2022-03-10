test_that("low-level messages", {
  expect_snapshot(
    error = TRUE,
    tune:::siren("a", "werewolf")
  )
  expect_snapshot(tune:::siren("bat", "info"))
  expect_snapshot(tune:::siren("bat", "go"))
  expect_snapshot(tune:::siren("bat", "danger"))
  expect_snapshot(tune:::siren("bat", "warning"))

  skip_on_os("windows")
  expect_snapshot(tune:::siren("bat", "success"))
})

test_that("tune_log", {
  ctrl_t <- control_grid(verbose = TRUE)
  ctrl_f <- control_grid(verbose = FALSE)
  rs <- rsample::vfold_cv(mtcars)$splits[[1]]

  expect_snapshot(tune:::tune_log(ctrl_t, rs, task = "cube", type = "go"))
  expect_snapshot(tune:::tune_log(ctrl_t, NULL, task = "cube", type = "go"))
  expect_silent(tune:::tune_log(ctrl_f, NULL, task = "cube", type = "go"))

  skip_on_os("windows")
  expect_snapshot(tune:::tune_log(ctrl_t, rs, task = "cube", type = "success"))
})

test_that("log issues", {
  ctrl_f <- control_grid(verbose = FALSE)

  rs <- rsample::vfold_cv(mtcars)$splits[[1]]

  res_1 <- tune:::catcher(log("a"))
  res_2 <- tune:::catcher(log(1))
  res_3 <- tune:::catcher(log(-1))

  note_1 <- tibble::tibble(location = "Roshar", type = "Alethi", note = "I'm a stick")
  note_2 <- tibble::tibble(location = "toledo", type = "error", note = "Error in log(\"a\"): non-numeric argument to mathematical function")

  expect_snapshot(
    expect_equal(
      tune:::log_problems(note_1, ctrl_f, rs, "toledo", res_1, bad_only = FALSE),
      dplyr::bind_rows(note_1, note_2)
    )
  )

  expect_silent(tune:::log_problems(note_1, ctrl_f, rs, "toledo", res_2, bad_only = FALSE))

  note_3 <- tibble::tibble(location = "toledo", type = "warning", note = "NaNs produced")
  expect_snapshot(
    expect_equal(
      tune:::log_problems(note_1, ctrl_f, rs, "toledo", res_3, bad_only = FALSE),
      dplyr::bind_rows(note_1, note_3)
    )
  )
})


test_that("catch and log issues", {
  ctrl_f <- control_grid(verbose = FALSE)
  rs <- rsample::vfold_cv(mtcars)$splits[[1]]
  null <- NULL

  expect_snapshot(
    out_1 <- tune:::catch_and_log(log("a"), ctrl_f, rs, "toledo", bad_only = FALSE, notes = null)
  )
  expect_true(inherits(out_1, "try-error"))
  expect_silent(out_2 <- tune:::catch_and_log(log(1), ctrl_f, rs, "toledo", bad_only = FALSE, notes = null))
  expect_true(out_2 == 0)
  expect_snapshot(
    out_3 <- tune:::catch_and_log(log(-1), ctrl_f, rs, "toledo", bad_only = FALSE, notes = null)
  )
  expect_true(is.nan(out_3))
  expect_snapshot(
    out_5 <- tune:::catch_and_log(log("a"), ctrl_f, NULL, "toledo", bad_only = FALSE, notes = null)
  )
  expect_true(inherits(out_5, "try-error"))
  expect_snapshot(
    out_6 <- tune:::catch_and_log(log(-1), ctrl_f, NULL, "toledo", bad_only = FALSE, notes = null)
  )
  expect_true(is.nan(out_6))
})

test_that("logging iterations", {
  ctrl_t <- control_grid(verbose = TRUE)
  ctrl_f <- control_grid(verbose = FALSE)
  sc_1 <- list(
    best_val = 7,
    best_iter = 2,
    last_impr = 3,
    uncertainty = 0,
    overall_iter = 1,
    metrics = .8,
    max = FALSE
  )

  expect_snapshot(tune:::log_best(ctrl_t, 10, sc_1))
  expect_silent(tune:::log_best(ctrl_f, 10, sc_1))
})

test_that("logging search info", {
  ctrl_t <- control_grid(verbose = TRUE)
  tb_1 <- tibble::tibble(.mean = 1:3)

  expect_silent(tune:::check_and_log_flow(ctrl_t, tb_1))
  expect_snapshot(
    error = TRUE,
    tune:::check_and_log_flow(ctrl_t, tb_1 %>% mutate(.mean = .mean * NA))
  )
  expect_snapshot(
    error = TRUE,
    tune:::check_and_log_flow(ctrl_t, tb_1 %>% mutate(.mean = .mean * NA) %>% slice(1))
  )
})

test_that("current results", {
  ctrl_t <- control_grid(verbose = TRUE)
  ctrl_f <- control_grid(verbose = FALSE)
  tb_2 <-
    tibble::tibble(
      .metric = rep(letters[1:2], each = 4),
      mean = 1:8,
      .iter = 1:8,
      std_err = (1:8) / 10
    )

  expect_snapshot(
    tune:::log_progress(ctrl_t, tb_2, maximize = FALSE, objective = "a")
  )
  expect_snapshot(
    tune:::log_progress(ctrl_t, tb_2, maximize = TRUE, objective = "b")
  )
  expect_silent(tune:::log_progress(ctrl_f, tb_2, maximize = TRUE, objective = "a"))

  skip_on_os("windows")

  expect_snapshot(
    tune:::log_progress(ctrl_t, tb_2, maximize = TRUE, objective = "a")
  )
})


test_that("show parameters", {
  ctrl_t <- control_grid(verbose = TRUE)
  ctrl_f <- control_grid(verbose = FALSE)

  expect_snapshot(tune:::param_msg(ctrl_t, iris[1, 4:5]))
  expect_silent(tune:::param_msg(ctrl_f, iris[1, 4:5]))
})


test_that("acquisition functions", {
  ctrl_t <- control_grid(verbose = TRUE)
  ctrl_f <- control_grid(verbose = FALSE)

  expect_silent(tune:::acq_summarizer(ctrl_t, 1))
  expect_silent(tune:::acq_summarizer(ctrl_t, 1, conf_bound()))
  expect_silent(tune:::acq_summarizer(ctrl_f, 1, conf_bound()))
  expect_snapshot(tune:::acq_summarizer(ctrl_t, 1, conf_bound(I)))
  expect_snapshot(tune:::acq_summarizer(ctrl_t, 1, exp_improve(I)))
  expect_snapshot(tune:::acq_summarizer(ctrl_t, 1, prob_improve(I)))
})

## -----------------------------------------------------------------------------

test_that("message_wrap", {
  text <-
    paste(
      "A data frame of tuning combinations or a positive integer. The data",
      "frame should have columns for each parameter being tuned and rows for",
      "tuning parameter candidates. An integer denotes the number of candidate",
      "parameter sets to be created automatically.",
      collapse = ""
    )
  verify_output(
    test_path("message_wrap/width_30.txt"),
    message_wrap(text, width = 30),
    crayon = TRUE
  )
  verify_output(
    test_path("message_wrap/width_50_prefix.txt"),
    message_wrap(text, width = 50, prefix = "'grid':"),
    crayon = TRUE
  )
  verify_output(
    test_path("message_wrap/width_50_prefix_red_text.txt"),
    message_wrap(text, width = 50, prefix = "'grid':", color_text = cli::col_red),
    crayon = TRUE
  )
  verify_output(
    test_path("message_wrap/width_50_prefix_red_text_blue_prefix.txt"),
    message_wrap(
      text,
      width = 50,
      prefix = "'grid':",
      color_text = cli::col_red,
      color_prefix = cli::col_blue
    ),
    crayon = TRUE
  )
})
