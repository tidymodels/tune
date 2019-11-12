context("logging")

# ------------------------------------------------------------------------------

ctrl_t <- control_grid(verbose = TRUE)
ctrl_f <- control_grid(verbose = FALSE)

rs <- rsample::vfold_cv(mtcars)$splits[[1]]

res_1 <- tune:::catcher(log("a"))
res_2 <- tune:::catcher(log(1))
res_3 <- tune:::catcher(log(-1))

sc_1 <- list(best_val = 7, best_iter = 2, last_impr = 3, uncertainty = 0,
  overall_iter = 1, metrics = .8, max = FALSE)

tb_1 <- tibble::tibble(.mean = 1:3)

tb_2 <-
  tibble::tibble(
    .metric = rep(letters[1:2], each = 4),
    mean = 1:8,
    .iter = 1:8,
    std_err = (1:8)/10
  )

# ------------------------------------------------------------------------------

test_that('low-level messages', {
  expect_error(tune:::siren("a", "werewolf"), "`type` should be one of: ")
  expect_message(tune:::siren("bat", "info"), "i")
  expect_message(tune:::siren("bat", "go"), cli::symbol$pointer)
  expect_message(tune:::siren("bat", "danger"), "x")
  expect_message(tune:::siren("bat", "warning"), "!")

  skip_on_os("windows")
  expect_message(tune:::siren("bat", "success"), tune::tune_symbol$success)
})

test_that('tune_log', {

  expect_message(tune:::tune_log(ctrl_t, rs, task = "cube", type = "go"), "cube")
  expect_message(tune:::tune_log(ctrl_t, rs, task = "cube", type = "go"), "Fold01")
  expect_message(tune:::tune_log(ctrl_t, NULL, task = "cube", type = "go"), "(?!.*Fold)", perl = TRUE)
  expect_silent(tune:::tune_log(ctrl_f, NULL, task = "cube", type = "go"))

  skip_on_os("windows")
  expect_message(tune:::tune_log(ctrl_t, rs, task = "cube", type = "success"), tune::tune_symbol$success)
})

test_that('log issues', {

  note_1 <- "note 1"
  expect_message(
    expect_equal(
      tune:::log_problems(note_1, ctrl_f, rs, "toledo", res_1, bad_only = FALSE),
      c("note 1", 'toledo: Error in log(\"a\"): non-numeric argument to mathematical function')
    ),
    "non-numeric argument to mathematical function"
  )

  note_2 <- NULL
  expect_silent(tune:::log_problems(note_2, ctrl_f, rs, "toledo", res_2, bad_only = FALSE))

  note_3 <- NULL
  expect_message(
    expect_equal(
      tune:::log_problems(note_3, ctrl_f, rs, "toledo", res_3, bad_only = FALSE),
      "toledo: NaNs produced"
    ),
    'Fold01: toledo: NaNs produced'
  )

  note_4 <- NULL
  expect_message(
    expect_equal(
      tune:::log_problems(note_4, ctrl_f, rs, "toledo", res_3, bad_only = FALSE),
      "toledo: NaNs produced"
    ),
    '!', fixed = TRUE
  )

  expect_message(
    expect_equal(
      tune:::log_problems(note_4, ctrl_f, NULL, "toledo", res_1, bad_only = FALSE),
      "toledo: Error in log(\"a\"): non-numeric argument to mathematical function"
    ),
    "(?!.*Fold)", perl = TRUE
  )

  expect_message(
    expect_equal(
      tune:::log_problems(note_4, ctrl_f, NULL, "toledo", res_3, bad_only = FALSE),
      "toledo: NaNs produced"
    ),
    "(?!.*Fold)", perl = TRUE
  )

  expect_silent(
    tune:::log_problems(note_4, ctrl_f, rs, "toledo", res_2, bad_only = FALSE)
  )

})


test_that('catch and log issues', {

  null <- NULL

  expect_message(
    out_1 <- tune:::catch_and_log(log("a"), ctrl_f, rs, "toledo", bad_only = FALSE, notes = null),
    'Fold01: toledo: Error in log("a")', fixed = TRUE
  )
  expect_true(inherits(out_1, "try-error"))
  expect_silent(out_2 <- tune:::catch_and_log(log(1), ctrl_f, rs, "toledo", bad_only = FALSE, notes = null))
  expect_true(out_2 == 0)
  expect_message(
    out_3 <- tune:::catch_and_log(log(-1), ctrl_f, rs, "toledo", bad_only = FALSE, notes = null),
    'Fold01: toledo: NaNs produced', fixed = TRUE
  )
  expect_true(is.nan(out_3))
  expect_message(
    out_4 <- tune:::catch_and_log(log(-1), ctrl_f, rs, "toledo", bad_only = FALSE, notes = null),
    '!', fixed = TRUE
  )
  expect_true(is.nan(out_4))
  expect_message(
    out_5 <- tune:::catch_and_log(log("a"), ctrl_f, NULL, "toledo", bad_only = FALSE, notes = null),
    "(?!.*Fold)", perl = TRUE
  )
  expect_true(inherits(out_5, "try-error"))
  expect_message(
    out_6 <- tune:::catch_and_log(log(-1), ctrl_f, NULL, "toledo", bad_only = FALSE, notes = null),
    "(?!.*Fold)", perl = TRUE
  )
  expect_true(is.nan(out_6))
  expect_silent(
    out_7 <- tune:::catch_and_log(log(1), ctrl_f, rs, "toledo", bad_only = FALSE, notes = null)
  )
  expect_true(out_7 == 0)
})

test_that('logging iterations', {

  expect_message(tune:::log_best(ctrl_t, 10, sc_1), 'Iteration 10')
  expect_message(tune:::log_best(ctrl_t, 10, sc_1), '0.8=7 (@iter 2)', fixed = TRUE)
  expect_silent(tune:::log_best(ctrl_f, 10, sc_1))

})

test_that('logging search info', {

  expect_silent(tune:::check_and_log_flow(ctrl_t, tb_1))
  expect_message(
    expect_error(
      tune:::check_and_log_flow(ctrl_t, tb_1 %>% mutate(.mean = .mean * NA))
    ),
    "Skipping to next iteration"
  )
  expect_message(
    expect_error(
      tune:::check_and_log_flow(ctrl_t, tb_1 %>% mutate(.mean = .mean * NA) %>% slice(1))
    ),
    "Halting search"
  )

})

test_that('current results', {

  expect_message(
    tune:::log_progress(ctrl_t, tb_2, maximize = FALSE, objective = "a"),
    cli::symbol$circle_cross
  )
  expect_message(
    tune:::log_progress(ctrl_t, tb_2, maximize = TRUE, objective = "b"),
    "+/-"
  )
  expect_silent(tune:::log_progress(ctrl_f, tb_2, maximize = TRUE, objective = "a"))

  skip_on_os("windows")

  expect_message(
    tune:::log_progress(ctrl_t, tb_2, maximize = TRUE, objective = "a"),
    cli::symbol$heart
  )

  expect_message(
    tune:::log_progress(ctrl_t, tb_2, maximize = TRUE, objective = "a"),
    "Newest results"
  )
})


test_that('show parameters', {

  expect_message(tune:::param_msg(ctrl_t, iris[1, 4:5]), "i")
  expect_message(tune:::param_msg(ctrl_t, iris[1, 4:5]), "Petal.Width=0.2, Species=setosa")
  expect_silent(tune:::param_msg(ctrl_f, iris[1, 4:5]))

})


test_that('acquisition functions', {

  expect_silent(tune:::acq_summarizer(ctrl_t, 1))
  expect_silent(tune:::acq_summarizer(ctrl_t, 1, conf_bound()))
  expect_silent(tune:::acq_summarizer(ctrl_f, 1, conf_bound()))
  expect_message(tune:::acq_summarizer(ctrl_t, 1, conf_bound(I)), "Kappa value: 1")
  expect_message(tune:::acq_summarizer(ctrl_t, 1, exp_improve(I)), "Trade-off value: 1")
  expect_message(tune:::acq_summarizer(ctrl_t, 1, prob_improve(I)), "Trade-off value: 1")

})


