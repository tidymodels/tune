test_that("low-level messages", {
  expect_snapshot(
    error = TRUE,
    siren("a", "werewolf")
  )
  expect_snapshot(siren("bat", "info"))
  expect_snapshot(siren("bat", "go"))
  expect_snapshot(siren("bat", "danger"))
  expect_snapshot(siren("bat", "warning"))

  skip_on_os("windows")
  expect_snapshot(siren("bat", "success"))
})

test_that("tune_log", {
  ctrl_t <- control_grid(verbose = TRUE)
  ctrl_f <- control_grid(verbose = FALSE)
  rs <- labels(rsample::vfold_cv(mtcars)$splits[[1]])

  expect_snapshot(tune_log(ctrl_t, rs, task = "cube", type = "go"))
  expect_snapshot(tune_log(ctrl_t, NULL, task = "cube", type = "go"))
  expect_silent(tune_log(ctrl_f, NULL, task = "cube", type = "go"))

  skip_on_os("windows")
  expect_snapshot(tune_log(ctrl_t, rs, task = "cube", type = "success"))
})

test_that("log issues", {
  ctrl_f <- control_grid(verbose = FALSE)

  rs <- labels(rsample::vfold_cv(mtcars)$splits[[1]])

  res_1 <- catcher(log("a"))
  res_2 <- catcher(log(1))
  res_3 <- catcher(log(-1))

  note_1 <- tibble::tibble(location = "Roshar", type = "Alethi", note = "I'm a stick")
  note_2 <- tibble::tibble(location = "toledo", type = "error", note = "Error in log(\"a\"): non-numeric argument to mathematical function")

  expect_snapshot(
    problems_1 <-
      log_problems(note_1, ctrl_f, rs, "toledo", res_1, bad_only = FALSE)
  )

  expect_equal(
    dplyr::select(problems_1, -trace),
    dplyr::bind_rows(note_1, note_2)
  )

  expect_null(problems_1$trace[[1]])
  expect_s3_class(problems_1$trace[[2]], "rlang_trace")

  expect_silent(log_problems(note_1, ctrl_f, rs, "toledo", res_2, bad_only = FALSE))

  note_3 <- tibble::tibble(location = "toledo", type = "warning", note = "NaNs produced")
  expect_snapshot(
    problems_2 <-
      log_problems(note_1, ctrl_f, rs, "toledo", res_3, bad_only = FALSE)
  )

  expect_equal(
    dplyr::select(problems_2, -trace),
    dplyr::bind_rows(note_1, note_3)
  )

  expect_null(problems_2$trace[[1]])
  expect_s3_class(problems_2$trace[[2]], "rlang_trace")
})


test_that("catch and log issues", {
  ctrl_f <- control_grid(verbose = FALSE)
  rs <- labels(rsample::vfold_cv(mtcars)$splits[[1]])
  null <- NULL

  expect_snapshot(
    out_1 <- .catch_and_log(
      log("a"),
      control = ctrl_f,
      split_labels = rs,
      "toledo",
      bad_only = FALSE,
      notes = null
    )
  )
  expect_true(inherits(out_1, "try-error"))
  expect_silent(out_2 <- .catch_and_log(
    log(1),
    control = ctrl_f,
    split_labels = rs,
    "toledo",
    bad_only = FALSE,
    notes = null
  ))
  expect_true(out_2 == 0)
  expect_snapshot(
    out_3 <- .catch_and_log(
      log(-1),
      control = ctrl_f,
      split_labels = rs,
      "toledo",
      bad_only = FALSE,
      notes = null
    )
  )
  expect_true(is.nan(out_3))
  expect_snapshot(
    out_5 <- .catch_and_log(
      log("a"),
      control = ctrl_f,
      split_labels = NULL,
      "toledo",
      bad_only = FALSE,
      notes = null
    )
  )
  expect_true(inherits(out_5, "try-error"))
  expect_snapshot(
    out_6 <- .catch_and_log(
      log(-1),
      control = ctrl_f,
      split_labels = NULL,
      "toledo",
      bad_only = FALSE,
      notes = null
    )
  )
  expect_true(is.nan(out_6))
})

test_that("logging iterations", {
  ctrl_t <- control_bayes(verbose_iter = TRUE)
  ctrl_f <- control_bayes(verbose_iter = FALSE)
  sc_1 <- list(
    best_val = 7,
    best_iter = 2,
    last_impr = 3,
    uncertainty = 0,
    overall_iter = 1,
    metrics = .8,
    max = FALSE
  )

  expect_snapshot(log_best(ctrl_t, 10, sc_1))
  expect_silent(log_best(ctrl_f, 10, sc_1))
})

test_that("logging search info", {
  ctrl_t <- control_bayes(verbose_iter = TRUE)
  tb_1 <- tibble::tibble(.mean = 1:3)

  expect_silent(check_and_log_flow(ctrl_t, tb_1))
  expect_snapshot(
    error = TRUE,
    check_and_log_flow(ctrl_t, tb_1 %>% mutate(.mean = .mean * NA))
  )
  expect_snapshot(
    error = TRUE,
    check_and_log_flow(ctrl_t, tb_1 %>% mutate(.mean = .mean * NA) %>% slice(1))
  )
})

test_that("current results", {
  ctrl_t <- control_bayes(verbose_iter = TRUE)
  ctrl_f <- control_bayes(verbose_iter = FALSE)
  tb_2 <-
    tibble::tibble(
      .metric = rep(letters[1:2], each = 4),
      mean = 1:8,
      .iter = 1:8,
      std_err = (1:8) / 10
    )

  expect_snapshot(
    log_progress(ctrl_t, tb_2, maximize = FALSE, objective = "a")
  )
  expect_snapshot(
    log_progress(ctrl_t, tb_2, maximize = TRUE, objective = "b")
  )
  expect_silent(log_progress(ctrl_f, tb_2, maximize = TRUE, objective = "a"))

  skip_on_os("windows")

  expect_snapshot(
    log_progress(ctrl_t, tb_2, maximize = TRUE, objective = "a")
  )
})


test_that("show parameters", {
  ctrl_t <- control_bayes(verbose_iter = TRUE)
  ctrl_f <- control_bayes(verbose_iter = FALSE)

  expect_snapshot(param_msg(ctrl_t, iris[1, 4:5]))
  expect_silent(param_msg(ctrl_f, iris[1, 4:5]))
})


test_that("acquisition functions", {
  ctrl_t <- control_bayes(verbose_iter = TRUE)
  ctrl_f <- control_bayes(verbose_iter = FALSE)

  expect_silent(acq_summarizer(ctrl_t, 1))
  expect_silent(acq_summarizer(ctrl_t, 1, conf_bound()))
  expect_silent(acq_summarizer(ctrl_f, 1, conf_bound()))
  expect_snapshot(acq_summarizer(ctrl_t, 1, conf_bound(I)))
  expect_snapshot(acq_summarizer(ctrl_t, 1, exp_improve(I)))
  expect_snapshot(acq_summarizer(ctrl_t, 1, prob_improve(I)))
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

test_that("interactive logger works (fit_resamples, warning + error)", {
  skip_if(allow_parallelism(FALSE), "Will not catalog: parallelism is enabled")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("kknn")

  local_mocked_bindings(
    is_testing = function() {FALSE},
    initialize_catalog = redefer_initialize_catalog(rlang::current_env())
  )

  raise_warning <- function(x) {warning("ope! yikes.")}
  raise_error <- function(x) {stop("AHHhH")}

  set.seed(1)
  expect_snapshot(
    {res_fit <-
      fit_resamples(
        parsnip::nearest_neighbor("regression", "kknn"),
        Sale_Price ~ .,
        rsample::vfold_cv(modeldata::ames[, c(72, 40:45)], 5),
        control = control_resamples(
          extract = function(x) {raise_warning(); raise_error()})
    )},
    transform = catalog_lines
  )

  # `catalog_summary_test` written to this env via `redefer_initialize_catalog()`
  expect_snapshot(catalog_summary_test)
})

test_that("interactive logger works (fit_resamples, rlang warning + error)", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("kknn")

  skip_if(allow_parallelism(FALSE), "Will not catalog: parallelism is enabled")
  local_mocked_bindings(
    is_testing = function() {FALSE},
    initialize_catalog = redefer_initialize_catalog(rlang::current_env())
  )

  raise_warning_rl <- function(x) {rlang::warn("ope! yikes. (but rlang)")}
  raise_error_rl <- function(x) {rlang::abort("AHHhH (but rlang)")}

  set.seed(1)
  expect_snapshot(
    {res_fit <-
      fit_resamples(
        parsnip::nearest_neighbor("regression", "kknn"),
        Sale_Price ~ .,
        rsample::vfold_cv(modeldata::ames[, c(72, 40:45)], 5),
        control = control_resamples(
          extract = function(x) {raise_warning_rl(); raise_error_rl()}
        )
    )},
    transform = catalog_lines
  )

  # `catalog_summary_test` written to this env via `redefer_initialize_catalog()`
  expect_snapshot(catalog_summary_test)
})


test_that("interactive logger works (fit_resamples, multiline)", {
  skip_if(allow_parallelism(FALSE), "Will not catalog: parallelism is enabled")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("kknn")

  local_mocked_bindings(
    is_testing = function() {FALSE},
    initialize_catalog = redefer_initialize_catalog(rlang::current_env())
  )
  skip_on_cran()

  raise_multiline_conditions <- function(x) {
    cli::cli_warn(c("hmmm what's happening", "uuuhhHhH"))
    cli::cli_abort(c("aHHHksdjvndiuf", "!" = "),:"))
    x
  }

  set.seed(1)
  expect_snapshot(
    {res_fit <-
      fit_resamples(
        parsnip::nearest_neighbor("regression", "kknn"),
        Sale_Price ~ .,
        rsample::vfold_cv(modeldata::ames[, c(72, 40:45)], 5),
        control = control_resamples(extract = raise_multiline_conditions)
    )},
    transform = catalog_lines
  )

  # `catalog_summary_test` written to this env via `redefer_initialize_catalog()`
  expect_snapshot(catalog_summary_test)
})

test_that("interactive logger works (fit_resamples, occasional error)", {
  skip_if(allow_parallelism(FALSE), "Will not catalog: parallelism is enabled")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("kknn")

  local_mocked_bindings(
    is_testing = function() {FALSE},
    initialize_catalog = redefer_initialize_catalog(rlang::current_env())
  )
  skip_on_cran()

  raise_error_later <- function() {local({
    count <- 0
    function(x) {
      count <<- count + 1
      if (count > 3) {
        stop("this errors now! ha!")
      }
      "hi"
    }
  })}
  later <- raise_error_later()

  set.seed(1)
  expect_snapshot(
    {res_fit <-
      fit_resamples(
        parsnip::nearest_neighbor("regression", "kknn"),
        Sale_Price ~ .,
        rsample::vfold_cv(modeldata::ames[, c(72, 40:45)], 5),
        control = control_resamples(extract = later)
    )},
    transform = catalog_lines
  )

  # `catalog_summary_test` written to this env via `redefer_initialize_catalog()`
  expect_snapshot(catalog_summary_test)
})

test_that("interactive logger works (fit_resamples, occasional errors)", {
  skip_if(allow_parallelism(FALSE), "Will not catalog: parallelism is enabled")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("kknn")

  local_mocked_bindings(
    is_testing = function() {FALSE},
    initialize_catalog = redefer_initialize_catalog(rlang::current_env())
  )

  skip_on_cran()

  raise_error_once <- function() {local({
    first <- TRUE
    function(x) {
      if (first) {
        first <<- FALSE
        stop("oh no")
      }

      "hi"
    }
  })}

  raise_error_later <- function() {local({
    count <- 0
    function(x) {
      count <<- count + 1
      if (count > 3) {
        stop("this errors now! ha!")
      }
      "hi"
    }
  })}

  once <- raise_error_once()
  later <- raise_error_later()

  set.seed(1)
  expect_snapshot(
    {res_fit <-
      fit_resamples(
        parsnip::nearest_neighbor("regression", "kknn"),
        Sale_Price ~ .,
        rsample::vfold_cv(modeldata::ames[, c(72, 40:45)], 10),
        control = control_resamples(extract = function(x) {once(); later()})
    )},
    transform = catalog_lines
  )

  # `catalog_summary_test` written to this env via `redefer_initialize_catalog()`
  expect_snapshot(catalog_summary_test)
})


test_that("interactive logger works (fit_resamples, many distinct errors)", {
  skip_if(allow_parallelism(FALSE), "Will not catalog: parallelism is enabled")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("kknn")

  local_mocked_bindings(
    is_testing = function() {FALSE},
    initialize_catalog = redefer_initialize_catalog(rlang::current_env())
  )

  skip_on_cran()
  # Enough characters to see 'E: x1'
  rlang::local_options(cli.width = 84)

  raise_error_numbered <- function() {local({
    count <- 0
    function(x) {
      count <<- count + 1
      stop(paste0("error number ", count))
      "hi"
    }
  })}
  numbered <- raise_error_numbered()

  set.seed(1)
  expect_snapshot(
    {res_fit <-
      fit_resamples(
        parsnip::nearest_neighbor("regression", "kknn"),
        Sale_Price ~ .,
        rsample::vfold_cv(modeldata::ames[, c(72, 40:45)], 5),
        control = control_resamples(extract = numbered)
    )},
    transform = catalog_lines
  )

  # `catalog_summary_test` written to this env via `redefer_initialize_catalog()`
  expect_snapshot(catalog_summary_test)
})

test_that("interactive logger works (tune grid, error)", {
  skip_if(allow_parallelism(FALSE), "Will not catalog: parallelism is enabled")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("kknn")

  local_mocked_bindings(
    is_testing = function() {FALSE},
    initialize_catalog = redefer_initialize_catalog(rlang::current_env())
  )
  skip_on_cran()

  raise_error <- function(x) {stop("AHHhH")}

  set.seed(1)
  expect_snapshot(
    {res_fit <-
      tune_grid(
        parsnip::nearest_neighbor("regression", "kknn", dist_power = tune()),
        Sale_Price ~ .,
        rsample::vfold_cv(modeldata::ames[, c(72, 40:45)], 5),
        grid = 5,
        control = control_grid(extract = raise_error)
    )},
    transform = catalog_lines
  )

  # `catalog_summary_test` written to this env via `redefer_initialize_catalog()`
  expect_snapshot(catalog_summary_test)
})

test_that("interactive logger works (bayesian, error)", {
  skip_if(allow_parallelism(FALSE), "Will not catalog: parallelism is enabled")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("kknn")

  local_mocked_bindings(
    is_testing = function() {FALSE},
    initialize_catalog = redefer_initialize_catalog(rlang::current_env())
  )

  skip_on_cran()

  raise_error <- function(x) {stop("AHHhH")}

  set.seed(1)
  expect_snapshot(
    {res_grid <-
      tune_bayes(
        parsnip::nearest_neighbor("regression", "kknn", dist_power = tune()),
        Sale_Price ~ .,
        rsample::vfold_cv(modeldata::ames[, c(72, 40:45)], 5),
        initial = 5,
        iter = 5,
        control = control_bayes(extract = raise_error)
    )},
    transform = catalog_lines
  )

  # `catalog_summary_test` written to this env via `redefer_initialize_catalog()`
  expect_snapshot(catalog_summary_test)
})
