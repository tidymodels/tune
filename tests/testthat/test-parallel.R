test_that("no parallelism", {
  skip_if_not_installed("mirai")

  svm_spec <- svm_rbf(mode = "classification")
  ctrl_no <- control_grid(allow_par = FALSE)
  ctrl_java <- control_grid(pkgs = "rJava")

  # ---------------------------------------------------------------------------
  # default

  expect_equal(tune:::choose_framework(), "sequential")
  expect_snapshot(tune:::choose_framework(verbose = TRUE))

  # ----------------------------------------------------------------------------
  # Due to

  expect_equal(tune:::choose_framework(control = ctrl_no), "sequential")
  expect_equal(tune:::choose_framework(svm_spec), "sequential")
})

test_that("enable future parallelism", {
  skip_if_not_installed("future")
  skip_if_not_installed("mirai")

  # temporary issue with GHA
  skip_on_os("mac")

  svm_spec <- svm_rbf(mode = "classification")
  ctrl_no <- control_grid(allow_par = FALSE)
  ctrl_java <- control_grid(pkgs = "rJava")

  library(future)

  # ----------------------------------------------------------------------------
  # sequential; not enough workers

  expect_equal(tune:::choose_framework(), "sequential")
  expect_snapshot(tune:::choose_framework(verbose = TRUE))

  # ----------------------------------------------------------------------------
  # parallel

  plan(multisession(workers = 2))
  expect_equal(tune:::choose_framework(), "future")
  expect_snapshot(tune:::choose_framework(verbose = TRUE))
  expect_equal(tune:::choose_framework(svm_spec), "future")

  # ----------------------------------------------------------------------------
  # sequential due to restrictions

  expect_equal(tune:::choose_framework(control = ctrl_no), "sequential")
  expect_snapshot(tune:::choose_framework(control = ctrl_no, verbose = TRUE))

  expect_equal(tune:::choose_framework(control = ctrl_java), "sequential")
  expect_snapshot(tune:::choose_framework(control = ctrl_java, verbose = TRUE))

  plan(multisession(workers = 1))

  expect_equal(tune:::choose_framework(), "sequential")
  expect_snapshot(tune:::choose_framework(verbose = TRUE))

  plan(sequential)
  detach("package:future", character.only = TRUE)

  expect_equal(tune:::choose_framework(), "sequential")
  expect_snapshot(tune:::choose_framework(verbose = TRUE))
})

test_that("enable mirai parallelism", {
  skip_if_not_installed("mirai", minimum_version = "2.4.0")

  svm_spec <- svm_rbf(mode = "classification")
  ctrl_no <- control_grid(allow_par = FALSE)
  ctrl_java <- control_grid(pkgs = "rJava")

  library(mirai)

  # ----------------------------------------------------------------------------
  # sequential; not enough workers

  expect_equal(tune:::choose_framework(), "sequential")
  expect_snapshot(tune:::choose_framework(verbose = TRUE))

  # ----------------------------------------------------------------------------
  # parallel

  daemons(2)

  expect_equal(tune:::choose_framework(), "mirai")
  expect_snapshot(tune:::choose_framework(verbose = TRUE))
  expect_equal(tune:::choose_framework(svm_spec), "mirai")

  # ----------------------------------------------------------------------------
  # sequential due to restrictions

  expect_equal(tune:::choose_framework(control = ctrl_no), "sequential")
  expect_snapshot(tune:::choose_framework(control = ctrl_no, verbose = TRUE))

  expect_equal(tune:::choose_framework(control = ctrl_java), "sequential")
  expect_snapshot(tune:::choose_framework(control = ctrl_java, verbose = TRUE))

  daemons(0)
  daemons(1)

  expect_equal(tune:::choose_framework(), "sequential")
  expect_snapshot(tune:::choose_framework(verbose = TRUE))

  # teardown
  daemons(0)
  ## Give daemons a chance to shutdown
  Sys.sleep(1)

  expect_equal(tune:::choose_framework(), "sequential")
  expect_snapshot(tune:::choose_framework(verbose = TRUE))
})

test_that("break parallelism tie", {
  skip_if_not_installed("mirai", minimum_version = "2.4.0")
  skip_if_not_installed("future")
  skip_on_cran()

  # temporary issue with GHA
  skip_on_os("mac")

  library(mirai)
  daemons(2)

  library(future)
  plan(multisession(workers = 2))

  expect_equal(tune:::choose_framework(), "mirai")
  expect_snapshot(tune:::choose_framework(verbose = TRUE))

  # teardown
  daemons(0)
  ## Give daemons a chance to shutdown
  Sys.sleep(1)

  plan(sequential)
})

test_that("loop execution code", {
  skip_if_not_installed("mirai")

  # sequential
  expect_snapshot(tune:::loop_call("resamples", "sequential", list()))
  expect_snapshot(
    tune:::loop_call(
      "resamples",
      "sequential",
      list(a = quote(a))
    )
  )
  expect_snapshot(tune:::loop_call("everything", "sequential", list()))
  expect_snapshot(
    tune:::loop_call(
      "everything",
      "sequential",
      list(a = quote(a))
    )
  )

  # future
  expect_snapshot(tune:::loop_call("resamples", "future", list()))
  expect_snapshot(
    tune:::loop_call(
      "resamples",
      "future",
      list(a = quote(a))
    )
  )
  expect_snapshot(tune:::loop_call("everything", "future", list()))
  expect_snapshot(
    tune:::loop_call(
      "everything",
      "future",
      list(a = quote(a))
    )
  )

  # mirai
  expect_snapshot(tune:::loop_call("resamples", "mirai", list()))
  expect_snapshot(
    tune:::loop_call(
      "resamples",
      "mirai",
      list(a = quote(a))
    )
  )
  expect_snapshot(tune:::loop_call("everything", "mirai", list()))
  expect_snapshot(
    tune:::loop_call(
      "everything",
      "mirai",
      list(a = quote(a))
    )
  )
})


test_that("same results using mirai", {
  skip_if_not_installed("mirai")
  skip_if_not_installed("xgboost")
  skip_if_not_installed("modeldata")
  skip_on_cran()

  set.seed(1)
  dat <- modeldata::sim_regression(500)
  rs <- vfold_cv(dat)

  # ------------------------------------------------------------------------------

  mod <- boost_tree(min_n = tune(), trees = 20, learn_rate = tune()) |>
    set_mode("regression")

  simple_wflow <- workflow(outcome ~ ., mod)

  set.seed(1)
  seq_res <-
    simple_wflow |>
    tune_grid(
      resamples = rs,
      grid = 4
    )

  seq_mtr <- collect_metrics(seq_res)

  tmp <- mirai::daemons(2)

  set.seed(1)
  mirai_res <-
    simple_wflow |>
    tune_grid(
      resamples = rs,
      grid = 4
    )

  mirai_mtr <- collect_metrics(mirai_res)

  expect_equal(seq_mtr, mirai_mtr)

  tmp <- mirai::daemons(0)
})


test_that("same results using future", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  skip_if_not_installed("xgboost")
  skip_if_not_installed("modeldata")
  skip_on_cran()

  # temporary issue with GHA
  skip_on_os("mac")

  set.seed(1)
  dat <- modeldata::sim_regression(500)
  rs <- vfold_cv(dat)

  # ------------------------------------------------------------------------------

  mod <- boost_tree(min_n = tune(), trees = 20, learn_rate = tune()) |>
    set_mode("regression")

  simple_wflow <- workflow(outcome ~ ., mod)

  set.seed(1)
  seq_res <-
    simple_wflow |>
    tune_grid(
      resamples = rs,
      grid = 4
    )

  seq_mtr <- collect_metrics(seq_res)

  future::plan(future::multisession(workers = 2))

  set.seed(1)
  future_res <-
    simple_wflow |>
    tune_grid(
      resamples = rs,
      grid = 4
    )

  future_mtr <- collect_metrics(future_res)

  expect_equal(seq_mtr, future_mtr)

  future::plan("sequential")
})


test_that("generating parallel seeds does not affect the space-time continuum", {
  og_seed <- .Random.seed

  before_kind <- RNGkind()
  set.seed(1)
  before_rnd <- runif(1)

  set.seed(1)
  tmp <- tune:::get_parallel_seeds(3)
  after_rng <- runif(1)

  expect_equal(3, length(tmp))
  expect_equal(before_rnd, after_rng)
  expect_equal(before_kind, RNGkind())

  # The seeds work
  assign(".Random.seed", tmp[[1]], envir = .GlobalEnv)
  unif_1 <- runif(1)

  assign(".Random.seed", tmp[[1]], envir = .GlobalEnv)
  unif_2 <- runif(1)

  expect_equal(unif_1, unif_2)
  assign(".Random.seed", og_seed, envir = .GlobalEnv)
})
