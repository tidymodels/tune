test_that("no parallelism", {
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

  daemons(0)
  ## Give daemons a chance to shutdown
  Sys.sleep(1)

  detach("package:mirai", character.only = TRUE)

  expect_equal(tune:::choose_framework(), "sequential")
  expect_snapshot(tune:::choose_framework(verbose = TRUE))
})

test_that("break parallelism tie", {
  skip_if_not_installed("mirai", minimum_version = "2.4.0")
  skip_if_not_installed("future")
  skip_on_cran()

  library(mirai)
  daemons(2)

  library(future)
  plan(multisession(workers = 2))

  expect_equal(tune:::choose_framework(), "mirai")
  expect_snapshot(tune:::choose_framework(verbose = TRUE))

  daemons(0)
  ## Give daemons a chance to shutdown
  Sys.sleep(1)
  detach("package:mirai", character.only = TRUE)

  plan(sequential)
  detach("package:future", character.only = TRUE)
})
