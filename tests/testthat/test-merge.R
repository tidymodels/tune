context("merge grid values into objects")

# ------------------------------------------------------------------------------

library(rlang)
source("helper-objects.R")

# ------------------------------------------------------------------------------

isomap_grid <-
  tibble::tribble(
    ~imputation,          ~threshold, ~num_terms, ~neighbors,
     8L,   0.196735380217433,        45L,         5L,
    10L,   0.068447329569608,         4L,        10L,
     4L,   0.984495810698718,        14L,         5L,
     9L,   0.645629113074392,        18L,         9L,
     3L,   0.297346580307931,        40L,         7L,
     5L,   0.939461424015462,        16L,         9L,
     8L,   0.913900346728042,        36L,         2L,
     1L,   0.780177258886397,        33L,         8L,
     7L,   0.730175697244704,         2L,         5L,
     8L, 0.00493585434742272,        21L,         2L
  )

bst_grid <- tibble("funky name \n" = 1:4, rules = rep(c(TRUE, FALSE), each = 2))

# ------------------------------------------------------------------------------

test_that('recipe merges', {

  expect_error(
    isomap_updated <- merge(isomap_rec, isomap_grid),
    NA
  )
  check_merged_tibble(isomap_updated)
  for (i in 1:nrow(isomap_grid)) {
    expect_equal(
      isomap_updated$x[[i]]$steps[[4]]$neighbors, isomap_grid$imputation[[i]]
    )
    expect_equal(
      isomap_updated$x[[i]]$steps[[5]]$threshold, isomap_grid$threshold[[i]]
    )
    expect_equal(
      isomap_updated$x[[i]]$steps[[8]]$num_terms, isomap_grid$num_terms[[i]]
    )
    expect_equal(
      isomap_updated$x[[i]]$steps[[8]]$neighbors, isomap_grid$neighbors[[i]]
    )
  }

})

test_that('partially recipe merge', {

  expect_error(
    isomap_updated <- merge(isomap_rec, isomap_grid[, -1]),
    NA
  )
  check_merged_tibble(isomap_updated, complete = FALSE)
  for (i in 1:nrow(isomap_grid)) {
    expect_equal(
      isomap_updated$x[[i]]$steps[[4]]$neighbors, tune("imputation")
    )
    expect_equal(
      isomap_updated$x[[i]]$steps[[5]]$threshold, isomap_grid$threshold[[i]]
    )
    expect_equal(
      isomap_updated$x[[i]]$steps[[8]]$num_terms, isomap_grid$num_terms[[i]]
    )
    expect_equal(
      isomap_updated$x[[i]]$steps[[8]]$neighbors, isomap_grid$neighbors[[i]]
    )
  }

})

test_that('umerged recipe merge', {

  expect_error(
    isomap_updated <- merge(isomap_rec, bst_grid),
    NA
  )
  check_merged_tibble(isomap_updated, complete = FALSE)
  for (i in 1:nrow(bst_grid)) {
    expect_equal(
      isomap_updated$x[[i]]$steps[[4]]$neighbors, tune("imputation")
    )
    expect_equal(
      isomap_updated$x[[i]]$steps[[5]]$threshold, tune()
    )
    expect_equal(
      isomap_updated$x[[i]]$steps[[8]]$num_terms, tune()
    )
    expect_equal(
      isomap_updated$x[[i]]$steps[[8]]$neighbors, tune()
    )
  }

})

# ------------------------------------------------------------------------------



test_that('model spec merges', {

  expect_error(
    bst_updated <- merge(bst_model, bst_grid),
    NA
  )
  check_merged_tibble(bst_updated, "model_spec")
  for (i in 1:nrow(bst_grid)) {
    expect_equal(
      bst_updated$x[[i]]$args$trees,
      as_quosure(bst_grid[["funky name \n"]][[i]], empty_env())
    )
    expect_equal(
      bst_updated$x[[i]]$eng_args$rules,
      as_quosure(bst_grid$rules[[i]], empty_env())
    )
  }

})

test_that('partially model spec merge', {

  expect_error(
    bst_updated <- merge(bst_model, bst_grid[, -1]),
    NA
  )
  check_merged_tibble(bst_updated, "model_spec", complete = FALSE)
  for (i in 1:nrow(bst_grid)) {
    expect_equal(
      rlang::get_expr(bst_updated$x[[i]]$args$trees), tune("funky name \n")
    )
    expect_equal(
      bst_updated$x[[i]]$eng_args$rules,
      as_quosure(bst_grid$rules[[i]], empty_env())
    )
  }

})

test_that('umerged model spec merge', {

  other_grid <- bst_grid
  names(bst_grid) <- letters[1:2]
  expect_error(
    bst_not_updated <- merge(bst_model, other_grid),
    NA
  )
  check_merged_tibble(bst_not_updated, "model_spec", complete = FALSE)
  # for (i in 1:nrow(other_grid)) {
  #   if (i == 1) {
  #     print(rlang::get_expr(bst_not_updated$x[[i]]$args$trees))
  #     print(rlang::get_expr(bst_not_updated$x[[i]]$eng_args$rules))
  #   }
  #   expect_equal(
  #     rlang::get_expr(bst_not_updated$x[[i]]$args$trees), tune("funky name \n")
  #   )
  #   expect_equal(
  #     rlang::get_expr(bst_not_updated$x[[i]]$eng_args$rules), tune()
  #   )
  # }

})

