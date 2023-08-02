library(dplyr)

# ------------------------------------------------------------------------------
# dplyr_reconstruct()

test_that("dplyr_reconstruct() returns a tune_results subclass if `x` retains tune_results structure", {
  for (x in helper_tune_results) {
    expect_identical(dplyr_reconstruct(x, x), x)
    expect_s3_class_tune_results(dplyr_reconstruct(x, x))
  }
})

test_that("dplyr_reconstruct() returns bare tibble if `x` loses tune_results structure", {
  for (x in helper_tune_results) {
    col <- x[1]
    row <- x[0, ]

    expect_s3_class_bare_tibble(dplyr_reconstruct(col, x))
    expect_s3_class_bare_tibble(dplyr_reconstruct(row, x))
  }
})

# ------------------------------------------------------------------------------
# dplyr_col_modify()

test_that("can add columns and retain tune_results class", {
  for (x in helper_tune_results) {
    cols <- list(x = rep(1, vec_size(x)))

    result <- dplyr_col_modify(x, cols)

    expect_s3_class_tune_results(result)
    expect_identical(result$x, cols$x)
  }
})

test_that("modifying tune_results columns removes tune_results class", {
  for (x in helper_tune_results) {
    cols <- list(splits = rep(1, vec_size(x)))

    result <- dplyr_col_modify(x, cols)

    expect_s3_class_bare_tibble(result)
    expect_identical(result$splits, cols$splits)
  }

  for (x in helper_tune_results) {
    cols <- list(id = rep(1, vec_size(x)))

    result <- dplyr_col_modify(x, cols)

    expect_s3_class_bare_tibble(result)
    expect_identical(result$id, cols$id)
  }
})

test_that("replacing tune_results columns with the exact same column retains tune_results class", {
  for (x in helper_tune_results) {
    cols <- list(splits = x$splits)

    result <- dplyr_col_modify(x, cols)

    expect_s3_class_tune_results(result)
    expect_identical(result, x)
  }
})

# ------------------------------------------------------------------------------
# dplyr_row_slice()

test_that("row slicing generally removes the tune_results subclass", {
  for (x in helper_tune_results) {
    expect_s3_class_bare_tibble(dplyr_row_slice(x, 0))
  }
})

test_that("row slicing and duplicating any rows removes the tune_results subclass", {
  for (x in helper_tune_results) {
    loc <- seq_len(nrow(x))
    loc[length(loc)] <- 1L
    expect_s3_class_bare_tibble(dplyr_row_slice(x, loc))
  }
})

test_that("row slicing and selecting everything keeps the tune_results subclass", {
  for (x in helper_tune_results) {
    loc <- seq_len(nrow(x))
    expect_s3_class_tune_results(dplyr_row_slice(x, loc))
  }
})

test_that("tune_results subclass is kept if row order is changed but all rows are present", {
  for (x in helper_tune_results) {
    loc <- rev(seq_len(nrow(x)))
    expect_s3_class_tune_results(dplyr_row_slice(x, loc))
  }
})

# ------------------------------------------------------------------------------
# summarise()

test_that("summarise() always drops the tune_results class", {
  for (x in helper_tune_results) {
    expect_s3_class_bare_tibble(summarise(x, y = 1))
    expect_s3_class_bare_tibble(summarise(x, splits = splits[1], id = id[1]))
  }
})


# ------------------------------------------------------------------------------
# mutate()

test_that("mutate() can keep tune_results class", {
  for (x in helper_tune_results) {
    expect_s3_class_tune_results(mutate(x, x = 1))
    expect_identical(mutate(x, x = 1)$x, rep(1, vec_size(x)))
  }
})

test_that("mutate() drops tune_results class if any tune_results columns are touched", {
  for (x in helper_tune_results) {
    expect_s3_class_bare_tibble(mutate(x, splits = 1))
    expect_s3_class_bare_tibble(mutate(x, id = 1))

    expect_identical(mutate(x, splits = 1)$splits, rep(1, vec_size(x)))
    expect_identical(mutate(x, id = 1)$id, rep(1, vec_size(x)))
  }
})

test_that("mutate() keeps tune_results class if replacement tune_results column is same as original", {
  for (x in helper_tune_results) {
    expect_s3_class_tune_results(mutate(x, splits = splits))
    expect_s3_class_tune_results(mutate(x, id = id))
  }
})

test_that("adding a column that looks like an `id` drops the class", {
  for (x in helper_tune_results) {
    expect_s3_class_bare_tibble(mutate(x, id9 = 1))
  }
})

# ------------------------------------------------------------------------------
# arrange()

test_that("arrange() keeps tune_results class when row order is modified", {
  for (x in helper_tune_results) {
    x <- mutate(x, rn = row_number())
    expect_s3_class_tune_results(arrange(x, desc(rn)))
  }
})

test_that("arrange() keeps tune_results class when row order is untouched", {
  for (x in helper_tune_results) {
    expect_s3_class_tune_results(arrange(x))

    x <- mutate(x, rn = row_number())
    expect_s3_class_tune_results(arrange(x, rn))
  }
})

# ------------------------------------------------------------------------------
# filter()

test_that("filter() drops tune_results class when rows are modified", {
  for (x in helper_tune_results) {
    expect_s3_class_bare_tibble(filter(x, 0 == 1))
    expect_s3_class_bare_tibble(filter(x, is.numeric(id)))
  }
})

test_that("filter() keeps tune_results class if row order is untouched", {
  for (x in helper_tune_results) {
    expect_s3_class_tune_results(filter(x))
    expect_s3_class_tune_results(filter(x, is.character(id)))
  }
})

# ------------------------------------------------------------------------------
# rename()

test_that("renaming can keep the tune_results class", {
  for (x in helper_tune_results) {
    x <- mutate(x, a = 1)
    x <- rename(x, b = a)
    expect_s3_class_tune_results(x)
  }
})

test_that("renaming `id` at all drops the tune_results class", {
  for (x in helper_tune_results) {
    x <- rename(x, id9 = id)
    expect_s3_class_bare_tibble(x)
  }
})

test_that("renaming `id` to a non-id name drops the tune_results class", {
  for (x in helper_tune_results) {
    x <- rename(x, stuff = id)
    expect_s3_class_bare_tibble(x)
  }
})

# ------------------------------------------------------------------------------
# select()

test_that("select() can keep tune_results class", {
  for (x in helper_tune_results) {
    expect_s3_class_tune_results(select(x, everything()))
  }
})

test_that("select() drops tune_results class if any tune_results columns aren't selected", {
  for (x in helper_tune_results) {
    expect_s3_class_bare_tibble(select(x, id))
    expect_s3_class_bare_tibble(select(x, splits))
  }
})

# ------------------------------------------------------------------------------
# slice()

test_that("slice() drops tune_results class when rows are modified", {
  for (x in helper_tune_results) {
    expect_s3_class_bare_tibble(slice(x, 0))
  }
})

test_that("slice() keeps tune_results class when rows are untouched", {
  for (x in helper_tune_results) {
    expect_s3_class_tune_results(slice(x, seq_len(nrow(x))))
  }
})

# ------------------------------------------------------------------------------
# group_by()

test_that("group_by() always returns a bare grouped-df or bare tibble", {
  for (x in helper_tune_results) {
    expect_s3_class_bare_tibble(group_by(x))
    expect_s3_class(group_by(x, splits), c("grouped_df", "tbl_df", "tbl", "data.frame"), exact = TRUE)
  }
})

# ------------------------------------------------------------------------------
# ungroup()

test_that("ungroup() returns a tune_results", {
  for (x in helper_tune_results) {
    expect_s3_class_tune_results(ungroup(x))
  }
})

# ------------------------------------------------------------------------------
# relocate()

test_that("can relocate() and keep the class", {
  for (x in helper_tune_results) {
    x <- relocate(x, id)
    expect_s3_class_tune_results(x)
  }
})

# ------------------------------------------------------------------------------
# distinct()

test_that("distinct() keeps the class if everything is intact", {
  for (x in helper_tune_results) {
    expect_s3_class_tune_results(distinct(x))
  }
})

test_that("distinct() drops the class if any tune_results columns are lost", {
  for (x in helper_tune_results) {
    expect_s3_class_bare_tibble(distinct(x, splits))
  }
})

# ------------------------------------------------------------------------------
# left_join()

test_that("left_join() can keep tune_results class if tune_results structure is intact", {
  for (x in helper_tune_results) {
    expect_s3_class_tune_results(left_join(x, x, by = names(x)))

    y <- tibble(id = x$id[[1]], x = 1)
    expect_s3_class_tune_results(left_join(x, y, by = "id"))
  }
})

test_that("left_join() can lose tune_results class if rows are added", {
  for (x in helper_tune_results) {
    y <- tibble(id = x$id[[1]], x = 1:2)
    expect_s3_class_bare_tibble(
      left_join(x, y, by = "id", relationship = "many-to-many")
    )
  }
})

# ------------------------------------------------------------------------------
# right_join()

test_that("right_join() can keep tune_results class if tune_results structure is intact", {
  for (x in helper_tune_results) {
    expect_s3_class_tune_results(right_join(x, x, by = names(x)))

    x_names <- names(x)
    y_names <- x_names[col_starts_with_id(x_names)]

    # Also add `.iter` column
    if (inherits(x, "iteration_results")) {
      y_names <- c(y_names, x_names[col_equals_dot_iter(x_names)])
    }

    y <- mutate(select(x, dplyr::all_of(y_names)), x = 1)
    expect_s3_class_tune_results(right_join(x, y, by = y_names))
  }
})

test_that("right_join() can lose tune_results class if rows are added", {
  for (x in helper_tune_results) {
    y <- tibble(id = x$id[[1]], x = 1:2)
    expect_s3_class_bare_tibble(
      right_join(x, y, by = "id", relationship = "many-to-many")
    )
  }
})

test_that("right_join() restores to the type of first input", {
  for (x in helper_tune_results) {
    y <- tibble(id = x$id[[1]], x = 1)
    # technically tune_results structure is intact, but `y` is a bare tibble!
    expect_s3_class_bare_tibble(right_join(y, x, by = "id", multiple = "all"))
  }
})

# ------------------------------------------------------------------------------
# full_join()

test_that("full_join() can keep tune_results class if tune_results structure is intact", {
  for (x in helper_tune_results) {
    expect_s3_class_tune_results(full_join(x, x, by = names(x)))
  }
})

test_that("full_join() can lose tune_results class if rows are added", {
  for (x in helper_tune_results) {
    y <- tibble(id = "foo", x = 1)
    expect_s3_class_bare_tibble(full_join(x, y, by = "id"))
  }
})

# ------------------------------------------------------------------------------
# anti_join()

test_that("anti_join() can keep tune_results class if tune_results structure is intact", {
  for (x in helper_tune_results) {
    y <- tibble(id = "foo")
    expect_s3_class_tune_results(anti_join(x, y, by = "id"))
  }
})

test_that("anti_join() can lose tune_results class if rows are removed", {
  for (x in helper_tune_results) {
    y <- tibble(id = x$id[[1]], x = 1)
    expect_s3_class_bare_tibble(anti_join(x, y, by = "id"))
  }
})

# ------------------------------------------------------------------------------
# semi_join()

test_that("semi_join() can keep tune_results class if tune_results structure is intact", {
  for (x in helper_tune_results) {
    expect_s3_class_tune_results(semi_join(x, x, by = names(x)))
  }
})

test_that("semi_join() can lose tune_results class if rows are removed", {
  for (x in helper_tune_results) {
    y <- tibble(id = "foo", x = 1)
    expect_s3_class_bare_tibble(semi_join(x, y, by = "id"))
  }
})

# ------------------------------------------------------------------------------
# nest_join()

test_that("nest_join() can keep tune_results class if tune_results structure is intact", {
  for (x in helper_tune_results) {
    y <- mutate(x, foo = "bar")
    expect_s3_class_tune_results(nest_join(x, y, by = names(x)))
  }
})

# ------------------------------------------------------------------------------
# bind_rows()

test_that("bind_rows() keeps the class if there are no new rows/cols and the first object is an tune_results subclass", {
  for (x in helper_tune_results) {
    expect_s3_class_tune_results(bind_rows(x))
    expect_s3_class_tune_results(bind_rows(x, tibble()))
    expect_s3_class_bare_tibble(bind_rows(tibble(), x))
  }
})

test_that("bind_rows() drops the class with new rows", {
  for (x in helper_tune_results) {
    expect_s3_class_bare_tibble(bind_rows(x, x))
  }
})

# ------------------------------------------------------------------------------
# bind_cols()

test_that("bind_cols() keeps the class if there are no new rows and the first object is an tune_results subclass", {
  for (x in helper_tune_results) {
    expect_s3_class_tune_results(bind_cols(x))
    expect_s3_class_tune_results(bind_cols(x, tibble(x = 1)))
    expect_s3_class_bare_tibble(bind_cols(tibble(x = 1), x))
  }
})

test_that("bind_cols() drops the class with new rows", {
  # Use tune_results subclass with 1 row, these get recycled
  x <- helper_tune_results$apparent
  expect_s3_class_bare_tibble(bind_cols(x, tibble(x = 1:2)))
})
