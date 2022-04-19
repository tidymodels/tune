# ------------------------------------------------------------------------------
# vec_restore()

test_that("vec_restore() returns a tune_results subclass if `x` retains tune_results structure", {
  for (x in helper_tune_results) {
    expect_identical(vec_restore(x, x), x)
    expect_s3_class_tune_results(vec_restore(x, x))
  }
})

test_that("vec_restore() returns bare tibble if `x` loses tune_results structure", {
  for (x in helper_tune_results) {
    col <- x[1]
    row <- x[0, ]

    expect_s3_class_bare_tibble(vec_restore(col, x))
    expect_s3_class_bare_tibble(vec_restore(row, x))
  }
})

# ------------------------------------------------------------------------------
# vec_ptype2()

test_that("vec_ptype2() is working", {
  for (x in helper_tune_results) {
    tbl <- tibble::tibble(x = 1)
    df <- data.frame(x = 1)

    # tune_results-tune_results
    expect_identical(vec_ptype2(x, x), vec_ptype2(new_bare_tibble(x), new_bare_tibble(x)))

    # tune_results-tbl_df
    expect_identical(vec_ptype2(x, tbl), vec_ptype2(new_bare_tibble(x), tbl))
    expect_identical(vec_ptype2(tbl, x), vec_ptype2(tbl, new_bare_tibble(x)))

    # tune_results-df
    expect_identical(vec_ptype2(x, df), vec_ptype2(new_bare_tibble(x), df))
    expect_identical(vec_ptype2(df, x), vec_ptype2(df, new_bare_tibble(x)))
  }
})

# ------------------------------------------------------------------------------
# vec_cast()

test_that("vec_cast() is working", {
  for (x in helper_tune_results) {
    tbl <- new_bare_tibble(x)
    df <- as.data.frame(tbl)

    # tune_results-tune_results
    expect_snapshot(error = TRUE, {
      vec_cast(x, x)
    })

    # tune_results-tbl_df
    expect_identical(vec_cast(x, tbl), tbl)
    expect_snapshot(error = TRUE, {
      vec_cast(tbl, x)
    })

    # tune_results-df
    expect_identical(vec_cast(x, df), df)
    expect_snapshot(error = TRUE, {
      vec_cast(df, x)
    })
  }
})

# ------------------------------------------------------------------------------
# vctrs methods

test_that("vec_ptype() returns a bare tibble", {
  for (x in helper_tune_results) {
    expect_identical(vec_ptype(x), vec_ptype(new_bare_tibble(x)))
    expect_s3_class_bare_tibble(vec_ptype(x))
  }
})

test_that("vec_slice() generally returns a bare tibble", {
  for (x in helper_tune_results) {
    expect_identical(vec_slice(x, 0), vec_slice(new_bare_tibble(x), 0))
    expect_s3_class_bare_tibble(vec_slice(x, 0))
  }
})

test_that("vec_slice() can return a tune_results if all rows are selected", {
  for (x in helper_tune_results) {
    expect_identical(vec_slice(x, TRUE), x)
    expect_s3_class_tune_results(vec_slice(x, TRUE))
  }
})

test_that("vec_c() returns a bare tibble", {
  for (x in helper_tune_results) {
    tbl <- new_bare_tibble(x)

    expect_identical(vec_c(x), vec_c(tbl))
    expect_identical(vec_c(x, x), vec_c(tbl, tbl))
    expect_identical(vec_c(x, tbl), vec_c(tbl, tbl))

    expect_s3_class_bare_tibble(vec_c(x))
    expect_s3_class_bare_tibble(vec_c(x, x))
  }
})

test_that("vec_rbind() returns a bare tibble", {
  for (x in helper_tune_results) {
    tbl <- new_bare_tibble(x)

    expect_identical(vec_rbind(x), vec_rbind(tbl))
    expect_identical(vec_rbind(x, x), vec_rbind(tbl, tbl))
    expect_identical(vec_rbind(x, tbl), vec_rbind(tbl, tbl))

    expect_s3_class_bare_tibble(vec_rbind(x))
    expect_s3_class_bare_tibble(vec_cbind(x, x)) %>% suppressMessages()
  }
})

test_that("vec_cbind() returns a bare tibble", {
  for (x in helper_tune_results) {
    tbl <- new_bare_tibble(x)

    expect_identical(vec_cbind(x), vec_cbind(tbl))
    expect_identical(vec_cbind(x, x), vec_cbind(tbl, tbl)) %>% suppressMessages()
    expect_identical(vec_cbind(x, tbl), vec_cbind(tbl, tbl)) %>% suppressMessages()

    expect_s3_class_bare_tibble(vec_cbind(x))
    expect_s3_class_bare_tibble(vec_cbind(x, x)) %>% suppressMessages()
  }
})
