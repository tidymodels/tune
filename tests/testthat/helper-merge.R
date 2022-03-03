check_merged_tibble <- function(x, type = "recipe", complete = TRUE) {
  expect_true(tibble::is_tibble(x))
  expect_equal(names(x), "x")
  expect_true(all(purrr::map_lgl(x$x, inherits, type)))
  if (complete) {
    any_args <- purrr::map_int(x$x, ~ tune_args(.x) %>% nrow())
    expect_true(!any(any_args > 0))
  }
  invisible(TRUE)
}
