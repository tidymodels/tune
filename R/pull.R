extract_details <- function(object, extractor) {
  if (is.null(extractor)) {
    return(list())
  }
  extractor(object)
}

# ------------------------------------------------------------------------------

# Grab the new results, make sure that they align row-wise with the rsample
# object and then bind columns
pulley <- function(resamples, res, col, order) {
  if (all(purrr::map_lgl(res, inherits, "simpleError"))) {
    res <-
      resamples |>
      mutate(col = purrr::map(splits, ~NULL)) |>
      setNames(c(names(resamples), col))
    return(res)
  }

  all_null <- all(purrr::map_lgl(res, is.null))

  id_cols <- grep("^id", names(resamples), value = TRUE)

  resamples <- vctrs::vec_slice(resamples, order)

  pulled_vals <- purrr::map(res, ~ .x[[col]]) |> purrr::list_rbind()

  if (nrow(pulled_vals) == 0) {
    res <-
      resamples |>
      mutate(col = purrr::map(splits, ~NULL)) |>
      setNames(c(names(resamples), col))
    return(res)
  }

  pulled_vals <- tidyr::nest(pulled_vals, data = -starts_with("id"))
  names(pulled_vals)[ncol(pulled_vals)] <- col

  res <- new_bare_tibble(resamples)
  res <- full_join(res, pulled_vals, by = id_cols)
  res <- reup_rs(resamples, res)
  res
}

maybe_repair <- function(x) {
  not_null <- !purrr::map_lgl(x, is.null)
  is_tibb <- purrr::map_lgl(x, tibble::is_tibble)
  ok <- not_null & is_tibb
  if (!any(ok)) {
    return(x)
  }

  good_val <- which(ok)[1]
  template <- x[[good_val]][0, ]

  insert_val <- function(x, y) {
    if (is.null(x)) {
      x <- y
    }
    x
  }

  x <- purrr::map(x, insert_val, y = template)
  x
}

ensure_tibble <- function(x) {
  if (is.null(x)) {
    res <- tibble::new_tibble(list(.notes = character(0)), nrow = 0)
  } else {
    res <- tibble::new_tibble(list(.notes = x), nrow = length(x))
  }
  res
}

append_outcome_names <- function(all_outcome_names, outcome_names) {
  c(all_outcome_names, list(outcome_names))
}

#' Convenience functions to extract model
#'
#' `r lifecycle::badge("soft-deprecated")`
#'
#' Use [`extract_fit_engine()`][extract_fit_engine.tune_results()] instead of `extract_model()`.
#'
#' When extracting the fitted results, the workflow is easily accessible. If
#' there is only interest in the model, this functions can be used
#' as a shortcut
#' @param x A fitted workflow object.
#' @return A fitted model.
#' @export
extract_model <- function(x) {
  lifecycle::deprecate_warn(
    "0.1.6",
    "extract_model()",
    "extract_fit_engine()"
  )
  parsnip_fit <- extract_fit_parsnip(x)
  model <- parsnip_fit$fit
  model
}
