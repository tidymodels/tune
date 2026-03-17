# nocov start

# For use in setting the `tune_symbol` active binding in `.onLoad()`

tune_symbol_utf8 <- list(
  "success" = "\u2713"
)

tune_symbol_windows <- list(
  "success" = "\u221A"
)

tune_symbol_ascii <- list(
  "success" = "v"
)

## -----------------------------------------------------------------------------

# For use in setting the `tune_color` active binding in `.onLoad()`

tune_color_dark <- list(
  symbol = list(
    "warning" = cli::col_yellow,
    "go" = cli::col_white,
    "danger" = cli::col_red,
    "success" = cli::col_green,
    "info" = cli::col_blue
  ),
  message = list(
    "warning" = cli::col_yellow,
    "go" = cli::col_white,
    "danger" = cli::col_red,
    "success" = cli::col_white,
    "info" = cli::col_white
  )
)

tune_color_light <- list(
  symbol = list(
    "warning" = cli::col_yellow,
    "go" = cli::col_black,
    "danger" = cli::col_red,
    "success" = cli::col_green,
    "info" = cli::col_blue
  ),
  message = list(
    "warning" = cli::col_yellow,
    "go" = cli::col_black,
    "danger" = cli::col_red,
    "success" = cli::col_black,
    "info" = cli::col_black
  )
)

# ------------------------------------------------------------------------------

# cli:::is_latex_output()
is_latex_output <- function() {
  if (!("knitr" %in% loadedNamespaces())) {
    return(FALSE)
  }

  get("is_latex_output", asNamespace("knitr"))()
}

# cli:::is_windows()
is_windows <- function() {
  .Platform$OS.type == "windows"
}

# nocov end

#' Get colors for tune text.
#'
#' @keywords internal
#' @export
#' @rdname empty_ellipses
get_tune_colors <- function() tune_color
