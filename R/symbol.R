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

# ------------------------------------------------------------------------------

# For use in setting the `tune_color` active binding in `.onLoad()`

tune_color_dark <- list(
  symbol = list(
    "warning" = crayon::yellow,
    "go" = crayon::white,
    "danger" = crayon::red,
    "success" = crayon::green,
    "info" = crayon::blue
  ),
  message = list(
    "warning" = crayon::yellow,
    "go" = crayon::white,
    "danger" = crayon::red,
    "success" = crayon::white,
    "info" = crayon::white
  )
)

tune_color_light <- list(
  symbol = list(
    "warning" = crayon::yellow,
    "go" = crayon::black,
    "danger" = crayon::red,
    "success" = crayon::green,
    "info" = crayon::blue
  ),
  message = list(
    "warning" = crayon::yellow,
    "go" = crayon::black,
    "danger" = crayon::red,
    "success" = crayon::black,
    "info" = crayon::black
  )
)

# ------------------------------------------------------------------------------

# cli:::is_latex_output()
is_latex_output <- function () {
  if (!("knitr" %in% loadedNamespaces())) {
    return(FALSE)
  }

  get("is_latex_output", asNamespace("knitr"))()
}

# cli:::is_windows()
is_windows <- function () {
  .Platform$OS.type == "windows"
}

# nocov end
