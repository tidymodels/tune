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
