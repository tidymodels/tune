# nocov start

.onLoad <- function(libname, pkgname) {
  ns <- rlang::ns_env("tune")

  ns$manange_global_limit()

  # Modified version of the cli .onLoad()
  # We can't use cli::symbol$tick because the width of the character
  # looks awful when you output it alongside info / warning characters
  makeActiveBinding(
    "tune_symbol",
    function() {
      # If `cli.unicode` is set we use that
      opt <- getOption("cli.unicode", NULL)

      if (!is.null(opt)) {
        if (isTRUE(opt)) {
          return(tune_symbol_utf8)
        } else {
          return(tune_symbol_ascii)
        }
      }

      # Otherwise we try to auto-detect
      if (cli::is_utf8_output()) {
        tune_symbol_utf8
      } else if (is_latex_output()) {
        tune_symbol_ascii
      } else if (is_windows()) {
        tune_symbol_windows
      } else {
        tune_symbol_ascii
      }
    },
    ns
  )

  makeActiveBinding(
    "tune_color",
    function() {
      opt <- getOption("tidymodels.dark", NULL)

      if (!is.null(opt)) {
        if (isTRUE(opt)) {
          return(tune_color_dark)
        } else {
          return(tune_color_light)
        }
      }

      tune_color_light
    },
    ns
  )
}

# nocov end
