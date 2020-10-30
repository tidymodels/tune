# nocov start

.onLoad <- function(libname, pkgname) {
  ns <- rlang::ns_env("tune")


  # Modified version of the cli .onLoad()
  # We can't use cli::symbol$tick because the width of the character
  # looks awful when you output it alongside info / warning characters
  makeActiveBinding(
    "tune_symbol",
    function() {
      # If `cli.unicode` is set we use that
      opt <- getOption("cli.unicode",  NULL)

      if (!is.null(opt)) {
        if (isTRUE(opt)) return(tune_symbol_utf8) else return(tune_symbol_ascii)
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
      opt <- getOption("tidymodels.dark",  NULL)

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

  # lazily register autoplot
  s3_register("ggplot2::autoplot", "tune_results")
  # register tunable + required_pkgs
  maybe_register_S3_methods(ns)

  if (dplyr_pre_1.0.0()) {
    vctrs::s3_register("dplyr::mutate", "tune_results", method = mutate_tune_results)
    vctrs::s3_register("dplyr::arrange", "tune_results", method = arrange_tune_results)
    vctrs::s3_register("dplyr::filter", "tune_results", method = filter_tune_results)
    vctrs::s3_register("dplyr::rename", "tune_results", method = rename_tune_results)
    vctrs::s3_register("dplyr::select", "tune_results", method = select_tune_results)
    vctrs::s3_register("dplyr::slice", "tune_results", method = slice_tune_results)

    vctrs::s3_register("dplyr::mutate", "resample_results", method = mutate_resample_results)
    vctrs::s3_register("dplyr::arrange", "resample_results", method = arrange_resample_results)
    vctrs::s3_register("dplyr::filter", "resample_results", method = filter_resample_results)
    vctrs::s3_register("dplyr::rename", "resample_results", method = rename_resample_results)
    vctrs::s3_register("dplyr::select", "resample_results", method = select_resample_results)
    vctrs::s3_register("dplyr::slice", "resample_results", method = slice_resample_results)

    vctrs::s3_register("dplyr::mutate", "iteration_results", method = mutate_iteration_results)
    vctrs::s3_register("dplyr::arrange", "iteration_results", method = arrange_iteration_results)
    vctrs::s3_register("dplyr::filter", "iteration_results", method = filter_iteration_results)
    vctrs::s3_register("dplyr::rename", "iteration_results", method = rename_iteration_results)
    vctrs::s3_register("dplyr::select", "iteration_results", method = select_iteration_results)
    vctrs::s3_register("dplyr::slice", "iteration_results", method = slice_iteration_results)
  } else {
    vctrs::s3_register("dplyr::dplyr_reconstruct", "tune_results", method = dplyr_reconstruct_tune_results)
    vctrs::s3_register("dplyr::dplyr_reconstruct", "resample_results", method = dplyr_reconstruct_resample_results)
    vctrs::s3_register("dplyr::dplyr_reconstruct", "iteration_results", method = dplyr_reconstruct_iteration_results)
  }
}

# This package has specific methods for the `tunable()` and `required_pkgs()`
# generic. That generic is defined in the `generics` package. As of R 4.0, we need to register them.
maybe_register_S3_methods <- function(ns) {

  names <- names(ns)

  # ----------------------------------------------------------------------------

  tunable_names <- grep("^tunable.",  names, fixed = TRUE, value = TRUE)
  tunable_classes <- gsub("tunable.", "", tunable_names)

  for (i in seq_along(tunable_names)) {
    class <- tunable_classes[[i]]
    vctrs::s3_register("generics::tunable", class)
  }

  # ----------------------------------------------------------------------------

  req_pkgs_names <- grep("^required_pkgs\\.", names, value = TRUE)
  req_pkgs_classes <- gsub("required_pkgs.", "", req_pkgs_names)

  for (i in seq_along(req_pkgs_names)) {
    class <- req_pkgs_classes[[i]]
    vctrs::s3_register("generics::required_pkgs", class)
  }

  invisible()
}


# nocov end
