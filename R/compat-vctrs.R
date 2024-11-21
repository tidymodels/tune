#' @import vctrs
NULL

# ------------------------------------------------------------------------------
# tune_results

#' @export
vec_restore.tune_results <- function(x, to, ...) {
  tune_results_reconstruct(x, to)
}


#' @export
vec_ptype2.tune_results.tune_results <- function(x, y, ...) {
  stop_never_called("vec_ptype2.tune_results.tune_results")
}
#' @export
vec_ptype2.tune_results.tbl_df <- function(x, y, ...) {
  stop_never_called("vec_ptype2.tune_results.tbl_df")
}
#' @export
vec_ptype2.tbl_df.tune_results <- function(x, y, ...) {
  stop_never_called("vec_ptype2.tbl_df.tune_results")
}
#' @export
vec_ptype2.tune_results.data.frame <- function(x, y, ...) {
  stop_never_called("vec_ptype2.tune_results.data.frame")
}
#' @export
vec_ptype2.data.frame.tune_results <- function(x, y, ...) {
  stop_never_called("vec_ptype2.data.frame.tune_results")
}


#' @export
vec_cast.tune_results.tune_results <- function(x, to, ...) {
  stop_incompatible_cast_tune_results(x, to, ...)
}
#' @export
vec_cast.tune_results.tbl_df <- function(x, to, ...) {
  stop_incompatible_cast_tune_results(x, to, ...)
}
#' @export
vec_cast.tbl_df.tune_results <- function(x, to, ...) {
  tib_cast(x, to, ...)
}
#' @export
vec_cast.tune_results.data.frame <- function(x, to, ...) {
  stop_incompatible_cast_tune_results(x, to, ...)
}
#' @export
vec_cast.data.frame.tune_results <- function(x, to, ...) {
  df_cast(x, to, ...)
}

# ------------------------------------------------------------------------------
# resample_results

#' @export
vec_restore.resample_results <- function(x, to, ...) {
  resample_results_reconstruct(x, to)
}


#' @export
vec_ptype2.resample_results.resample_results <- function(x, y, ...) {
  stop_never_called("vec_ptype2.resample_results.resample_results")
}
#' @export
vec_ptype2.resample_results.tbl_df <- function(x, y, ...) {
  stop_never_called("vec_ptype2.resample_results.tbl_df")
}
#' @export
vec_ptype2.tbl_df.resample_results <- function(x, y, ...) {
  stop_never_called("vec_ptype2.tbl_df.resample_results")
}
#' @export
vec_ptype2.resample_results.data.frame <- function(x, y, ...) {
  stop_never_called("vec_ptype2.resample_results.data.frame")
}
#' @export
vec_ptype2.data.frame.resample_results <- function(x, y, ...) {
  stop_never_called("vec_ptype2.data.frame.resample_results")
}


#' @export
vec_cast.resample_results.resample_results <- function(x, to, ...) {
  stop_incompatible_cast_resample_results(x, to, ...)
}
#' @export
vec_cast.resample_results.tbl_df <- function(x, to, ...) {
  stop_incompatible_cast_resample_results(x, to, ...)
}
#' @export
vec_cast.tbl_df.resample_results <- function(x, to, ...) {
  tib_cast(x, to, ...)
}
#' @export
vec_cast.resample_results.data.frame <- function(x, to, ...) {
  stop_incompatible_cast_resample_results(x, to, ...)
}
#' @export
vec_cast.data.frame.resample_results <- function(x, to, ...) {
  df_cast(x, to, ...)
}

# ------------------------------------------------------------------------------
# iteration_results

#' @export
vec_restore.iteration_results <- function(x, to, ...) {
  iteration_results_reconstruct(x, to)
}


#' @export
vec_ptype2.iteration_results.iteration_results <- function(x, y, ...) {
  stop_never_called("vec_ptype2.iteration_results.iteration_results")
}
#' @export
vec_ptype2.iteration_results.tbl_df <- function(x, y, ...) {
  stop_never_called("vec_ptype2.iteration_results.tbl_df")
}
#' @export
vec_ptype2.tbl_df.iteration_results <- function(x, y, ...) {
  stop_never_called("vec_ptype2.tbl_df.iteration_results")
}
#' @export
vec_ptype2.iteration_results.data.frame <- function(x, y, ...) {
  stop_never_called("vec_ptype2.iteration_results.data.frame")
}
#' @export
vec_ptype2.data.frame.iteration_results <- function(x, y, ...) {
  stop_never_called("vec_ptype2.data.frame.iteration_results")
}


#' @export
vec_cast.iteration_results.iteration_results <- function(x, to, ...) {
  stop_incompatible_cast_iteration_results(x, to, ...)
}
#' @export
vec_cast.iteration_results.tbl_df <- function(x, to, ...) {
  stop_incompatible_cast_iteration_results(x, to, ...)
}
#' @export
vec_cast.tbl_df.iteration_results <- function(x, to, ...) {
  tib_cast(x, to, ...)
}
#' @export
vec_cast.iteration_results.data.frame <- function(x, to, ...) {
  stop_incompatible_cast_iteration_results(x, to, ...)
}
#' @export
vec_cast.data.frame.iteration_results <- function(x, to, ...) {
  df_cast(x, to, ...)
}

# ------------------------------------------------------------------------------

stop_incompatible_cast_tune_results <- function(x, to, ..., x_arg = "", to_arg = "") {
  details <- "Can't cast to a <tune_results> because attributes are likely incompatible."
  stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg, details = details)
}

stop_incompatible_cast_resample_results <- function(x, to, ..., x_arg = "", to_arg = "") {
  details <- "Can't cast to a <resample_results> because attributes are likely incompatible."
  stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg, details = details)
}

stop_incompatible_cast_iteration_results <- function(x, to, ..., x_arg = "", to_arg = "") {
  details <- "Can't cast to a <iteration_results> because attributes are likely incompatible."
  stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg, details = details)
}

stop_never_called <- function(fn) {
  cli::cli_abort("Internal error: {.fn {fn}} should never be called.")
}
