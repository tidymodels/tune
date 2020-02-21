## adapted from
## https://github.com/hadley/dtplyr/blob/2308ff25e88bb81fe84f9051e37ddd9d572189ee/R/compat-dplyr-0.6.0.R
## and based on
## https://github.com/tidyverse/googledrive/commit/95455812d2e0d6bdf92b5f6728e3265bf65d8467#diff-ba61d4f2ccd992868e27305a9ab68a3c


# ------------------------------------------------------------------------------
# most common dplyr verbs do not affect the attributes of an object with class
# 'tune_resulst' except for `mutate()`. We create a new `mutate()` for that
# class and restore anything that was removed by the dplyr method.

reset_tune_results <- function(x) {
  stopifnot(inherits(x, "data.frame"))
  structure(x, class = class(tibble()))
}

# Detect when the object really has class `tune_results` based on the
# columns and data structure.
is_tune_results <- function(x) {
  tibble::is_tibble(x) &&
    all(c("splits", ".notes", ".metrics") %in% names(x)) &&
    length(grepl("^id", names(x))) > 0
}

# Fix the object if classes and/or attributes were stripped off.
maybe_tune_results <- function(x, extras = NULL, att = NULL) {
  if (is_tune_results(x)) {
    x <- reset_tune_results(x)

    ## possibly reset attributes that dplyr methods removed
    current_att <- names(attributes(x))
    prev_att <- names(att)
    missing_att <- setdiff(prev_att, current_att)
    if (length(missing_att) > 0) {
      for (i in missing_att) {
        attr(x, i) <- att[[i]]
      }
    }

    current_cls <- class(x)
    missing_cls <- setdiff(extras, current_cls)
    ## Add an missing classes
    if (length(missing_cls) > 0) {
      class(x) <- c(extras, class(x))
    }
  } else {
    x <- tibble::as_tibble(x)
  }
  x
}

# The S3 method for `tune_results` calls `dplyr::mutate()` then restores the
# classes and attributes.
mutate.tune_results <- function(.data, ...) {
  extra_classes <- setdiff(class(.data), class(tibble()))
  orig_att <- attributes(.data)
  maybe_tune_results(NextMethod(), extras = extra_classes, att = orig_att)
}
