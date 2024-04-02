new_rng_snapshots <- utils::compareVersion("3.6.0", as.character(getRversion())) > 0

# New (as of 4.3.0) a new option generates different snapshots
rankdeficient_version <- any(names(formals("predict.lm")) == "rankdeficient")


helper_objects_tune <- function() {
  rec_tune_1 <-
    recipes::recipe(mpg ~ ., data = mtcars) %>%
    recipes::step_normalize(recipes::all_predictors()) %>%
    recipes::step_pca(recipes::all_predictors(), num_comp = tune())

  rec_no_tune_1 <-
    recipes::recipe(mpg ~ ., data = mtcars) %>%
    recipes::step_normalize(recipes::all_predictors())

  lm_mod <- parsnip::linear_reg() %>% parsnip::set_engine("lm")

  svm_mod <- parsnip::svm_rbf(mode = "regression", cost = tune()) %>%
    parsnip::set_engine("kernlab")

  list(
    rec_tune_1 = rec_tune_1,
    rec_no_tune_1 = rec_no_tune_1,
    lm_mod = lm_mod,
    svm_mod = svm_mod
  )
}

# When not interactive, the cli progress bar is occasionally "reset" at random
# intervals throughout tuning, which means that intermediate counts may appear
# in snapshots. In an output like:
#
# > A | error:   AHHhH
# There were issues with some computations   A: x2
# There were issues with some computations   A: x4
# There were issues with some computations   A: x5
# There were issues with some computations   A: x5
#
# ...we just want what the interactive user would see, e.g.:
#
# > A | error:   AHHhH
# There were issues with some computations   A: x5
catalog_lines <- function(pattern) {
  local({
    # A local variable; once we've found the line with the intended pattern,
    # don't print it anymore
    found_pattern <- FALSE
    # Return function to pass to `expect_snapshot(transform)`
    function(lines) {
      matches <- grepl(pattern, lines, fixed = TRUE)
      if (any(matches) & !found_pattern) {
        found_pattern <<- TRUE
        # Possible that there may be more than one match; return the last
        return(lines[max(which(matches))])
      }

      # Otherwise, we're looking for the unique messages
      lines[grepl("^>", lines)]
    }
  })
}
