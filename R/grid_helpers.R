# recipe-oriented helpers

train_recipe <- function(split, object, grid) {
  tmp_rec <- object$pre$recipe$recipe
  if (!is.null(grid)) {
    tmp_rec <- merge(tmp_rec, grid)$x[[1]]
  }
  tmp_rec <-
    recipes::prep(
      tmp_rec,
      training = rsample::analysis(split),
      fresh = TRUE
    )
  tmp_rec
}

train_model_from_recipe <- function(object, recipe, grid, ...) {
  tmp_fit <- get_wflow_model(object)
  if (!is.null(grid)) {
    tmp_fit <- merge(tmp_fit, grid)$x[[1]]
  }

  tmp_fit <-
    parsnip::fit_xy(
      tmp_fit,
      x = recipes::juice(recipe, recipes::all_predictors()),
      y = recipes::juice(recipe, recipes::all_outcomes()) %>% dplyr::pull(1),
      ...
    )

  tmp_fit
}

predict_model_from_recipe <- function(split, model, recipe, grid, perf, ...) {
  y_names <- outcome_names(recipe)
  new_vals <-
    recipes::bake(recipe,
                  rsample::assessment(split),
                  all_predictors(),
                  all_outcomes())
  x_vals <- new_vals %>% dplyr::select(-one_of(y_names))
  orig_rows <- as.integer(split, data = "assessment")

  # Determine the type of prediction that is required
  type_info <- perf_info(perf)
  types <- unique(type_info$type)

  # Split `grid` from the parameters used to fit the model and any poential
  # sub-model parameters
  submod_col <- names(grid) == ".submodels"
  fixed_param <- grid[, !submod_col]

  res <- NULL
  merge_vars <- c(".row", names(fixed_param))

  for (type_iter in types) {
    # Regular predictions
    tmp_res <-
      predict(model, x_vals, type = type_iter) %>%
      mutate(.row = orig_rows) %>%
      cbind(fixed_param, row.names = NULL)

    if (any(submod_col)) {
      submod_length <- map_int(grid$.submodels[[1]], length)
      has_submodels <- any(submod_length > 0)

      if (has_submodels) {
        submod_param <- names(grid$.submodels[[1]])
        mp_call <-
          call2(
            "multi_predict",
            .ns = "parsnip",
            object = expr(model),
            new_data = expr(x_vals),
            type = type_iter,
            !!!grid$.submodels[[1]]
          )
        tmp_res <-
          eval_tidy(mp_call) %>%
          mutate(.row = orig_rows) %>%
          unnest(cols = dplyr::starts_with(".pred")) %>%
          cbind(fixed_param %>% dplyr::select(-one_of(submod_param)),
                row.names = NULL) %>%
          dplyr::select(dplyr::one_of(names(tmp_res))) %>%
          dplyr::bind_rows(tmp_res)
      }
    }
    if (!is.null(res)) {
      res <- dplyr::full_join(res, tmp_res, by = merge_vars)
    } else {
      res <- tmp_res
    }
    rm(tmp_res)
  } # end type loop

  # Add outcome data
  outcome_dat <-
    new_vals %>%
    dplyr::select(dplyr::one_of(y_names)) %>%
    dplyr::mutate(.row = orig_rows)

  res <- dplyr::full_join(res, outcome_dat, by = ".row")
  tibble::as_tibble(res)
}

# ------------------------------------------------------------------------------
# Formula-oriented helpers

# process the formula to get terms (and data)
exec_formula <- function(split, object) {
  f <- object$pre$formula_processor$formula_processor
  mf <- model.frame(f, data = as.data.frame(split$data), na.action = "na.pass")
  trms <- attr(mf, "terms")
  attr(trms, ".Environment") <- rlang::base_env()
  attr(mf, "terms") <- NULL

  y_cols <- mf_outcome_cols(trms)
  y_dat <- mf[, y_cols, drop = FALSE]

  x_dat <- model.matrix(trms, as.data.frame(split$data), na.action = "na.pass")
  x_dat <- no_int(x_dat)
  list(terms = trms, x = x_dat, y = y_dat)
}

# execute the terms on the assessment set
exec_terms <- function(split, trms) {
  dat <- rsample::assessment(split)
  dat <- as.data.frame(dat)
  mf <- model.frame(trms, data = dat, na.action = "na.pass")
  attr(mf, "terms") <- NULL

  y_cols <- mf_outcome_cols(trms)
  # To pull the right columns, we need the y name that might inlcude the
  # expression (e.g. "log10(Sale_Price)"). However, we otherwise use the
  # naked variable name everywhere else (e.g. just "Sale_Price"), so let's
  # reset the names after subsetting
  y_dat <- mf[, y_cols, drop = FALSE]
  # naked names:
  colnames(y_dat) <- outcome_names(trms)

  x_dat <- model.matrix(trms, dat, na.action = "na.pass")
  x_dat <- no_int(x_dat)
  list(x = x_dat, y = y_dat)
}


no_int <- function(x) {
  nms <- colnames(x)
  is_int <- nms == "(Intercept)"
  if (any(is_int)) {
    x <- x[, !is_int, drop = FALSE]
  }
  as.data.frame(x)
}

# Get the outcome columns and protext against cases with log10(y) in formula
mf_outcome_cols <- function(x) {
  y_call <- attr(x, "predvars")[attr(x, "response") + 1]
  y_call <- y_call[[1]]
  # If y_call represents a single variable, deparse it.
  if (!is.name(y_call) && isTRUE(all.equal(y_call[[1]], rlang::call2("cbind")))) {
    # multivariate
    cl_args <- rlang::call_args(y_call)
    cl_args <- purrr::map_chr(cl_args, rlang::expr_text)
  } else {
    # univariate
    cl_args <- rlang::expr_text(y_call)
  }
  cl_args
}


train_model_from_df <- function(object, dat, grid, ...) {
  tmp_fit <- get_wflow_model(object)
  if (!is.null(grid)) {
    tmp_fit <- merge(tmp_fit, grid)$x[[1]]
  }

  if (ncol(dat$y)) {
    dat$y <- dat$y[[1]]
  }

  tmp_fit <-
    parsnip::fit_xy(
      tmp_fit,
      x = dat$x,
      y = dat$y,
      ...
    )

  tmp_fit
}

predict_model_from_terms <- function(split, model, trms, grid, perf, ...) {
  dat <- exec_terms(split, trms)

  orig_rows <- as.integer(split, data = "assessment")

  # Determine the type of prediction that is required
  type_info <- perf_info(perf)
  types <- unique(type_info$type)

  # Split `grid` from the parameters used to fit the model and any poential
  # sub-model parameters
  submod_col <- names(grid) == ".submodels"
  fixed_param <- grid[, !submod_col]

  res <- NULL
  merge_vars <- c(".row", names(fixed_param))

  for (type_iter in types) {
    # Regular predictions
    tmp_res <-
      predict(model, dat$x, type = type_iter) %>%
      mutate(.row = orig_rows) %>%
      cbind(fixed_param, row.names = NULL)

    if (any(submod_col)) {
      submod_length <- map_int(grid$.submodels[[1]], length)
      has_submodels <- any(submod_length > 0)

      if (has_submodels) {
        submod_param <- names(grid$.submodels[[1]])
        mp_call <-
          call2(
            "multi_predict",
            .ns = "parsnip",
            object = expr(model),
            new_data = expr(dat$x),
            type = type_iter,
            !!!grid$.submodels[[1]]
          )
        tmp_res <-
          eval_tidy(mp_call) %>%
          mutate(.row = orig_rows) %>%
          unnest(cols = dplyr::starts_with(".pred")) %>%
          cbind(fixed_param %>% dplyr::select(-one_of(submod_param)),
                row.names = NULL) %>%
          dplyr::select(dplyr::one_of(names(tmp_res))) %>%
          dplyr::bind_rows(tmp_res)
      }
    }
    if (!is.null(res)) {
      res <- dplyr::full_join(res, tmp_res, by = merge_vars)
    } else {
      res <- tmp_res
    }
    rm(tmp_res)
  } # end type loop

  # Add outcome data
  outcome_dat <-
    dat$y %>%
    dplyr::mutate(.row = orig_rows)

  res <- dplyr::full_join(res, outcome_dat, by = ".row")
  tibble::as_tibble(res)
}

# ------------------------------------------------------------------------------

get_wflow_model <- function(object) {
  object$fit$model$model
}

get_wflow_pre <- function(object) {
  object$pre$recipe$recipe
}

# get_wflow_post
