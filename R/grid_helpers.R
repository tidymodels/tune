# recipe-oriented helpers

train_recipe <- function(split, workflow, grid) {
  original_recipe <- get_wflow_recipe(workflow)

  if (!is.null(grid)) {
    updated_recipe <- merge(original_recipe, grid)$x[[1]]
  } else {
    updated_recipe <- original_recipe
  }

  workflow <- set_wflow_recipe(workflow, updated_recipe)

  training <- rsample::analysis(split)

  workflow <- .fit_pre(workflow, training)

  # Always reset to the original recipe so `parameters()` can be used on this
  # object. The prepped updated recipe is stored in the mold.
  workflow <- set_wflow_recipe(workflow, original_recipe)

  workflow
}

train_model_from_recipe <- function(workflow, grid, control) {
  original_spec <- get_wflow_model(workflow)

  if (!is.null(grid)) {
    updated_spec <- merge(original_spec, grid)$x[[1]]
  } else {
    updated_spec <- original_spec
  }

  workflow <- set_wflow_model(workflow, updated_spec)

  workflow <- .fit_model(workflow, control)

  # Always reset to the original spec so `parameters()` can be used on this
  # object. The fit model is stored in `workflow$fit$fit`
  workflow <- set_wflow_model(workflow, original_spec)

  workflow
}

predict_model <- function(split, workflow, grid, metrics) {
  model <- get_wflow_fit(workflow)

  forged <- forge_from_workflow(split, workflow)

  x_vals <- forged$predictors
  y_vals <- forged$outcomes

  orig_rows <- as.integer(split, data = "assessment")

  # Determine the type of prediction that is required
  type_info <- metrics_info(metrics)
  types <- unique(type_info$type)

  # Split `grid` from the parameters used to fit the model and any potential
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
            !!!make_submod_arg(grid, model)
          )
        tmp_res <-
          eval_tidy(mp_call) %>%
          mutate(.row = orig_rows) %>%
          unnest(cols = dplyr::starts_with(".pred")) %>%
          cbind(fixed_param %>% dplyr::select(-one_of(submod_param)),
                row.names = NULL) %>%
          # go back to user-defined name
          dplyr::rename(!!!make_rename_arg(grid, model)) %>%
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
  y_vals <- dplyr::mutate(y_vals, .row = orig_rows)
  res <- dplyr::full_join(res, y_vals, by = ".row")

  tibble::as_tibble(res)
}

make_submod_arg <- function(grid, model) {
  # Assumes only one submodel parameter per model
  real_name <-
    parsnip::get_from_env(paste(class(model$spec)[1], "args", sep = "_")) %>%
    dplyr::filter(has_submodel & engine == model$spec$engine) %>%
    dplyr::pull(parsnip)
  submods <- grid$.submodels[[1]]
  names(submods) <- real_name
  submods
}

make_rename_arg <- function(grid, model) {
  # Assumes only one submodel parameter per model
  real_name <-
    parsnip::get_from_env(paste(class(model$spec)[1], "args", sep = "_")) %>%
    dplyr::filter(has_submodel & engine == model$spec$engine) %>%
    dplyr::pull(parsnip)
  submods <- grid$.submodels[[1]]
  res <- list(real_name)
  names(res) <- names(submods)
  res
}


# ------------------------------------------------------------------------------
# Formula-oriented helpers

train_formula <- function(split, workflow) {
  training <- rsample::analysis(split)
  .fit_pre(workflow, training)
}

# TODO - remove me?
exec_formula <- function(split, workflow) {
  dat <- rsample::analysis(split)
  dat <- as.data.frame(dat)
  f <- get_wflow_form(workflow)
  mf <- model.frame(f, data = dat, na.action = "na.pass")
  trms <- attr(mf, "terms")
  attr(trms, ".Environment") <- rlang::base_env()
  attr(mf, "terms") <- NULL

  y_cols <- mf_outcome_cols(trms)
  y_dat <- mf[, y_cols, drop = FALSE]

  x_dat <- model.matrix(trms, dat, na.action = "na.pass")
  x_dat <- no_int(x_dat)
  list(terms = trms, x = x_dat, y = y_dat)
}

# TODO - Remove me?
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


train_model_from_mold <- function(workflow, grid, control) {
  spec <- get_wflow_model(workflow)

  if (!is.null(grid)) {
    spec <- merge(spec, grid)$x[[1]]
  }

  workflow <- set_wflow_model(workflow, spec)

  .fit_model(workflow, control)
}

# ------------------------------------------------------------------------------

has_wflow_recipe <- function(workflow) {
  any(names(workflow$pre$actions) == "recipe")
}

has_wflow_formula <- function(workflow) {
  any(names(workflow$pre$actions) == "formula")
}

get_wflow_mold <- function(workflow) {
  workflow$pre$mold
}

get_wflow_fit <- function(workflow) {
  workflow$fit$fit
}

set_wflow_model <- function(workflow, spec) {
  workflow$fit$actions$model$spec <- spec
  workflow
}

set_wflow_recipe <- function(workflow, recipe) {
  workflow$pre$actions$recipe$recipe <- recipe
  workflow
}

get_wflow_model <- function(workflow) {
  workflow$fit$actions$model$spec
}

get_wflow_recipe <- function(workflow) {
  workflow$pre$actions$recipe$recipe
}

get_wflow_form <- function(workflow) {
  workflow$pre$actions$formula$formula
}

# get_wflow_post



