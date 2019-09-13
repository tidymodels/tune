# ------------------------------------------------------------------------------
# currently assumes recipes and no post-processing and regression
# and lots of other stuff


#' Model tuning via grid search
#'
#' @param object A model workflow object.
#' @param rs An `rset()` object.
#' @param grid A data frame of tuning combinations or `NULL`.
#' @param perf A `yardstick::metric_set()` or `NULL`.
#' @param control An object used to modify the tuning process.
#' @return A tibble of results.
#'
#' @details
#' If no tuning grid is provided, a semi-random grid (via
#' `dials::grid_latin_hypercube()`) is create with 10 candidate parameter
#' combinations.
#'
#' If no metric set is used, one is set. For regression models, the root mean
#' squared error and coefficient of determination are computed. For classification,
#' the log-likelihood and overall accuracy are computed.
#' @export
tune_grid <- function(object, rs, grid = NULL, perf = NULL, control = grid_control()) {

  check_rset(rs)
  check_object(object)
  check_grid_control(control)
  perf <- check_perf(perf, object)
  grid <- check_grid(grid, object)

  code_path <- quarterback(object)

  rs <- rlang::eval_tidy(code_path)

  all_bad <- is_cataclysmic(rs)
  if (all_bad) {
    warning(
      "All models failed in tune_grid(). From the first failure:",
      as.character(res$.metrics[[1]]),
      call. = FALSE
      )
  }

  class(rs) <- c("tune_results", class(rs))
  rs
}

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
  tmp_fit <- object$fit$model$model
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

quarterback <- function(x) {
  y <- param_set(x)
  sources <- unique(y$source)
  has_form <- names(x$pre) == "formula_processor"
  tune_rec <- any(sources == "recipe") & !has_form
  tune_model <- any(sources == "model_spec")

  args <- list(splits = expr(rs), grid = expr(grid),
               wflow = expr(object), perf = expr(perf),
               ctrl = expr(control))
  dplyr::case_when(
     tune_rec & !tune_model ~ rlang::call2("tune_rec", !!!args),
     tune_rec &  tune_model ~ rlang::call2("tune_rec_and_mod", !!!args),
    !tune_rec &  tune_model ~ rlang::call2("tune_mod_with_recipe", !!!args),
     has_form &  tune_model ~ rlang::call2("tune_mod_with_formula", !!!args),
     TRUE ~ rlang::call2("tune_nothing")
  )
}

empty_perf <- tibble::tibble(
  .metric = NA_character_,
  .estimator = NA_character_,
  .estimate = NA_real_
)

#' Control the grid search process
#'
#' @param verbose A logical for logging results as they are generated.
#' @param allow_par A logical to allow parallel processing (if a parallel
#' backend is registered).
#' @param extract An optional function with at least one argument (or `NULL`)
#' that can be used to retain arbitrary objects from the model fit object,
#' recipe, or other elements of the workflow.
#' @param save_pred A logical for whether the out-of-sample predictions should
#' be saved for each model _evaluated_.
#'
#'@details
#'
#' For `extract`, this function can be used to output the model object, the
#'  recipe (if used), or some components of either or both. When evaluated, the
#'  function's sole argument has a named list with elements `recipe` and
#'  `model`. If the formula method is used, the recipe element will be `NULL`.
#'
#' The results of the `extract` function are added to a list column in the
#'  output called `extract`. Each element of this list is a tibble with tuning
#'  parameter column and a list column (also called `.extract`) that contains
#'  the results of the function. If no extraction function is used, there is no
#'  `.extract` column in the resulting object.
#' @export
grid_control <- function(verbose = FALSE, allow_par = TRUE,
                         extract = NULL, save_pred = FALSE) {
  # add options for `save_predictions`, and other stuff.
  # seeds per resample
  list(verbose = verbose, allow_par = allow_par, extract = extract,
       save_pred = save_pred)
}
