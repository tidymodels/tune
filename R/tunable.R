#' Find recommended methods for generating parameter values
#'
#' [tunable()] determines which parameters in an object _can_ be tuned along
#' with information about the parameters.
#' @param x An object, such as a workflow or `parsnip` model
#' specification.
#' @param ... Not currently used.
#' @return A tibble with a column for the parameter `name`, information on the
#'  _default_ method for generating a corresponding parameter object, the
#'  `source` of the parameter (e.g. "model_spec", etc.), and the `component`
#'  within the source. For the `component` column, a little more specificity is
#'  given about the location of the parameter (e.g. "boost_tree" for models).
#'  The `component_id` column contains the unique step `id` field or, for
#'  models, a logical for whether the model specification argument was a main
#'  parameter or one associated with the engine.
#' @details
#' For a model specification, an engine must be chosen.
#'
#' If the object has no tunable parameters, a tibble with no rows is returned.
#'
#' The information about the default parameter object takes the form of a
#' named list with an element for the function call and an optional element for
#' the source of the function (e.g. the `dials` package). For model
#' specifications, if the parameter is unknown to the underlying `tunable`
#' method, a `NULL` is returned.
#' @keywords internal
#' @examples
#' \donttest{
#' library(parsnip)
#'
#' boost_tree() %>%
#'   set_engine("xgboost") %>%
#'   tunable()
#'
#' boost_tree() %>%
#'   set_engine("C5.0", rules = TRUE) %>%
#'   tunable()
#' }
#'
#' @rdname tunable
#' @export
tunable.model_spec <- function(x, ...) {
  mod_env <- rlang::ns_env("parsnip")$parsnip

  if (is.null(x$engine)) {
    stop("Please declare an engine first using `set_engine()`.", call. = FALSE)
  }

  arg_name <- paste0(mod_type(x), "_args")
  if (!(any(arg_name == names(mod_env)))) {
    stop("The `parsnip` model database doesn't know about the arguments for ",
         "model `", mod_type(x), "`. Was it registered?",
         sep = "", call. = FALSE)
  }

  arg_vals <-
    mod_env[[arg_name]] %>%
    dplyr::filter(engine == x$engine) %>%
    dplyr::select(name = parsnip, call_info = func) %>%
    dplyr::full_join(
      tibble::tibble(name = c(names(x$args), names(x$eng_args))),
      by = "name"
    ) %>%
    dplyr::mutate(
      source = "model_spec",
      component = mod_type(x),
      component_id = dplyr::if_else(name %in% names(x$args), "main", "engine")
    )

  if (nrow(arg_vals) > 0) {
    has_info <- purrr::map_lgl(arg_vals$call_info, is.null)
    rm_list <- !(has_info & (arg_vals$component_id == "main"))

    arg_vals <- arg_vals[rm_list,]
  }
  arg_vals %>% dplyr::select(name, call_info, source, component, component_id)
}

mod_type <- function(.mod) class(.mod)[class(.mod) != "model_spec"][1]

# ------------------------------------------------------------------------------

#' @export
#' @rdname tunable
tunable.workflow <- function(x, ...) {
  model <- workflows::pull_workflow_spec(x)
  param_data <- tunable(model)

  if (has_preprocessor_recipe(x)) {
    recipe <- workflows::pull_workflow_preprocessor(x)
    recipe_param_data <- tunable(recipe)

    param_data <- dplyr::bind_rows(param_data, recipe_param_data)
  }

  param_data
}

# ------------------------------------------------------------------------------


add_engine_parameters <- function(pset, engines) {
  is_engine_param <- pset$name %in% engines$name
  if (any(is_engine_param)) {
    engine_names <- pset$name[is_engine_param]
    pset <- pset[!is_engine_param,]
    pset <-
      dplyr::bind_rows(pset, engines %>% dplyr::filter(name %in% engines$name))
  }
  pset
}

c5_tree_engine_args <-
  tibble::tibble(
    name = c(
      "CF",
      "noGlobalPruning",
      "winnow",
      "fuzzyThreshold",
      "bands"
    ),
    call_info = list(
      list(pkg = "dials", fun = "confidence_factor"),
      list(pkg = "dials", fun = "no_global_pruning"),
      list(pkg = "dials", fun = "predictor_winnowing"),
      list(pkg = "dials", fun = "fuzzy_thresholding"),
      list(pkg = "dials", fun = "rule_bands")
    ),
    source = "model_spec",
    component = "decision_tree",
    component_id = "engine"
  )

c5_boost_engine_args <- c5_tree_engine_args
c5_boost_engine_args$component <- "boost_tree"

ranger_engine_args <-
  tibble::tibble(
    name = c(
      "regularization.factor",
      "regularization.usedepth",
      "alpha",
      "minprop",
      "splitrule",
      "num.random.splits"
    ),
    call_info = list(
      list(pkg = "dials", fun = "regularization_factor"),
      list(pkg = "dials", fun = "regularize_depth"),
      list(pkg = "dials", fun = "significance_threshold"),
      list(pkg = "dials", fun = "lower_quantile"),
      list(pkg = "dials", fun = "splitting_rule"),
      list(pkg = "dials", fun = "num_random_splits")
    ),
    source = "model_spec",
    component = "rand_forest",
    component_id = "engine"
  )

randomForest_engine_args <-
  tibble::tibble(
    name = c("maxnodes"),
    call_info = list(
      list(pkg = "dials", fun = "max_nodes")
    ),
    source = "model_spec",
    component = "rand_forest",
    component_id = "engine"
  )

earth_engine_args <-
  tibble::tibble(
    name = c("nk"),
    call_info = list(
      list(pkg = "dials", fun = "max_num_terms")
    ),
    source = "model_spec",
    component = "mars",
    component_id = "engine"
  )


# ------------------------------------------------------------------------------


#' @rdname tunable
#' @export
tunable.linear_reg <- function(x, ...) {
  res <- NextMethod()
  if (x$engine == "glmnet") {
    res$call_info[res$name == "mixture"] <-
     list(list(pkg = "dials", fun = "mixture", range = c(0.05, 1.00)))
  }
  res
}


#' @rdname tunable
#' @export
tunable.logistic_reg <- function(x, ...) {
  res <- NextMethod()
  if (x$engine == "glmnet") {
    res$call_info[res$name == "mixture"] <-
      list(list(pkg = "dials", fun = "mixture", range = c(0.05, 1.00)))
  }
  res
}


#' @rdname tunable
#' @export
tunable.multinomial_reg <- function(x, ...) {
  res <- NextMethod()
  if (x$engine == "glmnet") {
    res$call_info[res$name == "mixture"] <-
      list(list(pkg = "dials", fun = "mixture", range = c(0.05, 1.00)))
  }
  res
}


#' @rdname tunable
#' @export
tunable.boost_tree <- function(x, ...) {
  res <- NextMethod()
  if (x$engine == "xgboost") {
    res$call_info[res$name == "sample_size"] <-
      list(list(pkg = "dials", fun = "sample_prop"))
  } else {
    if (x$engine == "C5.0") {
      res <- add_engine_parameters(res, c5_boost_engine_args)
      res$call_info[res$name == "trees"] <-
        list(list(pkg = "dials", fun = "trees", range = c(1, 100)))
    }
  }
  res
}

#' @rdname tunable
#' @export
tunable.rand_forest <- function(x, ...) {
  res <- NextMethod()
  if (x$engine == "ranger") {
    res <- add_engine_parameters(res, ranger_engine_args)
  }
  if (x$engine == "randomForest") {
    res <- add_engine_parameters(res, randomForest_engine_args)
  }

  res
}

#' @rdname tunable
#' @export
tunable.mars <- function(x, ...) {
  res <- NextMethod()
  if (x$engine == "earth") {
    res <- add_engine_parameters(res, earth_engine_args)
  }
  res
}

#' @rdname tunable
#' @export
tunable.decision_tree <- function(x, ...) {
  res <- NextMethod()
  if (x$engine == "C5.0") {
    res <- add_engine_parameters(res, c5_tree_engine_args)
  }
  res
}

