#' Find recommended methods for generating parameter values
#'
#' [tunable()] determines which parameters in an object _can_ be tuned along
#' with information about the parameters.
#' @param x An object, such as a recipe, recipe step, or `parsnip` model
#' specification.
#' @param ... Not currently used.
#' @return A tibble with a column for the parameter `name`, information on the
#'  _default_ method for generating a corresponding parameter object, the
#'  `source` of the parameter (e.g. "recipe", etc.), and the `component` within
#'  the source. For the `component` column, a little more specificity is given
#'  about the location of the parameter (e.g. "step_normalize" or recipes or
#'  "boost_tree" for models). The `component_id` column contains the unique step
#'  `id` field or, for models, a logical for whether the model specification
#'  argument was a main parameter or one associated with the engine.
#' @details
#' For a model specification, an engine must be chosen.
#'
#' If the object has no tunable parameters, a tibble with no rows is returned.
#'
#' The information about the default parameter object takes the form of a
#' named list with an element for the function call and an optional element for
#' the source of the function (e.g. the `dials` package). For model
#' specifications, If the parameter is unknown to the underlying `tunable`
#' method, a `NULL` is returned.
#' @keywords internal
#' @examples
#' \donttest{
#' library(recipes)
#'
#' recipe(mpg ~ ., data = mtcars) %>%
#'   step_knnimpute(all_predictors()) %>%
#'   step_pca(all_predictors()) %>%
#'   tunable()
#'
#' recipe(mpg ~ ., data = mtcars) %>%
#'   step_normalize(all_predictors()) %>%
#'   tunable()
#'
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
#' @export
tunable <- function(x, ...) {
  UseMethod("tunable")
}

#' @rdname tunable
#' @export
no_param <-
  tibble::tibble(
    name = NA_character_,
    call_info = list(),
    source = NA_character_,
    component = NA_character_,
    component_id = NA_character_
  )

step_type <- function(.step) class(.step)[class(.step) != "step"][1]
mod_type <- function(.mod) class(.mod)[class(.mod) != "model_spec"][1]

#' @rdname tunable
#' @export
tunable.step <- function(x, ...) {
  no_param
}

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
      component_id = ifelse(name %in% names(x$args), "main", "engine"))

  if (nrow(arg_vals) > 0) {
    has_info <- purrr::map_lgl(arg_vals$call_info, is.null)
    rm_list <- !(has_info & (arg_vals$component_id == "main"))

    arg_vals <- arg_vals[rm_list,]
  }
  arg_vals %>% dplyr::select(name, call_info, source, component, component_id)
}

# ------------------------------------------------------------------------------

#' @rdname tunable
#' @export
tunable.step_embed <- function(x, ...) {
  tibble::tibble(
    name = c("num_terms", "hidden_units"),
    call_info = list(
      list(pkg = "dials", fun = "num_terms"),
      list(pkg = "dials", fun = "hidden_units")
    ),
    source = "recipe",
    component = "step_embed",
    component_id = x$id
  )
}

#' @rdname tunable
#' @export
tunable.step_umap <- function(x, ...) {
  tibble::tibble(
    name = c("neighbors", "num_comp", "min_dist", "learn_rate", "epochs"),
    call_info = list(
      list(pkg = "dials", fun = "neighbors"),
      list(pkg = "dials", fun = "num_comp"),
      list(pkg = "dials", fun = "min_dist"),
      list(pkg = "dials", fun = "learn_rate"),
      list(pkg = "dials", fun = "epochs")
    ),
    source = "recipe",
    component = "step_umap",
    component_id = x$id
  )
}

#' @rdname tunable
#' @export
tunable.step_woe <- function(x, ...) {
  tibble::tibble(
    name = c("Laplace"),
    call_info = list(
      list(pkg = "dials", fun = "Laplace")
    ),
    source = "recipe",
    component = "step_woe",
    component_id = x$id
  )
}

#' @rdname tunable
#' @export
tunable.step_texthash <- function(x, ...) {
  tibble::tibble(
    name = c("signed", "num_terms"),
    call_info = list(
      list(pkg = "dials", fun = "signed_hash"),
      list(pkg = "dials", fun = "num_hash", range = c(8, 12))
    ),
    source = "recipe",
    component = "step_texthash",
    component_id = x$id
  )
}

#' @rdname tunable
#' @export
tunable.step_tf <- function(x, ...) {
  tibble::tibble(
    name = c("weight_scheme", "num_terms"),
    call_info = list(
      list(pkg = "dials", fun = "weight_scheme"),
      list(pkg = "dials", fun = "weight")
    ),
    source = "recipe",
    component = "step_tf",
    component_id = x$id
  )
}

#' @rdname tunable
#' @export
tunable.step_tokenfilter <- function(x, ...) {
  tibble::tibble(
    name = c("max_times", "min_times", "max_tokens"),
    call_info = list(
      list(pkg = "dials", fun = "max_times"),
      list(pkg = "dials", fun = "min_times"),
      list(pkg = "dials", fun = "max_tokens")
    ),
    source = "recipe",
    component = "step_tokenfilter",
    component_id = x$id
  )
}

#' @rdname tunable
#' @export
tunable.step_tokenize <- function(x, ...) {
  tibble::tibble(
    name = c("token"),
    call_info = list(
      list(pkg = "dials", fun = "token")
    ),
    source = "recipe",
    component = "step_tokenize",
    component_id = x$id
  )
}

#' @rdname tunable
#' @export
tunable.recipe <- function(x, ...) {
  if (length(x$steps) == 0) {
    res <- no_param
  } else {
    res <- purrr::map_dfr(x$steps, tunable)
    if (nrow(res) > 0) {
      res <- res[!is.na(res$name),]
    }
  }
  res
}

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
      res$call_info[res$name == "trees"] <-
        list(list(pkg = "dials", fun = "trees", range = c(1, 100)))
    }
  }
  res
}


#' @rdname tunable
#' @export
tunable.nearest_neighbor <- function(x, ...) {
  res <- NextMethod()
  if (x$engine == "kknn") {
    res$call_info[res$name == "dist_power"] <-
      list(list(pkg = "dials", fun = "dist_power", range = c(0.0, 1.5)))
  }
  res
}

