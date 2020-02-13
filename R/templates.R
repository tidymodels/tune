# Code to make code recommendations
# ------------------------------------------------------------------------------
# Functions to interrogate tidymodels objects

model_mode <- function(rec) {
  var_roles <- summary(rec)
  y_types <- var_roles$type[var_roles$role == "outcome"]
  y_types <- unique(y_types)
  if (length(y_types) > 1) {
    rlang::abort("outcomes are of different types.")
  }
  if (all(y_types == "numeric")) {
    mod_mode <- "regression"
  } else {
    mod_mode <- "classification"
  }
  mod_mode
}

y_lvl <- function(rec) {
  mod_mode <- model_mode(rec)
  if (mod_mode == "regression") {
    return(NULL)
  }
  var_roles <- summary(rec)
  y_cols <- var_roles$variable[var_roles$role == "outcome"]
  y_dat <- rec$template %>% select(one_of(y_cols)) %>% pull(1)
  length(levels(y_dat))
}

has_factor_pred <- function(x) {
  info <- summary(x)
  pred_types <- info$type[info$role == "predictor"]
  any(pred_types == "nominal")
}

num_pred_col <- function(x) {
  info <- summary(x)
  sum(info$role == "predictor")
}

# ------------------------------------------------------------------------------
# helper functions

expr_width <- 74L

assign_value <- function(name, value, cr = TRUE) {
  value <- rlang::enexpr(value)
  value <- rlang::expr_text(value, width = expr_width)
  chr_assign(name, value, cr)
}
chr_assign <- function(name, value, cr = TRUE) {
  name <- paste(name, "<-")
  if (cr) {
    res <- c(name, paste0("\n  ", value))
  } else {
    res <- paste(name, value)
  }
  res
}
pipe_value <- function(base, value) {
  # Find last non-comment line, add a `%>%` to the end, then add another line
  value <- rlang::enexpr(value)
  value <- rlang::expr_text(value, width = expr_width)
  clean_base <- gsub("\\n", "", base)
  clean_base <- trimws(base, which = "left")
  not_comment <- seq_along(base)[!grepl("^#", clean_base)]
  n <- max(1, max(not_comment))
  base[n] <- paste(base[n], "%>%")
  c(base, paste0("\n  ", value))
}
add_comment <- function(base, value, add = TRUE) {
  if (!add) {
    return(base)
  }
  if (!is.character(value)) {
    rlang::abort("`value` must be character.")
  }
  value <- strwrap(value, width = expr_width, prefix = "# ")
  c(base, paste0("\n  ", value))
}
add_steps_dummy_vars <- function(base, hot = FALSE, add = FALSE) {
  base <- base %>%
    pipe_value(step_novel(all_nominal(), -all_outcomes()))
  if (hot) {
    base <- base %>%
      add_comment(dummy_hot_msg, add) %>%
      pipe_value(step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE))
  } else {
    base <- base  %>%
      add_comment(dummy_msg, add) %>%
      pipe_value(step_dummy(all_nominal(), -all_outcomes()))
  }
  base
}
add_steps_normalization <- function(base) {
  base %>%
    pipe_value(step_zv(all_predictors())) %>%
    pipe_value(step_normalize(all_predictors(), -all_nominal()))
}
factor_check <- function(base, rec, add) {
  var_roles <- summary(rec)
  nominal <- var_roles$variable[var_roles$type == "nominal"]
  is_str <-
    purrr::map_lgl(rec$template %>% dplyr::select(dplyr::one_of(nominal)),
                   rlang::is_character)
  if (any(is_str)) {
    nominal <- rlang::syms(nominal[is_str])
    selector <- rlang::expr(one_of(!!!nominal))
    step_expr <- rlang::expr(step_string2factor(!!selector))
    base <-
      base %>%
      add_comment(string_to_factor_msg, add = add) %>%
      pipe_value(!!step_expr)
  }
  base
}


template_workflow <- function(prefix) {
  paste0(prefix, "_wflw") %>%
    assign_value(workflow()) %>%
    pipe_value(add_recipe(!!rlang::sym(paste0(prefix, "_recipe")))) %>%
    pipe_value(add_model(!!rlang::sym(paste0(prefix, "_model"))))
}

template_tune_with_grid <- function(prefix) {
  tune_expr <-
    rlang::call2("tune_grid",
          sym(paste0(prefix, "_workflow")),
          resamples = expr(stop("add your rsample object")),
          grid = sym(paste0(prefix, "_grid")))
  assign_value(paste0(prefix, "_tune"), !!tune_expr)
}
template_tune_no_grid <- function(prefix, seed = sample.int(10^5, 1)) {
  tune_expr <-
    rlang::call2(
      "tune_grid",
      sym(paste0(prefix, "_workflow")),
      resamples = expr(stop("add your rsample object")),
      grid = 20
    )

  c(paste0("set.seed(", seed,")\n"),
    assign_value(paste0(prefix, "_tune"), !!tune_expr))
}

# Take the call to the template function and turn it into a call to `recipe()`
initial_recipe_call <- function(cl) {
  cl$tune <- NULL
  cl$verbose <-  NULL
  rec_cl <- cl
  rec_cl[[1]] <- rlang::expr(recipe)
  rec_cl
}

# ------------------------------------------------------------------------------

zv_msg <- paste(
  "Before centering and scaling the numeric predictors, any predictors with",
  "a single unique value are filtered out."
)
dist_msg <-
  paste(
    "Since distance calculations are used, the predictor",
    "variables should be on the same scale."
  )
reg_msg <-
  paste(
    "Regularization methods sum up functions of the model slope coefficients.",
    "Because of this, the predictor variables should be on the same scale."
  )
dummy_msg <-
  paste(
    "This model requires the predictors to be numeric. The most common method to",
    "convert qualitative predictors to numeric is to create binary indicator",
    "variables (aka dummy variables) from these predictors."
  )
dummy_hot_msg <-
  paste(
    dummy_msg,
    "However, for this model, binary indicator variables can be made for",
    "each of the levels of the factors (known as 'one-hot encoding')."
  )
string_to_factor_msg <-
  paste(
    "For modeling, it is preferred to encode qualitative data as factors",
    "(instead of character)."
  )

# ------------------------------------------------------------------------------
# Functions to create model code
# Alternate prefixes: scaffold? suggest?

#' Template code functions for specific models
#'
#' These functions make suggestions for code when using a few common models.
#' They print out code to the console that could be considered minimal syntax
#' for their respective techniques. Each creates a prototype recipe and workflow
#' object that can be edited or updated as the data require.
#'
#' @param formula A simple model formula with no in-line functions. This will
#' be used to template the recipe object as well as determining which outcome
#' and predictor columns will be used.
#' @param data A data frame with the columns used in the analysis.
#' @param verbose A single logical that determined whether comments are added to
#' the printed code explaining why certain lines are used.
#' @param tune A single logical that controls if code for model tuning should be
#' printed.
#' @return Invisible `NULL` but code is printed to the console.
#' @details
#' Based on the columns in `data`, certain recipe steps printed. For example, if
#' a model requires that qualitative predictors be converted to numeric (say,
#' using dummy variables) then an additional `step_dummy()` is added. Otherwise
#' that recipe step is not included in the output.
#'
#' The syntax is opinionated and should not be considered the exact answer for
#' every data analysis. It has reasonable defaults.
#' @examples
#' template_glmnet(Species ~ ., data = iris)
#' template_glmnet(Sepal.Length ~ ., data = iris, verbose = TRUE)
#' @export
#' @rdname templates
template_glmnet <- function(formula, data, verbose = FALSE, tune = TRUE) {
  rec_cl <- initial_recipe_call(match.call())
  rec_syntax <-
    "glmn_recipe" %>%
    assign_value(!!rec_cl)

  rec <- recipes::recipe(formula, data)

  rec_syntax <-
    rec_syntax %>%
    factor_check(rec, add = verbose)

  if (has_factor_pred(rec)) {
    rec_syntax <- add_steps_dummy_vars(rec_syntax, hot = TRUE, add = verbose)
  }
  rec_syntax <-
    rec_syntax %>%
    add_comment(paste(reg_msg, zv_msg), add = verbose) %>%
    add_steps_normalization()

  mod_mode <- model_mode(rec)

  if (mod_mode == "classification") {
    num_lvl <- y_lvl(rec)
    if (num_lvl == 2) {
      mod_syntax <-
        "glmn_model" %>%
        assign_value(logistic_reg(penalty = tune(), mixture = tune())) %>%
        pipe_value(set_mode("classification"))

    } else {
      mod_syntax <-
        "glmn_model" %>%
        assign_value(multinom_reg(penalty = tune(), mixture = tune())) %>%
        pipe_value(set_mode("classification"))

    }
  } else {
    mod_syntax <-
      "glmn_model" %>%
      assign_value(linear_reg(penalty = tune(), mixture = tune())) %>%
      pipe_value(set_mode("regression"))
  }

  mod_syntax <-
    mod_syntax %>%
    pipe_value(set_engine("glmnet"))

  cat(rec_syntax, "\n\n")
  cat(mod_syntax, "\n\n")
  cat(template_workflow("glmn"), "\n\n")

  glmn_grid <- rlang::expr(
    glmn_grid <- expand.grid(penalty = 10 ^ seq(-6,-1, length.out = 20),
                             mixture = c(0.05, .2, .4, .6, .8, 1))
  )
  cat(rlang::expr_text(glmn_grid, width = expr_width), "\n\n")
  cat(template_tune_with_grid("glmn"), "\n\n")
  invisible(NULL)
}

#' @export
#' @rdname templates
template_xgboost <- function(formula, data, verbose = FALSE, tune = TRUE) {
  rec_cl <- initial_recipe_call(match.call())
  rec_syntax <-
    "xgb_recipe" %>%
    assign_value(!!rec_cl)

  rec <- recipe(formula, data)

  rec_syntax <-
    rec_syntax %>%
    factor_check(rec, add = verbose)

  if (has_factor_pred(rec)) {
    rec_syntax <- add_steps_dummy_vars(rec_syntax, hot = TRUE, add = verbose)
  }

  rec_syntax <- pipe_value(rec_syntax, step_zv(all_predictors()))

  mod_syntax <-
    "xgb_model" %>%
    assign_value(boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(),
                            learn_rate = tune(), loss_reduction = tune(),
                            sample_size = tune()))

  mod_mode <- model_mode(rec)

  if (mod_mode == "classification") {
    mod_syntax <- mod_syntax %>% pipe_value(set_mode("classification"))
  } else {
    mod_syntax <- mod_syntax %>% pipe_value(set_mode("regression"))
  }

  mod_syntax <-
    mod_syntax %>%
    pipe_value(set_engine("xgboost"))

  cat(rec_syntax, "\n\n")
  cat(mod_syntax, "\n\n")
  cat(template_workflow("xgb"), "\n\n")
  cat(template_tune_no_grid("xgb"), "\n\n", sep = "")
  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname templates
template_knn <- function(formula, data, verbose = FALSE, tune = TRUE) {
  rec_cl <- initial_recipe_call(match.call())
  rec_syntax <-
    "knn_recipe" %>%
    assign_value(!!rec_cl)

  rec <- recipes::recipe(formula, data)

  rec_syntax <-
    rec_syntax %>%
    factor_check(rec, add = verbose)

  if (has_factor_pred(rec)) {
    rec_syntax <- add_steps_dummy_vars(rec_syntax, add = verbose)
  }
  rec_syntax <-
    add_steps_normalization(rec_syntax) %>%
    add_comment(paste(dist_msg, zv_msg), add = verbose) %>%
    add_steps_normalization()

  mod_mode <- model_mode(rec)

  if (mod_mode == "classification") {
    mod_syntax <-
      "knn_model" %>%
      assign_value(nearest_neighbor(neighbors = tune(), weight_func = tune())) %>%
      pipe_value(set_mode("classification"))
  } else {
    mod_syntax <-
      "knn_model" %>%
      assign_value(nearest_neighbor(neighbors = tune(), weight_func = tune())) %>%
      pipe_value(set_mode("regression"))
  }

  mod_syntax <-
    mod_syntax %>%
    pipe_value(set_engine("kknn"))

  cat(rec_syntax, "\n\n")
  cat(mod_syntax, "\n\n")
  cat(template_workflow("knn"), "\n\n")
  cat(template_tune_no_grid("knn"), "\n\n", sep = "")
  invisible(NULL)
}
