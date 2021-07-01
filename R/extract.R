#' Extract elements of `tune` objects
#'
#' @description
#' These functions extract various elements from a tune object. If they do
#' not exist yet, an error is thrown.
#'
#' - `extract_preprocessor()` returns the formula, recipe, or variable
#'   expressions used for preprocessing.
#'
#' - `extract_spec_parsnip()` returns the parsnip model specification.
#'
#' - `extract_fit_parsnip()` returns the parsnip model fit object.
#'
#' - `extract_fit_engine()` returns the engine specific fit embedded within
#'   a parsnip model fit. For example, when using [parsnip::linear_reg()]
#'   with the `"lm"` engine, this would return the underlying `lm` object.
#'
#' - `extract_mold()` returns the preprocessed "mold" object returned
#'   from [hardhat::mold()]. It contains information about the preprocessing,
#'   including either the prepped recipe, the formula terms object, or
#'   variable selectors.
#'
#' - `extract_recipe()` returns the recipe. The `estimated` argument specifies
#'    whether the fitted or original recipe is returned.
#'
#' - `extract_workflow()` returns the workflow object if the control option
#'    `save_workflow = TRUE` was used. The workflow will only have been
#'    estimated for objects produced by [last_fit()].
#'
#' @param x A workflow
#' @param estimated A logical for whether the original (unfit) recipe or the
#' fitted recipe should be returned.
#' @param ... Not currently used.
#' @details
#' These functions supersede `extract_model()`.
#'
#' Extracting the underlying engine fit can be helpful for describing the
#'  model (via `print()`, `summary()`, `plot()`, etc.) or for variable
#'  importance/explainers.
#'
#' However, users should not invoke the `predict()` method on an extracted
#'  model. There may be preprocessing operations that `workflows` has executed on
#'  the data prior to giving it to the model. Bypassing these can lead to errors
#'  or silently generating incorrect predictions.
#'
#' *Good*:
#' ```r
#'    workflow_fit %>% predict(new_data)
#' ```
#'
#' *Bad*:
#' ```r
#'    workflow_fit %>% extract_fit_engine()  %>% predict(new_data)
#'    # or
#'    workflow_fit %>% extract_fit_parsnip() %>% predict(new_data)
#' ```
#' @return
#' The extracted value from the `tune` object, `x`, as described in the
#' description section.
#'
#' @name extract-tune
#' @examples
#' library(recipes)
#' library(rsample)
#' library(parsnip)
#'
#' set.seed(6735)
#' tr_te_split <- initial_split(mtcars)
#'
#' spline_rec <- recipe(mpg ~ ., data = mtcars) %>%
#'   step_ns(disp)
#'
#' lin_mod <- linear_reg() %>%
#'   set_engine("lm")
#'
#' spline_res <- last_fit(lin_mod, spline_rec, split = tr_te_split)
#'
#' extract_preprocessor(spline_res)
#'
#' # The `spec` is the parsnip spec before it has been fit.
#' # The `fit` is the fit parsnip model.
#' extract_spec_parsnip(spline_res)
#' extract_fit_parsnip(spline_res)
#' extract_fit_engine(spline_res)
#'
#' # The mold is returned from `hardhat::mold()`, and contains the
#' # predictors, outcomes, and information about the preprocessing
#' # for use on new data at `predict()` time.
#' extract_mold(spline_res)
#'
#' # A useful shortcut is to extract the fitted recipe from the workflow
#' extract_recipe(spline_res)
#'
#' # That is identical to
#' identical(
#'   extract_mold(spline_res)$blueprint$recipe,
#'   extract_recipe(spline_res)
#' )
NULL

#' @export
#' @rdname extract-tune
extract_workflow.last_fit <- function(x, ...) {
  x$.workflow[[1]]
}


#' @export
#' @rdname extract-tune
extract_workflow.tune_results <- function(x, ...) {
  .get_tune_workflow(x)
}

#' @export
#' @rdname extract-tune
extract_spec_parsnip.tune_results <- function(x, ...) {
  extract_spec_parsnip(extract_workflow(x))
}

#' @export
#' @rdname extract-tune
extract_recipe.tune_results <- function(x, estimated = TRUE, ...) {
  extract_recipe(extract_workflow(x), estimated = estimated)
}

#' @export
#' @rdname extract-tune
extract_fit_parsnip.tune_results <- function(x, ...) {
  extract_fit_parsnip(extract_workflow(x))
}

#' @export
#' @rdname extract-tune
extract_fit_engine.tune_results <- function(x, ...) {
  extract_fit_engine(extract_workflow(x))
}

#' @export
#' @rdname extract-tune
extract_mold.tune_results <- function(x, ...) {
  extract_mold(extract_workflow(x))
}

#' @export
#' @rdname extract-tune
extract_preprocessor.tune_results <- function(x, ...) {
  extract_preprocessor(extract_workflow(x))
}
