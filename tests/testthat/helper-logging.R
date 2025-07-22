step_logging_helper <-
  function(
    recipe,
    ...,
    type = NULL,
    role = "predictor",
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("logging_helper")
  ) {
    recipes::add_step(
      recipe,
      step_logging_helper_new(
        terms = rlang::enquos(...),
        type = type,
        role = role,
        trained = trained,
        skip = skip,
        id = id
      )
    )
  }

step_logging_helper_new <-
  function(terms, type, role, trained, skip, id) {
    recipes::step(
      subclass = "logging_helper",
      terms = terms,
      type = type,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  }

prep.step_logging_helper <- function(x, training, info = NULL, ...) {
  if (identical(x$type, "error")) {
    stop("testing error")
  }
  if (identical(x$type, "warning")) {
    warning("testing warning")
  }
  if (identical(x$type, "message")) {
    message("testing message")
  }

  step_logging_helper_new(
    terms = x$terms,
    type = x$type,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

bake.step_logging_helper <- function(object, new_data, ...) {
  new_data
}

catalog_lines <- function(lines) {
  lines[grepl("^>", lines)]
}
