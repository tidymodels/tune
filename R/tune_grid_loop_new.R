tune_grid_loop_new <- function(
  resamples,
  grid,
  workflow,
  param_info,
  metrics,
  eval_time,
  control
) {
  mtr_info <- tibble::as_tibble(metrics)

  control <- check_parallel_over(control, resamples)

  # We'll significantly alter the rsample object but will need it intact later;
  # Make a copy and save some information before proceeding
  rset_info <- pull_rset_attributes(resamples)
  split_args <- rsample::.get_split_args(resamples)
  # Break it up row-wise to facilitate mapping/parallelism
  resamples <- vec_list_rowwise(resamples)

  # add an on.exit(); esc puts us in code

  # Notes on debugging:
  # 1. You can set `options(future.debug = TRUE)` to help
  # 2. If you are debugging loopy, use the control option `allow_par = FALSE`;
  #    that will use `lapply()` so that you can see output.

  # ------------------------------------------------------------------------------
  # Collect "static" data into a single object for a cleaner interface

  static <- make_static(
    workflow,
    param_info = param_info,
    metrics = metrics,
    eval_time = eval_time,
    split_args = split_args,
    control = control
  )

  tm_pkgs <- c("rsample", "workflows", "hardhat", "tune", "parsnip", "tailor")
  load_pkgs <- c(required_pkgs(workflow), control$pkgs, tm_pkgs)
  load_pkgs <- unique(load_pkgs)

  par_opt <- list(
    future.label = "tune-grid-%d",
    future.stdout = TRUE,
    future.seed = TRUE,
    # future.globals = c(), # add options from control?
    future.packages = quote(load_pkgs)
  )

  # ------------------------------------------------------------------------------
  # Control execution

  if (control$parallel_over == "resamples") {
    # The default: Loop over splits, process whole grid

    cl <- loop_call(control, par_opt)
    res <- rlang::eval_bare(cl)
  } else {
    # TODO go back and revaluate if the helper functions to schedule certain
    # stages can be used at this level of the computations (see: Frick(2025))

    # If multiple resamples but preprocessing is cheap (or just a validation set).
    # Loop over grid rows and splits
    candidates <- get_row_wise_grid(workflow, grid)
    # Break all combinations of resamples and candidates into a list of integers
    # for each combination.
    inds <- tidyr::crossing(s = seq_along(candidates), b = seq_along(resamples))
    inds <- vec_list_rowwise(inds)

    cl <- loop_call(control, par_opt)
    res <- rlang::eval_bare(cl)
  }

  # ------------------------------------------------------------------------------
  # Separate results into different components

  res <- dplyr::bind_rows(res)
  resamples <- dplyr::bind_rows(resamples)
  id_cols <- grep("^id", names(resamples), value = TRUE)

  if (control$parallel_over == "resamples") {
    res <- dplyr::full_join(resamples, res, by = id_cols)
  } else {
    # Clean up for variable columns and make into a function
    pool_cols <- grep("^\\.", names(res), value = TRUE)
    res <- res |>
      dplyr::summarize(
        dplyr::across(dplyr::matches("^\\."), ~ list(purrr::list_rbind(.x))),
        .by = c(!!!id_cols)
      ) |>
      dplyr::full_join(resamples, by = id_cols)
  }

  res <- res |>
    dplyr::relocate(
      splits,
      dplyr::starts_with("id"),
      .metrics,
      .notes,
      dplyr::any_of(".predictions"),
      dplyr::any_of(".extracts")
    )

  tune_contructor(
    resamples = res,
    parameters = param_info,
    metrics = metrics,
    eval_time = eval_time,
    eval_time_target = NULL,
    outcomes = static$y_name,
    rset_info = rset_info,
    workflow = workflow
  )
}

vec_list_rowwise <- function(x) {
  vctrs::vec_split(x, by = 1:nrow(x))$val
}

check_parallel_over <- function(control, resamples) {
  if (is.null(control$parallel_over)) {
    control$parallel_over <- "resamples"
  }
  if (length(resamples$splits) == 1) {
    control$parallel_over <- "everything"
  }
  control
}

# TODO for futures to do code inspection to figure out globals, we might need
# to explicitly have the code written out.
loop_call <- function(ctrl, opts) {
  if (ctrl$allow_par) {
    if (ctrl$parallel_over == "resamples") {
      cl <- rlang::call2(
        "future_lapply",
        .ns = "future.apply",
        X = quote(resamples),
        FUN = "loopy",
        quote(grid),
        quote(static),
        !!!opts
      )
    } else {
      cl <- rlang::call2(
        "future_lapply",
        .ns = "future.apply",
        X = quote(inds),
        FUN = "loopy2",
        quote(resamples),
        quote(candidates),
        quote(static),
        !!!opts
      )
    }
  } else {
    if (ctrl$parallel_over == "resamples") {
      cl <- rlang::call2(
        "lapply",
        X = quote(resamples),
        FUN = "loopy",
        quote(grid),
        quote(static)
      )
    } else {
      cl <- rlang::call2(
        "lapply",
        X = quote(inds),
        FUN = "loopy2",
        quote(resamples),
        quote(candidates),
        quote(static)
      )
    }
  }
  cl
}

# `new_tune_results()` is exported so let's make a different one

tune_contructor <- function(
  resamples,
  parameters,
  metrics,
  eval_time,
  eval_time_target,
  outcomes = character(0),
  rset_info,
  ...,
  cls = character(0)
) {
  new_bare_tibble(
    x = resamples,
    parameters = parameters,
    metrics = metrics,
    eval_time = eval_time,
    eval_time_target = eval_time_target,
    outcomes = outcomes,
    rset_info = rset_info,
    ...,
    class = c(cls, "tune_results")
  )
}

