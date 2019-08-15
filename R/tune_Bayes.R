#' Baeysian optmization of model parameters.
#'
#' @param object A workflow object.
#' @param rs A resample object.
#' @param param_info A `dials::param_set()` object or `NULL`. If none is given,
#' a parameters set is derived from the workflow.
#' @param metrics A `metric_set()`
#' @param objective A character string for what metric should be optimized or
#' an aquisition function object.
#' @param initial An initial set of results in a tidy format.
#' @param control A control object
#' @export
tune_Bayes <-
  function(object, rs, iter = 10, param_info = NULL, metrics = NULL, objective = NULL,
           initial = NULL, control = Bayes_control(), ...) {
    # object must be a workflow

    if (is.null(param_info)) {
      param_info <- param_set(object)
    }

    # check to see finalized

    perf <- yardstick::metric_set(rmse, rsq)

    if (is.null(initial) || is.numeric(initial)) {
      initial_grid <- create_initial_set(param_info, rs, initial)
    } else {
      initial_grid <- initial
    }

    res <- initial %>% dplyr::mutate(.iter = 0)

    for (i in 1:iter) {

      if (control$verbose) {
        message(cli::rule(left = crayon::bold(paste("Iteration", i))))
      }
      hist_summarizer(control, res, objective = "rmse")

      set.seed(i)
      gp_mod <- fit_gp(res %>% dplyr::select(-.iter), param_info,
                       metric = "rmse", control, ...)

      # get aqu functions
      candidates <-
        pred_gp(gp_mod, param_info, control = control) %>%
        dplyr::arrange(.mean) %>%
        dplyr::slice(1)

      tmp_res <- more_results(object, rs, candidates, perf, control)

      if (!inherits(tmp_res, "try-error")) {
        res <- dplyr::bind_rows(res, summarizer(tmp_res) %>% dplyr::mutate(.iter = i))
      }

      current_summarizer(control, res, objective = "rmse")

    }
    res
  }

# TODO
# - convergance crietion
# - aquisition funcitons
# - random point iters
# - avoid duplicates (anti-join?) or points very close
# - save pred mean and sd?

create_initial_set <- function(param, rs, n = NULL) {
  if (is.null(n)) {
    n <- nrow(param) + 1
  }
  dials::grid_latin_hypercube(param, size = n)
}

# ------------------------------------------------------------------------------

# TODO what happens to integers and logicals?
encode_set <- function(x, pset, as_matrix = FALSE, ...) {
  is_qual <- purrr::map_lgl(pset$object, ~ inherits(.x, "qual_param"))
  if (any(is_qual)) {
    idx <- which(is_qual)
    for (i in idx) {
      x[[ pset$id[i] ]] <-
        dials::encode_unit(pset$object[[i]], x[[ pset$id[i] ]], direction = "forward")
    }
  }

  has_trans <- purrr::map_lgl(pset$object, ~ !is.null(.x$trans))
  if (any(has_trans)) {
    idx <- which(has_trans)
    for (i in idx) {
      x[[ pset$id[i] ]] <-
        dials::value_transform(pset$object[[i]], x[[ pset$id[i] ]])
    }
  }

  if (as_matrix) {
    x <- as.matrix(x)
  }
  x
}

fit_gp <- function(dat, pset, metric, control, ...) {
  Bayes_msg(control, "GP model", fini = FALSE, cool = TRUE)
  dat <-
    dat %>%
    dplyr::filter(.metric == metric) %>%
    dplyr::select(dplyr::one_of(pset$id), mean)

  x <- encode_set(dat %>% dplyr::select(-mean), pset, as_matrix = TRUE)

  tmp_output <- capture.output(
    gp_fit <- try(kernlab::gausspr(x = x, y = dat$mean, variance.model = TRUE),
                  silent = TRUE)
  )
  if (inherits(gp_fit, "try-error")) {
    Bayes_msg(control, "GP model", fini = TRUE, cool = FALSE)
  } else {
    Bayes_msg(control, "GP model", fini = TRUE, cool = TRUE)
  }

  gp_fit
}


pred_gp <- function(object, pset, size = 5000, aqf = NULL, control) {
  Bayes_msg(control, "Generating candidates", fini = FALSE, cool = TRUE)
  pred_grid <- dials::grid_latin_hypercube(pset, size = size)
  if (inherits(object, "try-error")) {
    Bayes_msg(control, "Generating candidates", fini = TRUE, cool = FALSE)
    return(pred_grid %>% dplyr::mutate(.mean = NA_real_, .sd =  NA_real_))
  }

  x <- encode_set(pred_grid, pset, as_matrix = TRUE)
  mean_pred <- kernlab::predict(object, x, type = "response")
  sd_pred <- kernlab::predict(object, x, type = "sdeviation")

  Bayes_msg(control, "Generating candidates", fini = TRUE, cool = TRUE)

  pred_grid %>%
    dplyr::mutate(.mean = mean_pred[,1], .sd = sd_pred)
}

hist_summarizer <- function(control, x, minimize = TRUE, objective = NULL, digits = 4) {
  if (!control$verbose) {
    return(invisible(NULL))
  }
  x <- dplyr::filter(x, .metric == objective)
  if (minimize) {
    bst <- which.min(x$mean)
  } else {
    bst <- which.max(x$mean)
  }
  bst_iter <- x$.iter[bst]
  bst_val <- x$mean[bst]
  msg <-
    paste0(
      cli::symbol$play,
      " Current best:\t\t",
      objective,
      "=",
      signif(bst_val, digits = digits),
      " (@iter ",
      bst_iter,
      ")"
    )
  message(msg)
}

current_summarizer <- function(control, x, minimize = TRUE, objective = NULL, digits = 4) {
  if (!control$verbose) {
    return(invisible(NULL))
  }

  x <- dplyr::filter(x, .metric == objective)
  if (minimize) {
    bst <- which.min(x$mean)
  } else {
    bst <- which.max(x$mean)
  }
  bst_iter <- x$.iter[bst]
  max_iter <- max(x$.iter)
  bst_val <- x$mean[x$.iter == max_iter]
  msg <- paste0(" Newest results:\t", objective, "=", signif(bst_val, digits = digits))

  if (bst_iter == max_iter) {
    msg <- paste0(crayon::red(cli::symbol$heart), msg)
  } else {
    msg <- paste0(crayon::silver(cli::symbol$circle_cross), msg)
  }
  message(msg)
}

Bayes_msg <- function(control, msg, fini = FALSE, cool = TRUE) {
  if (!control$verbose) {
    return(invisible(NULL))
  }

  if (!fini) {
    msg <- paste0(cli::symbol$play, " ", msg)
  } else {
    if (cool) {
      msg <- paste0(crayon::green(cli::symbol$tick), " ", msg)
    } else {
      msg <- paste0(crayon::red(cli::symbol$cross), " ", msg)
    }
  }
  message(msg)
}


more_results <- function(object, rs, candidates, perf, control) {
  Bayes_msg(control, "Estimating performance", fini = FALSE, cool = TRUE)

  tmp_res <-
    try(
      tune_grid(
        object,
        rs,
        grid = candidates,
        perf = perf,
        control = grid_control(verbose = FALSE)
      ),
      silent = TRUE
    )

  if (inherits(tmp_res, "try-error")) {
    Bayes_msg(control, "Estimating performance", fini = TRUE, cool = FALSE)
  } else {
    Bayes_msg(control, "Estimating performance", fini = TRUE, cool = TRUE)
  }
  tmp_res
}

# ------------------------------------------------------------------------------

#' Control the Bayesian search process
#'
#' @param verbose A logical for logging results as they are generated.
#'
#' @export
Bayes_control <- function(verbose = FALSE) {
  # add options for `extract`, `save_predictions`, `allow_parallel`, and other stuff.
  # seeds per resample
  list(verbose = verbose)
}

