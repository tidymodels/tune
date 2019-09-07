#' Bayesian optimization of model parameters.
#'
#' @param object A workflow object.
#' @param rs A resample object.
#' @param param_info A `dials::param_set()` object or `NULL`. If none is given,
#' a parameters set is derived from the workflow.
#' @param perf A `yardstick::metric_set()` object containing information on how
#' models will be evaluated for performance. The first metric in `perf` is the
#' one that will be optimized.
#' @param iter The maximum number of search iterations.
#' @param objective A character string for what metric should be optimized or
#' an acquisition function object.
#' @param initial An initial set of results in a tidy format.
#' @param control A control object
#' @param ... Not currently used.
#' @export
tune_Bayes <-
  function(object, rs, iter = 10, param_info = NULL, perf = NULL,
           objective = exp_improve(),
           initial = NULL, control = Bayes_control(), ...) {
    start_time <- proc.time()[3]

    check_rset(rs)
    check_object(object, check_dials = is.null(param_info))
    check_Bayes_control(control)
    perf <- check_perf(perf, object)
    perf_data <- perf_info(perf)
    perf_name <- perf_data$.metric[1]
    maximize <- perf_data$direction[perf_data$.metric == perf_name] == "maximize"

    if (is.null(param_info)) {
      param_info <- param_set(object)
    }

    unsummarized <- check_initial(initial, param_info, object, rs, perf, control)
    mean_stats <- summarize(unsummarized)

    check_time(start_time, control$time_limit)

    on.exit({
      warning("Optimization stopped prematurely; returning current results.", call. = FALSE)
      return(unsummarized)
    })

    best_res <-
      mean_stats %>%
      dplyr::filter(.metric == perf_name) %>%
      dplyr::filter(!is.na(mean))

    if (maximize) {
      best_res <-
        best_res %>%
        dplyr::arrange(desc(mean)) %>%
        slice(1)
    } else {
      best_res <-
        best_res %>%
        dplyr::arrange(mean) %>%
        slice(1)
    }
    best_val <- best_res$mean[1]
    best_iter <- best_res$.iter[1]
    last_impr <- 0
    overall_iter <- max(mean_stats$.iter)

    if (control$verbose) {
      message(paste("Optimizing", perf_name, "using", objective$label))
    }

    for (i in (1:iter) + overall_iter) {

      if (control$verbose) {
        message("")
        message(cli::rule(left = crayon::bold(paste("Iteration", i))))
        message("")
      }
      hist_summarizer(control, best_val, best_iter, perf_name)

      check_time(start_time, control$time_limit)

      set.seed(control$seed[1] + i)
      gp_mod <- fit_gp(mean_stats %>% dplyr::select(-.iter), pset = param_info,
                       metric = perf_name, control = control, ...)

      check_time(start_time, control$time_limit)

      # get aqu functions
      set.seed(control$seed[1] + i + 1)
      candidates <- pred_gp(gp_mod, param_info, control = control,
                            current = mean_stats %>% dplyr::select(dplyr::one_of(param_info$id)))

      check_time(start_time, control$time_limit)

      acq_summarizer(control, iter = i, objective = objective)

      candidates <-
        dplyr::bind_cols(candidates,
                         stats::predict(objective, candidates, iter = i,
                                        maximize = maximize, best_val))

      check_time(start_time, control$time_limit)

      if (all(is.na(candidates$.mean))) {
        if (nrow(candidates) < 2) {
          Bayes_msg(control, "Halting search", fini = TRUE, cool = FALSE)
          break
        } else {
          Bayes_msg(control, "Skipping ot next iteration", fini = TRUE, cool = FALSE)
          next
        }
      }

      if (last_impr < control$uncertain) {
        candidates <- candidates %>% dplyr::arrange(dplyr::desc(objective)) %>% dplyr::slice(1)
      } else {
        message(paste(crayon::blue(cli::symbol$circle_question_mark),
                       "Uncertainty sample"))
        candidates <-
          candidates %>%
          dplyr::arrange(dplyr::desc(.sd)) %>%
          dplyr::slice(1:floor(.1*nrow(candidates))) %>%
          dplyr::sample_n(1)
        last_impr <- 0
      }

      check_time(start_time, control$time_limit)

      param_msg(control, candidates)
      set.seed(control$seed[1] + i + 2)
      tmp_res <- more_results(object, rs, candidates, perf, control)

      check_time(start_time, control$time_limit)

      all_bad <- is_cataclysmic(tmp_res)

      if (!inherits(tmp_res, "try-error") & !all_bad) {
        unsummarized <- dplyr::bind_rows(unsummarized, tmp_res %>% mutate(.iter = i))
        rs_estimate <- summarize(tmp_res)
        mean_stats <- dplyr::bind_rows(mean_stats, rs_estimate %>% dplyr::mutate(.iter = i))
        current_val <-
          tmp_res %>%
          summarize() %>%
          dplyr::filter(.metric == perf_name) %>%
          dplyr::pull(mean)

        if (maximize) {
          is_better <- current_val > best_val
        } else {
          is_better <- current_val < best_val
        }

        if (is_better) {
          last_impr <- 0
          best_val <- current_val
          best_iter <- i
        } else {
          last_impr <- last_impr + 1
        }
        current_summarizer(control, x = mean_stats, maximize = maximize, objective = perf_name)
      } else {
        if (all_bad) {
          Bayes_msg(control, "Estimating performance", fini = TRUE, cool = FALSE)
        }
        last_impr <- last_impr + 1
      }
      check_time(start_time, control$time_limit)
    }
    on.exit()
    unsummarized
  }

create_initial_set <- function(param, n = NULL) {
  if (is.null(n)) {
    n <- nrow(param) + 1
  }
  dials::grid_latin_hypercube(param, size = n)
}


# ------------------------------------------------------------------------------

# TODO what happens to  logicals?

encode_set <- function(x, pset, as_matrix = FALSE, ...) {
  # change the numeric variables to the transformed scale (if any)
  has_trans <- purrr::map_lgl(pset$object, ~ !is.null(.x$trans))
  if (any(has_trans)) {
    idx <- which(has_trans)
    for (i in idx) {
      x[[ pset$id[i] ]] <-
        dials::value_transform(pset$object[[i]], x[[ pset$id[i] ]])
    }
  }

  is_quant <- purrr::map_lgl(pset$object, inherits, "quant_param")
  # Convert all data to the [0, 1] scale based on their possible range (not on
  # their observed range)
  x[, is_quant] <- purrr::map2_dfc(pset$object[is_quant], x[, is_quant],
                                   encode_unit, direction = "forward")

  # Ensure that the right levels are used to create dummy variables
  if (any(!is_quant)) {
    for (i in which(!is_quant)) {
      x[[i]] <- factor(x[[i]], levels = pset$object[[i]]$values)
    }
  }

  if (as_matrix) {
    x <- stats::model.matrix(~ .  + 0, data = x)
  }
  x
}

fit_gp <- function(dat, pset, metric, control, ...) {
  Bayes_msg(control, "Fitting Gaussian process model", fini = FALSE, cool = TRUE)
  dat <-
    dat %>%
    dplyr::filter(.metric == metric) %>%
    dplyr::select(dplyr::one_of(pset$id), mean)

  x <- encode_set(dat %>% dplyr::select(-mean), pset, as_matrix = TRUE)

  tmp_output <- capture.output(
    gp_fit <-
      try(GPfit::GP_fit(X = x, Y = dat$mean),
          silent = TRUE)
  )
  if (inherits(gp_fit, "try-error")) {
    Bayes_msg(control, "Gaussian process model failed", fini = TRUE, cool = FALSE)
  } else {
    Bayes_msg(control, "Gaussian process model complete", fini = TRUE, cool = TRUE)
  }

  gp_fit
}


pred_gp <- function(object, pset, size = 5000, current, control) {
  pred_grid <-
    dials::grid_latin_hypercube(pset, size = size) %>%
    dplyr::distinct() %>%
    dplyr::anti_join(current, by = pset$id)

  if (inherits(object, "try-error") | nrow(pred_grid) == 0) {
    Bayes_msg(control, "Could not generate candidates", fini = TRUE, cool = FALSE)
    return(pred_grid %>% dplyr::mutate(.mean = NA_real_, .sd =  NA_real_))
  }

  Bayes_msg(control, paste("Generating", nrow(pred_grid), "candidates"),
            fini = FALSE, cool = TRUE)

  x <- encode_set(pred_grid, pset, as_matrix = TRUE)
  gp_pred <- predict(object, x)

  Bayes_msg(control, "Predicted candidates", fini = TRUE, cool = TRUE)

  pred_grid %>%
    dplyr::mutate(.mean = gp_pred$Y_hat, .sd = sqrt(gp_pred$MSE))
}

hist_summarizer <- function(control, value, iter, nm, digits = 4) {
  if (!control$verbose) {
    return(invisible(NULL))
  }
  msg <-
    paste0(
      cli::symbol$star,
      " Current best:\t",
      nm,
      "=",
      signif(value, digits = digits),
      " (@iter ",
      iter,
      ")"
    )
  message(msg)
}

current_summarizer <- function(control, x, maximize = TRUE, objective = NULL, digits = 4) {
  if (!control$verbose) {
    return(invisible(NULL))
  }

  x <- dplyr::filter(x, .metric == objective)
  if (maximize) {
    bst <- which.max(x$mean)
  } else {
    bst <- which.min(x$mean)
  }
  bst_iter <- x$.iter[bst]
  max_iter <- max(x$.iter)
  bst_val <- x$mean[x$.iter == max_iter]
  bst_se <- x$std_err[x$.iter == max_iter]
  msg <-
    paste0(" Newest results:\t",
           objective,
           "=",
           signif(bst_val, digits = digits))
  if (!is.na(bst_se) && bst_se > 0) {
    msg <- paste0(msg,  " (+/-", signif(bst_se, digits = digits - 1), ")")
  }

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


param_msg <- function(control, candidate) {
  if (!control$verbose) {
    return(invisible(NULL))
  }
  candidate <- candidate[, !(names(candidate) %in% c(".mean", ".sd", "objective"))]
  p_chr <- paste0(names(candidate), "=", format(as.data.frame(candidate), digits = 3))
  message(
    paste0(
      cli::symbol$square,
      " ",
      glue::glue_collapse(p_chr, width = options()$width - 5, sep = ", ")
    )
  )
}


acq_summarizer <- function(control, iter, objective = NULL, digits = 4) {
  if (!control$verbose) {
    return(invisible(NULL))
  }
  if (inherits(objective, "conf_bound") && is.function(objective$kappa)) {
    val <- paste0(cli::symbol$info, " Kappa value: ",
                  signif(objective$kappa(iter), digits = digits))
  } else {
    if (inherits(objective, c("exp_improve", "prob_improve")) &&
        is.function(objective$trade_off)) {
      val <- paste0(cli::symbol$info, " Trade-off value: ",
                    signif(objective$trade_off(iter), digits = digits))

    } else {
      val <- NULL
    }
  }
  if (!is.null(val)) {
    message(val)
  }
  invisible(NULL)
}

# ------------------------------------------------------------------------------


more_results <- function(object, rs, candidates, perf, control) {
  Bayes_msg(control, "Estimating performance", fini = FALSE, cool = TRUE)

  candidates <- candidates[, !(names(candidates) %in% c(".mean", ".sd", "objective"))]
  p_chr <- paste0(names(candidates), "=", format(as.data.frame(candidates), digits = 3))

  tmp_res <-
    try(
      tune_grid(
        object,
        rs,
        grid = candidates,
        perf = perf,
        control = grid_control(verbose = FALSE, extract = control$extract,
                               save_pred = control$save_pred)
      ),
      silent = TRUE
    )

  if (inherits(tmp_res, "try-error")) {
    Bayes_msg(control, "Estimating performance", fini = TRUE, cool = FALSE)
  } else {
    all_bad <- is_cataclysmic(tmp_res)
    if (all_bad) {
      p_chr <- glue::glue_collapse(p_chr, width = options()$width - 28, sep = ", ")
      msg <- paste("All models failed for:", p_chr)
      Bayes_msg(control, msg, fini = TRUE, cool = FALSE)
      tmp_res <- simpleError(msg)
    } else {
      Bayes_msg(control, "Estimating performance", fini = TRUE, cool = TRUE)
    }
  }
  tmp_res
}


is_cataclysmic <- function(x) {
  is_err <- purrr::map_lgl(x$.metrics, inherits, c("simpleError", "error"))
  if (any(!is_err)) {
    is_good <- purrr::map_lgl(x$.metrics[!is_err],
                               ~ tibble::is_tibble(.x) && nrow(.x) > 0)
    is_err[!is_err] <- !is_good
  }
  all(is_err)
}


# ------------------------------------------------------------------------------

#' Control the Bayesian search process
#'
#' @param verbose A logical for logging results as they are generated.
#' @param uncertain The number of iterations with no improvment before an
#'  uncertainty sample is created where a sample with high predicted variance is
#'  chosen.
#' @param seed An integer for controlling the random number stream.
#' @param extract An optional function to collection any information from the
#' model or other objects. See `grid_control()` for details. Note that if
#' initial results were already generated using `tune_grid()`, care must be
#' taken if the Bayesian search has a different extraction function.
#' @param save_pred A logical to save the out-of-sample predictions from
#' each resample and each parameter combination. See `grid_control()` for details.
#' @param time_limit A number for the minimum number of _minutes_ (elapsed)
#'  that the function should execute. The elapsed time is evaluated at internal
#'  checkpoints and, if over time, the results at that time are returned (with a
#'  warning). This means that the `time_limit` is not an exact limit, but a
#'  minimum time limit.
#' @export
Bayes_control <-
  function(verbose = FALSE,
           uncertain = Inf,
           seed = sample.int(10^5, 1),
           extract = NULL,
           save_pred = FALSE,
           time_limit = NA) {
    # add options for `allow_parallel`, and other stuff.
    # seeds per resample
    list(
      verbose = verbose,
      uncertain = uncertain,
      seed = seed,
      extract = extract,
      save_pred = save_pred,
      time_limit = time_limit
    )
  }


check_time <- function(origin, limit) {
  if (is.na(limit)) {
    return(invisible(NULL))
  }
  now_time <- proc.time()[3]
  if (now_time - origin >= limit * 60) {
    stop(paste("The time limit of", limit, "minutes has been reached."), call. = FALSE)
  }
  invisible(NULL)
}

# May be better to completely refactor things to a high-level call then use
# base's setTimeLimit().
