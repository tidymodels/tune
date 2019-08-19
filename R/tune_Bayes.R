#' Baeysian optmization of model parameters.
#'
#' @param object A workflow object.
#' @param rs A resample object.
#' @param param_info A `dials::param_set()` object or `NULL`. If none is given,
#' a parameters set is derived from the workflow.
#' @param metrics A `metric_set()`
#' @param iter The maximum number of search iterations.
#' @param objective A character string for what metric should be optimized or
#' an aquisition function object.
#' @param initial An initial set of results in a tidy format.
#' @param control A control object
#' @param ... Not currently used.
#' @export
tune_Bayes <-
  function(object, rs, iter = 10, param_info = NULL, perf = NULL, objective = NULL,
           initial = NULL, control = Bayes_control(), ...) {
    check_rset(rs)
    check_object(object, check_dials = TRUE)
    check_Bayes_control(control)
    perf <- check_perf(perf, object)
    perf_data <- perf_info(perf)
    perf_name <- get_objective_name(objective, perf)
    maximize <- perf_data$direction[perf_data$.metric == perf_name] == "maximize"

    if (is.null(param_info)) {
      param_info <- param_set(object)
    }

    initial_grid <- check_initial(initial, param_info)

    if (!any(names(initial_grid) == ".iter")) {
      res <- initial_grid %>% dplyr::mutate(.iter = 0)
    } else {
      res <- initial_grid
    }

    best_res <-
      res %>%
      dplyr::filter(.metric == perf_name) %>%
      dplyr::filter(!is.na(mean))

    if (maximize) {
      best_res <-
        best_res %>%
        arrange(desc(mean)) %>%
        slice(1)
    } else {
      best_res <-
        best_res %>%
        arrange(mean) %>%
        slice(1)
    }
    best_val <- best_res$mean[1]
    best_iter <- best_res$.iter[1]
    last_impr <- 0
    overall_iter <- max(res$.iter)

    for (i in (1:iter) + overall_iter) {

      if (control$verbose) {
        message("")
        message(cli::rule(left = crayon::bold(paste("Iteration", i))))
        message("")
      }
      hist_summarizer(control, best_val, best_iter, perf_name)

      set.seed(control$seed[1] + i)
      gp_mod <- fit_gp(res %>% dplyr::select(-.iter), param_info,
                       metric = perf_name, control, ...)

      # get aqu functions
      set.seed(control$seed[1] + i + 1)
      candidates <- pred_gp(gp_mod, param_info, control = control,
                            current = res %>% dplyr::select(dplyr::one_of(param_info$id)))

      if (all(is.na(candidates$.mean))) {
        if (nrow(candidates) < 2) {
          Bayes_msg(control, "Halting search", fini = TRUE, cool = FALSE)
          break
        } else {
          Bayes_msg(control, "Skipping ot next iteration", fini = TRUE, cool = FALSE)
          next
        }
      }

      if (last_impr < control$random_value) {
        if (maximize) {
          candidates <- candidates %>% dplyr::arrange(dplyr::desc(.mean)) %>% dplyr::slice(1)
        } else {
          candidates <- candidates %>% dplyr::arrange(.mean) %>% dplyr::slice(1)
        }
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

      param_msg(control, candidates)
      set.seed(control$seed[1] + i + 2)
      tmp_res <- more_results(object, rs, candidates, perf, control)

      all_bad <- is_cataclysmic(tmp_res)

      if (!inherits(tmp_res, "try-error") & !all_bad) {
        rs_estimate <- estimate(tmp_res)
        res <- dplyr::bind_rows(res, rs_estimate %>% dplyr::mutate(.iter = i))
        current_val <- tmp_res %>% estimate() %>% dplyr::filter(.metric == perf_name) %>% dplyr::pull(mean)

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
        current_summarizer(control, res, objective = perf_name)
      } else {
        if (all_bad) {
          Bayes_msg(control, "Estimating performance", fini = TRUE, cool = FALSE)
        }
        last_impr <- last_impr + 1
      }
    }
    res
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
  Bayes_msg(control, "Fitting Gaussian process model", fini = FALSE, cool = TRUE)
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
    Bayes_msg(control, "Gaussian process model failed", fini = TRUE, cool = FALSE)
  } else {
    Bayes_msg(control, "Gaussian process model complete", fini = TRUE, cool = TRUE)
  }

  gp_fit
}


pred_gp <- function(object, pset, size = 5000, aqf = NULL, current, control) {
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
  mean_pred <- kernlab::predict(object, x, type = "response")
  sd_pred <- kernlab::predict(object, x, type = "sdeviation")

  Bayes_msg(control, "Predicted candidates", fini = TRUE, cool = TRUE)

  pred_grid %>%
    dplyr::mutate(.mean = mean_pred[,1], .sd = sd_pred)
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
  candidate <- candidate[, !(names(candidate) %in% c(".mean", ".sd"))]
  p_chr <- paste0(names(candidate), "=", format(as.data.frame(candidate), digits = 3))
  message(
    paste0(
      cli:::symbol$square,
      " ",
      glue::glue_collapse(p_chr, width = options()$width - 5, sep = ", ")
    )
  )
}

more_results <- function(object, rs, candidates, perf, control) {
  Bayes_msg(control, "Estimating performance", fini = FALSE, cool = TRUE)

  candidates <- candidates[, !(names(candidates) %in% c(".mean", ".sd"))]
  p_chr <- paste0(names(candidates), "=", format(as.data.frame(candidates), digits = 3))

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
  is_err <- purrr:::map_lgl(x$.metrics, inherits, c("simpleError", "error"))
  if (any(!is_err)) {
    is_good <- purrr:::map_lgl(x$.metrics[!is_err],
                               ~ tibble::is_tibble(.x) && nrow(.x) > 0)
    is_err[!is_err] <- !is_good
  }
  all(is_err)
}


# ------------------------------------------------------------------------------

#' Control the Bayesian search process
#'
#' @param verbose A logical for logging results as they are generated.
#' @param random_value The number of iterations with no improvment before an
#' uncertainty sample is created where a sample with high predicted variance is
#' chosen.
#' @param seed An integer for controlling the random number stream.
#' @export
Bayes_control <- function(verbose = FALSE, random_value = 3, seed = sample.int(10^5, 1)) {
  # add options for `extract`, `save_predictions`, `allow_parallel`, and other stuff.
  # seeds per resample
  list(verbose = verbose, random_value = random_value, seed = seed)
}

