# Determine any qualitative parameters and their ranges

find_qual_param <- function(pset) {
  is_qual <- purrr::map_lgl(pset$object, ~ inherits(.x, "qual_param"))
  if (!any(is_qual)) {
    return(list())
  }

  pset <- pset[is_qual, ]

  possible_lvl <- purrr::map(pset$object, ~ .x$values)
  names(possible_lvl) <- pset$id
  possible_lvl
}

# Leave as-is but scale the others; order by parameter id in pset

# Create the kernel for the continuous parameters
# Add a new kernel for each qualitative parameter

make_kernel <- function(pset, lvls) {
  qual_ind <- which(pset$id %in% names(lvls))
  quant_ind <- setdiff(seq_along(pset$id), qual_ind)
  quant_avail <- length(quant_ind) > 0

  if (length(qual_ind) == 0) {
    return(GauPro::k_Matern32(D = length(quant_ind)))
  }

  num_kernels <- length(qual_ind) + quant_avail

  kernels <- vector(mode = "list", length = num_kernels)
  kern_count <- 0

  if (quant_avail) {
    kernels[[1]] <- GauPro::k_IgnoreIndsKernel(
      k = GauPro::k_Matern32(D = length(quant_ind)),
      ignoreinds = qual_ind
    )
    kern_count <- 1
  }

  for (i in seq_along(qual_ind)) {
    kern_count <- kern_count + 1
    kernels[[kern_count]] <- GauPro::k_FactorKernel(
      D = 1,
      xindex = qual_ind[i],
      nlevels = length(lvls[[1]])
    )
  }

  for (i in 1:num_kernels) {
    if (i == 1) {
      res <- kernels[[1]]
    } else {
      res <- res * kernels[[i]]
    }
  }
  res
}

check_gp <- function(x) {
  model_fail <- inherits(x, "try-error")
  if (!model_fail) {
    loo_res <- summary(x)
    loo_bad <- loo_res$coverage95LOO < 0.1
    loo_rsq <- loo_res$r.squaredLOO
  } else {
    loo_bad <- TRUE
    loo_rsq <- 0.0
  }
  # convergence?
  list(use = !loo_bad & !model_fail & loo_rsq > 0.1, rsq = loo_rsq)
}

# encode_set() was created to work on all types of tuning parameters; usage of
# GauPro means that we should not encode qualitative tuning parameters so we
# need a wrapper
partial_encode <- function(dat, pset) {
  qual_info <- find_qual_param(pset)

  if (any(names(dat) == "mean")) {
    outcomes <- dat$mean
  }

  normalized <- encode_set(
    dat %>% dplyr::select(dplyr::all_of(pset$id)),
    pset = pset,
    as_matrix = FALSE
  )

  # Replace with the original data when qualitative parameters
  # GauPro::gpkm can take factor encodings to work
  for (i in seq_along(qual_info)) {
    nm <- names(qual_info)[i]
    normalized[[nm]] <- factor(dat[[nm]], levels = qual_info[[i]])
  }
  if (any(names(dat) == "mean")) {
    normalized$.outcome <- dat$mean
  }
  normalized
}

# ------------------------------------------------------------------------------

# TODO not catching warnings such as
# Warning messages:
#   1: In self$pred_one_matrix(XX = XX, se.fit = se.fit, covmat = covmat,  :
#     Too small s2 predictions are being set to 4.2948089596254e-08 (2908 values, min=-30.8111711641092).
#   2: covmat is not being altered.

fit_gp <- function(
  dat,
  pset,
  metric,
  eval_time = NULL,
  control,
  previous = NULL,
  ...
) {
  # TODO check dots for options; no longer used; update docs

  dat <- dat %>% dplyr::filter(.metric == metric)

  if (!is.null(eval_time)) {
    dat <- dat %>% dplyr::filter(.eval_time == eval_time)
  }

  dat <- dat %>%
    check_gp_data() %>%
    dplyr::select(dplyr::all_of(pset$id), mean)

  qual_info <- find_qual_param(pset)
  num_pred <- nrow(pset)
  num_cand <- nrow(dat)

  normalized <- partial_encode(dat, pset)

  gp_kernel <- make_kernel(pset, qual_info)

  if (num_cand <= num_pred && num_cand > 0 & control$verbose_iter) {
    msg <- cli::format_inline(
      "The Gaussian process model is being fit using {num_pred} feature{?s} but
			only has {num_cand} data point{?s} to do so. This may cause errors or a
			poor model fit."
    )
    message_wrap(
      msg,
      prefix = "!",
      color_text = get_tune_colors()$message$warning
    )
  }

  if (!is.null(previous)) {
    if (!previous$use) {
      previous <- NULL
    }
  }

  # TODO use better handler to avoid warnings, use log_catalog to send to notes
  # TODO pass `...` here?
  if (is.null(previous)) {
    withr::with_seed(
      114,
      gp_fit <- try(
        GauPro::gpkm(
          .outcome ~ .,
          data = normalized,
          kernel = gp_kernel,
          verbose = 0,
          restarts = 5,
          nug.est = FALSE,
          parallel = FALSE
        ),
        silent = TRUE
      )
    )
  } else {
    # TODO we should remove the updating; not a great idea
    new_val <- normalized %>% dplyr::slice_tail(n = 1)
    new_x <- as.matrix(new_val[, pset$id])
    new_y <- new_val$.outcome

    withr::with_seed(
      114,
      gp_fit <- try(previous$fit$update(new_x, new_y), silent = TRUE)
    )
  }

  new_check <- check_gp(gp_fit)

  if (control$verbose_iter) {
    if (new_check$use) {
      msg <- cli::format_inline(
        "Gaussian process model (LOO R\u00B2: {round(new_check$rsq * 100, 1)}%)"
      )
      message_wrap(
        msg,
        prefix = cli::symbol$tick,
        color_text = get_tune_colors()$message$success
      )
    } else {
      message_wrap(
        "Gaussian process model failed",
        prefix = cli::symbol$tick,
        color_text = get_tune_colors()$message$danger
      )
    }
  }

  list(
    fit = gp_fit,
    use = new_check$use,
    rsq = new_check$rsq,
    tr = normalized
  )
}

# ------------------------------------------------------------------------------

pred_gp <- function(object, pset, size = 5000, current = NULL, control) {
  candidates <- dials::grid_space_filling(
    pset,
    size = size,
    type = "latin_hypercube"
  ) %>%
    dplyr::distinct()

  if (!object$use) {
    x <- partial_encode(candidates, pset)
    x_old <- object$tr
    x_old <- x_old[, names(x)]

    keep_ind <- dissim_sample(x_old, x, pset, max_n = Inf)
    candidates <- candidates[keep_ind, ] %>%
      dplyr::mutate(.mean = NA_real_, .sd = NA_real_)

    msg <- cli::format_inline(
      "Generating a candidate as far away from existing points as possible."
    )
    message_wrap(
      msg,
      prefix = cli::symbol$info,
      color_text = get_tune_colors()$message$info
    )

    return(candidates)
  } else {
    message_wrap(
      paste("Generating", nrow(candidates), "candidates"),
      prefix = cli::symbol$info,
      color_text = get_tune_colors()$message$info
    )
  }

  if (!is.null(current)) {
    candidates <- candidates %>%
      dplyr::anti_join(current, by = pset$id)
  }

  if (nrow(candidates) == 0) {
    message_wrap(
      "No remaining candidate models",
      prefix = cli::symbol$tick,
      color_text = get_tune_colors()$message$warning
    )
    return(candidates %>% dplyr::mutate(.mean = NA_real_, .sd = NA_real_))
  }

  x <- partial_encode(candidates, pset)

  gp_pred <- object$fit$pred(x, se.fit = TRUE)

  gp_pred <- tibble::as_tibble(gp_pred) %>%
    dplyr::select(.mean = mean, .sd = se)
  dplyr::bind_cols(candidates, gp_pred)
}


pick_candidate <- function(results, info, control) {
  bad_gp <- all(is.na(results$.mean))
  if (!bad_gp & info$uncertainty < control$uncertain) {
    results <- results %>%
      dplyr::arrange(dplyr::desc(objective)) %>%
      dplyr::slice(1)
  } else {
    if (control$verbose_iter) {
      message_wrap(
        "Uncertainty sample",
        prefix = cli::symbol$info,
        color_text = get_tune_colors()$message$info
      )
    }
    results <- results %>%
      dplyr::arrange(dplyr::desc(.sd)) %>%
      dplyr::slice(1:floor(.1 * nrow(results))) %>%
      dplyr::sample_n(1)
  }
  results
}

dissim_sample <- function(ref_data, candidates, pset, max_n = Inf) {
  max_n <- min(max_n, nrow(candidates))
  candidates <- candidates[1:max_n, , drop = FALSE]
  all_data <- dplyr::bind_rows(ref_data, candidates)

  # Deal with any qualitative predictors by casting them to c(0,1)
  qual_info <- find_qual_param(pset)
  if (length(qual_info) > 0) {
    for (i in seq_along(qual_info)) {
      nm <- names(qual_info)[i]
      uniq <- sort(unique(all_data[[nm]]))
      all_data[[nm]] <- as.character(all_data[[nm]])
      all_data[[nm]] <- factor(all_data[[nm]], levels = uniq)
    }
  }
  all_data <- stats::model.matrix(~ . + 0, data = all_data)

  n_ref <- nrow(ref_data)
  n_all <- nrow(all_data)
  distances <- stats::dist(all_data)
  distances <- as.matrix(distances)
  distances <- distances[1:n_ref, (n_ref + 1):n_all]
  min_distances <- apply(distances, 2, function(x) min(x[x > 0]))
  max_ind <- which.max(min_distances)[1]
  max_ind
}
