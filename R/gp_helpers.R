# Determine any qualitative parameters and their ranges

find_qual_param <- function(pset) {
	is_qual <- map_lgl(pset$object, ~inherits(.x, "qual_param"))
	if (!any(is_qual)) {
		return(list())
	}

	pset <- pset[is_qual, ]

	possible_lvl <- purrr::map(pset$object, ~.x$values)
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
		kernels[[kern_count]] <- k_FactorKernel(
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
	  # withr::set seed
	  loo_res <- summary(x)
	  # TODO maybe capture these?
	  loo_bad <- loo_res$coverage95LOO < 0.1
	  loo_rsq <- loo_res$r.squaredLOO
	} else {
	  loo_bad <- TRUE
	  loo_rsq <- 0.0
	}
	# convergence?
	list(use = !loo_bad | model_fail | loo_rsq > 0.1, rsq = loo_rsq)
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
  # TODO make these scaled
  for (i in seq_along(qual_info)) {
    nm <- names(qual_info)[i]
    normalized[[nm]] <- dat[[nm]]
  }
  if (any(names(dat) == "mean")) {
    normalized$.outcome <- dat$mean
  }
  normalized
}

# ------------------------------------------------------------------------------

fit_gp <- function(
	dat,
	pset,
	metric,
	eval_time = NULL,
	control,
	previous = NULL,
	...
) {
	dat <- dat %>% dplyr::filter(.metric == metric)

	if (!is.null(eval_time)) {
		dat <- dat %>% dplyr::filter(.eval_time == eval_time)
	}

	dat <- dat %>%
		# check_gp_data() %>%
		dplyr::select(dplyr::all_of(pset$id), mean)

	qual_info <- find_qual_param(pset)

	normalized <- partial_encode(dat, pset)

	gp_kernel <- make_kernel(pset, qual_info)

	# if (nrow(x) <= ncol(x) + 1 && nrow(x) > 0) {
	#   msg <-
	#     paste(
	#       "The Gaussian process model is being fit using ", ncol(x),
	#       "features but only has", nrow(x), "data points to do so. This may cause",
	#       "errors or a poor model fit."
	#     )
	#   message_wrap(msg, prefix = "!", color_text = get_tune_colors()$message$warning)
	# }

	if (!is.null(previous)) {
	 if (!previous$use) {
	   previous <- NULL
	 }
	}

	if (is.null(previous)) {
	  # TODO withr::with_seed
	  set.seed(114)
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
	} else {
	  new_val <- normalized %>% dplyr::slice_tail(n = 1)
	  new_x <- as.matrix(new_val[, pset$id])
	  new_y <- new_val$.outcome

		gp_fit <- try(previous$fit$update(new_x, new_y), silent = TRUE)
	}

	new_check <- check_gp(gp_fit)

	list(fit = gp_fit, use = new_check$use, rsq = new_check$rsq)
}
