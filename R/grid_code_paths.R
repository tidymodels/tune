
tune_nothing <- function() {
  stop("No tuning parameters were given.", call. = FALSE)
}

tune_rec_and_mod <- function(rs, grid, object, perf, ctrl) {
  B <- nrow(rs)
  fit_ctrl <- parsnip::fit_control(verbosity = 0, catch = TRUE)
  param_names <- names(grid)
  model_param <-
    param_set(object) %>%
    dplyr::filter(source == "model_spec") %>%
    dplyr::pull(id)
  rec_param <-
    param_set(object) %>%
    dplyr::filter(source == "recipe") %>%
    dplyr::pull(id)

  rec_grid <- tidyr::nest(grid, !!!model_param)

  # ----------------------------------------------------------------------------

  for (rs_iter in 1:B) {
    perf_est <- NULL
    split <- rs$splits[[rs_iter]]

    # --------------------------------------------------------------------------

    num_rec <- nrow(rec_grid)
    for (rec_iter in 1:num_rec) {
      rec_msg <- paste0("recipe ", format(1:num_rec)[rec_iter], "/", num_rec)

      # current recipe parameters only
      rec_grid_vals <-
        rec_grid %>%
        dplyr::slice(rec_iter) %>%
        dplyr::select(-data)

      grid_msg(ctrl, split, rec_msg)
      tmp_rec <- train_recipe(split, object, rec_grid_vals)
      grid_msg(ctrl, split, rec_msg, fini = TRUE)

      # All model tune parameters associated with the current recipe
      # parameters
      mod_grid_vals <-
        rec_grid %>%
        dplyr::slice(rec_iter) %>%
        dplyr::select(-one_of(rec_param)) %>%
        tidyr::unnest()

      # Determine the _minimal_ number of models to fit in order to get
      # predictions on all models.
      mod_grid_vals <- parsnip::min_grid(object$fit$model$model, mod_grid_vals)

      # ------------------------------------------------------------------------

      num_mod <- nrow(mod_grid_vals)
      for (mod_iter in 1:num_mod) {
        mod_msg <- paste0(rec_msg, ", model ", format(1:num_mod)[mod_iter], "/", num_mod)

        fixed_param <- mod_grid_vals %>% dplyr::slice(mod_iter) %>% dplyr::select(-.submodels)
        submd_param <- mod_grid_vals %>% dplyr::slice(mod_iter) %>% dplyr::select(.submodels)
        submd_param <- submd_param$.submodels[[1]]

        grid_msg(ctrl, split, mod_msg)
        tmp_fit <-
          train_model_from_recipe(object, tmp_rec, fixed_param, control = fit_ctrl)

        # check for failure
        if (!inherits(tmp_fit$fit, "try-error")) {
          grid_msg(ctrl, split, mod_msg, fini = TRUE)

          all_param <- dplyr::bind_cols(rec_grid_vals, mod_grid_vals[mod_iter, ])

          tmp_pred <-
            try(
              predict_model_from_recipe(
                split, tmp_fit, tmp_rec, all_param
              ),
              silent = TRUE
            )

          tmp_est <- estimate_perf(tmp_pred, perf, object)

          perf_est <- dplyr::bind_rows(perf_est, tmp_est)

        } else {
          # Failed model
          grid_msg(ctrl, split, mod_msg, fini = TRUE, cool = FALSE)
          cat(tmp_fit$fit, "\n")
        }

      } # end model loop

      if (!is.null(perf_est)) {
        rs$.metrics[[rs_iter]] <- perf_est
      }

    } # end recipe loop

  } # end resample loop

  rs
}

# ------------------------------------------------------------------------------

tune_rec <- function(rs, grid, object, perf, ctrl) {
  B <- nrow(rs)
  fit_ctrl <- parsnip::fit_control(verbosity = 0, catch = TRUE)
  param_names <- names(grid)

  # ----------------------------------------------------------------------------

  for (rs_iter in 1:B) {
    split <- rs$splits[[rs_iter]]
    perf_est <- NULL

    num_rec <- nrow(grid)
    for (param_iter in 1:num_rec) {
      rec_msg <- paste0("recipe ", format(1:num_rec)[param_iter], "/", num_rec)

      grid_msg(ctrl, split, rec_msg)
      tmp_rec <- train_recipe(split, object, grid[param_iter, ])
      grid_msg(ctrl, split, rec_msg, fini = TRUE)

      grid_msg(ctrl, split, paste0(rec_msg, ", model 1/1"))
      tmp_fit <-
        train_model_from_recipe(object, tmp_rec, NULL, control = fit_ctrl)

      # check for failure
      if (!inherits(tmp_fit$fit, "try-error")) {
        grid_msg(ctrl, split, paste0(rec_msg, ", model 1/1"), fini = TRUE)

        tmp_pred <-
          predict_model_from_recipe(
            split, tmp_fit, tmp_rec, grid[param_iter, ]
          )

        tmp_est <- estimate_perf(tmp_pred, perf, object)

        perf_est <- dplyr::bind_rows(perf_est, tmp_est)

      } else {
        # Failed model
        grid_msg(ctrl, split, paste0(rec_msg, ", model 1/1"),
                  fini = TRUE, cool = FALSE)
      }
    }
    if (!is.null(perf_est)) {
      rs$.metrics[[rs_iter]] <- perf_est
    }
  }

  rs
}

# ------------------------------------------------------------------------------

tune_mod_with_recipe <- function(rs, grid, object, perf, ctrl) {
  B <- nrow(rs)
  fit_ctrl <- parsnip::fit_control(verbosity = 0, catch = TRUE)
  param_names <- names(grid)

  # ----------------------------------------------------------------------------

  for (rs_iter in 1:B) {
    split <- rs$splits[[rs_iter]]
    perf_est <- NULL

    grid_msg(ctrl, split, "recipe 1/1")
    tmp_rec <- train_recipe(split, object, NULL)
    grid_msg(ctrl, split, "recipe", fini = TRUE)
    y_names <- outcome_names(tmp_rec)

    # Determine the _minimal_ number of models to fit in order to get
    # predictions on all models.
    mod_grid_vals <- parsnip::min_grid(object$fit$model$model, grid)

    num_mod <- nrow(mod_grid_vals)
    for (mod_iter in 1:num_mod) {
      mod_msg <- paste0("recipe 1/1, model ", format(1:num_mod)[mod_iter], "/", num_mod)

      grid_msg(ctrl, split, mod_msg)
      tmp_fit <-
        train_model_from_recipe(object, tmp_rec, mod_grid_vals[mod_iter,], control = fit_ctrl)


      # check for failure
      if (!inherits(tmp_fit$fit, "try-error")) {

        grid_msg(ctrl, split, mod_msg, fini = TRUE)

        tmp_pred <-
          predict_model_from_recipe(
            split, tmp_fit, tmp_rec, mod_grid_vals[mod_iter, ]
          )

        tmp_est <- estimate_perf(tmp_pred, perf, object)

        perf_est <- dplyr::bind_rows(perf_est, tmp_est)
      } else {
        # Failed model
        grid_msg(ctrl, split, mod_msg, fini = TRUE, cool = FALSE)
        cat(tmp_fit$fit, "\n")
      }

    } # end model loop

    if (!is.null(perf_est)) {
      rs$.metrics[[rs_iter]] <- perf_est
    }

  } # end resample loop

  rs
}


tune_mod_with_formula <- function(rs, grid, object, perf, ctrl) {
  stop("to-do")
}
