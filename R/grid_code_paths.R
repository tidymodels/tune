# TODOs
# - Flatten tune_rec and tune_mod so that parallel is more workable?
# - have an on.exit to capture current results?
# - check min_grid for cases where id != name in model


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
    pull(id)
  rec_param <-
    param_set(object) %>%
    dplyr::filter(source == "recipe") %>%
    pull(id)

  rec_grid <- tidyr::nest(grid, !!!model_param)

  # ----------------------------------------------------------------------------

  for (rs_iter in 1:B) {
    perf_est <- NULL

    # --------------------------------------------------------------------------

    for (rec_iter in 1:nrow(rec_grid)) {

      # current recipe parameters only
      rec_grid_vals <-
        rec_grid %>%
        dplyr::slice(rec_iter) %>%
        dplyr::select(-data)

      messenger(ctrl, rs$splits[[rs_iter]], "recipe")
      tmp_rec <- train_recipe(rs$splits[[rs_iter]], object, rec_grid_vals)
      messenger(ctrl, rs$splits[[rs_iter]], "recipe", fini = TRUE)

      y_names <- outcome_names(tmp_rec)

      # All model tune parameters associated with the current recipe
      # parameters
      mod_grid_vals <-
        rec_grid %>%
        dplyr::slice(rec_iter) %>%
        dplyr::select(-one_of(rec_param)) %>%
        unnest()

      # Determine the _minimal_ number of models to fit in order to get
      # predictions on all models.
      mod_grid_vals <- parsnip::min_grid(object$fit$model$model, mod_grid_vals)

      # ------------------------------------------------------------------------

      for (mod_iter in 1:nrow(mod_grid_vals)) {

        fixed_param <- mod_grid_vals %>% slice(mod_iter) %>% select(-.submodels)
        submd_param <- mod_grid_vals %>% slice(mod_iter) %>% select(.submodels)
        submd_param <- submd_param$.submodels[[1]]

        messenger(ctrl, rs$splits[[rs_iter]], "model")
        tmp_fit <-
          train_model_from_recipe(object, tmp_rec, fixed_param, control = fit_ctrl)

        # check for failure
        if (!inherits(tmp_fit$fit, "try-error")) {
          messenger(ctrl, rs$splits[[rs_iter]], "model", fini = TRUE)

          all_param <- bind_cols(rec_grid_vals, mod_grid_vals[mod_iter, ])

          tmp_pred <-
            try(
              predict_model_from_recipe(
                rs$splits[[rs_iter]], tmp_fit, tmp_rec, all_param
              ),
              silent = TRUE
            )

          tmp_est <- estimate_perf(tmp_pred, perf, object)

          perf_est  <- dplyr::bind_rows(perf_est, tmp_est)

        } else {
          # Failed model
          messenger(ctrl, rs$splits[[rs_iter]], "model", fini = TRUE, cool = FALSE)
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

    for (param_iter in 1:nrow(grid)) {

      messenger(ctrl, rs$splits[[rs_iter]], "recipe")
      tmp_rec <- train_recipe(rs$splits[[rs_iter]], object, grid[param_iter, ])
      messenger(ctrl, rs$splits[[rs_iter]], "recipe", fini = TRUE)
      y_names <- outcome_names(tmp_rec)

      messenger(ctrl, rs$splits[[rs_iter]], "model")
      tmp_fit <-
        train_model_from_recipe(object, tmp_rec, NULL, control = fit_ctrl)

      # check for failure
      if (!inherits(tmp_fit$fit, "try-error")) {
        messenger(ctrl, rs$splits[[rs_iter]], "model", fini = TRUE)

        tmp_pred <-
          predict_model_from_recipe(
            rs$splits[[rs_iter]], tmp_fit, tmp_rec, grid[param_iter, ]
          )

        tmp_est <- estimate_perf(tmp_pred, perf, object)

        if (param_iter == 1) {
          perf_est <- tmp_est
        } else {
          perf_est  <- dplyr::bind_rows(perf_est, tmp_est)
        }

      } else {
        # Failed model
        messenger(ctrl, rs$splits[[rs_iter]], "model", fini = TRUE, cool = FALSE)
        tmp_est <- empty_perf
      }
    }
    rs$.metrics[[rs_iter]] <- perf_est
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

    messenger(ctrl, rs$splits[[rs_iter]], "recipe")
    tmp_rec <- train_recipe(rs$splits[[rs_iter]], object, NULL)
    messenger(ctrl, rs$splits[[rs_iter]], "recipe", fini = TRUE)
    y_names <- outcome_names(tmp_rec)

    # Determine the _minimal_ number of models to fit in order to get
    # predictions on all models.
    mod_grid_vals <- parsnip::min_grid(object$fit$model$model, grid)

    for (mod_iter in 1:nrow(mod_grid_vals)) {

      messenger(ctrl, rs$splits[[rs_iter]], "model")
      tmp_fit <-
        train_model_from_recipe(object, tmp_rec, mod_grid_vals[mod_iter,], control = fit_ctrl)


      # check for failure
      if (!inherits(tmp_fit$fit, "try-error")) {

        messenger(ctrl, rs$splits[[rs_iter]], "model", fini = TRUE)

        tmp_pred <-
          predict_model_from_recipe(
            rs$splits[[rs_iter]], tmp_fit, tmp_rec, mod_grid_vals[mod_iter, ]
          )

        tmp_est <- estimate_perf(tmp_pred, perf, object)


      } else {
        # Failed model
        messenger(ctrl, rs$splits[[rs_iter]], "model", fini = TRUE, cool = FALSE)
        cat(tmp_fit$fit, "\n")

        tmp_est <- NULL
      }
      if (mod_iter == 1) {
        perf_est <- tmp_est
      } else {
        perf_est  <- dplyr::bind_rows(perf_est, tmp_est)
      }
    } # end model loop
    rs$.metrics[[rs_iter]] <- perf_est
  } # end resample loop

  rs
}


tune_mod_with_formula <- function(rs, grid, object, perf, ctrl) {
  stop("to-do")
}
