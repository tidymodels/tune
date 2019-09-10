
tune_nothing <- function() {
  stop("No tuning parameters were given.", call. = FALSE)
}

# ------------------------------------------------------------------------------

iter_rec_and_mod <- function(rs_iter, rs, grid, object, perf, ctrl) {

  fit_ctrl <- parsnip::fit_control(verbosity = 0, catch = TRUE)

  perf_est <- NULL
  extracted <- NULL
  pred_vals <- NULL

  split <- rs$splits[[rs_iter]]

  model_param <-
    param_set(object) %>%
    dplyr::filter(source == "model_spec") %>%
    dplyr::pull(id)
  rec_param <-
    param_set(object) %>%
    dplyr::filter(source == "recipe") %>%
    dplyr::pull(id)

  if (tidyr_new_interface()) {
    rec_grid <- tidyr::nest(grid, data = dplyr::one_of(model_param))
  } else {
    rec_grid <- tidyr::nest(grid, !!!model_param)
  }

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
      tidyr::unnest(cols = dplyr::one_of("data"))

    # Determine the _minimal_ number of models to fit in order to get
    # predictions on all models.
    mod_grid_vals <- min_grid(object$fit$model$model, mod_grid_vals)

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
              split, tmp_fit, tmp_rec, all_param, perf
            ),
            silent = TRUE
          )

        perf_est <- append_metrics(perf_est, tmp_pred, object, perf, split)
        pred_vals <- append_predictions(pred_vals, tmp_pred, split, ctrl)

      } else {
        # Failed model
        grid_msg(ctrl, split, mod_msg, fini = TRUE, cool = FALSE)
        cat(tmp_fit$fit, "\n")
      }

      tmp_extr <-
        all_param %>%
        dplyr::select(-.submodels) %>%
        dplyr::bind_cols(labels(split)) %>%
        mutate(
          .extract = list(
            extract_details(
              list(recipe = tmp_rec, model = tmp_fit$fit),
              ctrl$extract
            )
          )
        )

      extracted <- dplyr::bind_rows(extracted, tmp_extr)
    } # end model loop

  } # end recipe loop

  list(.metrics = perf_est, .extract = extracted, .predictions = pred_vals)
}

tune_rec_and_mod <- function(rs, grid, object, perf, ctrl) {
  B <- nrow(rs)

  `%op%` <- get_operator(ctrl$allow_par)

  all_pkg <- c(fe_pkg_list, mod_pkgs(object))

  lab_names <- names(labels(rs$splits[[1]]))

  results <-
    foreach::foreach(rs_iter = 1:B, .packages = all_pkg, .errorhandling = "pass") %op%
    iter_rec_and_mod(rs_iter, rs, grid, object, perf, ctrl)

  rs <- pull_metrics(rs, results)
  rs <- pull_extracts(rs, results, ctrl)
  rs <- pull_predictions(rs, results, ctrl)

  rs
}

# ------------------------------------------------------------------------------

iter_rec <- function(rs_iter, rs, grid, object, perf, ctrl) {
  fit_ctrl <- parsnip::fit_control(verbosity = 0, catch = TRUE)

  split <- rs$splits[[rs_iter]]
  perf_est <- NULL
  extracted <- NULL
  pred_vals <- NULL

  num_rec <- nrow(grid)

  for (param_iter in 1:num_rec) {
    rec_msg <- paste0("recipe ", format(1:num_rec)[param_iter], "/", num_rec)

    grid_msg(ctrl, split, rec_msg)
    tmp_rec <- train_recipe(split, object, grid[param_iter, ])
    grid_msg(ctrl, split, rec_msg, fini = TRUE)

    grid_msg(ctrl, split, paste0(rec_msg, ", model"))
    tmp_fit <-
      train_model_from_recipe(object, tmp_rec, NULL, control = fit_ctrl)

    # check for failure
    if (!inherits(tmp_fit$fit, "try-error")) {
      grid_msg(ctrl, split, paste0(rec_msg, ", model"), fini = TRUE)

      tmp_pred <-
        predict_model_from_recipe(
          split, tmp_fit, tmp_rec, grid[param_iter, ], perf
        )

      perf_est <- append_metrics(perf_est, tmp_pred, object, perf, split)
      pred_vals <- append_predictions(pred_vals, tmp_pred, split, ctrl)

    } else {
      # Failed model
      grid_msg(ctrl, split, paste0(rec_msg, ", model"),
               fini = TRUE, cool = FALSE)
    }

    tmp_extr <-
      grid[param_iter, ] %>%
      dplyr::bind_cols(labels(split)) %>%
      mutate(
        .extract = list(
          extract_details(
            list(recipe = tmp_rec, model = tmp_fit$fit),
            ctrl$extract
          )
        )
      )

    extracted <- dplyr::bind_rows(extracted, tmp_extr)

  } # recipe parameters

  list(.metrics = perf_est, .extract = extracted, .predictions = pred_vals)

}

tune_rec <- function(rs, grid, object, perf, ctrl) {
  B <- nrow(rs)

  `%op%` <- get_operator(ctrl$allow_par)

  all_pkg <- c(fe_pkg_list, mod_pkgs(object))

  results <-
    foreach::foreach(rs_iter = 1:B, .packages = all_pkg, .errorhandling = "pass") %op%
    iter_rec(rs_iter, rs, grid, object, perf, ctrl)

  rs <- pull_metrics(rs, results)
  rs <- pull_extracts(rs, results, ctrl)
  rs <- pull_predictions(rs, results, ctrl)

  rs
}


# ------------------------------------------------------------------------------

tune_mod_with_recipe <- function(rs, grid, object, perf, ctrl) {
  B <- nrow(rs)

  `%op%` <- get_operator(ctrl$allow_par)

  all_pkg <- c(fe_pkg_list, mod_pkgs(object))

  results <-
    foreach::foreach(rs_iter = 1:B, .packages = all_pkg, .errorhandling = "pass") %op%
    iter_mod_with_recipe(rs_iter, rs, grid, object, perf, ctrl)

  rs <- pull_metrics(rs, results)
  rs <- pull_extracts(rs, results, ctrl)
  rs <- pull_predictions(rs, results, ctrl)

  rs
}

iter_mod_with_recipe <- function(rs_iter, rs, grid, object, perf, ctrl) {
  fit_ctrl <- parsnip::fit_control(verbosity = 0, catch = TRUE)
  split <- rs$splits[[rs_iter]]
  perf_est <- NULL
  extracted <- NULL
  pred_vals <- NULL

  grid_msg(ctrl, split, "recipe")
  tmp_rec <- train_recipe(split, object, NULL)
  grid_msg(ctrl, split, "recipe", fini = TRUE)
  y_names <- outcome_names(tmp_rec)

  # Determine the _minimal_ number of models to fit in order to get
  # predictions on all models.
  mod_grid_vals <- min_grid(object$fit$model$model, grid)

  num_mod <- nrow(mod_grid_vals)
  for (mod_iter in 1:num_mod) {
    mod_msg <- paste0("model ", format(1:num_mod)[mod_iter], "/", num_mod)

    grid_msg(ctrl, split, mod_msg)
    tmp_fit <-
      train_model_from_recipe(object, tmp_rec, mod_grid_vals[mod_iter,], control = fit_ctrl)

    # check for failure
    if (!inherits(tmp_fit$fit, "try-error")) {

      grid_msg(ctrl, split, mod_msg, fini = TRUE)

      tmp_pred <-
        predict_model_from_recipe(
          split, tmp_fit, tmp_rec, mod_grid_vals[mod_iter, ], perf
        )

      perf_est  <- append_metrics(perf_est, tmp_pred, object, perf, split)
      pred_vals <- append_predictions(pred_vals, tmp_pred, split, ctrl)

    } else {
      # Failed model
      grid_msg(ctrl, split, mod_msg, fini = TRUE, cool = FALSE)
      cat(tmp_fit$fit, "\n")
    }

    tmp_extr <-
      mod_grid_vals[mod_iter, ] %>%
      dplyr::select(-.submodels) %>%
      dplyr::bind_cols(labels(split)) %>%
      mutate(
        .extract = list(
          extract_details(
            list(recipe = tmp_rec, model = tmp_fit$fit),
            ctrl$extract
          )
        )
      )

    extracted <- dplyr::bind_rows(extracted, tmp_extr)

  } # end model loop

  list(.metrics = perf_est, .extract = extracted, .predictions = pred_vals)

}

# ------------------------------------------------------------------------------


tune_mod_with_formula <- function(rs, grid, object, perf, ctrl) {
  stop("to-do")
}
