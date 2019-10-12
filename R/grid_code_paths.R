
tune_nothing <- function(rs, object, grid, perf, ctrl)  {
  B <- nrow(rs)

  `%op%` <- get_operator(ctrl$allow_par, object)

  lab_names <- names(labels(rs$splits[[1]]))

  results <-
    foreach::foreach(rs_iter = 1:B, .packages = "tune", .errorhandling = "pass") %op%
    iter_no_tune(rs_iter, rs, object, perf, ctrl)

  rs <- pull_metrics(rs, results, ctrl)
  rs <- pull_extracts(rs, results, ctrl)
  rs <- pull_predictions(rs, results, ctrl)

  rs
}

iter_no_tune <- function(rs_iter, rs, object, perf, ctrl) {
  load_pkgs(object)
  load_namespace(ctrl$pkgs)
  fit_ctrl <- parsnip::fit_control(verbosity = 0, catch = TRUE)

  split <- rs$splits[[rs_iter]]
  perf_est <- NULL
  extracted <- NULL
  pred_vals <- NULL

 # use fit.workflow and predict.workflow

  # list(.metrics = perf_est, .extracts = extracted, .predictions = pred_vals)

}
# ------------------------------------------------------------------------------

iter_rec_and_mod <- function(rs_iter, rs, grid, object, perf, ctrl) {
  load_pkgs(object)
  load_namespace(ctrl$pkgs)
  fit_ctrl <- parsnip::fit_control(verbosity = 0, catch = TRUE)

  perf_est <- NULL
  extracted <- NULL
  pred_vals <- NULL

  split <- rs$splits[[rs_iter]]

  model_param <-
    dials::parameters(object) %>%
    dplyr::filter(source == "model_spec") %>%
    dplyr::pull(id)
  rec_param <-
    dials::parameters(object) %>%
    dplyr::filter(source == "recipe") %>%
    dplyr::pull(id)

  if (tidyr_new_interface()) {
    rec_grid <- tidyr::nest(tibble::as_tibble(grid), data = dplyr::one_of(model_param))
  } else {
    rec_grid <- tidyr::nest(grid, !!!model_param)
  }

  # --------------------------------------------------------------------------

  num_rec <- nrow(rec_grid)
  for (rec_iter in 1:num_rec) {
    rec_msg <- paste0("recipe ", format(1:num_rec)[rec_iter], "/", num_rec)

    # Current recipe parameters only
    rec_grid_vals <-
      rec_grid %>%
      dplyr::slice(rec_iter) %>%
      dplyr::select(-data)
    tmp_rec <- catch_and_log(train_recipe(split, object, rec_grid_vals), ctrl, split, "recipe")

    # All model tune parameters associated with the current recipe parameters
    mod_grid_vals <-
      rec_grid %>%
      dplyr::slice(rec_iter) %>%
      dplyr::select(-one_of(rec_param)) %>%
      tidyr::unnest(cols = dplyr::one_of("data"))

    # Determine the _minimal_ number of models to fit in order to get
    # predictions on all models.
    mod_grid_vals <- get_wflow_model(object) %>% min_grid(mod_grid_vals)

    # ------------------------------------------------------------------------

    num_mod <- nrow(mod_grid_vals)
    for (mod_iter in 1:num_mod) {
      mod_msg <- paste0(rec_msg, ", model ", format(1:num_mod)[mod_iter], "/", num_mod)

      fixed_param <- mod_grid_vals %>% dplyr::slice(mod_iter) %>% dplyr::select(-.submodels)
      submd_param <- mod_grid_vals %>% dplyr::slice(mod_iter) %>% dplyr::select(.submodels)
      submd_param <- submd_param$.submodels[[1]]

      tmp_fit <-
        catch_and_log(
          train_model_from_recipe(object, tmp_rec, fixed_param, control = fit_ctrl),
          ctrl,
          split,
          mod_msg
        )

      # check for failure
      if (!inherits(tmp_fit$fit, "try-error")) {
        all_param <- dplyr::bind_cols(rec_grid_vals, mod_grid_vals[mod_iter, ])

        tmp_pred <-
          catch_and_log(
            predict_model_from_recipe(split, tmp_fit, tmp_rec, all_param, perf),
            ctrl,
            split,
            paste(mod_msg, "(predictions)"),
            bad_only = TRUE
          )

        perf_est <- append_metrics(perf_est, tmp_pred, object, perf, split)
        pred_vals <- append_predictions(pred_vals, tmp_pred, split, ctrl)

      }
      extracted <- append_extracts(extracted, tmp_rec, tmp_fit$fit, all_param, split, ctrl)
    } # end model loop

  } # end recipe loop

  list(.metrics = perf_est, .extracts = extracted, .predictions = pred_vals)
}

tune_rec_and_mod <- function(rs, grid, object, perf, ctrl) {
  B <- nrow(rs)

  `%op%` <- get_operator(ctrl$allow_par, object)

  lab_names <- names(labels(rs$splits[[1]]))

  results <-
    foreach::foreach(rs_iter = 1:B, .packages = "tune", .errorhandling = "pass") %op%
    iter_rec_and_mod(rs_iter, rs, grid, object, perf, ctrl)

  rs <- pull_metrics(rs, results, ctrl)
  rs <- pull_extracts(rs, results, ctrl)
  rs <- pull_predictions(rs, results, ctrl)

  rs
}

# ------------------------------------------------------------------------------

iter_rec <- function(rs_iter, rs, grid, object, perf, ctrl) {
  load_pkgs(object)
  load_namespace(ctrl$pkgs)
  fit_ctrl <- parsnip::fit_control(verbosity = 0, catch = TRUE)

  split <- rs$splits[[rs_iter]]
  perf_est <- NULL
  extracted <- NULL
  pred_vals <- NULL

  num_rec <- nrow(grid)

  for (param_iter in 1:num_rec) {
    param_vals <- grid[param_iter, ]
    rec_msg <- paste0("recipe ", format(1:num_rec)[param_iter], "/", num_rec)
    mod_msg <- paste0(rec_msg, ", model 1/1")

    tmp_rec <- catch_and_log(train_recipe(split, object, param_vals), ctrl, split, rec_msg)

    tmp_fit <-
      catch_and_log(
        train_model_from_recipe(object, tmp_rec, NULL, control = fit_ctrl),
        ctrl,
        split,
        mod_msg
      )


    # check for failure
    if (!inherits(tmp_fit$fit, "try-error")) {
      pred_msg <- paste(mod_msg, "(predictions)")
      tmp_pred <-
        catch_and_log(
          predict_model_from_recipe(split, tmp_fit, tmp_rec, param_vals, perf),
          ctrl,
          split,
          pred_msg,
          bad_only = TRUE
        )

      perf_est <- append_metrics(perf_est, tmp_pred, object, perf, split)
      pred_vals <- append_predictions(pred_vals, tmp_pred, split, ctrl)

    }

    extracted <- append_extracts(extracted, tmp_rec, tmp_fit$fit, grid[param_iter, ], split, ctrl)
  } # recipe parameters

  list(.metrics = perf_est, .extracts = extracted, .predictions = pred_vals)

}

tune_rec <- function(rs, grid, object, perf, ctrl) {
  B <- nrow(rs)

  `%op%` <- get_operator(ctrl$allow_par, object)

  results <-
    foreach::foreach(rs_iter = 1:B, .packages = "tune", .errorhandling = "pass") %op%
    iter_rec(rs_iter, rs, grid, object, perf, ctrl)

  rs <- pull_metrics(rs, results, ctrl)
  rs <- pull_extracts(rs, results, ctrl)
  rs <- pull_predictions(rs, results, ctrl)

  rs
}


# ------------------------------------------------------------------------------

tune_mod_with_recipe <- function(rs, grid, object, perf, ctrl) {
  B <- nrow(rs)

  `%op%` <- get_operator(ctrl$allow_par, object)

  results <-
    foreach::foreach(rs_iter = 1:B, .packages = "tune", .errorhandling = "pass") %op%
    iter_mod_with_recipe(rs_iter, rs, grid, object, perf, ctrl)

  rs <- pull_metrics(rs, results, ctrl)
  rs <- pull_extracts(rs, results, ctrl)
  rs <- pull_predictions(rs, results, ctrl)

  rs
}

iter_mod_with_recipe <- function(rs_iter, rs, grid, object, perf, ctrl) {
  load_pkgs(object)
  load_namespace(ctrl$pkgs)
  fit_ctrl <- parsnip::fit_control(verbosity = 0, catch = TRUE)
  split <- rs$splits[[rs_iter]]
  perf_est <- NULL
  extracted <- NULL
  pred_vals <- NULL

  # ----------------------------------------------------------------------------

  tune_log(ctrl, split, "recipe", type = "go")
  tmp_rec <- catcher(train_recipe(split, object, NULL))
  log_problems(ctrl, split, tmp_rec, loc = "recipe")
  tmp_rec <- tmp_rec$res

  # Determine the _minimal_ number of models to fit in order to get
  # predictions on all models.
  mod_grid_vals <- get_wflow_model(object) %>% min_grid(grid)

  num_mod <- nrow(mod_grid_vals)
  for (mod_iter in 1:num_mod) {
    mod_msg <- paste0("model ", format(1:num_mod)[mod_iter], "/", num_mod)

    tune_log(ctrl, split, mod_msg, type = "go")
    tmp_fit <-
      catcher(train_model_from_recipe(object, tmp_rec, mod_grid_vals[mod_iter,],
                                      control = fit_ctrl))
    log_problems(ctrl, split, tmp_fit, loc = mod_msg)
    tmp_fit <- tmp_fit$res

    # check for failure
    if (!inherits(tmp_fit$fit, "try-error")) {

      pred_msg <- paste(mod_msg, "(predictions)")
      tmp_pred <- catcher(
        predict_model_from_recipe(
          split, tmp_fit, tmp_rec, mod_grid_vals[mod_iter, ], perf
        )
      )
      log_problems(ctrl, split, tmp_pred, loc = pred_msg, bad_only = TRUE)
      tmp_pred <- tmp_pred$res

      perf_est  <- append_metrics(perf_est, tmp_pred, object, perf, split)
      pred_vals <- append_predictions(pred_vals, tmp_pred, split, ctrl)

    }

    extracted <- append_extracts(extracted, tmp_rec, tmp_fit$fit, mod_grid_vals[mod_iter, ], split, ctrl)
  } # end model loop

  list(.metrics = perf_est, .extracts = extracted, .predictions = pred_vals)

}

# ------------------------------------------------------------------------------


tune_mod_with_formula <- function(rs, grid, object, perf, ctrl) {
  B <- nrow(rs)

  `%op%` <- get_operator(ctrl$allow_par, object)

  results <-
    foreach::foreach(rs_iter = 1:B, .packages = "tune", .errorhandling = "pass") %op%
    iter_mod_with_formula(rs_iter, rs, grid, object, perf, ctrl)

  rs <- pull_metrics(rs, results, ctrl)
  rs <- pull_extracts(rs, results, ctrl)
  rs <- pull_predictions(rs, results, ctrl)

  rs
}

iter_mod_with_formula <- function(rs_iter, rs, grid, object, perf, ctrl) {
  load_pkgs(object)
  load_namespace(ctrl$pkgs)
  fit_ctrl <- parsnip::fit_control(verbosity = 0, catch = TRUE)
  split <- rs$splits[[rs_iter]]
  perf_est <- NULL
  extracted <- NULL
  pred_vals <- NULL

  # ----------------------------------------------------------------------------

  tmp_df <- catch_and_log(exec_formula(split, object), ctrl, split, "formula")
  tmp_trms <- tmp_df$terms
  tmp_df <- tmp_df[c("x", "y")]

  # Determine the _minimal_ number of models to fit in order to get
  # predictions on all models.
  mod_grid_vals <- get_wflow_model(object) %>% min_grid(grid)

  num_mod <- nrow(mod_grid_vals)
  for (mod_iter in 1:num_mod) {
    param_val <- mod_grid_vals[mod_iter, ]
    mod_msg <- paste0("model ", format(1:num_mod)[mod_iter], "/", num_mod)

    tmp_fit <-
      catch_and_log(
        train_model_from_df(object, tmp_df, param_val, control = fit_ctrl),
        ctrl,
        split,
        mod_msg
      )

    # check for failure
    if (!inherits(tmp_fit$fit, "try-error")) {

      pred_msg <- paste(mod_msg, "(predictions)")

      tmp_pred <-
        catch_and_log(
          predict_model_from_terms(split, tmp_fit, tmp_trms, param_val, perf),
          ctrl,
          split,
          mod_msg
        )

      perf_est  <- append_metrics(perf_est, tmp_pred, object, perf, split)
      pred_vals <- append_predictions(pred_vals, tmp_pred, split, ctrl)

    }

    extracted <- append_extracts(extracted, NULL, tmp_fit$fit, param_val, split, ctrl)
  } # end model loop

  list(.metrics = perf_est, .extracts = extracted, .predictions = pred_vals)

}

