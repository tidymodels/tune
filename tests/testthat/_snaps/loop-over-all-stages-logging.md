# preprocessor error doesn't stop grid

    Code
      res_fit <- tune_grid(parsnip::nearest_neighbor("regression", "kknn",
        dist_power = tune()), Sale_Price ~ ., folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
    Condition

# model error doesn't stop grid

    Code
      res_fit <- tune_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
    Condition

# prediction error doesn't stop grid

    Code
      res_fit <- tune_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      x Existing data has 0 rows.
      x Assigned data has 1465 rows.
      ! Can't recycle input of size 1465 to size 0.
    Condition

# capturing error correctly in notes

    Code
      res_fit <- tune_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      ! testing error
    Condition

# capturing warning correctly in notes

    Code
      res_fit <- tune_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message

# doesn't capturing message in notes

    Code
      res_fit <- tune_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message

# captures extract errors

    Code
      res_fit <- tune_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE, extract = extract_error))
    Message

# captures kknn R errors

    Code
      res_fit <- tune_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
    Condition

# captures cli styled errors

    Code
      res_fit <- tune_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      ! testing error
    Condition

# emitter works with errors

    Code
      res_fit <- tune_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      ! testing error
    Condition

