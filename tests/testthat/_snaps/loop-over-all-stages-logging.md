# preprocessor error doesn't stop grid

    Code
      res_fit <- tune_grid(parsnip::nearest_neighbor("regression", "kknn",
        dist_power = tune()), Sale_Price ~ ., folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      > A | error:   invalid type (list) for variable 'First_Flr_SF'
    Condition

# model error doesn't stop grid

    Code
      res_fit <- tune_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      > A | error:   invalid type (list) for variable 'First_Flr_SF'
    Condition

# prediction error doesn't stop grid

    Code
      res_fit <- tune_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      > A | error:   Assigned data `.ind` must be compatible with existing data.
    Condition

# capturing error correctly in notes

    Code
      res_fit <- tune_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      > A | error:   Error in `step_logging_helper()`:
    Condition

# capturing warning correctly in notes

    Code
      res_fit <- tune_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      > A | warning: testing warning

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
      > A | error:   extract error

# captures kknn R errors

    Code
      res_fit <- tune_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      > A | error:   NA/NaN/Inf in foreign function call (arg 1)
    Condition

# captures xgboost C errors

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
      > A | error:   Error in `step_logging_helper()`:
    Condition

# emitter works with errors

    Code
      res_fit <- tune_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      > A | error:   Error in `step_logging_helper()`:
    Condition

