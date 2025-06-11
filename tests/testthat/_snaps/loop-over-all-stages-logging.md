# preprocessor error doesn't stop grid

    Code
      res_fit <- melodie_grid(parsnip::nearest_neighbor("regression", "kknn",
        dist_power = tune()), Sale_Price ~ ., folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      > A | error: invalid type (list) for variable 'First_Flr_SF'
    Condition

# model error doesn't stop grid

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Condition

# prediction error doesn't stop grid

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Condition

# capturing error correctly in notes

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      > A | error: Error in `step_logging_helper()`:
    Condition

# capturing warning correctly in notes

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      > A | warning: testing warning

# doesn't capturing message in notes

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message

# captures extract errors

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE, extract = extract_error))

# captures kknn R errors

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Condition

# captures xgboost C errors

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Condition

# captures cli styled errors

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      > A | error: Error in `step_logging_helper()`:
    Condition

# emitter works with errors

    Code
      res_fit <- melodie_grid(wf_spec, folds, grid = 2, control = control_grid(
        allow_par = FALSE))
    Message
      > A | error: Error in `step_logging_helper()`:
    Condition

