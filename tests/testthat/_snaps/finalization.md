# cannot finalize with recipe parameters

    Code
      mod_1 %>% tune_grid(rec_1, resamples = rs, grid = 3)
    Condition
      Error in `check_parameters()`:
      ! Some model parameters require finalization but there are recipe parameters that require tuning.
      i Please use `extract_parameter_set_dials()` to set parameter ranges manually and supply the output to the `param_info` argument.

