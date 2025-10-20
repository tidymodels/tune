# workflow size warning

    Code
      set.seed(1)
      warns <- fit_resamples(lm_wflow, resamples = vfold_cv(MTCARS), control = control_resamples(save_workflow = TRUE, workflow_size = 2))
    Message
      i The workflow being saved is large (2.7 MB). If this was not intentional,
      please set the control setting `save_workflow` to be `FALSE` or change the
      threshold for this warning (currently 2 MB) with the `workflow_size` argument.

