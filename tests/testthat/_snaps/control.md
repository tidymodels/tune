# workflow size warning

    Code
      set.seed(1)
      warns <- fit_resamples(lm_wflow, resamples = vfold_cv(MTCARS), control = control_resamples(
        save_workflow = TRUE, workflow_size = 2))
    Message
      i The workflow being saved contains a recipe, which is 2.7 Mb in i memory. If
      this was not intentional, please set the control setting i `save_workflow =
      FALSE`.

