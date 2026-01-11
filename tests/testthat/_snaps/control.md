# workflow size warning

    Code
      set.seed(1)
      warns <- fit_resamples(lm_wflow, resamples = vfold_cv(MTCARS), control = control_resamples(save_workflow = TRUE, workflow_size = 2))
    Message
      i The workflow being saved is large (2.7 MB). If this was not intentional,
      please set the control setting `save_workflow` to be `FALSE` or change the
      threshold for this warning (currently 2 MB) with the `workflow_size` argument.

# control object print methods

    Code
      control_grid()
    Message
      Grid/resamples control object
        `verbose`: FALSE
        `allow_par`: TRUE
        `extract`: NULL
        `save_pred`: FALSE
        `pkgs`: NULL
        `save_workflow`: FALSE
        `event_level`: "first"
        `parallel_over`: NULL
        `backend_options`: NULL
        `workflow_size`: 100

---

    Code
      control_grid(verbose = TRUE, save_pred = TRUE)
    Message
      Grid/resamples control object
        `verbose`: TRUE
        `allow_par`: TRUE
        `extract`: NULL
        `save_pred`: TRUE
        `pkgs`: NULL
        `save_workflow`: FALSE
        `event_level`: "first"
        `parallel_over`: NULL
        `backend_options`: NULL
        `workflow_size`: 100

---

    Code
      control_grid(pkgs = c("pkg1", "pkg2"), extract = I)
    Message
      Grid/resamples control object
        `verbose`: FALSE
        `allow_par`: TRUE
        `extract`: <function>
        `save_pred`: FALSE
        `pkgs`: "pkg1" and "pkg2"
        `save_workflow`: FALSE
        `event_level`: "first"
        `parallel_over`: NULL
        `backend_options`: NULL
        `workflow_size`: 100

---

    Code
      control_bayes()
    Message
      Bayes control object
        `verbose`: FALSE
        `verbose_iter`: FALSE
        `allow_par`: TRUE
        `no_improve`: 10
        `uncertain`: Inf
        `seed`: 51663
        `extract`: NULL
        `save_pred`: FALSE
        `time_limit`: NA
        `pkgs`: NULL
        `save_workflow`: FALSE
        `save_gp_scoring`: FALSE
        `event_level`: "first"
        `parallel_over`: NULL
        `backend_options`: NULL
        `workflow_size`: 100

---

    Code
      control_bayes(verbose_iter = TRUE, no_improve = 5, save_gp_scoring = TRUE)
    Message
      Bayes control object
        `verbose`: FALSE
        `verbose_iter`: TRUE
        `allow_par`: TRUE
        `no_improve`: 5
        `uncertain`: Inf
        `seed`: 2986
        `extract`: NULL
        `save_pred`: FALSE
        `time_limit`: NA
        `pkgs`: NULL
        `save_workflow`: FALSE
        `save_gp_scoring`: TRUE
        `event_level`: "first"
        `parallel_over`: NULL
        `backend_options`: NULL
        `workflow_size`: 100

---

    Code
      control_last_fit()
    Message
      Last fit control object
        `verbose`: FALSE
        `allow_par`: FALSE
        `extract`: <function>
        `save_pred`: TRUE
        `pkgs`: NULL
        `save_workflow`: FALSE
        `event_level`: "first"
        `parallel_over`: NULL
        `backend_options`: NULL
        `workflow_size`: 100

---

    Code
      control_last_fit(verbose = TRUE)
    Message
      Last fit control object
        `verbose`: TRUE
        `allow_par`: FALSE
        `extract`: <function>
        `save_pred`: TRUE
        `pkgs`: NULL
        `save_workflow`: FALSE
        `event_level`: "first"
        `parallel_over`: NULL
        `backend_options`: NULL
        `workflow_size`: 100

# control object print methods with default = TRUE

    Code
      print(control_grid(verbose = TRUE, pkgs = c("pkg1", "pkg2")), default = TRUE)
    Message
      Grid/resamples control object
        `verbose`: TRUE
        `pkgs`: "pkg1" and "pkg2"

---

    Code
      print(control_bayes(verbose_iter = TRUE, no_improve = 5), default = TRUE)
    Message
      Bayes control object
        `verbose_iter`: TRUE
        `no_improve`: 5
        `seed`: 13797

---

    Code
      print(control_last_fit(verbose = TRUE), default = TRUE)
    Message
      Last fit control object
        `verbose`: TRUE
        `extract`: <function>

