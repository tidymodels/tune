# showing notes

    Code
      res_roles <- role_bp_wflow %>% fit_resamples(rs)
    Message
      x Fold01: preprocessor 1/1, model 1/1 (predictions):
        Error in `step_date()`:
        ! The following required column is missing from `new_data` in step 'st...
      x Fold02: preprocessor 1/1, model 1/1 (predictions):
        Error in `step_date()`:
        ! The following required column is missing from `new_data` in step 'st...
      x Fold03: preprocessor 1/1, model 1/1 (predictions):
        Error in `step_date()`:
        ! The following required column is missing from `new_data` in step 'st...
      x Fold04: preprocessor 1/1, model 1/1 (predictions):
        Error in `step_date()`:
        ! The following required column is missing from `new_data` in step 'st...
      x Fold05: preprocessor 1/1, model 1/1 (predictions):
        Error in `step_date()`:
        ! The following required column is missing from `new_data` in step 'st...
      x Fold06: preprocessor 1/1, model 1/1 (predictions):
        Error in `step_date()`:
        ! The following required column is missing from `new_data` in step 'st...
      x Fold07: preprocessor 1/1, model 1/1 (predictions):
        Error in `step_date()`:
        ! The following required column is missing from `new_data` in step 'st...
      x Fold08: preprocessor 1/1, model 1/1 (predictions):
        Error in `step_date()`:
        ! The following required column is missing from `new_data` in step 'st...
      x Fold09: preprocessor 1/1, model 1/1 (predictions):
        Error in `step_date()`:
        ! The following required column is missing from `new_data` in step 'st...
      x Fold10: preprocessor 1/1, model 1/1 (predictions):
        Error in `step_date()`:
        ! The following required column is missing from `new_data` in step 'st...
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.

---

    Code
      show_notes(res_roles)
    Output
      unique notes:
      --------------------------------------------------------------------------------
      Error in `step_date()`:
      ! The following required column is missing from `new_data` in step 'step_date': date.

---

    Code
      res_simple <- simple_wflow %>% fit_resamples(rs)
    Message
      ! Fold01: preprocessor 1/1, model 1/1 (predictions): prediction from a rank-deficient fit may be misleading
      ! Fold02: preprocessor 1/1, model 1/1 (predictions): prediction from a rank-deficient fit may be misleading
      ! Fold03: preprocessor 1/1, model 1/1 (predictions): prediction from a rank-deficient fit may be misleading
      ! Fold04: preprocessor 1/1, model 1/1 (predictions): prediction from a rank-deficient fit may be misleading
      ! Fold05: preprocessor 1/1, model 1/1 (predictions): prediction from a rank-deficient fit may be misleading
      ! Fold06: preprocessor 1/1, model 1/1 (predictions): prediction from a rank-deficient fit may be misleading
      ! Fold07: preprocessor 1/1, model 1/1 (predictions): prediction from a rank-deficient fit may be misleading
      ! Fold08: preprocessor 1/1, model 1/1 (predictions): prediction from a rank-deficient fit may be misleading
      ! Fold09: preprocessor 1/1, model 1/1 (predictions): prediction from a rank-deficient fit may be misleading
      ! Fold10: preprocessor 1/1, model 1/1 (predictions): prediction from a rank-deficient fit may be misleading

---

    Code
      show_notes(res_simple)
    Output
      unique notes:
      ------------------------------------------------------
      prediction from a rank-deficient fit may be misleading

---

    Code
      show_notes(.Last.tune.result)
    Output
      Great job! No notes to show.

---

    Code
      fit_lr <- parsnip::logistic_reg() %>% fit_resamples(class ~ ., rs)
    Message
      ! Fold01: preprocessor 1/1, model 1/1: glm.fit: algorithm did not converge, glm.fit: fitted probabilities numer...
      ! Fold01: internal:
        There was 1 warning in `dplyr::summarise()`.
        i In argument: `.estimate = metric_fn(...)`.
        Caused by warning:
        ! No control observations were detected in `truth` with control level ...
      ! Fold02: preprocessor 1/1, model 1/1: glm.fit: algorithm did not converge, glm.fit: fitted probabilities numer...
      ! Fold02: internal:
        There was 1 warning in `dplyr::summarise()`.
        i In argument: `.estimate = metric_fn(...)`.
        Caused by warning:
        ! No control observations were detected in `truth` with control level ...
      ! Fold03: preprocessor 1/1, model 1/1: glm.fit: algorithm did not converge, glm.fit: fitted probabilities numer...
      ! Fold03: internal:
        There was 1 warning in `dplyr::summarise()`.
        i In argument: `.estimate = metric_fn(...)`.
        Caused by warning:
        ! No control observations were detected in `truth` with control level ...
      ! Fold04: preprocessor 1/1, model 1/1: glm.fit: algorithm did not converge, glm.fit: fitted probabilities numer...
      ! Fold04: internal:
        There was 1 warning in `dplyr::summarise()`.
        i In argument: `.estimate = metric_fn(...)`.
        Caused by warning:
        ! No control observations were detected in `truth` with control level ...
      ! Fold05: preprocessor 1/1, model 1/1: glm.fit: algorithm did not converge, glm.fit: fitted probabilities numer...
      ! Fold05: internal:
        There was 1 warning in `dplyr::summarise()`.
        i In argument: `.estimate = metric_fn(...)`.
        Caused by warning:
        ! No control observations were detected in `truth` with control level ...
      ! Fold06: preprocessor 1/1, model 1/1: glm.fit: algorithm did not converge, glm.fit: fitted probabilities numer...
      ! Fold06: internal:
        There was 1 warning in `dplyr::summarise()`.
        i In argument: `.estimate = metric_fn(...)`.
        Caused by warning:
        ! No control observations were detected in `truth` with control level ...
      ! Fold07: preprocessor 1/1, model 1/1: glm.fit: algorithm did not converge, glm.fit: fitted probabilities numer...
      ! Fold07: internal:
        There was 1 warning in `dplyr::summarise()`.
        i In argument: `.estimate = metric_fn(...)`.
        Caused by warning:
        ! No control observations were detected in `truth` with control level ...
      ! Fold08: preprocessor 1/1, model 1/1: glm.fit: algorithm did not converge
      ! Fold09: preprocessor 1/1, model 1/1: glm.fit: algorithm did not converge, glm.fit: fitted probabilities numer...
      ! Fold09: internal:
        There was 1 warning in `dplyr::summarise()`.
        i In argument: `.estimate = metric_fn(...)`.
        Caused by warning:
        ! No control observations were detected in `truth` with control level ...
      ! Fold10: preprocessor 1/1, model 1/1: glm.fit: algorithm did not converge, glm.fit: fitted probabilities numer...
      ! Fold10: internal:
        There was 1 warning in `dplyr::summarise()`.
        i In argument: `.estimate = metric_fn(...)`.
        Caused by warning:
        ! No control observations were detected in `truth` with control level ...

---

    Code
      show_notes(fit_lr)
    Output
      unique notes:
      --------------------------------------------------------------------------------
      glm.fit: algorithm did not converge, glm.fit: fitted probabilities numerically 0 or 1 occurred
      --------------------------------------------------------------------------------
      There was 1 warning in `dplyr::summarise()`.
      i In argument: `.estimate = metric_fn(...)`.
      Caused by warning:
      ! No control observations were detected in `truth` with control level 'class_2'.
      --------------------------------------------------------------------------------
      glm.fit: algorithm did not converge

