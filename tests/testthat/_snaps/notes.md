# showing notes

    Code
      res_roles <- role_bp_wflow %>% fit_resamples(rs)
    Message
      x Fold01: preprocessor 1/1, model 1/1 (predictions):
        Error in `step_date()`:
        ! The following required column is missing from `new_data`: date.
      x Fold02: preprocessor 1/1, model 1/1 (predictions):
        Error in `step_date()`:
        ! The following required column is missing from `new_data`: date.
      x Fold03: preprocessor 1/1, model 1/1 (predictions):
        Error in `step_date()`:
        ! The following required column is missing from `new_data`: date.
      x Fold04: preprocessor 1/1, model 1/1 (predictions):
        Error in `step_date()`:
        ! The following required column is missing from `new_data`: date.
      x Fold05: preprocessor 1/1, model 1/1 (predictions):
        Error in `step_date()`:
        ! The following required column is missing from `new_data`: date.
      x Fold06: preprocessor 1/1, model 1/1 (predictions):
        Error in `step_date()`:
        ! The following required column is missing from `new_data`: date.
      x Fold07: preprocessor 1/1, model 1/1 (predictions):
        Error in `step_date()`:
        ! The following required column is missing from `new_data`: date.
      x Fold08: preprocessor 1/1, model 1/1 (predictions):
        Error in `step_date()`:
        ! The following required column is missing from `new_data`: date.
      x Fold09: preprocessor 1/1, model 1/1 (predictions):
        Error in `step_date()`:
        ! The following required column is missing from `new_data`: date.
      x Fold10: preprocessor 1/1, model 1/1 (predictions):
        Error in `step_date()`:
        ! The following required column is missing from `new_data`: date.
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.

---

    Code
      show_notes(res_roles)
    Output
      unique notes:
      -----------------------------------------------------------------
      Error in `step_date()`:
      ! The following required column is missing from `new_data`: date.

---

    Code
      res_simple <- simple_wflow %>% fit_resamples(rs)
    Message
      ! Fold01: preprocessor 1/1, model 1/1 (predictions): prediction from rank-deficient fit; consider predict(., rankdeficient="NA")
      ! Fold02: preprocessor 1/1, model 1/1 (predictions): prediction from rank-deficient fit; consider predict(., rankdeficient="NA")
      ! Fold03: preprocessor 1/1, model 1/1 (predictions): prediction from rank-deficient fit; consider predict(., rankdeficient="NA")
      ! Fold04: preprocessor 1/1, model 1/1 (predictions): prediction from rank-deficient fit; consider predict(., rankdeficient="NA")
      ! Fold05: preprocessor 1/1, model 1/1 (predictions): prediction from rank-deficient fit; consider predict(., rankdeficient="NA")
      ! Fold06: preprocessor 1/1, model 1/1 (predictions): prediction from rank-deficient fit; consider predict(., rankdeficient="NA")
      ! Fold07: preprocessor 1/1, model 1/1 (predictions): prediction from rank-deficient fit; consider predict(., rankdeficient="NA")
      ! Fold08: preprocessor 1/1, model 1/1 (predictions): prediction from rank-deficient fit; consider predict(., rankdeficient="NA")
      ! Fold09: preprocessor 1/1, model 1/1 (predictions): prediction from rank-deficient fit; consider predict(., rankdeficient="NA")
      ! Fold10: preprocessor 1/1, model 1/1 (predictions): prediction from rank-deficient fit; consider predict(., rankdeficient="NA")

---

    Code
      show_notes(res_simple)
    Output
      unique notes:
      ---------------------------------------------------------------------------
      prediction from rank-deficient fit; consider predict(., rankdeficient="NA")

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
      ! Fold01: internal: No control observations were detected in `truth` with control level 'cla...
      ! Fold02: preprocessor 1/1, model 1/1: glm.fit: algorithm did not converge, glm.fit: fitted probabilities numer...
      ! Fold02: internal: No control observations were detected in `truth` with control level 'cla...
      ! Fold03: preprocessor 1/1, model 1/1: glm.fit: algorithm did not converge, glm.fit: fitted probabilities numer...
      ! Fold03: internal: No control observations were detected in `truth` with control level 'cla...
      ! Fold04: preprocessor 1/1, model 1/1: glm.fit: algorithm did not converge, glm.fit: fitted probabilities numer...
      ! Fold04: internal: No control observations were detected in `truth` with control level 'cla...
      ! Fold05: preprocessor 1/1, model 1/1: glm.fit: algorithm did not converge, glm.fit: fitted probabilities numer...
      ! Fold05: internal: No control observations were detected in `truth` with control level 'cla...
      ! Fold06: preprocessor 1/1, model 1/1: glm.fit: algorithm did not converge, glm.fit: fitted probabilities numer...
      ! Fold06: internal: No control observations were detected in `truth` with control level 'cla...
      ! Fold07: preprocessor 1/1, model 1/1: glm.fit: algorithm did not converge, glm.fit: fitted probabilities numer...
      ! Fold07: internal: No control observations were detected in `truth` with control level 'cla...
      ! Fold08: preprocessor 1/1, model 1/1: glm.fit: algorithm did not converge
      ! Fold09: preprocessor 1/1, model 1/1: glm.fit: algorithm did not converge, glm.fit: fitted probabilities numer...
      ! Fold09: internal: No control observations were detected in `truth` with control level 'cla...
      ! Fold10: preprocessor 1/1, model 1/1: glm.fit: algorithm did not converge, glm.fit: fitted probabilities numer...
      ! Fold10: internal: No control observations were detected in `truth` with control level 'cla...

---

    Code
      show_notes(fit_lr)
    Output
      unique notes:
      --------------------------------------------------------------------------------
      glm.fit: algorithm did not converge, glm.fit: fitted probabilities numerically 0 or 1 occurred
      --------------------------------------------------------------------------------
      No control observations were detected in `truth` with control level 'class_2'.
      --------------------------------------------------------------------------------
      glm.fit: algorithm did not converge

