# showing notes

    Code
      res_roles <- role_bp_wflow %>% fit_resamples(rs)
    Message
      > A | error:   The following required column is missing from `new_data`: date.
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.

---

    Code
      show_notes(res_roles)
    Output
      unique notes:
      ---------------------------------------------------------------
      The following required column is missing from `new_data`: date.

---

    Code
      res_simple <- simple_wflow %>% fit_resamples(rs)
    Message
      > A | warning: prediction from rank-deficient fit; consider predict(., rankdeficient="NA")

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
      > A | warning: glm.fit: algorithm did not converge
      > B | warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
      > C | warning: No control observations were detected in `truth` with control level 'class_2'.

---

    Code
      show_notes(fit_lr)
    Output
      unique notes:
      -----------------------------------
      glm.fit: algorithm did not converge
      -----------------------------------
      glm.fit: fitted probabilities numerically 0 or 1 occurred
      -----------------------------------
      No control observations were detected in `truth` with control level 'class_2'.

