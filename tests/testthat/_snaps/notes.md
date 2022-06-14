# showing notes

    Code
      res_roles <- role_bp_wflow %>% fit_resamples(rs)
    Message
      x Fold01: preprocessor 1/1, model 1/1 (predictions): Error:
      ! The following cols a...
      x Fold02: preprocessor 1/1, model 1/1 (predictions): Error:
      ! The following cols a...
      x Fold03: preprocessor 1/1, model 1/1 (predictions): Error:
      ! The following cols a...
      x Fold04: preprocessor 1/1, model 1/1 (predictions): Error:
      ! The following cols a...
      x Fold05: preprocessor 1/1, model 1/1 (predictions): Error:
      ! The following cols a...
      x Fold06: preprocessor 1/1, model 1/1 (predictions): Error:
      ! The following cols a...
      x Fold07: preprocessor 1/1, model 1/1 (predictions): Error:
      ! The following cols a...
      x Fold08: preprocessor 1/1, model 1/1 (predictions): Error:
      ! The following cols a...
      x Fold09: preprocessor 1/1, model 1/1 (predictions): Error:
      ! The following cols a...
      x Fold10: preprocessor 1/1, model 1/1 (predictions): Error:
      ! The following cols a...
    Condition
      Warning:
      All models failed. See the `.notes` column.

---

    Code
      show_notes(res_roles)
    Output
      unique notes:
      --------------------------------------------------------------------------------
      Error:
      ! The following cols are missing from `new_data`: date.
      i There are also non-standard recipe roles for the column(s).
      i See `?update_role` for more information on how use non-standard recipe roles during prediction.

---

    Code
      res_simple <- simple_wflow %>% fit_resamples(rs)
    Message
      ! Fold01: preprocessor 1/1, model 1/1 (predictions): prediction from a rank-defici...
      ! Fold02: preprocessor 1/1, model 1/1 (predictions): prediction from a rank-defici...
      ! Fold03: preprocessor 1/1, model 1/1 (predictions): prediction from a rank-defici...
      ! Fold04: preprocessor 1/1, model 1/1 (predictions): prediction from a rank-defici...
      ! Fold05: preprocessor 1/1, model 1/1 (predictions): prediction from a rank-defici...
      ! Fold06: preprocessor 1/1, model 1/1 (predictions): prediction from a rank-defici...
      ! Fold07: preprocessor 1/1, model 1/1 (predictions): prediction from a rank-defici...
      ! Fold08: preprocessor 1/1, model 1/1 (predictions): prediction from a rank-defici...
      ! Fold09: preprocessor 1/1, model 1/1 (predictions): prediction from a rank-defici...
      ! Fold10: preprocessor 1/1, model 1/1 (predictions): prediction from a rank-defici...

---

    Code
      show_notes(res_simple)
    Output
      unique notes:
      ------------------------------------------------------
      prediction from a rank-deficient fit may be misleading

---

    Code
      show_notes(res_clean)
    Output
      Great job! No notes to show.

