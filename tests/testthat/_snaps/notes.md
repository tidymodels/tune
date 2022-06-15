# showing notes

    Code
      res_roles <- role_bp_wflow %>% fit_resamples(rs)
    Message
      x Fold01: preprocessor 1/1, model 1/1 (predictions):
        Error:
        ! The following cols are missing from `new_data`: date.
        i There are also non-standard recipe roles for the column(s).
        i See `?update_role` for more information on how use non-standard reci...
      x Fold02: preprocessor 1/1, model 1/1 (predictions):
        Error:
        ! The following cols are missing from `new_data`: date.
        i There are also non-standard recipe roles for the column(s).
        i See `?update_role` for more information on how use non-standard reci...
      x Fold03: preprocessor 1/1, model 1/1 (predictions):
        Error:
        ! The following cols are missing from `new_data`: date.
        i There are also non-standard recipe roles for the column(s).
        i See `?update_role` for more information on how use non-standard reci...
      x Fold04: preprocessor 1/1, model 1/1 (predictions):
        Error:
        ! The following cols are missing from `new_data`: date.
        i There are also non-standard recipe roles for the column(s).
        i See `?update_role` for more information on how use non-standard reci...
      x Fold05: preprocessor 1/1, model 1/1 (predictions):
        Error:
        ! The following cols are missing from `new_data`: date.
        i There are also non-standard recipe roles for the column(s).
        i See `?update_role` for more information on how use non-standard reci...
      x Fold06: preprocessor 1/1, model 1/1 (predictions):
        Error:
        ! The following cols are missing from `new_data`: date.
        i There are also non-standard recipe roles for the column(s).
        i See `?update_role` for more information on how use non-standard reci...
      x Fold07: preprocessor 1/1, model 1/1 (predictions):
        Error:
        ! The following cols are missing from `new_data`: date.
        i There are also non-standard recipe roles for the column(s).
        i See `?update_role` for more information on how use non-standard reci...
      x Fold08: preprocessor 1/1, model 1/1 (predictions):
        Error:
        ! The following cols are missing from `new_data`: date.
        i There are also non-standard recipe roles for the column(s).
        i See `?update_role` for more information on how use non-standard reci...
      x Fold09: preprocessor 1/1, model 1/1 (predictions):
        Error:
        ! The following cols are missing from `new_data`: date.
        i There are also non-standard recipe roles for the column(s).
        i See `?update_role` for more information on how use non-standard reci...
      x Fold10: preprocessor 1/1, model 1/1 (predictions):
        Error:
        ! The following cols are missing from `new_data`: date.
        i There are also non-standard recipe roles for the column(s).
        i See `?update_role` for more information on how use non-standard reci...
    Condition
      Warning:
      All models failed. Run `show_notes(.Last.tune.result)` for more information.

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
      show_notes(res_clean)
    Output
      Great job! No notes to show.

