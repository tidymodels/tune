# predict classification - no submodels - no calibration

    Code
      tune:::predict_all_types(wflow_fit, static_both, submodel_grid = NULL,
        predictee = "potato")
    Condition
      Error in `tune:::predict_all_types()`:
      ! `predictee` must be one of "assessment" or "calibration", not "potato".

---

    Code
      tune:::predict_all_types(wflow_fit, static_bad, submodel_grid = NULL,
        predictee = "calibration")
    Condition
      Error:
      ! Calibration data were requested but not reserved.

# predict classification - with submodels - no calibration

    Code
      tune:::predict_all_types(wflow_fit, static_both, submodel_grid = five_neighbors,
        predictee = "potato")
    Condition
      Error in `tune:::predict_all_types()`:
      ! `predictee` must be one of "assessment" or "calibration", not "potato".

---

    Code
      tune:::predict_all_types(wflow_fit, static_bad, submodel_grid = five_neighbors,
        predictee = "calibration")
    Condition
      Error:
      ! Calibration data were requested but not reserved.

