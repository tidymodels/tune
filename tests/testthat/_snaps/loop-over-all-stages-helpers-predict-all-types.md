# process_prediction_data errors when recipe drops rows

    Code
      tune:::process_prediction_data(wflow_fit, static)
    Condition
      Error in `tune:::process_prediction_data()`:
      ! Some assessment set rows are not available at prediction time.
      i Consider using `skip = TRUE` on any recipe steps that remove rows to avoid calling them on the assessment set.

# process_prediction_data errors when non-recipe preprocessor drops rows

    Code
      tune:::process_prediction_data(wflow_fit, static)
    Condition
      Error in `tune:::process_prediction_data()`:
      ! Some assessment set rows are not available at prediction time.
      i Did your preprocessing steps filter or remove rows?

