# `collect_predictions()` errors informatively if there is no `.predictions` column

    Code
      collect_predictions(lm_splines %>% dplyr::select(-.predictions))
    Condition
      Error in `collect_predictions()`:
      ! The `.predictions` column does not exist. Refit with the control argument `save_pred = TRUE` to save predictions.

# bad filter grid

    Code
      collect_predictions(svm_tune, parameters = tibble(wrong = "value"))
    Condition
      Error in `filter_predictions()`:
      ! `parameters` should only have columns: 'cost value'

# collecting notes - fit_resamples

    Code
      lm_splines <- fit_resamples(lin_mod, mpg ~ ., flds)
    Message
      ! Bootstrap1: preprocessor 1/1, model 1/1 (predictions): prediction from a rank-deficient fit may be misleading
      ! Bootstrap2: preprocessor 1/1, model 1/1 (predictions): prediction from a rank-deficient fit may be misleading

---

    Code
      lm_splines
    Output
      # Resampling results
      # Bootstrap sampling 
      # A tibble: 2 x 4
        splits          id         .metrics         .notes          
        <list>          <chr>      <list>           <list>          
      1 <split [32/13]> Bootstrap1 <tibble [2 x 4]> <tibble [1 x 3]>
      2 <split [32/17]> Bootstrap2 <tibble [2 x 4]> <tibble [1 x 3]>
      
      There were issues with some computations:
      
        - Warning(s) x2: prediction from a rank-deficient fit may be misleading
      
      Run `show_notes(.Last.tune.result)` for more information.

# collecting notes - last_fit

    Code
      lst <- last_fit(lin_mod, mpg ~ ., split)
    Message
      ! train/test split: preprocessor 1/1, model 1/1 (predictions): prediction from a rank-deficient fit may be misleading

---

    Code
      lst
    Output
      # Resampling results
      # Manual resampling 
      # A tibble: 1 x 6
        splits         id               .metrics .notes   .predictions     .workflow 
        <list>         <chr>            <list>   <list>   <list>           <list>    
      1 <split [24/8]> train/test split <tibble> <tibble> <tibble [8 x 4]> <workflow>
      
      There were issues with some computations:
      
        - Warning(s) x1: prediction from a rank-deficient fit may be misleading
      
      Run `show_notes(.Last.tune.result)` for more information.

