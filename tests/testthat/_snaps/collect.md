# `collect_predictions()` errors informatively if there is no `.predictions` column

    Code
      collect_predictions(lm_splines %>% dplyr::select(-.predictions))
    Condition
      Error in `collect_predictions()`:
      ! The .predictions column does not exist. Please refit with the control argument `save_pred = TRUE` to save predictions.

# `collect_predictions()` errors informatively applied to unsupported class

    Code
      collect_predictions(lm(mpg ~ disp, mtcars))
    Condition
      Error in `collect_predictions()`:
      ! No `collect_predictions()` exists for a <lm> object.

# bad filter grid

    Code
      collect_predictions(svm_tune, parameters = tibble(wrong = "value"))
    Condition
      Error in `filter_predictions()`:
      ! `parameters` should only have columns: "cost value".

# collecting notes - fit_resamples

    Code
      lm_splines <- fit_resamples(lin_mod, mpg ~ ., flds)
    Message
      ! Bootstrap1: preprocessor 1/1, model 1/1 (predictions): prediction from rank-deficient fit; consider predict(., rankdeficient="NA")
      ! Bootstrap2: preprocessor 1/1, model 1/1 (predictions): prediction from rank-deficient fit; consider predict(., rankdeficient="NA")

---

    Code
      lm_splines
    Output
      # Resampling results
      # Bootstrap sampling 
      # A tibble: 2 x 4
        splits          id         .metrics         .notes          
        <list>          <chr>      <list>           <list>          
      1 <split [32/13]> Bootstrap1 <tibble [2 x 4]> <tibble [1 x 4]>
      2 <split [32/17]> Bootstrap2 <tibble [2 x 4]> <tibble [1 x 4]>
      
      There were issues with some computations:
      
        - Warning(s) x2: prediction from rank-deficient fit; consider predict(., rankdefic...
      
      Run `show_notes(.Last.tune.result)` for more information.

# collecting notes - last_fit

    Code
      lst <- last_fit(lin_mod, mpg ~ ., split)
    Message
      ! train/test split: preprocessor 1/1, model 1/1 (predictions): prediction from rank-deficient fit; consider predict(., rankdeficient="NA")

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
      
        - Warning(s) x1: prediction from rank-deficient fit; consider predict(., rankdefic...
      
      Run `show_notes(.Last.tune.result)` for more information.

# `collect_notes()` errors informatively applied to unsupported class

    Code
      collect_notes(lm(mpg ~ disp, mtcars))
    Condition
      Error in `collect_notes()`:
      ! No `collect_notes()` exists for a <lm> object.

# collecting extracted objects - fit_resamples

    Code
      collect_extracts(res_fit)
    Output
      # A tibble: 5 x 3
        id         .extracts .config             
        <chr>      <list>    <chr>               
      1 Bootstrap1 <lm>      Preprocessor1_Model1
      2 Bootstrap2 <lm>      Preprocessor1_Model1
      3 Bootstrap3 <lm>      Preprocessor1_Model1
      4 Bootstrap4 <lm>      Preprocessor1_Model1
      5 Bootstrap5 <lm>      Preprocessor1_Model1

---

    Code
      collect_extracts(res_nothing)
    Condition
      Error in `collect_extracts()`:
      ! The `.extracts` column does not exist.
      i Please supply a control object (`?tune::control_grid()`) with a non-`NULL` `extract` argument during resample fitting.

---

    Code
      collect_extracts(res_error)
    Output
      # A tibble: 5 x 3
        id         .extracts      .config             
        <chr>      <list>         <chr>               
      1 Bootstrap1 <try-errr [1]> Preprocessor1_Model1
      2 Bootstrap2 <try-errr [1]> Preprocessor1_Model1
      3 Bootstrap3 <try-errr [1]> Preprocessor1_Model1
      4 Bootstrap4 <try-errr [1]> Preprocessor1_Model1
      5 Bootstrap5 <try-errr [1]> Preprocessor1_Model1

# `collect_extracts()` errors informatively applied to unsupported class

    Code
      collect_extracts(lm(mpg ~ disp, mtcars))
    Condition
      Error in `collect_extracts()`:
      ! No `collect_extracts()` exists for a <lm> object.

# `collect_metrics()` errors informatively applied to unsupported class

    Code
      collect_metrics(lm(mpg ~ disp, mtcars))
    Condition
      Error in `collect_metrics()`:
      ! No `collect_metrics()` exists for a <lm> object.

# `collect_metrics(type)` errors informatively with bad input

    Code
      collect_metrics(ames_grid_search, type = "boop")
    Condition
      Error in `collect_metrics()`:
      ! `type` must be one of "long" or "wide", not "boop".

---

    Code
      collect_metrics(ames_grid_search, type = NULL)
    Condition
      Error in `collect_metrics()`:
      ! `type` must be a string or character vector.

