# ellipses with last_fit

    Code
      linear_reg() %>% set_engine("lm") %>% last_fit(f, split, something = "wrong")
    Condition
      Warning:
      The `...` are not used in this function but one or more objects were passed: 'something'
    Output
      # Resampling results
      # Manual resampling 
      # A tibble: 1 x 6
        splits         id               .metrics .notes   .predictions     .workflow 
        <list>         <chr>            <list>   <list>   <list>           <list>    
      1 <split [24/8]> train/test split <tibble> <tibble> <tibble [8 x 4]> <workflow>

# argument order gives errors for recipe/formula

    Code
      last_fit(rec, lin_mod, split)
    Condition
      Error in `last_fit()`:
      ! The first argument to [last_fit()] should be either a model or workflow.

---

    Code
      last_fit(f, lin_mod, split)
    Condition
      Error in `last_fit()`:
      ! The first argument to [last_fit()] should be either a model or workflow.

# `last_fit()` when objects need tuning

    2 arguments have been tagged for tuning in these components: model_spec and recipe. 
    Please use one of the tuning functions (e.g. `tune_grid()`) to optimize them.

---

    1 argument has been tagged for tuning in this component: model_spec. 
    Please use one of the tuning functions (e.g. `tune_grid()`) to optimize them.

---

    1 argument has been tagged for tuning in this component: recipe. 
    Please use one of the tuning functions (e.g. `tune_grid()`) to optimize them.

