# cannot finalize with recipe parameters

    Code
      mod_1 %>% tune_grid(rec_1, resamples = rs, grid = 3)
    Condition
      Error in `check_parameters()`:
      ! Some model parameters require finalization but there are recipe parameters that require tuning.
      i Please use `extract_parameter_set_dials()` to set parameter ranges manually and supply the output to the `param_info` argument.

# finalize tailors

    Code
      print(adj_1)
    Message
      
      -- tailor ----------------------------------------------------------------------
      A regression postprocessor with 1 adjustment:
      
      * Constrain numeric predictions to be between [2, ?].

---

    Code
      print(adj_2)
    Message
      
      -- tailor ----------------------------------------------------------------------
      A regression postprocessor with 1 adjustment:
      
      * Constrain numeric predictions to be between [2, 3].

---

    Code
      print(adj_3)
    Message
      
      -- tailor ----------------------------------------------------------------------
      A regression postprocessor with 1 adjustment:
      
      * Constrain numeric predictions to be between [2, 3].

---

    Code
      print(adj_4)
    Message
      
      -- tailor ----------------------------------------------------------------------
      A regression postprocessor with 1 adjustment:
      
      * Constrain numeric predictions to be between [?, ?].

---

    Code
      finalize_tailor(linear_reg(), tibble())
    Condition
      Error in `finalize_tailor()`:
      ! `x` should be a tailor, not a <linear_reg> object.

# finalize workflows with tailors

    Code
      print(wflow_1)
    Output
      == Workflow ====================================================================
      Preprocessor: Formula
      Model: linear_reg()
      Postprocessor: tailor
      
      -- Preprocessor ----------------------------------------------------------------
      y ~ .
      
      -- Model -----------------------------------------------------------------------
      Linear Regression Model Specification (regression)
      
      Computational engine: lm 
      
      
      -- Postprocessor ---------------------------------------------------------------
    Message
      
      -- tailor ----------------------------------------------------------------------
      A regression postprocessor with 1 adjustment:
      
      * Constrain numeric predictions to be between [2, ?].
    Output
      NA
      NA
      NA

---

    Code
      print(wflow_2)
    Output
      == Workflow ====================================================================
      Preprocessor: Formula
      Model: linear_reg()
      Postprocessor: tailor
      
      -- Preprocessor ----------------------------------------------------------------
      y ~ .
      
      -- Model -----------------------------------------------------------------------
      Linear Regression Model Specification (regression)
      
      Computational engine: lm 
      
      
      -- Postprocessor ---------------------------------------------------------------
    Message
      
      -- tailor ----------------------------------------------------------------------
      A regression postprocessor with 1 adjustment:
      
      * Constrain numeric predictions to be between [2, 3].
    Output
      NA
      NA
      NA

---

    Code
      print(wflow_3)
    Output
      == Workflow ====================================================================
      Preprocessor: Formula
      Model: linear_reg()
      Postprocessor: tailor
      
      -- Preprocessor ----------------------------------------------------------------
      y ~ .
      
      -- Model -----------------------------------------------------------------------
      Linear Regression Model Specification (regression)
      
      Computational engine: lm 
      
      
      -- Postprocessor ---------------------------------------------------------------
    Message
      
      -- tailor ----------------------------------------------------------------------
      A regression postprocessor with 1 adjustment:
      
      * Constrain numeric predictions to be between [2, 3].
    Output
      NA
      NA
      NA

---

    Code
      print(wflow_4)
    Output
      == Workflow ====================================================================
      Preprocessor: Formula
      Model: linear_reg()
      Postprocessor: tailor
      
      -- Preprocessor ----------------------------------------------------------------
      y ~ .
      
      -- Model -----------------------------------------------------------------------
      Linear Regression Model Specification (regression)
      
      Computational engine: lm 
      
      
      -- Postprocessor ---------------------------------------------------------------
    Message
      
      -- tailor ----------------------------------------------------------------------
      A regression postprocessor with 1 adjustment:
      
      * Constrain numeric predictions to be between [?, ?].
    Output
      NA
      NA
      NA

