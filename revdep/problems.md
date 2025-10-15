# autostats

<details>

* Version: 0.4.1
* GitHub: https://github.com/Harrison4192/autostats
* Source code: https://github.com/cran/autostats
* Date/Publication: 2024-06-04 09:44:44 UTC
* Number of recursive dependencies: 199

Run `revdepcheck::revdep_details(, "autostats")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ...
    ```
      ‘autostats.Rmd’ using ‘UTF-8’... failed
      ‘tidyXgboost.Rmd’ using ‘UTF-8’... OK
     ERROR
    Errors in running code in vignettes:
    when running code in ‘autostats.Rmd’
      ...
        model      metric mean_score std_err
    1 xgboost    accuracy      0.960 0.01363
    2 xgboost brier_class      0.035 0.01288
    3 xgboost     roc_auc      0.995 0.00406
    
    > iris %>% filter(Species != "setosa") %>% auto_variable_contributions(species_formula)
    
      When sourcing ‘autostats.R’:
    Error: object 'Species' not found
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Ckmeans.1d.dp’ ‘broom.mixed’ ‘igraph’
      All declared Imports should be used.
    ```

# modeltime.resample

<details>

* Version: 0.2.3
* GitHub: https://github.com/business-science/modeltime.resample
* Source code: https://github.com/cran/modeltime.resample
* Date/Publication: 2023-04-12 15:50:02 UTC
* Number of recursive dependencies: 234

Run `revdepcheck::revdep_details(, "modeltime.resample")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       22. │         └─tidyselect:::eval_c(expr, data_mask, context_mask)
       23. │           └─tidyselect:::reduce_sels(node, data_mask, context_mask, init = init)
       24. │             └─tidyselect:::walk_data_tree(new, data_mask, context_mask)
       25. │               └─tidyselect:::as_indices_sel_impl(...)
       26. │                 └─tidyselect:::as_indices_impl(...)
       27. │                   └─tidyselect:::chr_as_locations(x, vars, call = call, arg = arg)
       28. │                     └─vctrs::vec_as_location(...)
       29. └─vctrs (local) `<fn>`()
       30.   └─vctrs:::stop_subscript_oob(...)
       31.     └─vctrs:::stop_subscript(...)
       32.       └─rlang::abort(...)
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 6 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ...
    ```
      ‘getting-started.Rmd’ using ‘UTF-8’... failed
      ‘panel-data.Rmd’ using ‘UTF-8’... OK
     ERROR
    Errors in running code in vignettes:
    when running code in ‘getting-started.Rmd’
      ...
    2         2 <workflow> PROPHET                 <lgl [1]>        
    3         3 <workflow> GLMNET                  <lgl [1]>        
    
    > resamples_fitted %>% plot_modeltime_resamples(.point_size = 3, 
    +     .point_alpha = 0.8, .interactive = FALSE)
    
      When sourcing ‘getting-started.R’:
    Error: Can't select columns that don't exist.
    ✖ Column `id` doesn't exist.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘crayon’ ‘dials’ ‘glue’ ‘parsnip’
      All declared Imports should be used.
    ```

# postcard

<details>

* Version: 1.0.1
* GitHub: https://github.com/NovoNordisk-OpenSource/postcard
* Source code: https://github.com/cran/postcard
* Date/Publication: 2025-07-01 07:10:02 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::revdep_details(, "postcard")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ...
    ```
      ‘model-fit.Rmd’ using ‘UTF-8’... OK
      ‘postcard.Rmd’ using ‘UTF-8’... OK
      ‘prospective-power.Rmd’ using ‘UTF-8’... failed
     ERROR
    Errors in running code in vignettes:
    when running code in ‘prospective-power.Rmd’
      ...
    +     power_ancova <- sapply(n_from:n_to, FUN = function(n) power_gs(n = n, 
    +         variance = variance, r  .... [TRUNCATED] 
    
    > data_power <- dplyr::bind_rows(iterate_power(var_bound_ancova) %>% 
    +     dplyr::mutate(n_desired = samplesize_gs(variance = var_bound_ancova, 
    +    .... [TRUNCATED] 
    
      When sourcing ‘prospective-power.R’:
    Error: could not find function "%>%"
    Execution halted
    ```

