# finetune

<details>

* Version: 1.1.0
* GitHub: https://github.com/tidymodels/finetune
* Source code: https://github.com/cran/finetune
* Date/Publication: 2023-04-19 07:40:02 UTC
* Number of recursive dependencies: 171

Run `revdepcheck::revdep_details(, "finetune")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-sa-decision.R:16:5'): simulated annealing decisions ────────────
      Error in `tune:::new_tune_results(., parameters = cart_param, outcomes = cart_outcomes, 
          metrics = cart_metrics, rset_info = cart_rset_info)`: argument "eval_time" is missing, with no default
      Backtrace:
    ...
          ▆
       1. ├─cart_search |> filter(.iter == iter_val) |> ... at test-sa-decision.R:16:5
       2. └─tune:::new_tune_results(...)
       3.   └─tune:::new_bare_tibble(...)
       4.     └─tibble::new_tibble(x, nrow = nrow(x), ..., class = class)
       5.       └─rlang::pairlist2(...)
      
      [ FAIL 1 | WARN 0 | SKIP 20 | PASS 107 ]
      Error: Test failures
      Execution halted
    ```

# tidyclust

<details>

* Version: 0.2.0
* GitHub: https://github.com/tidymodels/tidyclust
* Source code: https://github.com/cran/tidyclust
* Date/Publication: 2023-09-25 18:20:06 UTC
* Number of recursive dependencies: 168

Run `revdepcheck::revdep_details(, "tidyclust")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Error in `if (metric_info$direction == "maximize") {
          summary_res <- summary_res |> dplyr::arrange(dplyr::desc(mean))
      } else if (metric_info$direction == "minimize") {
          summary_res <- summary_res |> dplyr::arrange(mean)
      } else if (metric_info$direction == "zero") {
          summary_res <- summary_res |> dplyr::arrange(abs(mean))
      }`: argument is of length zero
      Backtrace:
          ▆
       1. └─testthat::expect_snapshot(tmp <- tune::show_best(res)) at test-tune_cluster.R:440:3
       2.   └─rlang::cnd_signal(state$error)
      
      [ FAIL 1 | WARN 0 | SKIP 47 | PASS 177 ]
      Error: Test failures
      Execution halted
    ```

# tidysdm

<details>

* Version: 0.9.3
* GitHub: https://github.com/EvolEcolGroup/tidysdm
* Source code: https://github.com/cran/tidysdm
* Date/Publication: 2024-01-17 20:50:02 UTC
* Number of recursive dependencies: 166

Run `revdepcheck::revdep_details(, "tidysdm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tidysdm-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot.simple_ensemble
    > ### Title: Plot the results of a simple ensemble
    > ### Aliases: autoplot.simple_ensemble
    > 
    > ### ** Examples
    > 
    > # we use the two_class_example from `workflowsets`
    ...
    2.26658549056941, 1.55070434710052, 2.61077822145814, 0.9766272632684,
    1.86637107997978, 0.675410781588896, 1.27855056115075, 1.39156478397907,
    1.03940782815086, 1.69628613309882, 1.32644435902627, 3.31674522338439,
    2.79572399056405, 3.37417921047442, 1.14087715338524, 1.56584397770581,
    1.74205067903299, 2.71615811524246, 1.97133695928657, [... truncated]
    Warning: Unknown or uninitialised column: `metric`.
    Error in if (!any(mtr_info$metric == metric)) { : 
      missing value where TRUE/FALSE needed
    Calls: |> ... add_member.tune_results -> <Anonymous> -> check_metric_in_tune_results
    Execution halted
    ```

*   checking tests ...
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─testthat::expect_warning(test_ens <- simple_ensemble() |> add_member(none_mars)) at test_simple_ensemble.R:14:3
        2. │ └─testthat:::expect_condition_matching(...)
        3. │   └─testthat:::quasi_capture(...)
        4. │     ├─testthat (local) .capture(...)
        5. │     │ └─base::withCallingHandlers(...)
    ...
        6. │     └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        7. ├─simple_ensemble() |> add_member(none_mars)
        8. ├─tidysdm::add_member(., none_mars)
        9. └─tidysdm:::add_member.tune_results(., none_mars)
       10.   └─(utils::getFromNamespace("choose_metric", "tune"))(metric, member)
       11.     └─tune::check_metric_in_tune_results(mtr_info, metric, call = call)
      
      [ FAIL 6 | WARN 10 | SKIP 0 | PASS 153 ]
      Error: Test failures
      Execution halted
    ```

