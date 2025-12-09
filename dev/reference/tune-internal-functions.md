# Internal functions used by other tidymodels packages

These are not to be meant to be invoked directly by users.

## Usage

``` r
forge_from_workflow(new_data, workflow)

finalize_workflow_preprocessor(workflow, grid_preprocessor)

.estimate_metrics(
  dat,
  metric,
  param_names,
  outcome_name,
  event_level,
  metrics_info = metrics_info(metrics)
)

.load_namespace(x)

initialize_catalog(control, env = rlang::caller_env(), workflow = NULL)

.catch_and_log(.expr, ..., bad_only = FALSE, notes, catalog = TRUE)
```

## Arguments

- new_data:

  A data frame or matrix of predictors to process.

- workflow:

  A workflow.

- grid_preprocessor:

  A tibble with parameter information.

- dat:

  A data set.

- metric:

  A metric set.

- param_names:

  A character vector of tuning parameter names.

- outcome_name:

  A character string for the column of `dat` that is the outcome.

- event_level:

  A logical passed from the control function.

- metrics_info:

  The output of `tune:::metrics_info(metrics)`—only included as an
  argument to allow for pre-computing.

- x:

  A character vector of package names.

- .expr:

  Code to execute.

- ...:

  Object to pass to the internal `update_printer()` function.

- bad_only:

  A logical for whether warnings and errors should be caught.

- notes:

  Character data to add to the logging.

- catalog:

  A logical passed to `update_printer()` giving whether the message is
  compatible with the issue cataloger. Defaults to `TRUE`. Updates that
  are always unique and do not represent a tuning "issue" can bypass the
  cataloger by setting `catalog = FALSE`.
