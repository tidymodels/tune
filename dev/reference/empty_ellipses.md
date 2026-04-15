# Get colors for tune text.

These are not intended for use by the general public.

## Usage

``` r
check_rset(x)

.check_grid(grid, workflow, pset = NULL, call = caller_env())

.needs_finalization(x, nms = character(0))

check_parameters(wflow, pset = NULL, data, grid_names = character(0))

.check_param_objects(pset)

check_workflow(x, ..., pset = NULL, check_dials = FALSE, call = caller_env())

check_metrics(x, object)

check_initial(
  x,
  pset,
  wflow,
  resamples,
  metrics,
  eval_time,
  ctrl,
  checks = "grid"
)

val_class_or_null(x, cls = "numeric", where = NULL)

val_class_and_single(x, cls = "numeric", where = NULL)

.config_key_from_metrics(x)

estimate_tune_results(x, ..., col_name = ".metrics")

.has_preprocessor(workflow)

.has_preprocessor_recipe(workflow)

.has_preprocessor_formula(workflow)

.has_preprocessor_variables(workflow)

.has_spec(workflow)

.set_workflow_spec(workflow, spec)

.set_workflow_recipe(workflow, recipe)

metrics_info(x)

new_iteration_results(
  x,
  parameters,
  metrics,
  eval_time,
  eval_time_target,
  outcomes = character(0),
  rset_info,
  workflow
)

.make_static(
  workflow,
  param_info,
  grid,
  metrics,
  eval_time,
  split_args,
  control,
  pkgs = "tune",
  strategy = "sequential",
  data = list(fit = NULL, pred = NULL, cal = NULL)
)

.get_data_subsets(wflow, split, split_args = NULL)

.get_config_key(grid, wflow)

.determine_pred_types(wflow, metrics)

.loop_over_all_stages(resamples, grid, static)

.loop_over_all_stages2(index, resamples, grid, static)

.update_model(grid, object, pset, step_id, nms, ..., source = "model_spec")

.update_recipe(grid, object, pset, step_id, nms, ...)

.update_parallel_over(control, resamples, grid)

loop_call(strategy, framework, opts)

get_tune_colors()

encode_set(x, pset, ..., as_matrix = FALSE)

.is_cataclysmic(x)

check_time(origin, limit)

pull_rset_attributes(x)

.set_workflow(workflow, control)

empty_ellipses(...)

is_recipe(x)

is_preprocessor(x)

is_workflow(x)

new_bare_tibble(x, ..., class = character())
```

## Arguments

- x:

  An object.

- workflow:

  The workflow used to fit the iteration results.

- pset:

  A `parameters` object.

- wflow:

  A `workflow` object.

- data:

  The training data.

- grid_names:

  A character vector of column names from the grid.

- ...:

  Other options

- check_dials:

  A logical for check for a NULL parameter object.

- object:

  **\[deprecated\]**

  A `workflow` object.

- resamples:

  An `rset` object.

- metrics:

  A metric set.

- eval_time:

  A numeric vector of time points where dynamic event time metrics
  should be computed (e.g. the time-dependent ROC curve, etc).

- ctrl:

  A `control_grid` object.

- cls:

  **\[deprecated\]** A character vector of possible classes

- where:

  **\[deprecated\]** A character string for the calling function.

- parameters:

  A `parameters` object.

- outcomes:

  A character vector of outcome names.

- rset_info:

  Attributes from an `rset` object.

- as_matrix:

  A logical for the return type.

- origin:

  The calculation start time.

- limit:

  The allowable time (in minutes).
