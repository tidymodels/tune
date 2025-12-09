# Get colors for tune text.

These are not intended for use by the general public.

## Usage

``` r
check_rset(x)

check_parameters(wflow, pset = NULL, data, grid_names = character(0))

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

get_tune_colors()

encode_set(x, pset, ..., as_matrix = FALSE)

check_time(origin, limit)

pull_rset_attributes(x)

empty_ellipses(...)

is_recipe(x)

is_preprocessor(x)

is_workflow(x)
```

## Arguments

- x:

  An object.

- wflow:

  A `workflow` object.

- pset:

  A `parameters` object.

- data:

  The training data.

- grid_names:

  A character vector of column names from the grid.

- ...:

  Other options

- check_dials:

  A logical for check for a NULL parameter object.

- object:

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

  A character vector of possible classes

- where:

  A character string for the calling function.

- parameters:

  A `parameters` object.

- outcomes:

  A character vector of outcome names.

- rset_info:

  Attributes from an `rset` object.

- workflow:

  The workflow used to fit the iteration results.

- as_matrix:

  A logical for the return type.

- origin:

  The calculation start time.

- limit:

  The allowable time (in minutes).
