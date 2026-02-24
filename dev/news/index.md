# Changelog

## tune (development version)

- In
  [`tune_grid()`](https://tune.tidymodels.org/dev/reference/tune_grid.md),
  a bug was fixed that caused inefficiency where preprocessing steps
  were applied to data being predicted several times (redundantly). We
  now execute that operation once.
  ([\#1101](https://github.com/tidymodels/tune/issues/1101))

- Prepends a period to the name, and exports the following functions.
  This is to improve integration with other packages:

  - `check_grid()` (e.g., now named
    [`.check_grid()`](https://tune.tidymodels.org/dev/reference/empty_ellipses.md))
  - `determine_pred_types()`
  - `get_config_key()`
  - `loop_over_all_stages()`
  - `update_parallel_over()`
  - `get_data_subsets()`

- When calculating resampling estimates, we can now use a weighted mean
  based on the number of rows in the assessment set. You can opt-in to
  this using the new
  [`add_resample_weights()`](https://tune.tidymodels.org/dev/reference/add_resample_weights.md)
  function. See
  [`?calculate_resample_weights`](https://tune.tidymodels.org/dev/reference/calculate_resample_weights.md)
  ([\#990](https://github.com/tidymodels/tune/issues/990))

- The warning threshold when check the size of a workflow is now a
  parameter to the control functions and has a new default of 100MB.
  ([\#914](https://github.com/tidymodels/tune/issues/914))

- A bug was fixed where `NULL` results generated during simulated
  annealing would cause errors when logging.

- Fixed a bug for cases where we tune a grid without a model parameter
  but with a postprocessing parameter
  ([\#1119](https://github.com/tidymodels/tune/issues/1119))

### Breaking Changes

- The Gaussian process model package was changed from to because the
  former is no longer actively maintained. There are some differences:

  - Fit diagnostics are computed and reported. If the fit quality is
    poor, an “uncertainty sample” that is furthest away from the
    existing data is used as the new candidate.
  - The GP no longer uses binary indicators for qualitative predictors.
    Instead, a “categorical kernel” is used for those parameter columns.
    Fewer starting values are required with this change.
  - For numeric predictors, the Matern 3/2 kernel is always used.

## tune 2.0.1

CRAN release: 2025-10-17

- Fixed a bug where
  [`int_pctl()`](https://rsample.tidymodels.org/reference/int_pctl.html)
  wouldn’t work on
  [`last_fit()`](https://tune.tidymodels.org/dev/reference/last_fit.md)
  outcomes when future parallelism was enabled.
  ([\#1099](https://github.com/tidymodels/tune/issues/1099))

## tune 2.0.0

CRAN release: 2025-09-01

### Changes to `tune_grid()`.

- A major rewrite/refactor of the underlying code that runs
  [`tune_grid()`](https://tune.tidymodels.org/dev/reference/tune_grid.md).
  This was an upgrade to add postprocessing and to modernize our
  parallel processing infrastructure.

- The pattern of `.config` values has changed.

  - For grid search, it changes from `Preprocessor{num}_Model{num}` to
    `pre{num}_mod{num}_post{num}`. The numbers include a zero when that
    element was static. For example, a value of `pre0_mod3_post4` means
    no preprocessors were tuned and the model and postprocessor(s) had
    at least three and four candidates, respectively.
  - For iterative search, the pattern is not `iter{num}` instead of
    `Iter{num}` and the numbers are now zero padded to sort better. For
    example, if there between 10 and 99 iterations, the first `.config`
    value is now `iter01` instead of `Iter1`.

- The package will now log a backtrace for errors and warnings that
  occur during tuning. When a tuning process encounters issues, see the
  new `trace` column in the `collect_notes(.Last.tune.result)` output to
  find precisely where the error occurred
  ([\#873](https://github.com/tidymodels/tune/issues/873)).

- Postprocessors can now be tuned. Currently, we support the tailor
  package.

### Parallel Processing

- Introduced support for parallel processing with mirai in addition to
  the currently supported framework future. See
  [`?parallelism`](https://tune.tidymodels.org/dev/reference/parallelism.md)
  to learn more
  ([\#1028](https://github.com/tidymodels/tune/issues/1028)).

- Sequential and parallel processing all use the same L’Ecuyer-CMRG
  seeds (conditional on `parallel_over`)
  ([\#1033](https://github.com/tidymodels/tune/issues/1033)).

### Breaking Changes

- The `foreach` package is no longer supported. Instead, use the future
  or mirai packages.

- The parallel backend(s) and the methods of constructing seeds for
  workers have changed. There will be a lack of reproducibility between
  objects created in this version of tune and previous versions.

### Other Changes

- [`int_pctl()`](https://rsample.tidymodels.org/reference/int_pctl.html)
  now includes an option (`keep_replicates`) to retain the individual
  bootstrap estimates. It also processes the resamples more efficiently
  ([\#1000](https://github.com/tidymodels/tune/issues/1000)).

- A [`min_grid()`](https://generics.r-lib.org/reference/min_grid.html)
  methods was added for `proportional_hazards` models so that their
  submodels are processed appropriately.

- Post-processing: new
  [`schedule_grid()`](https://tune.tidymodels.org/dev/reference/schedule_grid.md)
  for scheduling a grid including post-processing
  ([\#988](https://github.com/tidymodels/tune/issues/988)).

- Removed functions deprecated since tune version 1.6.0 (circa
  2021-07-21).

## tune 1.3.0

CRAN release: 2025-02-21

- The package will now warn when parallel processing has been enabled
  with foreach but not with future. See
  [`?parallelism`](https://tune.tidymodels.org/dev/reference/parallelism.html)
  to learn more about transitioning your code to future
  ([\#878](https://github.com/tidymodels/tune/issues/878),
  [\#866](https://github.com/tidymodels/tune/issues/866)). The next
  version of tune will move to a pure future implementation.

- When automatic grids are used,
  [`dials::grid_space_filling()`](https://dials.tidymodels.org/reference/grid_space_filling.html)
  is now used (instead of
  [`dials::grid_latin_hypercube()`](https://dials.tidymodels.org/reference/grid_max_entropy.html)).
  Overall, the new function produces optimized designs (not depending on
  random numbers). When using Bayesian models, we will use a Latin
  Hypercube since we produce 5,000 candidates, which is too slow to do
  with pre-optimized designs.

## tune 1.2.1

CRAN release: 2024-04-18

- Addressed issue in
  [`int_pctl()`](https://rsample.tidymodels.org/reference/int_pctl.html)
  where the function would error when parallelized using
  `makePSOCKcluster()`
  ([\#885](https://github.com/tidymodels/tune/issues/885)).

- Addressed issue where tuning functions would raise the error
  `object 'iteration' not found` with `plan(multisession)` and the
  control option `parallel_over = "everything"`
  ([\#888](https://github.com/tidymodels/tune/issues/888)).

## tune 1.2.0

CRAN release: 2024-03-20

### New Features

- tune now fully supports models in the “censored regression” mode.
  These models can be fit, tuned, and evaluated like the regression and
  classification modes.
  [tidymodels.org](https://www.tidymodels.org/learn/#category=survival%20analysis)
  has more information and tutorials on how to work with survival
  analysis models.

- Introduced support for parallel processing using the
  [future](https://www.futureverse.org/) framework. The tune package
  previously supported parallelism with foreach, and users can use
  either framework for now. In a future release, tune will begin the
  deprecation cycle for parallelism with foreach, so we encourage users
  to begin migrating their code now. See [the *Parallel Processing*
  section in the “Optimizations”
  article](https://tune.tidymodels.org/articles/extras/optimizations.html#parallel-processing)
  to learn more
  ([\#866](https://github.com/tidymodels/tune/issues/866)).

- Added a `type` argument to
  [`collect_metrics()`](https://tune.tidymodels.org/dev/reference/collect_predictions.md)
  to indicate the desired output format. The default, `type = "long"`,
  returns output as before, while `type = "wide"` pivots the output such
  that each metric has its own column
  ([\#839](https://github.com/tidymodels/tune/issues/839)).

- Added a new function,
  [`compute_metrics()`](https://tune.tidymodels.org/dev/reference/compute_metrics.md),
  that allows for computing new metrics after evaluating against
  resamples. The arguments and output formats are closely related to
  those from
  [`collect_metrics()`](https://tune.tidymodels.org/dev/reference/collect_predictions.md),
  but this function requires that the input be generated with the
  control option `save_pred = TRUE` and additionally takes a `metrics`
  argument with a metric set for new metrics to compute. This allows for
  computing new performance metrics without requiring users to re-fit
  and re-predict from each model
  ([\#663](https://github.com/tidymodels/tune/issues/663)).

- A method for rsample’s
  [`int_pctl()`](https://rsample.tidymodels.org/reference/int_pctl.html)
  function that will compute percentile confidence intervals on
  performance metrics for objects produced by
  [`fit_resamples()`](https://tune.tidymodels.org/dev/reference/fit_resamples.md),
  `tune_*()`, and
  [`last_fit()`](https://tune.tidymodels.org/dev/reference/last_fit.md).

- The Brier score is now part of the default metric set for
  classification models.

### Bug Fixes

- [`last_fit()`](https://tune.tidymodels.org/dev/reference/last_fit.md)
  will now error when supplied a fitted workflow
  ([\#678](https://github.com/tidymodels/tune/issues/678)).

- Fixes bug where `.notes` entries were sorted in the wrong order in
  tuning results for resampling schemes with IDs that aren’t already in
  alphabetical order
  ([\#728](https://github.com/tidymodels/tune/issues/728)).

- Fixes bug where `.config` entries in the `.extracts` column in
  [`tune_bayes()`](https://tune.tidymodels.org/dev/reference/tune_bayes.md)
  output didn’t align with the entries they ought to in the `.metrics`
  and `.predictions` columns
  ([\#715](https://github.com/tidymodels/tune/issues/715)).

- Metrics from apparent resamples are no longer included when estimating
  performance with
  [`estimate_tune_results()`](https://tune.tidymodels.org/dev/reference/empty_ellipses.md)
  (and thus with `collect_metrics(..., summarize = TRUE)` and
  `compute_metrics(..., summarize = TRUE)`,
  [\#714](https://github.com/tidymodels/tune/issues/714)).

- Handles edge cases for
  [`tune_bayes()`](https://tune.tidymodels.org/dev/reference/tune_bayes.md)’
  `iter` argument more soundly. For `iter = 0`, the output of
  [`tune_bayes()`](https://tune.tidymodels.org/dev/reference/tune_bayes.md)
  should match
  [`tune_grid()`](https://tune.tidymodels.org/dev/reference/tune_grid.md),
  and
  [`tune_bayes()`](https://tune.tidymodels.org/dev/reference/tune_bayes.md)
  will now error when `iter < 0`.
  [`tune_bayes()`](https://tune.tidymodels.org/dev/reference/tune_bayes.md)
  will now alter the state of RNG slightly differently, resulting in
  changed Bayesian optimization search output
  ([\#720](https://github.com/tidymodels/tune/issues/720)).

- [`augment()`](https://generics.r-lib.org/reference/augment.html)
  methods to `tune_results`, `resample_results`, and `last_fit` objects
  now always return tibbles
  ([\#759](https://github.com/tidymodels/tune/issues/759)).

### Other Changes

- Improved error message when needed packages aren’t installed
  ([\#727](https://github.com/tidymodels/tune/issues/727)).

- [`augment()`](https://generics.r-lib.org/reference/augment.html)
  methods to `tune_results`, `resample_results`, and `last_fit` objects
  now always returns tibbles
  ([\#759](https://github.com/tidymodels/tune/issues/759)).

- Improves documentation related to the hyperparameters associated with
  extracted objects that are generated from submodels. See the
  “Extracting with submodels” section of
  [`?collect_extracts`](https://tune.tidymodels.org/dev/reference/collect_predictions.md)
  to learn more.

- `eval_time` and `eval_time_target` attribute was added to tune
  objects. There are also
  [`.get_tune_eval_times()`](https://tune.tidymodels.org/dev/reference/tune_accessor.md)
  and
  [`.get_tune_eval_time_target()`](https://tune.tidymodels.org/dev/reference/tune_accessor.md)
  functions.

- [`collect_predictions()`](https://tune.tidymodels.org/dev/reference/collect_predictions.md)
  now reorders the columns so that all prediction columns come first
  ([\#798](https://github.com/tidymodels/tune/issues/798)).

- [`augment()`](https://generics.r-lib.org/reference/augment.html)
  methods to `tune_results`, `resample_results`, and `last_fit` objects
  now return prediction results in the first columns
  ([\#761](https://github.com/tidymodels/tune/issues/761)).

- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  will now meaningfully error if only 1 grid point is present, rather
  than producing a plot
  ([\#775](https://github.com/tidymodels/tune/issues/775)).

- Added notes on case weight usage to several functions
  ([\#805](https://github.com/tidymodels/tune/issues/805)).

- For iterative optimization routines,
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  will use integer breaks when `type = "performance"` or
  `type = "parameters"`.

### Breaking Changes

- Several functions gained an `eval_time` argument for the evaluation
  time of dynamic metrics for censored regression. The placement of the
  argument breaks passing-by-position for one or more other arguments to
  [`autoplot.tune_results()`](https://tune.tidymodels.org/dev/reference/autoplot.tune_results.md)
  and the developer-focused
  [`check_initial()`](https://tune.tidymodels.org/dev/reference/empty_ellipses.md)
  ([\#857](https://github.com/tidymodels/tune/issues/857)).

- Ellipses (…) are now used consistently in the package to require
  optional arguments to be named. For functions that previously had
  ellipses at the end of the function signature, they have been moved to
  follow the last argument without a default value: this applies to
  [`augment.tune_results()`](https://tune.tidymodels.org/dev/reference/augment.tune_results.md),
  [`collect_predictions.tune_results()`](https://tune.tidymodels.org/dev/reference/collect_predictions.md),
  [`collect_metrics.tune_results()`](https://tune.tidymodels.org/dev/reference/collect_predictions.md),
  [`select_best.tune_results()`](https://tune.tidymodels.org/dev/reference/show_best.md),
  [`show_best.tune_results()`](https://tune.tidymodels.org/dev/reference/show_best.md),
  and the developer-focused
  [`estimate_tune_results()`](https://tune.tidymodels.org/dev/reference/empty_ellipses.md),
  [`load_pkgs()`](https://tune.tidymodels.org/dev/reference/load_pkgs.md),
  and
  [`encode_set()`](https://tune.tidymodels.org/dev/reference/empty_ellipses.md).
  Several other functions that previously did not have ellipses in their
  signatures gained them: this applies to
  [`conf_mat_resampled()`](https://tune.tidymodels.org/dev/reference/conf_mat_resampled.md)
  and the developer-focused
  [`check_workflow()`](https://tune.tidymodels.org/dev/reference/empty_ellipses.md).
  Optional arguments previously passed by position will now error
  informatively prompting them to be named. These changes don’t apply in
  cases when the ellipses are currently in use to forward arguments to
  other functions
  ([\#863](https://github.com/tidymodels/tune/issues/863)).

## tune 1.1.2

CRAN release: 2023-08-23

- [`last_fit()`](https://tune.tidymodels.org/dev/reference/last_fit.md)
  now works with the 3-way validation split objects from
  [`rsample::initial_validation_split()`](https://rsample.tidymodels.org/reference/initial_validation_split.html).
  [`last_fit()`](https://tune.tidymodels.org/dev/reference/last_fit.md)
  and
  [`fit_best()`](https://tune.tidymodels.org/dev/reference/fit_best.md)
  now have a new argument `add_validation_set` to include or exclude the
  validation set in the dataset used to fit the model
  ([\#701](https://github.com/tidymodels/tune/issues/701)).

- Disambiguates the `verbose` and `verbose_iter` control options to
  better align with documented functionality. The former controls
  logging for general progress updates, while the latter only does so
  for the Bayesian search process.
  ([\#682](https://github.com/tidymodels/tune/issues/682))

## tune 1.1.1

CRAN release: 2023-04-11

- Fixed a bug introduced in tune 1.1.0 in `collect_()` functions where
  the `.iter` column was dropped.

## tune 1.1.0

CRAN release: 2023-04-04

tune 1.1.0 introduces a number of new features and bug fixes,
accompanied by various optimizations that substantially decrease the
total evaluation time to tune hyperparameters in the tidymodels.

### New features

- Introduced a new function
  [`fit_best()`](https://tune.tidymodels.org/dev/reference/fit_best.md)
  that provides a shorthand interface to fit a final model after
  parameter tuning.
  ([\#586](https://github.com/tidymodels/tune/issues/586))

- Refined machinery for logging issues during tuning. Rather than
  printing out warnings and errors as they appear, the package will now
  only print unique tuning issues, updating a dynamic summary message
  that maintains counts of each unique issue. This feature is only
  enabled for tuning sequentially and can be manually toggled with the
  `verbose` option.
  ([\#588](https://github.com/tidymodels/tune/issues/588))

- Introduced
  [`collect_extracts()`](https://tune.tidymodels.org/dev/reference/collect_predictions.md),
  a function for collecting extracted objects from tuning results. The
  format of results closely mirrors
  [`collect_notes()`](https://tune.tidymodels.org/dev/reference/collect_predictions.md),
  where the extracted objects are contained in a list-column alongside
  the resample ID and workflow `.config`.
  ([\#579](https://github.com/tidymodels/tune/issues/579))

### Bug fixes

- Fixed bug in
  [`select_by_pct_loss()`](https://tune.tidymodels.org/dev/reference/show_best.md)
  where the model with the greatest loss within the limit was returned
  rather than the most simple model whose loss was within the limit.
  ([\#543](https://github.com/tidymodels/tune/issues/543))

- Fixed bug in
  [`tune_bayes()`](https://tune.tidymodels.org/dev/reference/tune_bayes.md)
  where `.Last.tune.result` would return intermediate tuning results.
  ([\#613](https://github.com/tidymodels/tune/issues/613))

- Extended
  [`show_best()`](https://tune.tidymodels.org/dev/reference/show_best.md),
  [`select_best()`](https://tune.tidymodels.org/dev/reference/show_best.md),
  `select_by_one_std_error()`,
  [`select_by_pct_loss()`](https://tune.tidymodels.org/dev/reference/show_best.md)
  to accommodate metrics with a target value of zero (notably,
  [`yardstick::mpe()`](https://yardstick.tidymodels.org/reference/mpe.html)
  and
  [`yardstick::msd()`](https://yardstick.tidymodels.org/reference/msd.html)).
  ([\#243](https://github.com/tidymodels/tune/issues/243))

### Other changes

- Implemented various optimizations in tune’s backend that
  [substantially decrease the total evaluation
  time](https://www.simonpcouch.com/blog/speedups-2023/#tidymodels) to
  tune hyperparameters with the tidymodels.
  ([\#634](https://github.com/tidymodels/tune/issues/634),
  [\#635](https://github.com/tidymodels/tune/issues/635),
  [\#636](https://github.com/tidymodels/tune/issues/636),
  [\#637](https://github.com/tidymodels/tune/issues/637),
  [\#640](https://github.com/tidymodels/tune/issues/640),
  [\#641](https://github.com/tidymodels/tune/issues/641),
  [\#642](https://github.com/tidymodels/tune/issues/642),
  [\#648](https://github.com/tidymodels/tune/issues/648),
  [\#649](https://github.com/tidymodels/tune/issues/649),
  [\#653](https://github.com/tidymodels/tune/issues/653),
  [\#656](https://github.com/tidymodels/tune/issues/656),
  [\#657](https://github.com/tidymodels/tune/issues/657))

- Allowed users to supply list-columns in `grid` arguments. This change
  allows for manually specifying grid values that must be contained in
  list-columns, like functions or lists.
  ([\#625](https://github.com/tidymodels/tune/issues/625))

- Clarified error messages in `select_by_*` functions. Error messages
  now only note entries in `...` that are likely candidates for failure
  to [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html),
  and those error messages are no longer duplicated for each entry in
  `...`.

- Improved condition handling for errors that occur during extraction
  from workflows. While messages and warnings were appropriately
  handled, errors occurring due to misspecified `extract()` functions
  being supplied to `control_*()` functions were silently caught. As
  with warnings, errors are now surfaced both during execution and at
  [`print()`](https://rdrr.io/r/base/print.html)
  ([\#575](https://github.com/tidymodels/tune/issues/575)).

- Moved forward with the deprecation of
  [`parameters()`](https://dials.tidymodels.org/reference/parameters.html)
  methods for `workflow`s, `model_spec`s, and `recipes`. Each of these
  methods will now warn on every usage and will be defunct in a later
  release of the package.
  ([\#650](https://github.com/tidymodels/tune/issues/650))

- Various bug fixes and improvements to documentation.

## tune 1.0.1

CRAN release: 2022-10-09

- [`last_fit()`](https://tune.tidymodels.org/dev/reference/last_fit.md),
  [`fit_resamples()`](https://tune.tidymodels.org/dev/reference/fit_resamples.md),
  [`tune_grid()`](https://tune.tidymodels.org/dev/reference/tune_grid.md),
  and
  [`tune_bayes()`](https://tune.tidymodels.org/dev/reference/tune_bayes.md)
  do not automatically error if the wrong type of `control` object is
  passed. If the passed control object is not a superset of the one that
  is needed, the function will still error. As an example, passing
  [`control_grid()`](https://tune.tidymodels.org/dev/reference/control_grid.md)
  to
  [`tune_bayes()`](https://tune.tidymodels.org/dev/reference/tune_bayes.md)
  will fail but passing
  [`control_bayes()`](https://tune.tidymodels.org/dev/reference/control_bayes.md)
  to
  [`tune_grid()`](https://tune.tidymodels.org/dev/reference/tune_grid.md)
  will not. ([\#449](https://github.com/tidymodels/tune/issues/449))

- The
  [`collect_metrics()`](https://tune.tidymodels.org/dev/reference/collect_predictions.md)
  method for racing objects was removed (and is now in the finetune
  package).

- Improved prompts related to parameter tuning. When tuning parameters
  are supplied that are not compatible with the given engine, `tune_*()`
  functions will now error.
  ([\#549](https://github.com/tidymodels/tune/issues/549))

- The
  [`control_bayes()`](https://tune.tidymodels.org/dev/reference/control_bayes.md)
  got a new argument `verbose_iter` that is used to control the
  verbosity of the Bayesian calculations. This change means that the
  `verbose` argument is being passed to
  [`tune_grid()`](https://tune.tidymodels.org/dev/reference/tune_grid.md)
  to control its verbosity.

- The
  [`control_last_fit()`](https://tune.tidymodels.org/dev/reference/control_last_fit.md)
  function gained an argument `allow_par` that defaults to `FALSE`. This
  change addresses failures after
  [`last_fit()`](https://tune.tidymodels.org/dev/reference/last_fit.md)
  using modeling engines that require native serialization, and we
  anticipate little to no increase in time-to-fit resulting from this
  change. ([\#539](https://github.com/tidymodels/tune/issues/539),
  tidymodels/bonsai#52)

- [`show_notes()`](https://tune.tidymodels.org/dev/reference/show_notes.md)
  does a better jobs of… showing notes.
  ([\#558](https://github.com/tidymodels/tune/issues/558))

## tune 1.0.0

CRAN release: 2022-07-07

- [`show_notes()`](https://tune.tidymodels.org/dev/reference/show_notes.md)
  is a new function that can better help understand warnings and errors.

- Logging that occurs using the tuning and resampling functions now show
  multi-line error messages and warnings in multiple lines.

- When
  [`fit_resamples()`](https://tune.tidymodels.org/dev/reference/fit_resamples.md),
  [`last_fit()`](https://tune.tidymodels.org/dev/reference/last_fit.md),
  [`tune_grid()`](https://tune.tidymodels.org/dev/reference/tune_grid.md),
  or
  [`tune_bayes()`](https://tune.tidymodels.org/dev/reference/tune_bayes.md)
  complete without error (even if models fail), the results are *also*
  available via `.Last.tune.result`.

- [`last_fit()`](https://tune.tidymodels.org/dev/reference/last_fit.md)
  now accepts a `control` argument to allow users to control aspects of
  the last fitting process via
  [`control_last_fit()`](https://tune.tidymodels.org/dev/reference/control_last_fit.md)
  ([\#399](https://github.com/tidymodels/tune/issues/399)).

- Case weights are enabled for models that can use them.

- Some internal functions were exported for use by other packages.

- A check was added to
  [`fit_resamples()`](https://tune.tidymodels.org/dev/reference/fit_resamples.md)
  and
  [`last_fit()`](https://tune.tidymodels.org/dev/reference/last_fit.md)
  to give a more informative error message when a preprocessor or model
  have parameters marked for tuning.

- [`outcome_names()`](https://tune.tidymodels.org/dev/reference/outcome_names.md)
  works correctly when recipe has NA roles.
  ([\#518](https://github.com/tidymodels/tune/issues/518))

## tune 0.2.0

CRAN release: 2022-03-18

- The `.notes` column now contains information on the type of note
  (error or warning), the location where it occurred, and the note.
  Printing a tune result has different output describing the notes.

- [`collect_notes()`](https://tune.tidymodels.org/dev/reference/collect_predictions.md)
  can be used to gather any notes to a tibble.
  ([\#363](https://github.com/tidymodels/tune/issues/363))

- Parallel processing with PSOCK clusters is now more efficient, due to
  carefully avoiding sending extraneous information to each worker
  ([\#384](https://github.com/tidymodels/tune/issues/384),
  [\#396](https://github.com/tidymodels/tune/issues/396)).

- The engine arguments for xgboost `alpha`, `lambda`, and
  `scale_pos_weight` are now tunable.

- When the Bayesian optimization data contain missing values, these are
  removed before fitting the GP model. If all metrics are missing, no GP
  is fit and the current results are returned.
  ([\#432](https://github.com/tidymodels/tune/issues/432))

- Moved [`tune()`](https://hardhat.tidymodels.org/reference/tune.html)
  from tune to hardhat
  ([\#442](https://github.com/tidymodels/tune/issues/442)).

- The
  [`parameters()`](https://dials.tidymodels.org/reference/parameters.html)
  methods for `recipe`, `model_spec`, and `workflow` objects have been
  soft-deprecated in favor of
  [`extract_parameter_set_dials()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  methods ([\#428](https://github.com/tidymodels/tune/issues/428)).

## tune 0.1.6

CRAN release: 2021-07-21

- When using
  [`load_pkgs()`](https://tune.tidymodels.org/dev/reference/load_pkgs.md),
  packages that use random numbers on start-up do not affect the state
  of the RNG. We also added more control of the RNGkind to make it
  consistent with the user’s previous value
  ([\#389](https://github.com/tidymodels/tune/issues/389)).

- New `extract_*()` functions have been added that supersede many of the
  the existing `pull_*()` functions. This is part of a larger move
  across the tidymodels packages towards a family of generic
  `extract_*()` functions. Many `pull_*()` functions have been
  soft-deprecated, and will eventually be removed.
  ([\#378](https://github.com/tidymodels/tune/issues/378))

## tune 0.1.5

CRAN release: 2021-04-23

- Fixed a bug where the resampled confusion matrix is transposed when
  `conf_mat_resamped(tidy = FALSE)`
  ([\#372](https://github.com/tidymodels/tune/issues/372))

- False positive warnings no longer occur when using the `doFuture`
  package for parallel processing
  ([\#377](https://github.com/tidymodels/tune/issues/377))

## tune 0.1.4

CRAN release: 2021-04-20

- Fixed an issue in
  [`finalize_recipe()`](https://tune.tidymodels.org/dev/reference/finalize_model.md)
  which failed during tuning of recipe steps that contain multiple
  [`tune()`](https://hardhat.tidymodels.org/reference/tune.html)
  parameters in an single step.

- Changed
  [`conf_mat_resampled()`](https://tune.tidymodels.org/dev/reference/conf_mat_resampled.md)
  to return the same type of object as
  [`yardstick::conf_mat()`](https://yardstick.tidymodels.org/reference/conf_mat.html)
  when `tidy = FALSE`
  ([\#370](https://github.com/tidymodels/tune/issues/370)).

- The automatic parameter machinery for `sample_size` with the C5.0
  engine was changes to use
  [`dials::sample_prop()`](https://dials.tidymodels.org/reference/trees.html).

## tune 0.1.3

CRAN release: 2021-02-28

- The `rsample::pretty()` methods were extended to `tune_results`
  objects.

- Added `pillar` methods for formatting `tune` objects in list columns.

- A method for
  [`.get_fingerprint()`](https://rsample.tidymodels.org/reference/get_fingerprint.html)
  was added. This helps determine if `tune` objects used the same
  resamples.

## tune 0.1.2

CRAN release: 2020-11-17

- [`collect_predictions()`](https://tune.tidymodels.org/dev/reference/collect_predictions.md)
  was made generic.

- The default tuning parameter for the SVM polynomial degree was
  switched from
  [`dials::degree()`](https://dials.tidymodels.org/reference/degree.html)
  to
  [`dials::prod_degree()`](https://dials.tidymodels.org/reference/degree.html)
  since it must be an integer.

### Bug Fixes

- [`last_fit()`](https://tune.tidymodels.org/dev/reference/last_fit.md)
  and
  [`workflows::fit()`](https://generics.r-lib.org/reference/fit.html)
  will now give identical results for the same workflow when the
  underlying model uses random number generation
  ([\#300](https://github.com/tidymodels/tune/issues/300)).

- Fixed an issue where recipe tuning parameters could be randomly
  matched to the tuning grid incorrectly
  ([\#316](https://github.com/tidymodels/tune/issues/316)).

- [`last_fit()`](https://tune.tidymodels.org/dev/reference/last_fit.md)
  no longer accidentally adjusts the random seed
  ([\#264](https://github.com/tidymodels/tune/issues/264)).

- Fixed two bugs in the acquisition function calculations.

### Other Changes

- New `parallel_over` control argument to adjust the parallel processing
  method that tune uses.

- The `.config` column that appears in the returned tibble from tuning
  and fitting resamples has changed slightly. It is now always of the
  form `"Preprocessor<i>_Model<j>"`.

- [`predict()`](https://rdrr.io/r/stats/predict.html) can now be called
  on the workflow returned from
  [`last_fit()`](https://tune.tidymodels.org/dev/reference/last_fit.md)
  ([\#294](https://github.com/tidymodels/tune/issues/294),
  [\#295](https://github.com/tidymodels/tune/issues/295),
  [\#296](https://github.com/tidymodels/tune/issues/296)).

- tune now supports setting the `event_level` option from yardstick
  through the control objects
  (i.e. `control_grid(event_level = "second")`)
  ([\#240](https://github.com/tidymodels/tune/issues/240),
  [\#249](https://github.com/tidymodels/tune/issues/249)).

- tune now supports workflows created with the new
  [`workflows::add_variables()`](https://workflows.tidymodels.org/reference/add_variables.html)
  preprocessor.

- Better control the random number streams in parallel for
  [`tune_grid()`](https://tune.tidymodels.org/dev/reference/tune_grid.md)
  and
  [`fit_resamples()`](https://tune.tidymodels.org/dev/reference/fit_resamples.md)
  ([\#11](https://github.com/tidymodels/tune/issues/11))

- Allow `...` to pass options from
  [`tune_bayes()`](https://tune.tidymodels.org/dev/reference/tune_bayes.md)
  to [`GPfit::GP_fit()`](https://rdrr.io/pkg/GPfit/man/GP_fit.html).

- Additional checks are done for the initial grid that is given to
  [`tune_bayes()`](https://tune.tidymodels.org/dev/reference/tune_bayes.md).
  If the initial grid is small relative to the number of model terms, a
  warning is issued. If the grid is a single point, an error occurs.
  ([\#269](https://github.com/tidymodels/tune/issues/269))

- Formatting of some messages created by
  [`tune_bayes()`](https://tune.tidymodels.org/dev/reference/tune_bayes.md)
  now respect the width and wrap lines using the new
  [`message_wrap()`](https://tune.tidymodels.org/dev/reference/message_wrap.md)
  function.

- tune functions
  ([`tune_grid()`](https://tune.tidymodels.org/dev/reference/tune_grid.md),
  [`tune_bayes()`](https://tune.tidymodels.org/dev/reference/tune_bayes.md),
  etc) will now error if a model specification or model workflow are
  given as the first argument (the soft deprecation period is over).

- An [`augment()`](https://generics.r-lib.org/reference/augment.html)
  method was added for objects generated by `tune_*()`,
  [`fit_resamples()`](https://tune.tidymodels.org/dev/reference/fit_resamples.md),
  and
  [`last_fit()`](https://tune.tidymodels.org/dev/reference/last_fit.md).

## tune 0.1.1

CRAN release: 2020-07-08

### Breaking Changes

- [`autoplot.tune_results()`](https://tune.tidymodels.org/dev/reference/autoplot.tune_results.md)
  now requires objects made by version 0.1.0 or higher of tune.

- `tune` objects no longer keep the `rset` class that they have from the
  `resamples` argument.

### Other Changes

- [`autoplot.tune_results()`](https://tune.tidymodels.org/dev/reference/autoplot.tune_results.md)
  now produces a different plot when the tuning grid is a regular grid
  (i.e. factorial or nearly factorial in nature). If there are 5+
  parameters, the standard plot is produced. Non-regular grids are
  plotted in the same way (although see next bullet point). See
  [`?autoplot.tune_results`](https://tune.tidymodels.org/dev/reference/autoplot.tune_results.md)
  for more information.

- [`autoplot.tune_results()`](https://tune.tidymodels.org/dev/reference/autoplot.tune_results.md)
  now transforms the parameter values for the plot. For example, if the
  `penalty` parameter was used for a regularized regression, the points
  are plotted on the log-10 scale (its default transformation). For
  non-regular grids, the facet labels show the transformation type
  (e.g. `"penalty (log-10)"` or `"cost (log-2)"`). For regular grid, the
  x-axis is scaled using
  [`scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html).

- Finally,
  [`autoplot.tune_results()`](https://tune.tidymodels.org/dev/reference/autoplot.tune_results.md)
  now shows the parameter *labels* in a plot. For example, if a
  k-nearest neighbors model was used with `neighbors = tune()`, the
  parameter will be labeled as `"# Nearest Neighbors"`. When an ID was
  used, such as `neighbors = tune("K")`, this is used to identify the
  parameter.

- In other plotting news,
  [`coord_obs_pred()`](https://tune.tidymodels.org/dev/reference/coord_obs_pred.md)
  has been included for regression models. When plotting the observed
  and predicted values from a model, this forces the x- and y-axis to be
  the same range and uses an aspect ratio of 1.

- The outcome names are saved in an attribute called `outcomes` to
  objects with class `tune_results`. Also, several accessor functions
  (named \`.get_tune\_\*()) were added to more easily access such
  attributes.

- [`conf_mat_resampled()`](https://tune.tidymodels.org/dev/reference/conf_mat_resampled.md)
  computes the average confusion matrix across resampling statistics for
  a single model.

- [`show_best()`](https://tune.tidymodels.org/dev/reference/show_best.md),
  and the `select_*()` functions will now use the first metric in the
  metric set if no metric is supplied.

- [`filter_parameters()`](https://tune.tidymodels.org/dev/reference/filter_parameters.md)
  can trim the `.metrics` column of unwanted results (as well as columns
  `.predictions` and `.extracts`) from `tune_*` objects.

- In concert with `dials` \> 0.0.7, tuning engine-specific arguments is
  possible. Many known engine-specific tuning parameters and handled
  automatically.

- If a grid is given, parameters do not need to be finalized to be used
  in the `tune_*()` functions.

- Added a `save_workflow` argument to `control_*` functions that will
  result in the workflow object used to carry out tuning/fitting
  (regardless of whether a formula or recipe was given as input to the
  function) to be appended to the resulting `tune_results` object in a
  `workflow` attribute. The new
  [`.get_tune_workflow()`](https://tune.tidymodels.org/dev/reference/tune_accessor.md)
  function can be used to access the workflow.

- Many of the output columns in a `tune_results` object have an
  additional column called `.config`. This is meant to be a unique,
  qualitative value that used for sorting and merging. These values also
  correspond to the messages in the logging produced when
  `verbose = TRUE`.

## tune 0.1.0

CRAN release: 2020-04-02

### Breaking Changes

- The arguments to the main tuning/fitting functions
  ([`tune_grid()`](https://tune.tidymodels.org/dev/reference/tune_grid.md),
  [`tune_bayes()`](https://tune.tidymodels.org/dev/reference/tune_bayes.md),
  etc) have been reordered to better align with parsnip’s
  [`fit()`](https://generics.r-lib.org/reference/fit.html). The first
  argument to all these functions is now a model specification or model
  workflow. The previous versions are soft-deprecated as of 0.1.0 and
  will be deprecated as of 0.1.2.

### Other Changes

- Added more packages to be fully loaded in the workers when run in
  parallel using `doParallel`
  ([\#157](https://github.com/tidymodels/tune/issues/157)),
  ([\#159](https://github.com/tidymodels/tune/issues/159)), and
  ([\#160](https://github.com/tidymodels/tune/issues/160))

- [`collect_predictions()`](https://tune.tidymodels.org/dev/reference/collect_predictions.md)
  gains two new arguments. `parameters` allows for pre-filtering of the
  hold-out predictions by tuning parameters values. If you are only
  interested in one sub-model, this makes things much faster. The other
  option is `summarize` and is used when the resampling method has
  training set rows that are predicted in multiple holdout sets.

- [`select_best()`](https://tune.tidymodels.org/dev/reference/show_best.md),
  [`select_by_one_std_err()`](https://tune.tidymodels.org/dev/reference/show_best.md),
  and
  [`select_by_pct_loss()`](https://tune.tidymodels.org/dev/reference/show_best.md)
  no longer have a redundant `maximize` argument
  ([\#176](https://github.com/tidymodels/tune/issues/176)). Each metric
  set in yardstick now has a direction (maximize vs. minimize) built in.

### Bug Fixes

- [`tune_bayes()`](https://tune.tidymodels.org/dev/reference/tune_bayes.md)
  no longer errors with a recipe, which has tuning parameters, in
  combination with a parameter set, where the defaults contain unknown
  values ([\#168](https://github.com/tidymodels/tune/issues/168)).

## tune 0.0.1

CRAN release: 2020-02-11

- CRAN release.

- Changed license to MIT

## tune 0.0.0.9002

- The `...` arguments of
  [`tune_grid()`](https://tune.tidymodels.org/dev/reference/tune_grid.md)
  and
  [`tune_bayes()`](https://tune.tidymodels.org/dev/reference/tune_bayes.md)
  have been moved forward to force optional arguments to be named.

- New
  [`fit_resamples()`](https://tune.tidymodels.org/dev/reference/fit_resamples.md)
  for fitting a set of resamples that don’t require any tuning.

- Changed `summarise.tune_results()` back to `estimate.tune_results()`

## tune 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.
