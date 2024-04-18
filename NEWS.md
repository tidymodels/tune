# tune 1.2.1

* Addressed issue in `int_pctl()` where the function would error when parallelized using `makePSOCKcluster()` (#885).

* Addressed issue where tuning functions would raise the error `object 'iteration' not found` with `plan(multisession)` and the control option `parallel_over = "everything"` (#888).

# tune 1.2.0

## New Features

* tune now fully supports models in the "censored regression" mode. These models can be fit, tuned, and evaluated like the regression and classification modes. [tidymodels.org](https://www.tidymodels.org/learn/#category=survival%20analysis) has more information and tutorials on how to work with survival analysis models.

* Introduced support for parallel processing using the [future](https://www.futureverse.org/) framework. The tune package previously supported parallelism with foreach, and users can use either framework for now. In a future release, tune will begin the deprecation cycle for parallelism with foreach, so we encourage users to begin migrating their code now. See [the _Parallel Processing_ section in the "Optimizations" article](https://tune.tidymodels.org/articles/extras/optimizations.html#parallel-processing) to learn more (#866).

* Added a `type` argument to `collect_metrics()` to indicate the desired output format. The default, `type = "long"`, returns output as before, while `type = "wide"` pivots the output such that each metric has its own column (#839).

* Added a new function, `compute_metrics()`, that allows for computing new metrics after evaluating against resamples. The arguments and output formats are closely related to those from `collect_metrics()`, but this function requires that the input be generated with the control option `save_pred = TRUE` and additionally takes a `metrics` argument with a metric set for new metrics to compute. This allows for computing new performance metrics without requiring users to re-fit and re-predict from each model (#663).

* A method for rsample's `int_pctl()` function that will compute percentile confidence intervals on performance metrics for objects produced by `fit_resamples()`, `tune_*()`, and `last_fit()`. 

* The Brier score is now part of the default metric set for classification models.


## Bug Fixes

* `last_fit()` will now error when supplied a fitted workflow (#678).

* Fixes bug where `.notes` entries were sorted in the wrong order in tuning results for resampling schemes with IDs that aren't already in alphabetical order (#728).

* Fixes bug where `.config` entries in the `.extracts` column in `tune_bayes()` output didn't align with the entries they ought to in the `.metrics` and `.predictions` columns (#715).

* Metrics from apparent resamples are no longer included when estimating performance with `estimate_tune_results()` (and thus with `collect_metrics(..., summarize = TRUE)` and `compute_metrics(..., summarize = TRUE)`, #714).

* Handles edge cases for `tune_bayes()`' `iter` argument more soundly. For `iter = 0`, the output of `tune_bayes()` should match `tune_grid()`, and `tune_bayes()` will now error when `iter < 0`. `tune_bayes()` will now alter the state of RNG slightly differently, resulting in changed Bayesian optimization search output (#720).

* `augment()` methods to `tune_results`, `resample_results`, and `last_fit` objects now always return tibbles (#759).

## Other Changes

* Improved error message when needed packages aren't installed (#727).

* `augment()` methods to `tune_results`, `resample_results`, and `last_fit` objects now always returns tibbles (#759).

* Improves documentation related to the hyperparameters associated with extracted objects that are generated from submodels. See the "Extracting with submodels" section of `?collect_extracts` to learn more.

* `eval_time` and `eval_time_target` attribute was added to tune objects. There are also  `.get_tune_eval_times()` and `.get_tune_eval_time_target()` functions.

* `collect_predictions()` now reorders the columns so that all prediction columns come first (#798). 

* `augment()` methods to `tune_results`, `resample_results`, and `last_fit` objects now return prediction results in the first columns (#761).

* `autoplot()` will now meaningfully error if only 1 grid point is present, rather than producing a plot (#775).

* Added notes on case weight usage to several functions (#805).

* For iterative optimization routines, `autoplot()` will use integer breaks when `type = "performance"` or `type = "parameters"`.

## Breaking Changes

* Several functions gained an `eval_time` argument for the evaluation time of dynamic metrics for censored regression. The placement of the argument breaks passing-by-position for one or more other arguments to `autoplot.tune_results()` and the developer-focused `check_initial()` (#857).

* Ellipses (...) are now used consistently in the package to require optional arguments to be named. For functions that previously had ellipses at the end of the function signature, they have been moved to follow the last argument without a default value: this applies to `augment.tune_results()`, `collect_predictions.tune_results()`, `collect_metrics.tune_results()`, `select_best.tune_results()`, `show_best.tune_results()`, and the developer-focused `estimate_tune_results()`, `load_pkgs()`, and `encode_set()`. Several other functions that previously did not have ellipses in their signatures gained them: this applies to `conf_mat_resampled()` and the developer-focused `check_workflow()`. Optional arguments previously passed by position will now error informatively prompting them to be named. These changes don't apply in cases when the ellipses are currently in use to forward arguments to other functions (#863).

# tune 1.1.2

* `last_fit()` now works with the 3-way validation split objects from `rsample::initial_validation_split()`. `last_fit()` and `fit_best()` now have a new argument `add_validation_set` to include or exclude the validation set in the dataset used to fit the model (#701).

* Disambiguates the `verbose` and `verbose_iter` control options to better align with documented functionality. The former controls logging for general progress updates, while the latter only does so for the Bayesian search process. (#682) 

# tune 1.1.1

* Fixed a bug introduced in tune 1.1.0 in `collect_()` functions where the 
  `.iter` column was dropped.

# tune 1.1.0

tune 1.1.0 introduces a number of new features and bug fixes, accompanied by various optimizations that substantially decrease the total evaluation time to tune hyperparameters in the tidymodels.

## New features

* Introduced a new function `fit_best()` that provides a shorthand interface to fit a final model after parameter tuning. (#586)

* Refined machinery for logging issues during tuning. Rather than printing out warnings and errors as they appear, the package will now only print unique tuning issues, updating a dynamic summary message that maintains counts of each unique issue. This feature is only enabled for tuning sequentially and can be manually toggled with the `verbose` option. (#588)

* Introduced `collect_extracts()`, a function for collecting extracted objects from tuning results. The format of results closely mirrors `collect_notes()`, where the extracted objects are contained in a list-column alongside the resample ID and workflow `.config`. (#579)

## Bug fixes

* Fixed bug in `select_by_pct_loss()` where the model with the greatest loss within the limit was returned rather than the most simple model whose loss was within the limit. (#543)

* Fixed bug in `tune_bayes()` where `.Last.tune.result` would return intermediate tuning results. (#613)

* Extended `show_best()`, `select_best()`, `select_by_one_std_error()`, `select_by_pct_loss()` to accommodate metrics with a target value of zero (notably, `yardstick::mpe()` and `yardstick::msd()`). (#243)

## Other changes

* Implemented various optimizations in tune's backend that [substantially decrease the total evaluation time](https://www.simonpcouch.com/blog/speedups-2023/#tidymodels) to tune hyperparameters with the tidymodels. (#634, #635, #636, #637, #640, #641, #642, #648, #649, #653, #656, #657)

* Allowed users to supply list-columns in `grid` arguments. This change allows for manually specifying grid values that must be contained in list-columns, like functions or lists. (#625)

* Clarified error messages in `select_by_*` functions. Error messages now only note entries in `...` that are likely candidates for failure to `arrange()`, and those error messages are no longer duplicated for each entry in `...`.

* Improved condition handling for errors that occur during extraction from workflows. While messages and warnings were appropriately handled, errors occurring due to misspecified `extract()` functions being supplied to `control_*()` functions were silently caught. As with warnings, errors are now surfaced both during execution and at `print()` (#575).

* Moved forward with the deprecation of `parameters()` methods for `workflow`s, `model_spec`s, and `recipes`. Each of these methods will now warn on every usage and will be defunct in a later release of the package. (#650)

* Various bug fixes and improvements to documentation.

# tune 1.0.1

* `last_fit()`, `fit_resamples()`, `tune_grid()`, and `tune_bayes()` do not automatically error if  the wrong type of `control` object is passed. If the passed control object is not a superset of the one that is needed, the function will still error. As an example, passing `control_grid()` to `tune_bayes()` will fail but passing `control_bayes()` to `tune_grid()` will not. ([#449](https://github.com/tidymodels/tune/issues/449))

* The `collect_metrics()` method for racing objects was removed (and is now in the finetune package).

* Improved prompts related to parameter tuning. When tuning parameters are supplied that are not compatible with the given engine, `tune_*()` functions will now error. (#549)

* The `control_bayes()` got a new argument `verbose_iter` that is used to control the verbosity of the Bayesian calculations. This change means that the `verbose` argument is being passed to `tune_grid()` to control its verbosity.

* The `control_last_fit()` function gained an argument `allow_par` that defaults to `FALSE`. This change addresses failures after `last_fit()` using modeling engines that require native serialization, and we anticipate little to no increase in time-to-fit resulting from this change. (#539, tidymodels/bonsai#52)

* `show_notes()` does a better jobs of... showing notes. (#558)

# tune 1.0.0

* `show_notes()` is a new function that can better help understand warnings and errors. 

* Logging that occurs using the tuning and resampling functions now show multi-line error messages and warnings in multiple lines. 

* When `fit_resamples()`, `last_fit()`, `tune_grid()`, or `tune_bayes()` complete without error (even if models fail), the results are _also_ available via `.Last.tune.result`.

* `last_fit()` now accepts a `control` argument to allow users to control aspects of the last fitting process via `control_last_fit()` (#399).

* Case weights are enabled for models that can use them. 

* Some internal functions were exported for use by other packages. 

* A check was added to `fit_resamples()` and `last_fit()` to give a more informative error message when a preprocessor or model have parameters marked for tuning. 

* `outcome_names()` works correctly when recipe has NA roles. (#518)


# tune 0.2.0

* The `.notes` column now contains information on the type of note (error or warning), the location where it occurred, and the note. Printing a tune result has different output describing the notes. 

* `collect_notes()` can be used to gather any notes to a tibble. (#363)

* Parallel processing with PSOCK clusters is now more efficient, due to carefully avoiding sending extraneous information to each worker (#384, #396).

* The engine arguments for xgboost `alpha`, `lambda`, and `scale_pos_weight` are now tunable.

* When the Bayesian optimization data contain missing values, these are removed before fitting the GP model. If all metrics are missing, no GP is fit and the current results are returned. (#432)

* Moved `tune()` from tune to hardhat (#442).

* The `parameters()` methods for `recipe`, `model_spec`, and `workflow` objects have been soft-deprecated in favor of `extract_parameter_set_dials()` methods (#428).


# tune 0.1.6

* When using `load_pkgs()`, packages that use random numbers on start-up do not affect the state of the RNG. We also added more control of the RNGkind to make it consistent with the user's previous value (#389). 

* New `extract_*()` functions have been added that supersede many of the the existing `pull_*()` functions. This is part of a larger move across the tidymodels packages towards a family of generic `extract_*()` functions. Many `pull_*()` functions have been soft-deprecated, and will eventually be removed. (#378)

# tune 0.1.5

* Fixed a bug where the resampled confusion matrix is transposed when `conf_mat_resamped(tidy = FALSE)` (#372)

* False positive warnings no longer occur when using the `doFuture` package for parallel processing (#377)

# tune 0.1.4

* Fixed an issue in `finalize_recipe()` which failed during tuning of recipe steps that contain multiple `tune()` parameters in an single step.

* Changed `conf_mat_resampled()` to return the same type of object as `yardstick::conf_mat()` when `tidy = FALSE` (#370).

* The automatic parameter machinery for `sample_size` with the C5.0 engine was changes to use `dials::sample_prop()`. 

# tune 0.1.3

* The `rsample::pretty()` methods were extended to `tune_results` objects.  

* Added `pillar` methods for formatting `tune` objects in list columns. 

* A method for `.get_fingerprint()` was added. This helps determine if `tune` objects used the same resamples. 

# tune 0.1.2

* `collect_predictions()` was made generic. 
 
* The default tuning parameter for the SVM polynomial degree was switched from `dials::degree()` to `dials::prod_degree()` since it must be an integer. 

## Bug Fixes

* `last_fit()` and `workflows::fit()` will now give identical results for the same workflow when the underlying model uses random number generation (#300).

* Fixed an issue where recipe tuning parameters could be randomly matched to the tuning grid incorrectly (#316).

* `last_fit()` no longer accidentally adjusts the random seed (#264).

* Fixed two bugs in the acquisition function calculations.

## Other Changes

* New `parallel_over` control argument to adjust the parallel processing method that tune uses.

* The `.config` column that appears in the returned tibble from tuning and fitting resamples has changed slightly. It is now always of the form `"Preprocessor<i>_Model<j>"`.

* `predict()` can now be called on the workflow returned from `last_fit()` (#294, #295, #296).

* tune now supports setting the `event_level` option from yardstick through the control objects (i.e. `control_grid(event_level = "second")`) (#240, #249).

* tune now supports workflows created with the new `workflows::add_variables()` preprocessor.

* Better control the random number streams in parallel for `tune_grid()` and `fit_resamples()` (#11)

* Allow `...` to pass options from `tune_bayes()` to `GPfit::GP_fit()`. 

* Additional checks are done for the initial grid that is given to `tune_bayes()`. If the initial grid is small relative to the number of model terms, a warning is issued. If the grid is a single point, an error occurs. (#269)

* Formatting of some messages created by `tune_bayes()` now respect the width and wrap lines using the new `message_wrap()` function. 

* tune functions (`tune_grid()`, `tune_bayes()`, etc) will now error if a model specification or model workflow are given as the first argument (the soft deprecation period is over). 

* An `augment()` method was added for objects generated by `tune_*()`, `fit_resamples()`, and `last_fit()`.  

# tune 0.1.1

## Breaking Changes

* `autoplot.tune_results()` now requires objects made by version 0.1.0 or higher of tune. 

* `tune` objects no longer keep the `rset` class that they have from the `resamples` argument. 

## Other Changes

* `autoplot.tune_results()` now produces a different plot when the tuning grid is a regular grid (i.e. factorial or nearly factorial in nature). If there are 5+ parameters, the standard plot is produced. Non-regular grids are plotted in the same way (although see next bullet point). See `?autoplot.tune_results` for more information.

* `autoplot.tune_results()` now transforms the parameter values for the plot. For example, if the `penalty` parameter was used for a regularized regression, the points are plotted on the log-10 scale (its default transformation). For non-regular grids, the facet labels show the transformation type (e.g. `"penalty (log-10)"` or `"cost (log-2)"`). For regular grid, the x-axis is scaled using `scale_x_continuous()`. 

* Finally, `autoplot.tune_results()` now shows the parameter _labels_ in a plot. For example, if a k-nearest neighbors model was used with `neighbors = tune()`, the parameter will be labeled as `"# Nearest Neighbors"`. When an ID was used, such as `neighbors = tune("K")`, this is used to identify the parameter. 

* In other plotting news, `coord_obs_pred()` has been included for regression models. When plotting the observed and predicted values from a model, this forces the x- and y-axis to be the same range and uses an aspect ratio of 1. 

* The outcome names are saved in an attribute called `outcomes` to objects with class `tune_results`. Also, several accessor functions (named `.get_tune_*()) were added to more easily access such attributes.  

* `conf_mat_resampled()` computes the average confusion matrix across resampling statistics for a single model.  

* `show_best()`, and the `select_*()` functions will now use the first metric in the metric set if no metric is supplied. 

* `filter_parameters()` can trim the `.metrics` column of unwanted results (as well as columns `.predictions` and `.extracts`) from `tune_*` objects. 

* In concert with `dials` > 0.0.7, tuning engine-specific arguments is possible. Many known engine-specific tuning parameters and handled automatically. 

* If a grid is given, parameters do not need to be finalized to be used in the `tune_*()` functions. 

* Added a `save_workflow` argument to `control_*` functions that will result in the workflow object used to carry out tuning/fitting (regardless of whether a formula or recipe was given as input to the function) to be appended to the resulting `tune_results` object in a `workflow` attribute. The new `.get_tune_workflow()` function can be used to access the workflow.

* Many of the output columns in a `tune_results` object have an additional column called `.config`. This is meant to be a unique, qualitative value that used for sorting and merging. These values also correspond to the messages in the logging produced when `verbose = TRUE`. 

# tune 0.1.0

## Breaking Changes

* The arguments to the main tuning/fitting functions (`tune_grid()`, `tune_bayes()`, etc) have been reordered to better align with parsnip's `fit()`. The first argument to all these functions is now a model specification or model workflow. The previous versions are soft-deprecated as of 0.1.0 and will be deprecated as of 0.1.2.

## Other Changes

* Added more packages to be fully loaded in the workers when run in parallel using `doParallel` (#157),  (#159), and  (#160)

* `collect_predictions()` gains two new arguments. `parameters` allows for pre-filtering of the hold-out predictions by tuning parameters values. If you are only interested in one sub-model, this makes things much faster. The other option is `summarize` and is used when the resampling method has training set rows that are predicted in multiple holdout sets.  

* `select_best()`, `select_by_one_std_err()`, and `select_by_pct_loss()` no longer have a redundant `maximize` argument (#176). Each metric set in yardstick now has a direction (maximize vs. minimize) built in.

## Bug Fixes

* `tune_bayes()` no longer errors with a recipe, which has tuning parameters, in combination with a parameter set, where the defaults contain unknown values (#168).

# tune 0.0.1

* CRAN release.

* Changed license to MIT

# tune 0.0.0.9002

* The `...` arguments of `tune_grid()` and `tune_bayes()` have been moved
  forward to force optional arguments to be named.

* New `fit_resamples()` for fitting a set of resamples that don't require any
  tuning.

* Changed `summarise.tune_results()` back to `estimate.tune_results()`

# tune 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
