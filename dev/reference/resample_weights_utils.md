# Resampling weights utility functions

These are internal functions for handling variable resampling weights in
hyperparameter tuning.

## Usage

``` r
.get_resample_weights(x)

.create_weight_mapping(weights, id_names, metrics_data)

.weighted_sd(x, w)

.effective_sample_size(w)

.validate_resample_weights(weights, num_resamples)
```

## Arguments

- x:

  A tune_results object.

- weights:

  Numeric vector of weights.

- id_names:

  Character vector of ID column names.

- metrics_data:

  The metrics data frame.

- w:

  Numeric vector of weights.

- num_resamples:

  Integer number of resamples.

## Value

Various return values depending on the function.
