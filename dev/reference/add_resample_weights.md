# Add resample weights to an rset object

This function allows you to specify custom weights for resamples.
Weights are automatically normalized to sum to 1.

## Usage

``` r
add_resample_weights(rset, weights)
```

## Arguments

- rset:

  An rset object from rsample.

- weights:

  A numeric vector of weights, one per resample. Weights will be
  normalized.

## Value

The rset object with weights added as an attribute.

## Details

Resampling weights are useful when assessment sets (i.e., held out data)
have different sizes or when you want to upweight certain resamples in
the evaluation. The weights are stored as an attribute and used
automatically during metric aggregation.

## See also

[`calculate_resample_weights()`](https://tune.tidymodels.org/dev/reference/calculate_resample_weights.md),
[`extract_resample_weights()`](https://tune.tidymodels.org/dev/reference/extract_resample_weights.md)

## Examples

``` r
library(rsample)
folds <- vfold_cv(mtcars, v = 3)
# Give equal weight to all folds
weighted_folds <- add_resample_weights(folds, c(1, 1, 1))
# Emphasize the first fold
weighted_folds <- add_resample_weights(folds, c(0.5, 0.25, 0.25))
```
