# Calculate resample weights from resample sizes

This convenience function calculates weights proportional to the number
of observations in each resample's analysis set. Larger resamples get
higher weights. This ensures that resamples with more data have
proportionally more influence on the final aggregated metrics.

## Usage

``` r
calculate_resample_weights(rset)
```

## Arguments

- rset:

  An rset object from rsample.

## Value

A numeric vector of weights proportional to resample sizes, normalized
to sum to 1.

## Details

This is particularly useful for time-based resamples (e.g., expanding
window CV) or stratified sampling where resamples might have slightly
different sizes, in which resamples are imbalanced.

## See also

[`add_resample_weights()`](https://tune.tidymodels.org/dev/reference/add_resample_weights.md),
[`extract_resample_weights()`](https://tune.tidymodels.org/dev/reference/extract_resample_weights.md)

## Examples

``` r
library(rsample)
folds <- vfold_cv(mtcars, v = 3)
weights <- calculate_resample_weights(folds)
weighted_folds <- add_resample_weights(folds, weights)
```
