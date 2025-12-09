# Extract elements of `tune` objects

These functions extract various elements from a tune object. If they do
not exist yet, an error is thrown.

- [`extract_preprocessor()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the formula, recipe, or variable expressions used for
  preprocessing.

- [`extract_spec_parsnip()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the parsnip model specification.

- [`extract_fit_parsnip()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the parsnip model fit object.

- [`extract_fit_engine()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the engine specific fit embedded within a parsnip model fit.
  For example, when using
  [`parsnip::linear_reg()`](https://parsnip.tidymodels.org/reference/linear_reg.html)
  with the `"lm"` engine, this returns the underlying `lm` object.

- [`extract_mold()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the preprocessed "mold" object returned from
  [`hardhat::mold()`](https://hardhat.tidymodels.org/reference/mold.html).
  It contains information about the preprocessing, including either the
  prepped recipe, the formula terms object, or variable selectors.

- [`extract_recipe()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the recipe. The `estimated` argument specifies whether the
  fitted or original recipe is returned.

- [`extract_workflow()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the workflow object if the control option
  `save_workflow = TRUE` was used. The workflow will only have been
  estimated for objects produced by
  [`last_fit()`](https://tune.tidymodels.org/dev/reference/last_fit.md).

## Usage

``` r
# S3 method for class 'last_fit'
extract_workflow(x, ...)

# S3 method for class 'tune_results'
extract_workflow(x, ...)

# S3 method for class 'tune_results'
extract_spec_parsnip(x, ...)

# S3 method for class 'tune_results'
extract_recipe(x, ..., estimated = TRUE)

# S3 method for class 'tune_results'
extract_fit_parsnip(x, ...)

# S3 method for class 'tune_results'
extract_fit_engine(x, ...)

# S3 method for class 'tune_results'
extract_mold(x, ...)

# S3 method for class 'tune_results'
extract_preprocessor(x, ...)
```

## Arguments

- x:

  A `tune_results` object.

- ...:

  Not currently used.

- estimated:

  A logical for whether the original (unfit) recipe or the fitted recipe
  should be returned.

## Value

The extracted value from the `tune` tune_results, `x`, as described in
the description section.

## Details

These functions supersede `extract_model()`.

## Examples

``` r
# example code

library(recipes)
library(rsample)
library(parsnip)

set.seed(6735)
tr_te_split <- initial_split(mtcars)

spline_rec <- recipe(mpg ~ ., data = mtcars) |>
  step_spline_natural(disp)

lin_mod <- linear_reg() |>
  set_engine("lm")

spline_res <- last_fit(lin_mod, spline_rec, split = tr_te_split)

extract_preprocessor(spline_res)
#> 
#> ── Recipe ─────────────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Number of variables by role
#> outcome:    1
#> predictor: 10
#> 
#> ── Operations 
#> • Natural spline expansion: disp

# The `spec` is the parsnip spec before it has been fit.
# The `fit` is the fitted parsnip model.
extract_spec_parsnip(spline_res)
#> Linear Regression Model Specification (regression)
#> 
#> Computational engine: lm 
#> 
extract_fit_parsnip(spline_res)
#> parsnip model object
#> 
#> 
#> Call:
#> stats::lm(formula = ..y ~ ., data = data)
#> 
#> Coefficients:
#> (Intercept)          cyl           hp         drat           wt  
#>   72.720897    -4.748011    -0.004591    -3.009161    -3.729979  
#>        qsec           vs           am         gear         carb  
#>   -0.075139    -3.193673    -1.049969     1.365458    -0.087406  
#>     disp_01      disp_02      disp_03      disp_04      disp_05  
#>  -12.974946   -11.920106    -3.731118     1.796121    -8.190165  
#>     disp_06      disp_07      disp_08      disp_09      disp_10  
#>    6.461960    -2.387850     2.989175    15.749765     6.123262  
#> 
extract_fit_engine(spline_res)
#> 
#> Call:
#> stats::lm(formula = ..y ~ ., data = data)
#> 
#> Coefficients:
#> (Intercept)          cyl           hp         drat           wt  
#>   72.720897    -4.748011    -0.004591    -3.009161    -3.729979  
#>        qsec           vs           am         gear         carb  
#>   -0.075139    -3.193673    -1.049969     1.365458    -0.087406  
#>     disp_01      disp_02      disp_03      disp_04      disp_05  
#>  -12.974946   -11.920106    -3.731118     1.796121    -8.190165  
#>     disp_06      disp_07      disp_08      disp_09      disp_10  
#>    6.461960    -2.387850     2.989175    15.749765     6.123262  
#> 

# The mold is returned from `hardhat::mold()`, and contains the
# predictors, outcomes, and information about the preprocessing
# for use on new data at `predict()` time.
extract_mold(spline_res)
#> $predictors
#> # A tibble: 24 × 19
#>      cyl    hp  drat    wt  qsec    vs    am  gear  carb   disp_01
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>     <dbl>
#>  1     8   205  2.93  5.25  18.0     0     0     3     4 0        
#>  2     4    95  3.92  3.15  22.9     1     0     4     2 0.00304  
#>  3     6   175  3.62  2.77  15.5     0     1     5     6 0.0000580
#>  4     8   245  3.73  3.84  15.4     0     0     3     4 0        
#>  5     4    52  4.93  1.62  18.5     1     1     4     2 0.110    
#>  6     8   180  3.07  3.78  18       0     0     3     3 0        
#>  7     8   215  3     5.42  17.8     0     0     3     4 0        
#>  8     8   175  3.15  3.44  17.0     0     0     3     2 0        
#>  9     8   180  3.07  4.07  17.4     0     0     3     3 0        
#> 10     6   110  3.9   2.88  17.0     0     1     4     4 0        
#> # ℹ 14 more rows
#> # ℹ 9 more variables: disp_02 <dbl>, disp_03 <dbl>, disp_04 <dbl>,
#> #   disp_05 <dbl>, disp_06 <dbl>, disp_07 <dbl>, disp_08 <dbl>,
#> #   disp_09 <dbl>, disp_10 <dbl>
#> 
#> $outcomes
#> # A tibble: 24 × 1
#>      mpg
#>    <dbl>
#>  1  10.4
#>  2  22.8
#>  3  19.7
#>  4  13.3
#>  5  30.4
#>  6  15.2
#>  7  10.4
#>  8  18.7
#>  9  16.4
#> 10  21  
#> # ℹ 14 more rows
#> 
#> $blueprint
#> Recipe blueprint:
#> # Predictors: 10
#> # Outcomes: 1
#> Intercept: FALSE
#> Novel Levels: FALSE
#> Composition: tibble
#> 
#> 
#> $extras
#> $extras$roles
#> NULL
#> 
#> 

# A useful shortcut is to extract the fitted recipe from the workflow
extract_recipe(spline_res)
#> ── Recipe ─────────────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Number of variables by role
#> outcome:    1
#> predictor: 10
#> 
#> ── Training information 
#> Training data contained 24 data points and no incomplete rows.
#> 
#> ── Operations 
#> • Natural spline expansion: disp | Trained

# That is identical to
identical(
  extract_mold(spline_res)$blueprint$recipe,
  extract_recipe(spline_res)
)
#> [1] TRUE
```
