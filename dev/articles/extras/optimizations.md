# Optimizations and parallel processing

The tune package attempts to evaluate the candidate models in the
shortest amount of time. This vignette is a summary of those approaches.

## Sub-model speed-ups

For some types of models, such as boosted models or regularized models,
the number of models that are actually *fit* can be far less than the
number of models evaluated. For example, suppose a boosted tree is fit
with 1000 trees. Many boosting implementations let the user make
predictions for any number of trees less than what was originally fit
(1000 in this example). This “sub-model trick” can greatly speed up the
training time for many models (e.g. see [this
example](https://topepo.github.io/caret/using-your-own-model-in-train.html#illustrative-example-2-something-more-complicated---logitboost)
in the `caret` documentation).

In order to know what models allow this, the parsnip package contains a
[`multi_predict()`](https://parsnip.tidymodels.org/reference/multi_predict.html)
function that enables this feature. Printing the S3 methods for it lists
the possible models:

``` r
library(tidymodels)
methods("multi_predict")
```

    ##  [1] multi_predict._C5.0*        multi_predict._earth*      
    ##  [3] multi_predict._elnet*       multi_predict._glmnetfit*  
    ##  [5] multi_predict._lognet*      multi_predict._multnet*    
    ##  [7] multi_predict._torch_mlp*   multi_predict._train.kknn* 
    ##  [9] multi_predict._xgb.Booster* multi_predict.default*     
    ## see '?methods' for accessing help and source code

``` r
# There are arguments for the parameter(s) that can create multiple predictions.
# For xgboost, `trees` are cheap to evaluate: 
parsnip:::multi_predict._xgb.Booster |> 
  formals() |> 
  names()
```

    ## [1] "object"   "new_data" "type"     "trees"    "..."

The same feature does not exist for recipes though.

## Expensive pre-processing

When tuning a recipe and a model, it makes sense to avoid *recreating*
the recipe for each model.

For example, suppose that [Isomap multi-dimensional
scaling](https://en.wikipedia.org/wiki/Isomap) is used to pre-process
the data prior to tuning a K-nearest neighbor regression:

``` r
data(Chicago)
iso_rec <- 
  recipe(ridership ~ ., data = Chicago) |> 
  step_dummy(all_nominal()) |> 
  step_isomap(all_predictors(), num_terms = tune())
```

    ## 3 packages (dimRed, RSpectra, and RANN) are needed for this step but
    ## are not installed.

    ## To install run: `install.packages(c("dimRed", "RSpectra", "RANN"))`

``` r
knn_mod <- 
  nearest_neighbor(neighbors = tune(), weight_func = tune()) |> 
  set_engine("kknn") |> 
  set_mode("regression")
```

With the following grid:

``` r
grid <- 
  parameters(num_terms(c(1, 9)), neighbors(), weight_func()) |> 
  grid_regular(levels = c(5, 10, 7)) |> 
  arrange(num_terms, neighbors, weight_func)
grid 
```

    ## # A tibble: 350 × 3
    ##    num_terms neighbors weight_func 
    ##        <int>     <int> <chr>       
    ##  1         1         1 biweight    
    ##  2         1         1 cos         
    ##  3         1         1 epanechnikov
    ##  4         1         1 inv         
    ##  5         1         1 rectangular 
    ##  6         1         1 triangular  
    ##  7         1         1 triweight   
    ##  8         1         2 biweight    
    ##  9         1         2 cos         
    ## 10         1         2 epanechnikov
    ## # ℹ 340 more rows

To evaluate these 350 candidate models, we would have to compute the
same recipe 70 times *per resample*. Since Isomap is expensive, this is
really inefficient.

[`tune_grid()`](https://tune.tidymodels.org/dev/reference/tune_grid.md)
determines when this occurs and fits all 70 candidate models for each
unique configuration of the recipe. In essence, it nests the model
parameters inside the unique parameters of the recipe:

    ## # A tibble: 5 × 2
    ##   num_terms data             
    ##       <int> <list>           
    ## 1         1 <tibble [70 × 2]>
    ## 2         3 <tibble [70 × 2]>
    ## 3         5 <tibble [70 × 2]>
    ## 4         7 <tibble [70 × 2]>
    ## 5         9 <tibble [70 × 2]>

When `parallel_over = "resamples"`, the default, only 5 recipes are
prepared and, within each, all of the appropriate models are fit from
the same recipe. In this example, once the recipe with `num_terms = 1`
is created, the model parameters are iteratively tuned:

    ## # A tibble: 70 × 2
    ##    neighbors weight_func 
    ##        <int> <chr>       
    ##  1         1 biweight    
    ##  2         1 cos         
    ##  3         1 epanechnikov
    ##  4         1 inv         
    ##  5         1 rectangular 
    ##  6         1 triangular  
    ##  7         1 triweight   
    ##  8         2 biweight    
    ##  9         2 cos         
    ## 10         2 epanechnikov
    ## # ℹ 60 more rows

The same will be true for post-processing parameters being tuned. For
each unique set of recipe and model parameters, the post-processing
parameters will be evaluated without unnecessary re-fitting.

Also, when using a model formula, the model matrix is only created once
per resample.

## Parallel processing

The tune package allows users, when possible, to use multiple cores or
separate machines to fit models. The package is currently able to
parallelize over either the resampling loop of grid search (via
`parallel_over = "resamples"` in
[`control_grid()`](https://tune.tidymodels.org/dev/reference/control_grid.md),
the default) or both the resampling and preprocessing loops (via
`parallel_over = "everything"`). When `parallel_over = "everything"`, an
outer parallel loop will iterate over resamples and an inner parallel
loop will iterate over all unique combinations of preprocessor and model
tuning parameters for that specific resample. This will result in the
preprocessor being re-processed multiple times, but can be faster if
that preprocessing is extremely fast.

tune supports parallel processing using the
[future](https://www.futureverse.org/) and
[mirai](https://mirai.r-lib.org/) frameworks. future supports a variety
of technologies to share the computations and the choice of technology
is determined by the chosen *plan*. To run tuning code in parallel, just
provide a [`plan()`](https://future.futureverse.org/reference/plan.html)
and tune will take care of the rest. For example:

``` r
library(future)
plan("multisession")
```

For mirai, you can set the number of worker processes via

``` r
library(mirai)
daemons(4)
```

For either parallel frameworks, you don’t have to change your tidymodels
code.

### foreach (legacy)

Before the 1.2.0 release, tune supported parallelism using the foreach
framework. Support has been deprecated for backends registered for
foreach and will be fully removed in an upcoming release. foreach can
use a variety of technologies to share the computations and the choice
of technology is determined by which *parallel backend* package is
chosen (aka the `do{technology}` packages). For example, the doMC
package uses the forking mechanism on Unix-like systems to split the
computations across multiple cores. As of this writing, the backend
packages are doMC, doMPI, doParallel, doRedis, doSNOW, and
doAzureParallel (GitHub only).

Registering a parallel backend is also somewhat dependent of the
package. For doParallel, one could use:

``` r
all_cores <- parallel::detectCores(logical = FALSE)

library(doParallel)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)
```

To **transition from foreach to future**, remove lines loading any
foreach backend packages `library(do*)` as well as lines registering
that technology with `registerDo*()`, and add the following lines:

``` r
library(future)
plan(multisession)
```

Switch out `multisession` for another strategy if desired.

### In practice

One downside to parallel processing is that the different technologies
handle inputs and outputs differently. For example, multicore forking
*tends* to carry the loaded packages and objects into the worker
processes. Others do not. To make sure that the correct packages are
loaded (but not attached) in the workers is to use the `pkg` option in
[`control_grid()`](https://tune.tidymodels.org/dev/reference/control_grid.md).

Some helpful advice to avoid errors in parallel processing is to not use
variables in the global environment. These may not be found when the
code is run inside of a worker process. For example:

``` r
num_pcs <- 3

recipe(mpg ~ ., data = mtcars) |> 
  # Bad since num_pcs might not be found by a worker process
  step_pca(all_predictors(), num_comp = num_pcs)

recipe(mpg ~ ., data = mtcars) |> 
  # Good since the value is injected into the object
  step_pca(all_predictors(), num_comp = !!num_pcs)
```

This issue is likely to occur if
[`dplyr::one_of()`](https://tidyselect.r-lib.org/reference/one_of.html)
is used as a sector.

Also note that almost all of the logging provided by
[`tune_grid()`](https://tune.tidymodels.org/dev/reference/tune_grid.md)
will not be seen when running in parallel. Again, this is dependent on
the backend package and technology being used.
