<!-- README.md is generated from README.Rmd. Please edit that file -->

# tune

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/tidymodels/tune.svg?branch=master)](https://travis-ci.org/tidymodels/tune)
[![R build status](https://github.com/tidymodels/tune/workflows/R/badge.svg)](https://github.com/tidymodels/tune/actions?workflow=R-full)
[![Codecov test coverage](https://codecov.io/gh/tidymodels/tune/branch/master/graph/badge.svg)](https://codecov.io/gh/tidymodels/tune?branch=master)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of tune is to facilitate the tuning of hyper-parameters the tidymodels packages. It replies heavily on `recipes`, `parsnip`, and `dials`. 

## Installation

You can install the current development version using

```r
devtools::install_github("tidymodels/tune")
```

## Examples

There are a few package vignettes, the first being the [_Getting Started_](https://tidymodels.github.io/tune/articles/getting_started.html) document. Other examples resources:

 - [basic grid search](https://tidymodels.github.io/tune/articles/grid.html)
 - [Bayesian optimization example](https://tidymodels.github.io/tune/articles/extras/svm_classification.html)
 - [an advanced text mining example](https://tidymodels.github.io/tune/articles/extras/text_analysis.html)
 - [notes on optimizations and parallel processing](https://tidymodels.github.io/tune/articles/extras/optimizations.html)
 - [details on acquisition function for scoring parameter combinations](https://tidymodels.github.io/tune/articles/acquisition_functions.html)
 
## Important Notes

This is the development version. While the APIs are pretty stable they are subject to change due to user feedback and other issues. Please give as much feedback as possible regarding the good and bad parts. 

There are references to _workflow_ objects in `tune`. We have another package being developed called `workflows` that will streamline some of the `tidymodels` APIs. The functions in this package related to workflows don't do much right now and are placeholders for future functions. 

In this version, you can jointly tune pre-processing parameters (via a recipe) and model parameters. Once `workflows` is released, you can also optimize post-processing parameters at the same time (e.g. the probability threshold for two-class problems). 

 
