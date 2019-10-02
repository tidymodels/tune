---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->


# tune

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/tune)](https://CRAN.R-project.org/package=tune)
[![Azure pipelines build status](https://img.shields.io/azure-devops/build/topepo/tune/2)](https://dev.azure.com/topepo/tune/_build/latest?definitionId=1&branchName=master)
[![Azure pipelines test status](https://img.shields.io/azure-devops/tests/topepo/tune/2?color=brightgreen&compact_message)](https://dev.azure.com/topepo/tune/_build/latest?definitionId=1&branchName=master)
[![Azure pipelines coverage status](https://img.shields.io/azure-devops/coverage/topepo/tune/2)](https://dev.azure.com/topepo/tune/_build/latest?definitionId=1&branchName=master)
<!-- badges: end -->

The goal of tune is to facilitate the tuning of hyper-parameters the tidymodels packages. It replies heavily on `recipes`, `parsnip`, and `dials`. 

## Installation

You can install the current development version using

```r
devtools::install_github("tidymodels/tune")
```

## Examples

There are a few package vignettes, the first being the _Getting Started_ document. Other examples resources:

 - basic grid search
 - Bayesian optimization example
 - an advanced text mining example
 - notes on optimizations and parallel processing
 - details on acquisition function for scoring parameter combinations
 
## Important Notes

This is the development version. While the APIs are pretty stable they are subject to change due to user feedback and other issues. Please give as much feedback as possible regarding the good and bad parts. 

There are references to _workflow_ objects in `tune`. We have another package being developed called `workflows` that will streamline some of the `tidymodels` APIs. The functions in this package related to workflows don't do much right now and are placeholders for future functions. 

In this version, you can jointly tune pre-processing parameters (via a recipe) and model parameters. Once `workflows` is released, you can also optimize post-processing parameters at the same time (e.g. the probability threshold for two-class problems). 

 
