# Internal functions to help use parallel processing

Internal functions to help use parallel processing

## Usage

``` r
has_non_par_pkgs(object, control, verbose = FALSE)

future_installed()

mirai_installed()

get_future_workers(verbose)

get_mirai_workers(verbose)

choose_framework(
  object = NULL,
  control = NULL,
  verbose = FALSE,
  default = "mirai"
)

get_parallel_seeds(workers)

eval_mirai(.x, .f, ..., .args)

par_fns(framework)
```

## Arguments

- object:

  A workflow.

- control:

  A control object

- verbose:

  A logical for printing

- default:

  The default parallel processor.

- workers:

  The number of existing workers

- .x:

  A list.

- .f:

  A function

- ..., .args:

  Options to pass to other functions.
