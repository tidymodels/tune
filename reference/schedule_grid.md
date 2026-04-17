# Schedule a grid

Schedule a grid

## Usage

``` r
schedule_grid(grid, wflow)
```

## Arguments

- grid:

  A tibble containing the parameter grid.

- wflow:

  The workflow object for which we schedule the grid.

## Value

A schedule object, inheriting from either 'single_schedule',
'grid_schedule', or 'resample_schedule'.
