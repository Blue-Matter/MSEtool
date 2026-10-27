# Apply Management Procedures to an object of class Data

Apply Management Procedures to an object of class Data

## Usage

``` r
applyMP(
  Data,
  MPs = NA,
  reps = 100,
  nsims = NA,
  silent = FALSE,
  parallel = .sfIsRunning()
)
```

## Arguments

- Data:

  An object of class Data

- MPs:

  Name(s) of the MPs to run

- reps:

  Number of samples

- nsims:

  Optional. Number of simulations.

- silent:

  Logical. Should messages be suppressed?

- parallel:

  Logical. Whether to run MPs in parallel. Can be a vector of
  length(MPs)

## Value

A list with the first element a list of management recommendations, and
the second the updated Data object
