# Apply multi Management Procedures (class MMP) to a hierarchical list of Data class objects

Apply multi Management Procedures (class MMP) to a hierarchical list of
Data class objects

## Usage

``` r
applyMMP(
  DataList,
  MP = NA,
  reps = 1,
  nsims = NA,
  silent = FALSE,
  parallel = .sfIsRunning()
)
```

## Arguments

- DataList:

  A hierarchical list of
  [Data](https://msetool.openmse.com/reference/Data-class.md) objects
  (Fleets nested in Stocks)

- MP:

  Name of the MMP to run

- reps:

  Number of samples

- nsims:

  Optional. Number of simulations.

- silent:

  Logical. Should messages be suppressed?

- parallel:

  Logical. Whether to run MPs in parallel

## Value

A hierarchical list of management recommendations (object class Rec),
Fleets nested in Stocks
