# Run a Management Procedure

Run a Management Procedure

## Usage

``` r
runMP(Data, MPs = NA, reps = 100, perc = 0.5, chkMPs = FALSE, silent = FALSE)
```

## Arguments

- Data:

  A MSEtool Data object

- MPs:

  The name of the MP to run (or a vector or names)

- reps:

  Number of repetitions

- perc:

  Percentile to summarize reps (default is median)

- chkMPs:

  Logical. Should the MPs be checked before attempting to run them?

- silent:

  Logical. Should messages by suppressed?

## Value

invisibly returns the Data object
