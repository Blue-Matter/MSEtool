# Runs input control MPs on a Data object.

Function runs a MP (or MPs) of class 'Input' and returns a list: input
control recommendation(s) in element 1 and Data object in element 2.

## Usage

``` r
runInMP(Data, MPs = NA, reps = 100)
```

## Arguments

- Data:

  A object of class Data

- MPs:

  A vector of MPs of class 'Input'

- reps:

  Number of stochastic repetitions - often not used in input control
  MPs.

## Author

A. Hordyk
