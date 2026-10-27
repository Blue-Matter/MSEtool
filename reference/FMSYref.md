# Reference management procedures

Several reference MPs for your operating model to use in the management
strategy evaluation. FMSYref (and related) assume perfect information
about FMSY (FMSY is taken from the operating model stored at
`Data@Misc$ReferencePoints$ByYear$FMSY`), and set an effort limit (TAE)
so that F=FMSY (or some fraction of FMSY) in each year the MP is
applied. NFref sets annual catch to zero and is used for looking at
variability in stock with no fishing.

## Usage

``` r
FMSYref(x, Data, reps = 100, plot = FALSE)

FMSYref50(x, Data, reps = 100, plot = FALSE)

FMSYref75(x, Data, reps = 100, plot = FALSE)

NFref(x, Data, reps = 100, plot = FALSE)

curEref(x, Data, reps = 100, plot = FALSE)
```

## Arguments

- x:

  A position in the data object

- Data:

  A data object

- reps:

  The number of stochastic samples of the MP recommendation(s)

- plot:

  Logical. Show the plot?

## Value

An object of class
[`Rec`](https://msetool.openmse.com/reference/Rec-class.md) with the
`TAC` slot populated with a numeric vector of length `reps`

## Details

Note that you can out-perform `FMSYref` easily. The requirement for
fixed F is actually quite strict and is by no means the upper limit in
terms of yield. Don't panic if your method beats this one for yield,
especially for short-lived species of high temporal variability in
productivity!

## Functions

- `FMSYref()`: A reference FMSY method that fishes at FMSY

- `FMSYref50()`: A reference FMSY method that fishes at 50% of FMSY

- `FMSYref75()`: A reference FMSY method that fishes at 75% of FMSY

- `NFref()`: A reference MP that sets annual catch to almost zero
  (1e-15)

- `curEref()`: A reference MP that keeps fishing effort at the level of
  the last historical year

## Required Data

See [Data](https://msetool.openmse.com/reference/Data-class.md) for
information on the `Data` object  

## Author

T. Carruthers, A. Hordyk

## Examples

``` r
FMSYref(1, MSEtool::SimulatedData, plot=TRUE)
#>   Effort 
#> 1.888558 
FMSYref50(1, MSEtool::SimulatedData, plot=TRUE)
#>    Effort 
#> 0.9442788 
FMSYref75(1, MSEtool::SimulatedData, plot=TRUE)
#>   Effort 
#> 1.416418 
NFref(1, MSEtool::SimulatedData, plot=TRUE)

#> TAC (median) 
#>        1e-15 
curEref(1, MSEtool::SimulatedData)
#> Effort 
#>      1 
```
