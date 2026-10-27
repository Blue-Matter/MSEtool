# Sensitivity analysis

A function that determines the inputs for a given data-limited method of
class Output and then analyses the sensitivity of TAC estimates to
marginal differences in each input. The range used for sensitivity is
based on the user-specified CV for that input (e.g. CV_Mort, Mort)

## Usage

``` r
Sense(Data, MP, nsense = 6, reps = 100, perc = c(0.05, 0.5, 0.95), ploty = T)
```

## Arguments

- Data:

  A data-limited methods data object

- MP:

  A character string representing an MP applied in calculating the TAC
  recommendations in the DLM object

- nsense:

  The number of points over which to calculate the TAC (resolution)

- reps:

  The number of samples of the quota taken for the calculation of the
  TAC

- perc:

  The percentile of the sample TAC

- ploty:

  A logical switch, (T/F, should a plot be drawn?)

## Author

T. Carruthers

## Examples

``` r
if (FALSE) { # \dontrun{
Data <- Sense(MSEtool::Cobia, "AvC")
} # }
```
