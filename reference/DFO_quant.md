# Department of Fisheries and Oceans biomass quantile plot

A plot of biomass relative to BMSY quantiles over projected years

## Usage

``` r
DFO_quant(
  MSEobj,
  maxcol = 6,
  qcol = rgb(0.4, 0.8, 0.95),
  lcol = "dodgerblue4",
  curyr = 2018,
  quants = c(0.05, 0.25, 0.75, 0.95),
  addline = T,
  forreport = T
)
```

## Arguments

- MSEobj:

  An MSE object of class MSE produced by DLMtool function runMSE

- maxcol:

  Integer how many columns for panel plots?

- qcol:

  A color, the quantile coloration

- lcol:

  A color, the mean B/BMSY line

- curyr:

  The current calendar year

- quants:

  A vector 2 long for the quantiles e.g. 0.1 and 0.9 for the 10th and
  90th quantiles

- addline:

  Should two individual example simulations be added to the plot?

- forreport:

  Logical. Is it for a report? If true, one plot of six MPs in a row
  will be provided one after another.

## Author

T. Carruthers
