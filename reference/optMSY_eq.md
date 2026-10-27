# Internal wrapper function to calculate MSY reference points (now using MSYCalcs)

Internal wrapper function to calculate MSY reference points (now using
MSYCalcs)

## Usage

``` r
optMSY_eq(x, yr.ind = 1, StockPars, V, Wt_age_C)
```

## Arguments

- x:

  Simulation number

- yr.ind:

  Year index used in calculations

- StockPars:

  A list of stock parameters

- V:

  Array of selectivity-at-age

- Wt_age_C:

  Array of fishery weight at age. Optional. If missing,
  `StockPars$Wt_age` is used.

## Value

Results from `MSYCalcs`
