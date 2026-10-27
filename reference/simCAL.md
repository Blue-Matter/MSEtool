# Simulate Catch-at-Length Data

Simulate CAL and calculate length-at-first capture (LFC), mean length
(ML), modal length (Lc), and mean length over modal length (Lbar)

## Usage

``` r
simCAL(
  nsim,
  nyears,
  maxage,
  CAL_ESS,
  CAL_nsamp,
  nCALbins,
  CAL_binsmid,
  CAL_bins,
  vn,
  retL,
  Linfarray,
  Karray,
  t0array,
  LenCV
)
```

## Arguments

- nsim:

  Number of simulations

- nyears:

  Number of years

- maxage:

  Maximum age

- CAL_ESS:

  CAA effective sample size

- CAL_nsamp:

  CAA sample size

- nCALbins:

  number of CAL bins

- CAL_binsmid:

  mid-points of CAL bins

- CAL_bins:

  Boundary of CAL bins

- vn:

  Vulnerable numbers-at-age

- retL:

  Retention at length curve

- Linfarray:

  Array of Linf values by simulation and year

- Karray:

  Array of K values by simulation and year

- t0array:

  Array of t0 values by simulation and year

- LenCV:

  CV of length-at-age#'

## Value

named list with CAL array and LFC, ML, & Lc vectors
