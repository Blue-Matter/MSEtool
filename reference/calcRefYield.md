# Calculate Reference Yield

Calculate Reference Yield

## Usage

``` r
calcRefYield(x, StockPars, FleetPars, pyears, Ncurr, nyears, proyears)
```

## Arguments

- x:

  Integer, the simulation number

- StockPars:

  List of Stock Parameters

- FleetPars:

  List of Fleet Parameters

- pyears:

  The number of years to project forward. Equal to 'nyears' for
  optimizing for q.

- Ncurr:

  Array with current numbers-at-age (dim=c(nsim, maxage+1, nareas))

- nyears:

  Number of historical years

- proyears:

  Number of projection years

## Author

A. Hordyk
