# Optimize for catchability (q)

Function optimizes catchability (q, where F=qE) required to get to
user-specified stock depletion

## Usage

``` r
CalculateQ(x, StockPars, FleetPars, pyears, bounds = c(1e-05, 15), control)
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

- bounds:

  A numeric vector of length 2 with bounds for the optimizer

- control:

  List. Control parameters including `optVB=TRUE` to optimize for
  vulnerable biomass instead of SSB?

## Author

A. Hordyk
