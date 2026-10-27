# Optimize for catchability (q) and fishing dist for a MICE model

Function optimizes catchability (q, where F=qE) required to get to
user-specified stock depletion across stocks and fleets if there are
relationships among stocks

## Usage

``` r
getq_multi_MICE(
  x,
  StockPars,
  FleetPars,
  np,
  nf,
  nareas,
  maxage,
  nyears,
  N,
  VF,
  FretA,
  maxF = 0.9,
  MPA,
  CatchFrac,
  bounds = c(1e-05, 15),
  tol = 1e-06,
  Rel,
  SexPars,
  plusgroup,
  optVB = FALSE,
  silent = FALSE
)
```

## Arguments

- x:

  Integer, the simulation number

- StockPars:

  A list of sampled stock parameters, one list element per stock

- FleetPars:

  A hierarchical list of sampled fleet parameters, first list level is
  stock, second is fleet

- np:

  The number of stocks

- nf:

  The number of fleets

- nareas:

  The number of areas

- maxage:

  The maximum number of modeled ages

- nyears:

  The number of historical 'spool-up' years (from unfished to now)

- N:

  An array of stock numbers `[nsim,np,maxage,nyears,nareas]` - only the
  values from the first year are used

- VF:

  An array of vulnerability `[nsim,np,nf,maxage,nyears+proyears]`

- FretA:

  An array of retention `[nsim,np,nf,maxage,nyears+proyears]`

- maxF:

  A numeric value specifying the maximum fishing mortality for any
  single age class

- MPA:

  An array of spatial closures by year `[np,nf,nyears+proyears,nareas]`

- CatchFrac:

  A list of stock-specific fleet fractions of current catch
  list`[[stock]][nsim, nf]`

- bounds:

  Bounds for total q estimation

- tol:

  A numeric value that is the fraction of machine tolerance (once
  reduction in objective function steps below this, optimization ends)

- Rel:

  A list of inter-stock relationships see slot Rel of MOM object class

- SexPars:

  A list of sex-specific dynamics SSBfrom stock_age

- optVB:

  Logical, whether to optimize to vulnerable biomass (or spawning
  biomass otherwise)

- silent:

  Logical, whether to report the objective function for each simulation

## Author

T.Carruthers
