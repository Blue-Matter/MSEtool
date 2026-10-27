# Reconstruct historical dynamics

Function that reconstructs historical stock trends from fitted qs and
all other parameters including MICE components

## Usage

``` r
HistMICE(
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
  Rel,
  SexPars,
  qs,
  qfrac,
  plusgroup
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

  An of spatial closures by year `[np,nf,nyears+proyears,nareas]`

- Rel:

  A list of inter-stock relationships see slot Rel of MOM object class

- SexPars:

  A list of sex-specific relationships (SSBfrom, stock_age)

- qs:

  Vector of q values

- qfrac:

  Array of qfrac

- plusgroup:

  Plusgroup - np-length vector

## Author

T.Carruthers
