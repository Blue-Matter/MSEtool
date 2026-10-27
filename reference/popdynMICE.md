# Population dynamics for a MICE model (multiyear)

Calls popdynOneMICE iteratively to reconstruct a time series given MICE
model inputs

## Usage

``` r
popdynMICE(
  qsx,
  qfracx,
  np,
  nf,
  nyears,
  nareas,
  maxage,
  Nx,
  VFx,
  FretAx,
  Effind,
  movx,
  Spat_targ,
  M_ageArrayx,
  Mat_agex,
  Fec_agex,
  Asizex,
  WatAgex,
  Len_agex,
  Karrayx,
  Linfarrayx,
  t0arrayx,
  Marrayx,
  R0x,
  R0ax,
  SSBpRx,
  hsx,
  aRx,
  bRx,
  ax,
  bx,
  Perrx,
  SRrelx,
  Rel,
  SexPars,
  x,
  plusgroup,
  maxF,
  SSB0x,
  B0x,
  MPA,
  SRRfun,
  SRRpars,
  spawn_time_frac = rep(0, np)
)
```

## Arguments

- qsx:

  Total catchability

- qfracx:

  Vector `[fleet]`, the fraction of total qs by fleet

- np:

  Integer, the number of stocks

- nf:

  Integer, number of fleets

- nyears:

  Integer, number of historical years (unfished til today)

- nareas:

  Integer, the number of spatial areas

- maxage:

  Integer, maximum modeled age

- Nx:

  Array `[stock, age, year, area]` of stock numbers

- VFx:

  Array `[fleet, age, year, area]` of the vulnerability curve

- FretAx:

  Array `[fleet, age, year, area]` of the retention curve

- Effind:

  Array `[fleet, year]` of effort

- movx:

  Array `[stock,age,area,area]` of movement transitions

- Spat_targ:

  Matrix `[stock, fleet]` of spatial targeting parameter (0 evenly
  spatial distributed, 1 proportional to vulnerable biomass)

- M_ageArrayx:

  Array `[stock, age,year]` of Natural mortality rate at age

- Mat_agex:

  Array `[stock, age, year]` of maturity (spawning fraction) at age

- Fec_agex:

  Array `[stock, age, year]` of female spawning weight (fecundity) at
  age

- Asizex:

  Array `[stock, area]` Area size

- WatAgex:

  Array of weight-at-age

- Len_agex:

  Array of length-at-age

- Karrayx:

  Array of von B growth parameter K

- Linfarrayx:

  Array of von B asymptotic length parameter Linf

- t0arrayx:

  Array ofvon B theoretical age at zero length (t0)

- Marrayx:

  Array of mature natural mortality rate

- R0x:

  Vector `[stock]` unfished recruitment

- R0ax:

  Matrix `[stock, area]` unfished recruitment by area

- SSBpRx:

  Matrix `[stock, area]` spawning biomass per recruit by area

- hsx:

  Vector `[stock]` steepness of the stock recruitment curve

- aRx:

  Vector `[stock]` stock recruitment parameter alpha (for Ricker curve)

- bRx:

  Vector `[stock]` stock recruitment parameter beta (for Ricker curve)

- ax:

  Vector `[stock]` weight-length parameter a W=aL^b

- bx:

  Vector `[stock]` weight-length parameter b W=aL^b

- Perrx:

  Matrix `[stock, year]` process error - the lognormal factor for
  recruitment strength

- SRrelx:

  Integer vector `[stock]` the form of the stock recruitment
  relationship (1 = Beverton-Holt, 2= Ricker)

- Rel:

  A list of inter-stock relationships see slot Rel of MOM object class

- SexPars:

  A list of sex-specific relationships (SSBfrom, stock_age)

- x:

  Integer the simulation number

- plusgroup:

  Numeric vector. Use plus-group (1) or not (0)

- maxF:

  maximum F

- SSB0x:

  SSB0 for this simulation

- B0x:

  B0 for this simulation

- MPA:

  An array of spatial closures by year `[np,nf,nyears+proyears,nareas]`

- SRRfun:

  A list of custom stock-recruit functions. List of length `np`

- SRRpars:

  A list of custom stock-recruit parameters for simulation `x`. List of
  length `np`

- spawn_time_frac:

  vector of relative spawn timing. Length nstock

## Author

T.Carruthers
