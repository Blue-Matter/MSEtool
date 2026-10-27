# Population dynamics for a MICE model (single year)

Completes a single iteration of recruitment, mortality, fishing and
movement given MICE model inputs

## Usage

``` r
popdynOneMICE(
  np,
  nf,
  nareas,
  maxage,
  Ncur,
  Bcur,
  SSBcur,
  Vcur,
  FMretx,
  FMx,
  PerrYrp,
  hsx,
  aRx,
  bRx,
  movy,
  Spat_targ,
  SRrelx,
  M_agecur,
  Mat_agecur,
  Mat_agenext,
  Fec_agenext,
  Asizex,
  Kx,
  Linfx,
  t0x,
  Mx,
  R0x,
  R0ax,
  SSBpRx,
  ax,
  bx,
  Rel,
  SexPars,
  x,
  plusgroup,
  SSB0x,
  B0x,
  Len_agenext,
  Wt_agenext,
  SRRfun,
  SRRpars,
  ycur
)
```

## Arguments

- np:

  Integer, the number of stocks

- nf:

  Integer, number of fleets

- nareas:

  Integer, number of areas

- maxage:

  Integer, maximum modelled age

- Ncur:

  Array `[stock, age, area]` of stock numbers

- Bcur:

  Array `[stock, age, area]` of stock biomass

- SSBcur:

  Array `[stock, age, area]` of spawning biomass

- Vcur:

  Array `[fleet, age, area]` of the vulnerability curve

- FMretx:

  Array `[stock, fleet, age, area]` of the retention curve

- FMx:

  Array `[stock, fleet, age, area]` fishing mortality rate

- PerrYrp:

  Vector `[stock]` process error - the lognormal factor for recruitment
  strength (for next year)

- hsx:

  Vector `[stock]` steepness of the stock recruitment curve

- aRx:

  Vector `[stock]` stock recruitment parameter alpha (for Ricker curve)

- bRx:

  Vector `[stock]` stock recruitment parameter beta (for Ricker curve)

- movy:

  Array `[stock,age,area,area]` of movement transitions (for next year)

- Spat_targ:

  Matrix `[stock, fleet]` of spatial targeting parameter (0 evenly
  spatial distributed, 1 proportional to vulnerable biomass)

- SRrelx:

  Integer vector `[stock]` the form of the stock recruitment
  relationship (1 = Beverton-Holt, 2= Ricker)

- M_agecur:

  Matrix `[stock, age]` of Natural mortality rate at age

- Mat_agecur:

  Matrix `[stock, age]` of maturity (spawning fraction) at age (current
  year)

- Mat_agenext:

  Matrix `[stock, age]` of maturity (spawning fraction) at age (next
  year)

- Fec_agenext:

  Matrix `[stock, age]` of spawning weight (fecundity) at age (next
  year)

- Asizex:

  Matrix `[stock, area]` of relative area sizes

- Kx:

  Vector `[stock]` of von B growth parameter K (next year)

- Linfx:

  Vector `[stock]` of von B asymptotic length parameter Linf (next year)

- t0x:

  Vector `[stock]` of von B theoretical age at zero length (t0) (next
  year)

- Mx:

  Vector `[stock]` mature natural mortality rate (this year)

- R0x:

  Vector `[stock]` unfished recruitment

- R0ax:

  Matrix `[stock, area]` unfished recruitment by area

- SSBpRx:

  Matrix `[stock, area]` spawning biomass per recruit by area

- ax:

  Vector `[stock]` weight-length parameter a W=aL^b

- bx:

  Vector `[stock]` weight-length parameter b W=aL^b

- Rel:

  A list of inter-stock relationships see slot Rel of MOM object class

- SexPars:

  A list of sex-specific relationships (SSBfrom, stock_age)

- x:

  Integer. The simulation number

- plusgroup:

  Numeric vector. Use plus-group (1) or not (0)

- SSB0x:

  Unfished SSB0, Vector `[stock]` length.

- B0x:

  Unfished B0, Vector `[stock]` length.

- Len_agenext:

  Matrix `[stock, age]` of next year's length-at-age

- Wt_agenext:

  Matrix `[stock, age]` of next year's weight-at-age

- SRRfun:

  Optional. A stock-recruit function used if `SRrelc =3`

- SRRpars:

  Optional. A named list of arguments for `SRRfun`

- ycur:

  Integer. The current year of the simulation

## Author

T.Carruthers
