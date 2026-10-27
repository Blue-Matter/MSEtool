# Internal function for optimizing catchability (q) for a MICE model

Function returns objective function that fits both stock depletion and
catch fraction among fleets

## Usage

``` r
qestMICE(
  par,
  depc,
  CFc,
  mode = "opt",
  np,
  nf,
  nyears,
  nareas,
  maxage,
  Nx,
  VFx,
  FretAx,
  Effind,
  distx,
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
  SSB0x,
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
  optVB,
  VB0x,
  B0x,
  WtCx,
  maxF,
  MPA,
  SRRfun,
  SRRpars,
  spawn_time_frac = rep(0, np)
)
```

## Arguments

- par:

  Integer, the simulation number

- depc:

  Numeric vector, nstock long of specified stock depletion (SSB now /
  SSB0)

- CFc:

  Matrix `[nstock, nfleet]`, a catch fraction among fleets (sums to 1
  for each stock (row))

- mode:

  Character if 'opt' qestMICE returns the objective function otherwise
  the fitted values in a list

- nf:

  Integer, number of fleets

- nyears:

  Integer, number of historical years (unfished til today)

- nareas:

  Integer, number of areas (default is 2)

- maxage:

  Integer, maximum number of age classes for calculation

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

  Array `[stock, age, year]` of mature spawning weight at age

- Asizex:

  Matrix `[stock, area]` Area size

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

- SSB0x:

  Vector `[stock]` Unfished spawning stock biomass

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

  A list of sex-specific dynamics (SSBfrom, stcck_age)

- x:

  Integer. The simulation number

- plusgroup:

  Integer vector `[stock]` indicating if a plus group is used

- optVB:

  Logical, whether to optimize to vulnerable biomass (or spawning
  biomass otherwise)

- VB0x:

  Vector `[stock]` unfished vulnerable biomass

- B0x:

  Vector `[stock]` unfished total biomass

- WtCx:

  Array `[stock, fleet, n_age]` of weight at age in catch in the last
  historical year

- maxF:

  A numeric value specifying the maximum fishing mortality for any
  single age class

- MPA:

  An array of spatial closures by year `[np,nf,nyears+proyears,nareas]`

- SRRfun:

  Optional. Stock-recruit functions used if `SRrelc =3`. List of length
  `np`

- SRRpars:

  Optional. A named list of arguments for `SRRfun`. List of length `np`

- spawn_time_frac:

  Numeric. Fraction of the year when spawning occurs. Default = 0.

## Author

T.Carruthers
