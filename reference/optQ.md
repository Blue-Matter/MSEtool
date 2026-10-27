# Optimize q for a single simulation

Optimize q for a single simulation

## Usage

``` r
optQ(
  logQ,
  depc,
  SSB0c,
  nareas,
  maxage,
  Ncurr,
  pyears,
  M_age,
  Asize_c,
  MatAge,
  WtAge,
  FecAge,
  Vuln,
  Retc,
  Prec,
  movc,
  SRrelc,
  Effind,
  Spat_targc,
  hc,
  R0c,
  SSBpRc,
  aRc,
  bRc,
  maxF,
  MPA,
  plusgroup,
  VB0c,
  SBMSYc,
  SRRfun,
  SRRpars,
  control,
  spawn_time_frac = 0
)
```

## Arguments

- logQ:

  log q

- depc:

  Depletion value

- SSB0c:

  Unfished spawning biomass

- nareas:

  Number of areas

- maxage:

  Maximum age

- Ncurr:

  Current N-at-age

- pyears:

  Number of years to project population dynamics

- M_age:

  M-at-age

- Asize_c:

  Numeric vector (length nareas) with size of each area

- MatAge:

  Maturity-at-age

- WtAge:

  Weight-at-age

- FecAge:

  Mature weight-at-age (relative fecundity)

- Vuln:

  Vulnerability-at-age

- Retc:

  Retention-at-age

- Prec:

  Recruitment error by year

- movc:

  movement matrix

- SRrelc:

  SR parameter

- Effind:

  Historical fishing effort

- Spat_targc:

  Spatial targeting

- hc:

  Steepness

- R0c:

  Unfished recruitment by area

- SSBpRc:

  Unfished spawning biomass per recruit by area

- aRc:

  Ricker aR

- bRc:

  Ricker bR

- maxF:

  maximum F

- MPA:

  A matrix of spatial closures by year

- plusgroup:

  Integer. Default = 0 = no plus-group. Use 1 to include a plus-group

- VB0c:

  Unfished vulnerable biomass

- SBMSYc:

  Spawning biomass at MSY for simulation x

- control:

  List. Control parameters including `optVB=TRUE` to optimize for
  vulnerable biomass instead of SSB?

- spawn_time_frac:

  Fraction of the year when spawning occurs. Default = 0.

## Author

A. Hordyk
