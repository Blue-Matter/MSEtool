# Optimize yield for a single simulation

Optimize yield for a single simulation

## Usage

``` r
optYield(
  logFa,
  Asize_c,
  nareas,
  maxage,
  Ncurr,
  pyears,
  M_age,
  MatAge,
  WtAge,
  FecAge,
  WtAgeC,
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
  Qc,
  MPA,
  maxF,
  SSB0c,
  plusgroup = 0,
  SRRfun,
  SRRpars,
  spawn_time_frac = 0
)
```

## Arguments

- logFa:

  log apical fishing mortality

- Asize_c:

  A vector of length areas with relative size of areas

- nareas:

  Number of area

- maxage:

  Maximum age

- Ncurr:

  Current N-at-age

- pyears:

  Number of projection years

- M_age:

  M-at-age

- MatAge:

  Maturity-at-age

- WtAge:

  Weight-at-age (stock)

- FecAge:

  Mature-weight-at-age

- WtAgeC:

  Weight-at-age (fishery)

- Vuln:

  Vulnerability-at-age

- Retc:

  Retention-at-age

- Prec:

  Recruitment error

- movc:

  Movement matrix

- SRrelc:

  SR Relationship

- Effind:

  Historical effort

- Spat_targc:

  Spatial targeting

- hc:

  Steepness

- R0c:

  Unfished recruitment by area

- SSBpRc:

  Unfished spawning stock per recruit by area

- aRc:

  Ricker aR

- bRc:

  Ricker bR

- Qc:

  Catchability

- MPA:

  A matrix of spatial closures by year

- maxF:

  A numeric value specifying the maximum fishing mortality for any
  single age class

- SSB0c:

  SSB0

- plusgroup:

  Integer. Default = 0 = no plus-group. Use 1 to include a plus-group

- spawn_time_frac:

  Numeric. Fraction of the year when spawning occurs. Default = 0.

## Author

A. Hordyk
