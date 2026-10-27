# Internal function to calculate MSY Reference Points

Internal function to calculate MSY Reference Points

## Usage

``` r
MSYCalcs(
  logF,
  M_at_Age,
  Wt_at_Age,
  Mat_at_Age,
  Fec_at_Age,
  V_at_Age,
  Wt_at_Age_C,
  maxage,
  relRfun,
  SRRpars,
  R0x = 1,
  SRrelx = 3L,
  hx = 1,
  SSBpR = 0,
  opt = 1L,
  plusgroup = 1L,
  spawn_time_frac = 0
)
```

## Arguments

- logF:

  log fishing mortality

- M_at_Age:

  Vector of M-at-age

- Wt_at_Age:

  Vector of stock weight-at-age

- Mat_at_Age:

  Vector of maturity-at-age

- Fec_at_Age:

  Vector of mature weight-at-age

- V_at_Age:

  Vector of selectivity-at-age

- Wt_at_Age_C:

  Vector of fishery weight-at-age

- maxage:

  Maximum age

- relRfun:

  Optional. A function used to calculate reference points if `SRrelc =3`

- SRRpars:

  Optional. A named list of arguments for `SRRfun`

- R0x:

  R0 for this simulation. Set = 1 if SRrelx = 4 for per-recruit
  calculations

- SRrelx:

  SRR type for this simulation. Use 4 for per-recruit calculations, i.e.
  constant recruitment.

- hx:

  numeric. Steepness value for this simulation. Not used if SRrelx = 4.

- SSBpR:

  numeric. Unfished spawners per recruit for this simulation. Not used
  if SRrelx = 4.

- opt:

  Option. 1 = return -Yield, 2= return all MSY calcs

- plusgroup:

  Integer. Default = 0 = no plus-group. Use 1 to include a plus-group

- spawn_time_frac:

  Numeric. Fraction of the year when spawning occurs. Default = 0.

## Value

See `opt`
