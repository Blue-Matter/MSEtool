# Blow internal parallel optimization function

Find the current biomass at which it would take HZN mean generation
times to reach Bfrac x SSBMSY biomass level given zero catches

## Usage

``` r
Blow_opt(
  lnq,
  N,
  Asize_c,
  SSBMSYc,
  SSBpRc,
  MPA,
  SSB0c,
  nareas,
  retAc,
  MGThorizonc,
  Fc,
  Perrc,
  Mc,
  hc,
  Mac,
  Wac,
  Fecac,
  R0c,
  Vc,
  nyears,
  maxage,
  movc,
  Spat_targc,
  SRrelc,
  aRc,
  bRc,
  Bfrac,
  maxF,
  mode = 1,
  plusgroup = 0,
  SRRfun,
  SRRpars,
  spawn_time_frac = 0
)
```

## Arguments

- lnq:

  number: estimate of log catchability

- N:

  matrix maxage by nareas with initial numbers at age

- Asize_c:

  vector length nareas with size of each area

- SSBMSYc:

  number: spawning biomass at MSY

- SSBpRc:

  vector length nareas with SSBpR by area

- MPA:

  matrix of spatial closures

- SSB0c:

  SSB0

- nareas:

  numeric: number of areas

- retAc:

  matrix maxage by nyears+proyears of retention by age and year

- MGThorizonc:

  number: MGT x HZN

- Fc:

  vector nyears long of fishing mortality rate

- Perrc:

  vector nyears+maxage-1 long of recruitment deviations

- Mc:

  matrix maxage by nyears+proyears of natural mortality rate

- hc:

  number: steepness values

- Mac:

  matrix nages by nyears+proyears of maturity at age

- Wac:

  vector nages long of weight at age

- Fecac:

  vector nages long of mature weight at age

- R0c:

  number: unfished recruitment

- Vc:

  matrix of vulnerability maxage x nyears

- nyears:

  integer: number of historical years

- maxage:

  integer: maximum age

- movc:

  matrix of movement 2 x 2

- Spat_targc:

  number: spatial targeting parameters

- SRrelc:

  integer representing recruitment dynamics type 1: Bev Holt 2: Ricker

- aRc:

  number: recruitment parameter

- bRc:

  number: recruitment parameter

- Bfrac:

  fraction of SSBMSY that is the target

- maxF:

  maximum apical fishing mortality

- mode:

  1: find Blow 2:report blow 3:plot results

- plusgroup:

  Integer. Default = 0 = no plus-group. Use 1 to include a plus-group

- spawn_time_frac:

  Numeric. Fraction of the year when spawning occurs. Default = 0.

## Author

T. Carruthers with modifications by A. Hordyk
