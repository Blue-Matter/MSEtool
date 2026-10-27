# Blow parallel optimization function

Find the current biomass at which it would take HZN mean generation
times to reach Bfrac x SSBMSY biomass level given zero catches

## Usage

``` r
getBlow(
  x,
  N,
  Asize,
  SSBMSY,
  SSBpR,
  MPA,
  SSB0,
  nareas,
  retA,
  MGThorizon,
  Find,
  Perr,
  M_ageArray,
  hs,
  Mat_age,
  Wt_age,
  Fec_age,
  R0a,
  V,
  nyears,
  maxage,
  mov,
  Spat_targ,
  SRrel,
  aR,
  bR,
  Bfrac = 0.5,
  maxF,
  ploty = F,
  plusgroup = 0,
  SRRfun,
  SRRpars,
  spawn_time_frac
)
```

## Arguments

- x:

  position in a vector

- N:

  array of numbers at age by sim and area (dims nsim, maxage, nyears,
  nareas)

- Asize:

  matrix of area size (nsim by nareas)

- SSBMSY:

  vector nsim long of spawning biomass at MSY

- SSBpR:

  matrix nsim by nareas with SSB per recruit

- MPA:

  matrix nyears + proyears by nareas of spatial closures

- SSB0:

  vector of SSB0 length nsim

- nareas:

  numeric. number of areas

- retA:

  array of retention nsim x maxage x nyears

- MGThorizon:

  vector nsim long of MGT x HZN

- Find:

  matrix of fishing mortality rate nsim x nyears

- Perr:

  matrix of recruitment deviations nsim x nyears + maxage -1

- M_ageArray:

  array of natural mortality rate nsim x maxage x nyears + proyears

- hs:

  vector nsim long of steepness values

- Mat_age:

  array nsim x nages x nyears+proyears of maturity at age

- Wt_age:

  matrix nsim x nages of weight at age

- R0a:

  matrix nsim by nareas of unfished recruitment

- V:

  array of vulnerability nsim x maxage x nyears

- nyears:

  integer: number of historical years

- maxage:

  integer: maximum age

- mov:

  array of movement nsim x 2 x 2

- Spat_targ:

  vector of spatial targeting parameters

- SRrel:

  integer representing recruitment dynamics type 1: Bev Holt 2: Ricker

- aR:

  vector of recruitment parameters

- bR:

  vector of recruitment parameters

- Bfrac:

  fraction of SSBMSY that is the target

- maxF:

  maximum apical fishing mortality

- ploty:

  logical: should a plot be produced

- plusgroup:

  Integer. Default = 0 = no plus-group. Use 1 to include a plus-group

- spawn_time_frac:

  Numeric. Fraction of the year when spawning occurs. Default = 0.

## Author

T. Carruthers with modifications by A. Hordyk
