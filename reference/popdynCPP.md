# Population dynamics model in CPP

Project population forward pyears given current numbers-at-age and total
mortality, etc for the future years

## Usage

``` r
popdynCPP(
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
  Qc,
  Fapic,
  maxF,
  MPA,
  control,
  SSB0c,
  SRRfun,
  SRRpars,
  plusgroup = 0L,
  spawn_time_frac = 0
)
```

## Arguments

- nareas:

  The number of spatial areas

- maxage:

  The maximum age

- Ncurr:

  A numeric matrix (maxage+1, nareas) with current numbers-at-age in
  each area

- pyears:

  The number of years to project the population forward

- M_age:

  Numeric matrix (maxage+1, pyears) with natural mortality by age and
  year

- Asize_c:

  Numeric vector (length nareas) with size of each area

- MatAge:

  Numeric vector with proportion mature by age

- WtAge:

  Numeric matrix (maxage+1, pyears) with weight by age and year

- FecAge:

  Numeric matrix (maxage+1, pyears) with mature female weight by age and
  year

- Vuln:

  Numeric matrix (maxage+1, pyears) with vulnerability by age and year

- Retc:

  Numeric matrix (maxage+1, pyears) with retention by age and year

- Prec:

  Numeric vector (pyears) with recruitment error

- movc:

  Numeric array (nareas by nareas) with the movement matrix

- SRrelc:

  Integer indicating the stock-recruitment relationship to use (1 for
  Beverton-Holt, 2 for Ricker)

- Effind:

  Numeric vector (length pyears) with the fishing effort by year

- Spat_targc:

  Integer. Spatial targeting

- hc:

  Numeric. Steepness of stock-recruit relationship

- R0c:

  Numeric vector of length nareas with unfished recruitment by area

- SSBpRc:

  Numeric vector of length nareas with unfished spawning per recruit by
  area

- aRc:

  Numeric. Ricker SRR a value by area

- bRc:

  Numeric. Ricker SRR b value by area

- Qc:

  Numeric. Catchability coefficient

- Fapic:

  Numeric. Apical F value

- maxF:

  A numeric value specifying the maximum fishing mortality for any
  single age class

- MPA:

  Spatial closure by year and area

- control:

  Integer. 1 to use q and effort to calculate F, 2 to use Fapic
  (apical F) and vulnerability to calculate F.

- SRRfun:

  Optional. A stock-recruit function used if `SRrelc =3`

- SRRpars:

  Optional. A named list of arguments for `SRRfun`

- plusgroup:

  Integer. Include a plus-group (1) or not (0)?

- spawn_time_frac:

  Numeric. Fraction of the year when spawning occurs. Default = 0.

## Author

A. Hordyk
