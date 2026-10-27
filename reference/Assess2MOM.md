# Reads bootstrap estimates from a stock assessment model into a multi-fleet operating model.

A function that develops a multiple fleet operating model
([MOM](https://msetool.openmse.com/reference/MOM-class.md)) and either
models a unisex or 2-sex stock from arrays of abundance, fishing
mortality, and biological parameters. The user still needs to
parameterize most of the observation and implementation portions of the
operating model.

## Usage

``` r
Assess2MOM(
  Name = "MOM created by Assess2MOM",
  proyears = 50,
  interval = 2,
  CurrentYr = as.numeric(format(Sys.Date(), "%Y")),
  h = 0.999,
  Obs = MSEtool::Imprecise_Unbiased,
  Imp = MSEtool::Perfect_Imp,
  naa,
  faa,
  waa,
  Mataa,
  Maa,
  laa,
  fecaa,
  nyr_par_mu = 3,
  LowerTri = 1,
  recind = 0,
  plusgroup = TRUE,
  altinit = 0,
  fixq1 = TRUE,
  report = FALSE,
  silent = FALSE,
  ...
)
```

## Arguments

- Name:

  Character string. The name of the multi-OM.

- proyears:

  Positive integer. The number of projection years for MSE.

- interval:

  Positive integer. The interval at which management procedures will
  update the management advice in
  [multiMSE](https://msetool.openmse.com/reference/multiMSE.md), e.g., 1
  = annual updates.

- CurrentYr:

  Positive integer. The current year (e.g., final year of fitting to
  data)

- h:

  The steepness of the stock-recruitment curve. Either a single numeric
  or a length nsim vector.

- Obs:

  Either a single observation model to be used for all sexes and
  populations (class
  [Obs](https://msetool.openmse.com/reference/Obs-class.md)), or a list
  where `Obs[[f]]` is the
  [Obs](https://msetool.openmse.com/reference/Obs-class.md) object for
  fleet `f` (identical between sexes).

- Imp:

  Either a single implementation model to be used for all sexes and
  populations (class
  [Imp](https://msetool.openmse.com/reference/Imp-class.md)), or a list
  where `Imp[[f]]` is the
  [Obs](https://msetool.openmse.com/reference/Obs-class.md) object for
  fleet `f` (identical between sexes).

- naa:

  Numbers-at-age by sex `[first age is age zero]`. Four-dimensional
  numeric array `[sim, ages, year, p]`. `[p]` indexes the population,
  where `[p = 1]` for females and `[p = 2]` for males.

- faa:

  Fishing mortality rate-at-age by sex and fleet
  `[first age is age zero]`. Five-dimensional numeric array
  `[sim, ages, year, p, f]` where `[f]` indexes fishery fleet.

- waa:

  Weight-at-age `[first age is age zero]`. Four-dimensional numeric
  array `[sim, ages, year, p]`.

- Mataa:

  Maturity (spawning fraction)-at-age `[first age is age zero]`.
  Four-dimensional numeric array `[sim, ages, year, p]`.

- Maa:

  Natural mortality rate-at-age `[first age is age zero]`.
  Four-dimensional numeric array `[sim, ages, year, p]`.

- laa:

  Length-at-age `[first age is age zero]`. Four-dimensional numeric
  array `[sim, ages, year, p]`.

- fecaa:

  Fecundity at age `[first age is age zero]`. If missing, default
  fecundity is the product of maturity and weight at age.

- nyr_par_mu:

  Positive integer. The number of recent years that natural mortality,
  age vulnerability, weight, length and maturity parameters are averaged
  over for defining future projection conditions.

- LowerTri:

  Integer. The number of recent years for which model estimates of
  recruitment are ignored (not reliably estimated by the assessment)

- recind:

  Positive integer. The first age class that fish 'recruit to the
  fishery'. The default is 0 - ie the first position in the age
  dimension of naa is age zero

- plusgroup:

  Logical. Does the assessment assume that the oldest age class is a
  plusgroup?

- altinit:

  Integer. Various assumptions for how to set up the initial numbers. 0:
  standard, 1: no plus group, 2: temporary fix for MSEtool plus group
  initialization

- fixq1:

  Logical. Should q be fixed (ie assume the F-at-age array faa is
  accurate?

- report:

  Logical, if TRUE, a diagnostic will be reported showing the matching
  of the OM reconstructed numbers at age vs the assessment.

- silent:

  Whether to silence messages to the console.

- ...:

  Additional arguments (for all, either a numeric or a length nsim
  vector):

  - `SRrel` Stock-recruit relationship. (`1` for Beverton-Holt
    (default), `2` for Ricker)

  - `R0` unfished recruitment

  - `phi0` unfished spawners per recruit associated with R0 and h. With
    time-varying parameters, openMSE uses the mean phi0 in the first
    `ageM` (age of 50 percent maturity) years for the stock-recruit
    relationship. `Assess2OM` will re-calculate R0 and h in the
    operating model such that the stock-recruit `alpha` and `beta`
    parameters match values implied in the input.

  - `Perr` recruitment standard deviation (lognormal distribution) for
    sampling future recruitment

  - `AC` autocorrelation in future recruitment deviates.

## Value

An object of class
[MOM](https://msetool.openmse.com/reference/MOM-class.md).

## Details

Use a seed for the random number generator to sample future recruitment.

## See also

[SS2MOM](https://msetool.openmse.com/reference/SS2MOM.md)
[multiMSE](https://msetool.openmse.com/reference/multiMSE.md)
[Assess2OM](https://msetool.openmse.com/reference/Assess2OM.md)

## Author

Q. Huynh
